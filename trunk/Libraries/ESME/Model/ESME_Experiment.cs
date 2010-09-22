using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.IO;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Schema;
using System.Xml.Serialization;
using ESME.TransmissionLoss;
using HRC.Navigation;
using System.Windows.Forms;
using System.Drawing;
using ESME;

namespace ESME.Model
{
    [XmlRoot("ESME_Experiment")]
    public class ESME_Experiment
    {
        #region Public properties
        [XmlElement]
        public ExperimentInformation Information { get; set; }

        [XmlElement]
        public EnvironmentInformation Environment { get; set; }

        [XmlArray]
        public FixedSourceList FixedSources { get; set; }

        [XmlArray]
        public SpeciesList SpeciesList { get; set; }

        [XmlArray]
        public AnimatList Animats { get; set; }

        /// <summary>
        /// A list of analysis points that have been established but not yet calculated
        /// by TransmissionLossCalculator
        /// </summary>
        [XmlArray]
        public NewAnalysisPointList NewAnalysisPoints { get; set; }

        /// <summary>
        /// A list of TransmissionLossFields that are already calculated.  Read from a cache file.
        /// </summary>
        [XmlIgnore]
        public OldTransmissionLossFieldList TransmissionLossFields { get; set; }

        [XmlIgnore]
        public List<DataModel.ExperimentTreeItem> ExperimentTree { get; private set; }

        [XmlIgnore]
        public List<DisplayLayer> DisplayLayers { get; private set; }

        #endregion

        #region Private data members
        [XmlIgnore]
        private static bool PassedValidation = false;
        #endregion

        #region Constructor
        internal ESME_Experiment() { }
        #endregion

        #region Load and Save
        public static ESME_Experiment Load(string Filename)
        {
            var Assembly = System.Reflection.Assembly.GetExecutingAssembly();
            StringBuilder Schema = new StringBuilder();
            string[] SchemaResources =
            {
                "ESME.Schema.AcousticProperties.xsd",
                "ESME.Schema.AnalysisPoint.xsd",
                "ESME.Schema.BellhopRunfile.xsd",
                "ESME.Schema.EarthCoordinate.xsd",
                "ESME.Schema.ESME_Experiment.xsd",
                "ESME.Schema.SoundSource.xsd",
                "ESME.Schema.TransmissionLossField.xsd",
            };
            foreach (string Resource in SchemaResources)
            {
                using (StreamReader Reader = new StreamReader(Assembly.GetManifestResourceStream(Resource)))
                    Schema.Append(Reader.ReadToEnd());
            }
            var FileReader = new StreamReader(Filename);
            var File = FileReader.ReadToEnd();
            FileReader.Close();
            return Deserialize(File, Schema.ToString());
        }

        public void Save(string Filename)
        {
            Information.Modified = DateTime.Now;
            var FileWriter = new StreamWriter(Filename, false);
            FileWriter.Write(this.Serialize());
            FileWriter.Close();
        }
        #endregion

        #region Serialize/Deserialize
        public string Serialize()
        {
            MemoryStream ms = new MemoryStream();
            XmlSerializer serializer = new XmlSerializer(this.GetType());
            XmlWriterSettings Settings = new XmlWriterSettings
            {
                Encoding = Encoding.UTF8,
                Indent = true,
            };
            XmlWriter writer = XmlWriter.Create(ms, Settings);

            serializer.Serialize(writer, this);
            return Encoding.UTF8.GetString(ms.ToArray());
        }

        public static ESME_Experiment Deserialize(String XmlString, String SchemaXml)
        {
            StringReader schemaReader = new StringReader(SchemaXml);
            XmlSchema schema = XmlSchema.Read(schemaReader, new ValidationEventHandler(ValidationError));

            XmlReaderSettings xmlReaderSettings = new XmlReaderSettings();
            xmlReaderSettings.ValidationType = ValidationType.Schema;
            xmlReaderSettings.Schemas.Add(schema);
            xmlReaderSettings.ValidationEventHandler += new ValidationEventHandler(ValidationError);
            StringReader xmlStream = new StringReader(XmlString);
            XmlReader xmlReader = XmlReader.Create(xmlStream, xmlReaderSettings);
            PassedValidation = true;
            while (xmlReader.Read()) ; // Empty body, all we want to do is validate the XML string we're about to deserialize
            if (!PassedValidation)
                return null;

            StringReader reader = new StringReader(XmlString);
            XmlSerializer serializer = new XmlSerializer(typeof(ESME_Experiment));
            ESME_Experiment experiment = (ESME_Experiment)serializer.Deserialize(reader);
            experiment.Initialize();
            return experiment;
        }

        private static void ValidationError(object sender, ValidationEventArgs arguments)
        {
            PassedValidation = false;
            MessageBox.Show(arguments.Message, "Error loading ESME file", MessageBoxButtons.OK, MessageBoxIcon.Error);
        }

        private void Initialize()
        {
            Information.Initialize();
            NewAnalysisPoints.Initialize();
            FixedSources.Initialize(NewAnalysisPoints);
            SpeciesList.Initialize();
            Animats.Initialize(SpeciesList);
            ExperimentTree = ESME.DataModel.ExperimentTreeItem.Create(this);
            DisplayLayers = DisplayLayer.CreateDisplayLayers();
            Globals.CurrentExperimentName = Information.Name;
            Globals.CurrentLocationName = Environment.LocationName;
            Environment.Bathymetry = new ESME.Environment.Bathymetry(Globals.CurrentEnvironmentFile);
        }
        #endregion
    }
}
