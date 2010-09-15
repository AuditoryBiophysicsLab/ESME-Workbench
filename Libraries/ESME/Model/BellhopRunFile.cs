using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Schema;
using System.Xml.Serialization;
using ESME.Environment;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using HRC.Navigation;
using System.Reflection;

namespace ESME.Model
{
    [XmlRoot(
        Namespace = "http://esme.bu.edu/support/schemas/ESME_Experiment.xsd", 
        ElementName = "BellhopRunFile", 
        IsNullable = false)]
    public class BellhopRunFile
    {
        #region Private data members
        [XmlIgnore]
        private static bool PassedValidation = false;
        #endregion


        public BellhopRunFile(TransmissionLossJob transmissionLossJob,  EnvironmentInformation environmentInformation)
        {
            
        }

        public static BellhopRunFile Create(
            TransmissionLossJob transmissionLossJob,
            EnvironmentInformation environmentInformation, TransmissionLossSettings transmissionLossSettings)
        {
            int rangeCellCount = (int)Math.Round((transmissionLossJob.Radius / transmissionLossSettings.RangeCellSize)) + 1;
            
            BellhopRunFile BellhopRunFile = new BellhopRunFile
            {
                TransmissionLossJob = transmissionLossJob,
            };

            BottomProfile[] BottomProfiles = new BottomProfile[transmissionLossJob.NewAnalysisPoint.RadialCount];
            SoundSpeedProfile[] SoundSpeedProfiles = new SoundSpeedProfile[transmissionLossJob.NewAnalysisPoint.RadialCount];
            float[] bearings = new float[transmissionLossJob.NewAnalysisPoint.RadialCount];
            float MaxCalculationDepth_meters = float.MinValue;
            var bearingStep = 360.0f/transmissionLossJob.NewAnalysisPoint.RadialCount;  
            for (int i = 0; i < transmissionLossJob.NewAnalysisPoint.RadialCount; i++)
            {
                bearings[i] = bearingStep*i + transmissionLossJob.NewAnalysisPoint.RadialBearing;
                Transect curTransect = new Transect(null, transmissionLossJob.NewAnalysisPoint.Location, bearings[i], transmissionLossJob.Radius);
                BottomProfiles[i] = new BottomProfile(rangeCellCount, curTransect, environmentInformation.Bathymetry);
                MaxCalculationDepth_meters = Math.Max((float)BottomProfiles[i].MaxDepth_Meters, MaxCalculationDepth_meters);
                SoundSpeedProfiles[i] = environmentInformation.SoundSpeedField[curTransect.MidPoint];
            }

            int depthCellCount = (int)Math.Round((MaxCalculationDepth_meters / transmissionLossSettings.DepthCellSize)) + 1;
            for (int i = 0; i < transmissionLossJob.NewAnalysisPoint.RadialCount; i++)
            {
                string BellhopConfig;
                BellhopConfig = Bellhop.GetRadialConfiguration(
                    transmissionLossJob, SoundSpeedProfiles[i],
                    environmentInformation.Sediment, MaxCalculationDepth_meters,
                    rangeCellCount, depthCellCount, false, false, false, 1500);
                BellhopRunFile.BellhopRadials.Add(new BellhopRadial
                {
                    BearingFromSource_degrees = bearings[i],
                    Configuration = BellhopConfig,
                    BottomProfile = BottomProfiles[i].ToBellhopString(),
                });
            }
            return BellhopRunFile;
        }

        public TransmissionLossJob TransmissionLossJob { get; set; }
        public BellhopRadialList BellhopRadials { get; set; }
        [XmlIgnore]
        public string OriginalFilename { get; private set; }

        public BellhopRunFile()
        {
            BellhopRadials = new BellhopRadialList();
        }

        #region Load and Save
        public static BellhopRunFile Load(string Filename)
        {
            var Assembly = System.Reflection.Assembly.GetExecutingAssembly();
            StringBuilder Schema = new StringBuilder();
            string[] SchemaResources =
            {
                "ESME.Schema.EarthCoordinate.xsd",
                "ESME.Schema.AnalysisPoint.xsd",
                "ESME.Schema.AcousticProperties.xsd",
                "ESME.Schema.TransmissionLossField.xsd",
                "ESME.Schema.BellhopRunfile.xsd",
            };
            foreach (string Resource in SchemaResources)
            {
                using (StreamReader Reader = new StreamReader(Assembly.GetManifestResourceStream(Resource)))
                    Schema.Append(Reader.ReadToEnd());
            }
            var FileReader = new StreamReader(Filename);
            var File = FileReader.ReadToEnd();
            FileReader.Close();
            BellhopRunFile RunFile = Deserialize(File, Schema.ToString());
            RunFile.OriginalFilename = File;
            return RunFile;
        }

        public void Save(string Filename)
        {
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

        public static BellhopRunFile Deserialize(string XmlString, string SchemaXml)
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
            XmlSerializer serializer = new XmlSerializer(typeof(BellhopRunFile));
            BellhopRunFile runfile = (BellhopRunFile)serializer.Deserialize(reader);
            return runfile;
        }

        private static void ValidationError(object sender, ValidationEventArgs arguments)
        {
            PassedValidation = false;
        }
        #endregion
    }

    public class TransmissionLossSettings
    {
        public float RangeCellSize { get; set; }
        public float DepthCellSize { get; set; }
    }

    public class BellhopRadial
    {
        public string Base64EncodedConfiguration { get; set; }
        public string Base64EncodedBottomProfile { get; set; }
        public float BearingFromSource_degrees { get; set; }

        [XmlIgnore]
        public string Configuration
        {
            set { Base64EncodedConfiguration = ToBase64(value); }
            get { return FromBase64(Base64EncodedConfiguration); }
        }

        [XmlIgnore]
        public string BottomProfile
        {
            set { Base64EncodedBottomProfile = ToBase64(value); }
            get { return FromBase64(Base64EncodedBottomProfile); }
        }

        private string ToBase64(string sourceData)
        {
            return System.Convert.ToBase64String(System.Text.ASCIIEncoding.ASCII.GetBytes(sourceData));
        }

        private string FromBase64(string encodedData)
        {
            return System.Text.ASCIIEncoding.ASCII.GetString(System.Convert.FromBase64String(encodedData));
        }
    }

    public class BellhopRadialList : List<BellhopRadial>
    {
    }
}
