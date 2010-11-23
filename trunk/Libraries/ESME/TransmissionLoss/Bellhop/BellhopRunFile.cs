using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;
using ESME.Environment;
using ESME.Model;

namespace ESME.TransmissionLoss.Bellhop
{
    [XmlRoot(Namespace = "http://esme.bu.edu/support/schemas/ESME_Experiment.xsd", ElementName = "BellhopRunFile", IsNullable = false)]
    public class BellhopRunFile : IHasIDField
    {
        #region Private data members

        [XmlIgnore] static bool _passedValidation;

        #endregion

        private BellhopRunFile(TransmissionLossJob transmissionLossJob, EnvironmentInformation environmentInformation) { }
        private BellhopRunFile() { BellhopRadials = new BellhopRadialList(); }

        #region Load and Save

        public static BellhopRunFile Load(string filename)
        {
            var assembly = Assembly.GetExecutingAssembly();
            var schema = new StringBuilder();
            string[] schemaResources = {
                                           "ESME.Schema.EarthCoordinate.xsd", "ESME.Schema.AnalysisPoint.xsd", "ESME.Schema.AcousticProperties.xsd", "ESME.Schema.TransmissionLossField.xsd", "ESME.Schema.BellhopRunfile.xsd",
                                       };
            foreach (var resource in schemaResources.Where(resource => assembly.GetManifestResourceStream(resource) != null)) 
                using (var reader = new StreamReader(assembly.GetManifestResourceStream(resource))) 
                    schema.Append(reader.ReadToEnd());
            var fileReader = new StreamReader(filename);
            var file = fileReader.ReadToEnd();
            fileReader.Close();
            var runFile = Deserialize(file, schema.ToString());
            runFile.OriginalFilename = file;
            return runFile;
        }

        public void Save(string filename)
        {
            var fileWriter = new StreamWriter(filename, false);
            fileWriter.Write(Serialize());
            fileWriter.Close();
        }

        #endregion

        #region Serialize/Deserialize

        public string Serialize()
        {
            var ms = new MemoryStream();
            var serializer = new XmlSerializer(GetType());
            var settings = new XmlWriterSettings
                           {
                               Encoding = Encoding.UTF8,
                               Indent = true,
                           };
            var writer = XmlWriter.Create(ms, settings);

            serializer.Serialize(writer, this);
            return Encoding.UTF8.GetString(ms.ToArray());
        }

        public static BellhopRunFile Deserialize(string xmlString, string schemaXml)
        {
            if (xmlString == null) throw new ArgumentNullException("xmlString");
            var schemaReader = new StringReader(schemaXml);
            var schema = XmlSchema.Read(schemaReader, ValidationError);

            var xmlReaderSettings = new XmlReaderSettings
                                    {
                                        ValidationType = ValidationType.Schema
                                    };
            xmlReaderSettings.Schemas.Add(schema);
            xmlReaderSettings.ValidationEventHandler += ValidationError;
            var xmlStream = new StringReader(xmlString);
            var xmlReader = XmlReader.Create(xmlStream, xmlReaderSettings);
            _passedValidation = true;
            while (xmlReader.Read()) {}
            if (!_passedValidation) return null;

            var reader = new StringReader(xmlString);
            var serializer = new XmlSerializer(typeof (BellhopRunFile));
            var runfile = (BellhopRunFile) serializer.Deserialize(reader);
            return runfile;
        }

        static void ValidationError(object sender, ValidationEventArgs arguments) { _passedValidation = false; }

        #endregion

        public string Name { get; set; }
        public string Metadata { get; set; }
        public ulong IDField { get; set; }

        TransmissionLossJob _transmissionLossJob;
        public TransmissionLossJob TransmissionLossJob
        {
            get { return _transmissionLossJob; }
            set
            {
                if (value == _transmissionLossJob) return;
                _transmissionLossJob = value;
                Name = _transmissionLossJob.Name;
                Metadata = _transmissionLossJob.Metadata;
            }
        }

        public BellhopRadialList BellhopRadials { get; set; }

        [XmlIgnore]
        public string OriginalFilename { get; private set; }

        public static BellhopRunFile Create(TransmissionLossJob transmissionLossJob, EnvironmentInformation environmentInformation, TransmissionLossSettings transmissionLossSettings)
        {
            var rangeCellCount = (int) Math.Round((transmissionLossJob.Radius/transmissionLossSettings.RangeCellSize)) + 1;

            var bellhopRunFile = new BellhopRunFile
                                 {
                                     TransmissionLossJob = transmissionLossJob,
                                 };

            var bottomProfiles = new BottomProfile[transmissionLossJob.AnalysisPoint.RadialCount];
            var soundSpeedProfiles = new SoundSpeedProfile[transmissionLossJob.AnalysisPoint.RadialCount];
            var bearings = new float[transmissionLossJob.AnalysisPoint.RadialCount];
            var maxCalculationDepthMeters = float.MinValue;
            var bearingStep = 360.0f/transmissionLossJob.AnalysisPoint.RadialCount;
            for (var i = 0; i < transmissionLossJob.AnalysisPoint.RadialCount; i++)
            {
                bearings[i] = bearingStep*i + transmissionLossJob.AnalysisPoint.RadialBearing;
                var curTransect = new Transect(null, transmissionLossJob.AnalysisPoint.EarthCoordinate, bearings[i], transmissionLossJob.Radius);
                bottomProfiles[i] = new BottomProfile(rangeCellCount, curTransect, environmentInformation.Bathymetry);
                maxCalculationDepthMeters = Math.Max((float) bottomProfiles[i].MaxDepth, maxCalculationDepthMeters);
                soundSpeedProfiles[i] = environmentInformation.SoundSpeedField[curTransect.MidPoint];
            }

            var depthCellCount = (int) Math.Round((maxCalculationDepthMeters/transmissionLossSettings.DepthCellSize)) + 1;
            for (var i = 0; i < transmissionLossJob.AnalysisPoint.RadialCount; i++)
            {
                var bellhopConfig = Bellhop.GetRadialConfiguration(transmissionLossJob, soundSpeedProfiles[i], environmentInformation.Sediment, maxCalculationDepthMeters, rangeCellCount, depthCellCount, false, false, false, 1500);
                bellhopRunFile.BellhopRadials.Add(new BellhopRadial
                                                  {
                                                      BearingFromSourceDegrees = bearings[i],
                                                      Configuration = bellhopConfig,
                                                      BottomProfile = bottomProfiles[i].ToBellhopString(),
                                                  });
            }
            bellhopRunFile.IDField = transmissionLossJob.IDField;
            return bellhopRunFile;
        }
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
        public float BearingFromSourceDegrees { get; set; }

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

        static string ToBase64(string sourceData) { return Convert.ToBase64String(Encoding.ASCII.GetBytes(sourceData)); }

        static string FromBase64(string encodedData) { return Encoding.ASCII.GetString(Convert.FromBase64String(encodedData)); }
    }

    public class BellhopRadialList : List<BellhopRadial> {}
}