using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;
using ESME.Model;
using HRC.Navigation;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.TransmissionLoss
{
    [XmlRoot(Namespace = "http://esme.bu.edu/support/schemas/ESME_Experiment.xsd", IsNullable = false)]
    public class TransmissionLossFieldCache : List<TransmissionLossField>
    {
        #region Private data members

        [XmlIgnore] static bool _passedValidation;

        #endregion

        [XmlIgnore]
        public string FieldFileDirectory { get; set; }

        public event EventHandler<ProgressChangedEventArgs> ProgressChanged;

        protected virtual void OnProgressChanged(ProgressChangedEventArgs e) { if (ProgressChanged != null) ProgressChanged(this, e); }

        public void ScanDirectory(string fileDirectory)
        {
            string[] files = Directory.GetFiles(fileDirectory, "*.tlf");
            int curFile = 0;
            var eventArgs = new ProgressChangedEventArgs
                            {
                                CurrentProgress = curFile,
                                ProgressWhenComplete = files.Length,
                                JobDescription = "Updating Transmission Loss Field cache"
                            };
            foreach (string f in files)
            {
                OnProgressChanged(eventArgs);
                eventArgs.CurrentProgress = ++curFile;

                string f1 = f;
                if (Find(n => n.Filename == Path.GetFileName(f1)) == null)
                {
                    // If the filename we're processing is not found in our list
                    // we want to open the file, extract the metadata, and add a record to the list
                    try
                    {
                        TransmissionLossField fd = TransmissionLossField.LoadHeader(f);
#if false
                        Add(new OldTransmissionLossField
                                            {
                                                Filename = Path.GetFileName(f),
                                                MaxTLDepth_meters = (int)fd.MaxCalculationDepth,
                                                RadialCount = fd.Radials.Count(),
                                                OldAnalysisPoint = new OldAnalysisPoint
                                                                   {
                                                                       FieldRadius_meters = fd.Radius,
                                                                       Location = new EarthCoordinate(fd.Latitude, fd.Longitude),
                                                                       AcousticProperties = new AcousticProperties
                                                                                            {
                                                                                                DepressionElevationAngle = fd.VerticalLookAngle,
                                                                                                SourceDepth = fd.SourceDepth,
                                                                                                HighFrequency = fd.HighFrequency,
                                                                                                LowFrequency = fd.LowFrequency,
                                                                                                VerticalBeamWidth = fd.VerticalBeamWidth,
                                                                                            },
                                                                   },
                                            }); 
#endif
                        Add( new TransmissionLossField(f,true)
                             {
                                 Depths = fd.Depths,
                                 Ranges = fd.Ranges,
                                 Filename = fd.Filename,
                             });
                        continue;
                    }
                    catch (FileFormatException)
                    {
                        // File format is invalid, delete it.
                        File.Delete(f);
                    }
                }
            }
            eventArgs.CurrentProgress = eventArgs.ProgressWhenComplete;
            OnProgressChanged(eventArgs);
        }

        #region Load and Save

        public static TransmissionLossFieldCache Load(string Filename)
        {
            Assembly Assembly = Assembly.GetExecutingAssembly();
            //var Names = Assembly.GetManifestResourceNames();
            var Schema = new StringBuilder();
            string[] SchemaResources = {
                                           "ESME.Schema.Geo.xsd", "ESME.Schema.AnalysisPoint.xsd", "ESME.Schema.AcousticProperties.xsd", "ESME.Schema.TransmissionLossField.xsd",
                                       };
            foreach (string Resource in SchemaResources)
            {
                using (var Reader = new StreamReader(Assembly.GetManifestResourceStream(Resource))) Schema.Append(Reader.ReadToEnd());
            }
            TransmissionLossFieldCache Cache;
            using (var FileReader = new StreamReader(Filename)) Cache = Deserialize(FileReader.ReadToEnd(), Schema.ToString());
            Cache.FieldFileDirectory = Path.GetDirectoryName(Filename);
            return Cache;
        }

        public void Save(string Filename)
        {
            var FileWriter = new StreamWriter(Filename, false);
            FileWriter.Write(Serialize());
            FileWriter.Close();
        }

        #endregion

        #region Serialize/Deserialize

        public string Serialize()
        {
            var ms = new MemoryStream();
            var serializer = new XmlSerializer(GetType());
            var Settings = new XmlWriterSettings
                           {
                               Encoding = Encoding.UTF8,
                               Indent = true,
                           };
            XmlWriter writer = XmlWriter.Create(ms, Settings);

            serializer.Serialize(writer, this);
            return Encoding.UTF8.GetString(ms.ToArray());
        }

        public static TransmissionLossFieldCache Deserialize(string XmlString, string SchemaXml)
        {
            var schemaReader = new StringReader(SchemaXml);
            XmlSchema schema = XmlSchema.Read(schemaReader, ValidationError);

            var xmlReaderSettings = new XmlReaderSettings();
            xmlReaderSettings.ValidationType = ValidationType.Schema;
            xmlReaderSettings.Schemas.Add(schema);
            xmlReaderSettings.ValidationEventHandler += ValidationError;
            var xmlStream = new StringReader(XmlString);
            XmlReader xmlReader = XmlReader.Create(xmlStream, xmlReaderSettings);
            _passedValidation = true;
            while (xmlReader.Read()) ; // Empty body, all we want to do is validate the XML string we're about to deserialize
            if (!_passedValidation) return null;

            var reader = new StringReader(XmlString);
            var serializer = new XmlSerializer(typeof (TransmissionLossFieldCache));
            var runfile = (TransmissionLossFieldCache) serializer.Deserialize(reader);
            return runfile;
        }

        static void ValidationError(object sender, ValidationEventArgs arguments) { _passedValidation = false; }

        #endregion
    }
}