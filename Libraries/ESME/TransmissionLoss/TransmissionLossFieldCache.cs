using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ESME.Model;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Schema;
using System.Xml.Serialization;

namespace ESME.TransmissionLoss
{
    [XmlRoot(
        Namespace = "http://esme.bu.edu/support/schemas/ESME_Experiment.xsd",
        IsNullable = false)]
    public class TransmissionLossFieldCache : List<OldTransmissionLossField>
    {
        #region Private data members
        [XmlIgnore]
        private static bool PassedValidation = false;
        #endregion
        [XmlIgnore]
        public string FieldFileDirectory { get; set; }

        public event EventHandler<ProgressChangedEventArgs> ProgressChanged;

        protected virtual void OnProgressChanged(ProgressChangedEventArgs e)
        {
            if (ProgressChanged != null)
                ProgressChanged(this, e);
        }

        public void ScanDirectory(string FieldFileDirectory)
        {
            string[] files = Directory.GetFiles(FieldFileDirectory, "*.tlf");
            int curFile = 0;
            ProgressChangedEventArgs eventArgs = new ProgressChangedEventArgs
            {
                CurrentProgress = curFile,
                ProgressWhenComplete = files.Length,
                JobDescription = "Updating Transmission Loss Field cache"
            };
            foreach (string f in files)
            {
                OnProgressChanged(eventArgs);
                eventArgs.CurrentProgress = ++curFile;

                if (this.Find(n => n.Filename == Path.GetFileName(f)) == null)
                {
                    // If the filename we're processing is not found in our list
                    // we want to open the file, extract the metadata, and add a record to the list
                    try
                    {
                        TransmissionLossField fd = TransmissionLossField.LoadHeader(f);
                        Add(new OldTransmissionLossField
                        {
                            Filename = Path.GetFileName(f),
                            MaxTLDepth_meters = (int)fd.MaxCalculationDepth,
                            RadialCount = fd.Radials.Count(),
                            AnalysisPoint = new AnalysisPoint
                            {
                                FieldRadius_meters = fd.Radius,
                                Location = new HRC.Navigation.EarthCoordinate(fd.Latitude, fd.Longitude),
                                AcousticProperties = new AcousticProperties
                                {
                                    DepressionElevationAngle_degrees = fd.VerticalLookAngle,
                                    SourceDepth_meters = fd.SourceDepth,
                                    HighFrequency_Hz = fd.HighFrequency,
                                    LowFrequency_Hz = fd.LowFrequency,
                                    VerticalBeamWidth_degrees = fd.VerticalBeamWidth,
                                },
                            },
                        });
                        continue;
                    }
                    catch (ESME.Model.FileFormatException)
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
            var Assembly = System.Reflection.Assembly.GetExecutingAssembly();
            //var Names = Assembly.GetManifestResourceNames();
            StringBuilder Schema = new StringBuilder();
            string[] SchemaResources =
            {
                "ESME.Schema.EarthCoordinate.xsd",
                "ESME.Schema.AnalysisPoint.xsd",
                "ESME.Schema.AcousticProperties.xsd",
                "ESME.Schema.TransmissionLossField.xsd",
            };
            foreach (string Resource in SchemaResources)
            {
                using (StreamReader Reader = new StreamReader(Assembly.GetManifestResourceStream(Resource)))
                    Schema.Append(Reader.ReadToEnd());
            }
            TransmissionLossFieldCache Cache;
            using (StreamReader FileReader = new StreamReader(Filename))
                Cache = Deserialize(FileReader.ReadToEnd(), Schema.ToString());
            Cache.FieldFileDirectory = Path.GetDirectoryName(Filename);
            return Cache;
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

        public static TransmissionLossFieldCache Deserialize(string XmlString, string SchemaXml)
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
            XmlSerializer serializer = new XmlSerializer(typeof(TransmissionLossFieldCache));
            TransmissionLossFieldCache runfile = (TransmissionLossFieldCache)serializer.Deserialize(reader);
            return runfile;
        }

        private static void ValidationError(object sender, ValidationEventArgs arguments)
        {
            PassedValidation = false;
        }
        #endregion
    }
}
