using System;
using System.IO;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;
using Cinch;

namespace ESMEWorkBench.Data
{
    public class AppSettings
    {
        public string ScenarioEditorExecutablePath { get; set; }
        public string ScenarioDataDirectory { get; set; }
        public string EnvironmentDatabaseDirectory { get; set; }

        [XmlIgnore]
        static readonly string AppSettingsDirectory;
        static readonly string AppSettingsFile;

        static AppSettings()
        {
            AppSettingsDirectory = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), Path.GetFileNameWithoutExtension(Assembly.GetEntryAssembly().GetName().CodeBase));
            if (!Directory.Exists(AppSettingsDirectory)) Directory.CreateDirectory(AppSettingsDirectory);
            AppSettingsFile = Path.Combine(AppSettingsDirectory, "settings.xml");
        }

        public AppSettings() { }

        public AppSettings(AppSettings that)
        {
            CopyFrom(that);
        }

        public void CopyFrom(AppSettings that)
        {
            // Copy all fields from that to this)
            foreach (var field in typeof(AppSettings).GetFields())
                field.SetValue(this, field.GetValue(that));

            // Copy all properties from that to this)
            foreach (var property in typeof(AppSettings).GetProperties())
                property.SetValue(this, property.GetValue(that, null), null);
        }

        #region Load/Save

        /// <summary>
        /// Reload the application settings from the default file
        /// </summary>
        public void Reload() { CopyFrom(Load()); }

        /// <summary>
        /// Load the application settings from the default filename
        /// </summary>
        /// <returns></returns>
        public static AppSettings Load() { return Load(AppSettingsFile, null); }

        /// <summary>
        /// Load the persistent application data from a file
        /// </summary>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <returns></returns>
        public static AppSettings Load(string filename) { return Load(filename, null); }

        /// <summary>
        /// Load the persistent application data from a file
        /// </summary>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <param name="schemaResourceNames">An array of resource names containing the schema(s) expected to be found in this file</param>
        /// <returns></returns>
        public static AppSettings Load(string filename, string[] schemaResourceNames)
        {
            if (!File.Exists(filename)) return new AppSettings();

            string file;

            using (var fileReader = new StreamReader(filename))
                file = fileReader.ReadToEnd();

            if (schemaResourceNames != null)
            {
                var assembly = Assembly.GetExecutingAssembly();
                var schema = new StringBuilder();
                foreach (var resourceName in schemaResourceNames)
                {
                    // ReSharper disable AssignNullToNotNullAttribute
                    using (var reader = new StreamReader(assembly.GetManifestResourceStream(resourceName)))
                        schema.Append(reader.ReadToEnd());
                    // ReSharper restore AssignNullToNotNullAttribute
                }
                return Deserialize(file, schema.ToString());
            }

            return Deserialize(file, null);
        }

        /// <summary>
        /// Save the application data to the default filename
        /// </summary>
        public void Save() { Save(AppSettingsFile); }

        /// <summary>
        /// Save the persistent application data to a file
        /// </summary>
        /// <param name="filename">The name of the the file that will contain the saved data</param>
        public void Save(string filename)
        {
            var fileWriter = new StreamWriter(filename, false);
            fileWriter.Write(Serialize());
            fileWriter.Close();
        }

        #endregion

        #region Serialize/Deserialize

        private string Serialize()
        {
            var ms = new MemoryStream();
            var serializer = new XmlSerializer(typeof(AppSettings));
            var settings = new XmlWriterSettings
            {
                Encoding = Encoding.UTF8,
                Indent = true,
            };

            // ReSharper disable AssignNullToNotNullAttribute
            using (var writer = XmlWriter.Create(ms, settings))
                serializer.Serialize(writer, this);
            // ReSharper restore AssignNullToNotNullAttribute

            return Encoding.UTF8.GetString(ms.ToArray());
        }

        private static AppSettings Deserialize(String xmlData, String xmlSchema)
        {
            if (xmlSchema != null)
            {
                var schemaReader = new StringReader(xmlSchema);
                var schema = XmlSchema.Read(schemaReader, ValidationError);

                var xmlReaderSettings = new XmlReaderSettings { ValidationType = ValidationType.Schema };
                xmlReaderSettings.Schemas.Add(schema);
                xmlReaderSettings.ValidationEventHandler += ValidationError;

                var xmlStream = new StringReader(xmlData);
                var xmlReader = XmlReader.Create(xmlStream, xmlReaderSettings);
                while (xmlReader.Read()) { }
            }

            var reader = new StringReader(xmlData);
            var serializer = new XmlSerializer(typeof(AppSettings));
            var appSettings = (AppSettings)serializer.Deserialize(reader);
            return appSettings;
        }

        private static void ValidationError(object sender, ValidationEventArgs arguments)
        {
            throw new XmlSchemaValidationException(arguments.Message);
        }

        #endregion
    }
}
