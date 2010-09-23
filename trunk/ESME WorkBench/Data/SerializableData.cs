using System;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;

namespace ESMEWorkBench.Data
{
    [Serializable]
    public class SerializableData : INotifyPropertyChanged
    {
        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;

        #endregion

        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }
    }

    [Serializable]
    public class SerializableData<T> : SerializableData where T : new()
    {
        [XmlIgnore]
        public string FileName { get; set; }

        #region Load/Save

        public void CopyFrom(T that)
        {
            // Copy all fields from that to this)
            foreach (var field in typeof(T).GetFields())
                field.SetValue(this, field.GetValue(that));

            // Copy all properties from that to this)
            foreach (var property in typeof(T).GetProperties())
                property.SetValue(this, property.GetValue(that, null), null);
        }

        /// <summary>
        /// Reload the data from the default file
        /// </summary>
        public void Reload() { CopyFrom(Load()); }

        /// <summary>
        /// Load the data from the default filename
        /// </summary>
        /// <returns></returns>
        public T Load() { return Load(FileName, null); }

        /// <summary>
        /// Load the data from a file without validating against an XML schema
        /// </summary>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <returns></returns>
        public static T Load(string filename) { return Load(filename, null); }

        /// <summary>
        /// Load the data from a file, validating against a list of schema resources
        /// </summary>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <param name="schemaResourceNames">An array of resource names containing the schema(s) expected to be found in this file</param>
        /// <returns></returns>
        public static T Load(string filename, string[] schemaResourceNames)
        {
            if (!File.Exists(filename)) return new T();

            String file;

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
        /// Save the data to the default filename
        /// </summary>
        public void Save() { SaveAs(FileName); }

        /// <summary>
        /// Saves the data to a new filename, and that filename is set as the default filename
        /// </summary>
        /// <param name="fileName"></param>
        public void Save(string fileName)
        {
            FileName = fileName;
            SaveAs(FileName);
        }

        /// <summary>
        /// Save the data to a different file
        /// </summary>
        /// <param name="fileName">The name of the the file that will contain the saved data</param>
        public void SaveAs(string fileName)
        {
            var fileWriter = new StreamWriter(fileName, false);
            fileWriter.Write(Serialize());
            fileWriter.Close();
        }

        #endregion

        #region Serialize/Deserialize

        private string Serialize()
        {
            var ms = new MemoryStream();
            var serializer = new XmlSerializer(typeof(T));
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

        private static T Deserialize(String xmlData, String xmlSchema)
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
            var serializer = new XmlSerializer(typeof(T));
            var appSettings = (T)serializer.Deserialize(reader);
            return appSettings;
        }

        private static void ValidationError(object sender, ValidationEventArgs arguments)
        {
            throw new XmlSchemaValidationException(arguments.Message);
        }

        #endregion
    }
}