using System;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;

namespace HRC.Utility
{
    [Serializable]
    public class SerializableData : INotifyPropertyChanged
    {
        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }

        #endregion

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
            {
                try
                {
                    property.SetValue(this, property.GetValue(that, null), null);
                }
                catch
                {
                    //todo: dave to make this better. by checking if property has a setter or not. see AppSettings.Reload();
                }
            }
        }

        /// <summary>
        /// TRUE if the file should be kept open after reading and/or writing.
        /// Setting this to TRUE will have the side effect of making the file unavailable to be 
        /// read or written by any other application while the current app has the file open.
        /// </summary>
        public bool CloseAfterReadWrite { get; set; }

        /// <summary>
        /// Close the file if it's open.  If not open, no exception will be thrown
        /// </summary>
        public void Close()
        {

        }

        /// <summary>
        /// Reload the data from the default file
        /// </summary>
        public void Reload(Type[] extraTypes) { CopyFrom(Load(extraTypes)); }

        /// <summary>
        /// Load the data from the default filename
        /// </summary>
        /// <returns></returns>
        public T Load(Type[] extraTypes) { return Load(FileName, null, extraTypes); }

        /// <summary>
        /// Load the data from a file without validating against an XML schema
        /// </summary>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <param name="extraTypes"></param>
        /// <returns></returns>
        public static T Load(string filename, Type[] extraTypes) { return Load(filename, null, extraTypes); }

        /// <summary>
        /// Load the data from a file, validating against a list of schema resources
        /// </summary>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <param name="schemaResourceNames">An array of resource names containing the schema(s) expected to be found in this file</param>
        /// <param name="extraTypes"></param>
        /// <returns></returns>
        public static T Load(string filename, string[] schemaResourceNames, Type[] extraTypes)
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
                return Deserialize(file, schema.ToString(), extraTypes);
            }

            return Deserialize(file, null, extraTypes);
        }

        /// <summary>
        /// Save the data to the default filename
        /// </summary>
        public void Save(Type[] extraTypes) { SaveAs(FileName, extraTypes); }

        /// <summary>
        /// Saves the data to a new filename, and that filename is set as the default filename
        /// </summary>
        /// <param name="fileName"></param>
        /// <param name="extraTypes"></param>
        public void Save(string fileName, Type[] extraTypes)
        {
            FileName = fileName;
            SaveAs(FileName, extraTypes);
        }

        /// <summary>
        /// Save the data to a different file
        /// </summary>
        /// <param name="fileName">The name of the the file that will contain the saved data</param>
        /// <param name="extraTypes"></param>
        public void SaveAs(string fileName, Type[] extraTypes)
        {
            var fileWriter = new StreamWriter(fileName, false);
            fileWriter.Write(Serialize(extraTypes));
            fileWriter.Close();
        }

        #endregion

        #region Serialize/Deserialize

        private string Serialize(Type[] extraTypes)
        {
            var ms = new MemoryStream();
            var serializer = new XmlSerializer(typeof(T), extraTypes);
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

        private static T Deserialize(String xmlData, String xmlSchema, Type[] extraTypes)
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
            var serializer = new XmlSerializer(typeof(T), extraTypes);
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