using System;
using System.IO;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;

namespace ESMEWorkBench.Data
{
    public abstract class PersistentApplicationData<T> where T : IApplicationData
    {
        protected PersistentApplicationData() { }

        protected PersistentApplicationData(EventHandler valueChangedHandler)
        {
            if (valueChangedHandler != null)
                ValueChanged += valueChangedHandler;
        }

        protected abstract void OnSave();

        #region Load/Save

        /// <summary>
        /// Load the persistent application data from a file
        /// </summary>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <returns></returns>
        public static T Load(string filename) { return Load(filename, null); }
        
        /// <summary>
        /// Load the persistent application data from a file
        /// </summary>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <param name="schemaResourceNames">An array of resource names containing the schema(s) expected to be found in this file</param>
        /// <returns></returns>
        public static T Load(string filename, string[] schemaResourceNames)
        {
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
        /// Save the persistent application data to a file
        /// </summary>
        /// <param name="filename">The name of the the file that will contain the saved data</param>
        public void Save(string filename)
        {
            OnSave();
            var fileWriter = new StreamWriter(filename, false);
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

                var xmlReaderSettings = new XmlReaderSettings {ValidationType = ValidationType.Schema};
                xmlReaderSettings.Schemas.Add(schema);
                xmlReaderSettings.ValidationEventHandler += ValidationError;

                var xmlStream = new StringReader(xmlData);
                var xmlReader = XmlReader.Create(xmlStream, xmlReaderSettings);
                while (xmlReader.Read()) {}
            }

            var reader = new StringReader(xmlData);
            var serializer = new XmlSerializer(typeof(T));
            var applicationData = (T)serializer.Deserialize(reader);
            applicationData.Initialize();
            return applicationData;
        }

        private static void ValidationError(object sender, ValidationEventArgs arguments)
        {
            throw new XmlSchemaValidationException(arguments.Message);
        }

        #endregion

        #region public event ValueChanged
        
        public event EventHandler ValueChanged;

        protected virtual void OnValueChanged()
        {
            if (ValueChanged != null)
                ValueChanged(this, EventArgs);
        }

        static readonly EventArgs EventArgs = new EventArgs();

        #endregion
    }
}