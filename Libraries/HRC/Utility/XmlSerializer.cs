﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Text;
using System.Threading;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;

namespace HRC.Utility
{
    [Serializable]
    public class XmlSerializer<T> where T : class, new()
    {
        public T Data { get; set; }

        [XmlIgnore]
        public List<Type> ReferencedTypes { get; set; }

        #region Load/Save

        /// <summary>
        /// Load the data from a file without validating against an XML schema
        /// </summary>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <param name="referencedTypes">A list of types that are referenced by your class, including any types referenced by members or base classes</param>
        /// <returns></returns>
        public static T Load(string filename, List<Type> referencedTypes) { return Load(filename, null, referencedTypes); }

        /// <summary>
        /// Load the data from a file, validating against a list of schema resources
        /// </summary>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <param name="schemaResourceNames">An array of resource names containing the schema(s) expected to be found in this file</param>
        /// <param name="referencedTypes"></param>
        /// <returns></returns>
        public static T Load(string filename, string[] schemaResourceNames, List<Type> referencedTypes)
        {
            if (!File.Exists(filename)) return new T();

            String file;

            var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read);
            using (var fileReader = new StreamReader(stream))
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
                return Deserialize(file, schema.ToString(), referencedTypes);
            }

            return Deserialize(file, null, referencedTypes);
        }

        public string Xml
        {
            get { return Serialize(ReferencedTypes); }
        }

        /// <summary>
        /// Saves the data to a new filename, and that filename is set as the default filename
        /// </summary>
        /// <param name="fileName"></param>
        public void Save(string fileName)
        {
            Save(fileName, ReferencedTypes);
        }

        /// <summary>
        /// Save the data to a different file
        /// </summary>
        /// <param name="fileName">The name of the the file that will contain the saved data</param>
        /// <param name="referencedTypes"></param>
        public void Save(string fileName, List<Type> referencedTypes)
        {
            var retry = 10;
            Exception ex = null;
            while (--retry > 0)
            {
                try
                {
                    File.WriteAllText(fileName, Serialize(referencedTypes));
                    return;
                }
                catch (Exception e)
                {
                    ex = e;
                    Thread.Sleep(100);
                }
            }
            throw ex;
        }

        #endregion

        #region Serialize/Deserialize

        private string Serialize(List<Type> extraTypes)
        {
            var ms = new MemoryStream();
            var serializer = extraTypes == null ? new XmlSerializer(typeof(T)) : new XmlSerializer(typeof(T), extraTypes.ToArray());
            var settings = new XmlWriterSettings
            {
                Encoding = Encoding.UTF8,
                Indent = true,
            };

            // ReSharper disable AssignNullToNotNullAttribute
            using (var writer = XmlWriter.Create(ms, settings))
                serializer.Serialize(writer, Data);
            // ReSharper restore AssignNullToNotNullAttribute

            return Encoding.UTF8.GetString(ms.ToArray());
        }

        private static T Deserialize(String xmlData, String xmlSchema, List<Type> extraTypes)
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
            var serializer = extraTypes == null ? new XmlSerializer(typeof(T)) : new XmlSerializer(typeof(T), extraTypes.ToArray());
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