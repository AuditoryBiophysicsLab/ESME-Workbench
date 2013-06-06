using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;
using HRC.Collections;

namespace HRC.Utility
{
    [Serializable]
    public class StaticXmlSerializer
    {
        public static void Save<T>(string fileName, T source) where T : class, new()
        {
            var type = typeof(T);
            if (!SerializerCache.ContainsKey(type)) SerializerCache.Add(type, new SerializerCacheEntry());
            var cacheEntry = SerializerCache[type];

            cacheEntry.ReferencedTypes = cacheEntry.ReferencedTypes ?? FindReferencedTypesIn(type);
            cacheEntry.SerializationMethod = cacheEntry.SerializationMethod ?? typeof (XmlSerializer<T>).GetMethod("SaveStatic", new[] {typeof (T), typeof (string), typeof (List<Type>)});

            cacheEntry.SerializationMethod.Invoke(null, new object[] { source, fileName, cacheEntry.ReferencedTypes });
        }

        public static object Load(string fileName, Type type)
        {
            if (!SerializerCache.ContainsKey(type)) SerializerCache.Add(type, new SerializerCacheEntry());
            var cacheEntry = SerializerCache[type];

            cacheEntry.ReferencedTypes = cacheEntry.ReferencedTypes ?? FindReferencedTypesIn(type);
            cacheEntry.DeserializationMethod = cacheEntry.DeserializationMethod ?? typeof(XmlSerializer<>).MakeGenericType(type).GetMethod("Load", new[] { typeof(string), typeof(List<Type>) });

            return cacheEntry.DeserializationMethod.Invoke(null, new object[] { fileName, cacheEntry.ReferencedTypes });
        }

        public static List<Type> FindReferencedTypesIn(Type targetType)
        {
            var memberInfo = targetType.GetMembers(BindingFlags.Static | BindingFlags.FlattenHierarchy | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
            var referencedTypes = new List<Type>();
            foreach (var member in memberInfo)
            {
                if (!member.GetCustomAttributes(typeof(ReferencedTypesAttribute), true).Any()) continue;
                var fieldInfo = member as FieldInfo;
                var propertyInfo = member as PropertyInfo;
                if (propertyInfo != null)
                {
                    if (!typeof(IList<Type>).IsAssignableFrom(propertyInfo.PropertyType)) throw new InvalidReferencedTypesAttributeException(String.Format("Error on property {0}.{1}: Properties with the [ReferencedTypes] attribute must be assignable to type IList<Type>", propertyInfo.DeclaringType, propertyInfo.PropertyType));
                    referencedTypes.AddRange((List<Type>)propertyInfo.GetValue(null, BindingFlags.Static, null, null, null));
                }
                else if (fieldInfo != null)
                {
                    if (!typeof(IList<Type>).IsAssignableFrom(fieldInfo.FieldType)) throw new InvalidReferencedTypesAttributeException(String.Format("Error on field {0}.{1}: Fields with the [ReferencedTypes] attribute must be assignable to type IList<Type>", fieldInfo.DeclaringType, fieldInfo.FieldType));
                    referencedTypes.AddRange((List<Type>)fieldInfo.GetValue(null));
                }
            }
            return referencedTypes;
        }

        static readonly ObservableConcurrentDictionary<Type, SerializerCacheEntry> SerializerCache = new ObservableConcurrentDictionary<Type, SerializerCacheEntry>();

        private class SerializerCacheEntry
        {
            public List<Type> ReferencedTypes { get; set; }
            public MethodInfo SerializationMethod { get; set; }
            public MethodInfo DeserializationMethod { get; set; }
        }
    }
    [AttributeUsage(AttributeTargets.Property)]
    public class SettingAttribute : Attribute { }

    [AttributeUsage(AttributeTargets.Property | AttributeTargets.Field)]
    public class ReferencedTypesAttribute : Attribute { }

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
        public static T Load(string filename, List<Type> referencedTypes) { return Load(filename, referencedTypes, false); }

        /// <summary>
        /// Load the data from a file without validating against an XML schema
        /// </summary>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <param name="referencedTypes">A list of types that are referenced by your class, including any types referenced by members or base classes</param>
        /// <returns></returns>
        public static T LoadExistingFile(string filename, List<Type> referencedTypes) { return Load(filename, referencedTypes, true); }

        /// <summary>
        /// Load the data from a file, validating against a list of schema resources
        /// </summary>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <param name="referencedTypes"></param>
        /// <param name="nullIfFileAbsent"> </param>
        /// <returns></returns>
        static T Load(string filename, List<Type> referencedTypes, bool nullIfFileAbsent)
        {
            if (!File.Exists(filename)) return nullIfFileAbsent ? null : new T();

            String file;

            var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read);
            using (var fileReader = new StreamReader(stream))
                file = fileReader.ReadToEnd();

            return Deserialize(file, null, referencedTypes);
        }

        /// <summary>
        /// Load the data from a file without validating against an XML schema
        /// </summary>
        /// <param name="data"> </param>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <param name="referencedTypes">A list of types that are referenced by your class, including any types referenced by members or base classes</param>
        /// <returns></returns>
        public static void SaveStatic(T data, string filename, List<Type> referencedTypes)
        {
            var retry = 10;
            Exception ex = null;
            while (--retry > 0)
            {
                try
                {
                    File.WriteAllText(filename, SerializeStatic(data, referencedTypes));
                    return;
                }
                catch (Exception e)
                {
                    ex = e;
                    Thread.Sleep(100);
                }
            }
            if (ex != null) throw ex;
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
            if (ex != null) throw ex;
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

        private static string SerializeStatic(T data, List<Type> extraTypes)
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
                serializer.Serialize(writer, data);
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