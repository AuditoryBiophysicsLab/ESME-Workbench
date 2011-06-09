using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;
using HRC.Navigation;
using System.Windows;

namespace ESME.Environment
{
    public class EnvironmentData<T> : IList<T> where T: EarthCoordinate, new()
    {
        readonly List<T> _list = new List<T>();

        private static readonly List<Type> ReferencedTypes = new List<Type>
                                                                 {
                                                                     typeof (EarthCoordinate),
                                                                     typeof (Geo),
                                                                     typeof (Point),
                                                                 };
        public EnvironmentData() { }

        public virtual T this[EarthCoordinate location]
        {
            get
            {
                var minDistance = double.MaxValue;
                T closestSample = null;
                foreach (var item in _list)
                {
                    var curDistance = item.DistanceKilometers(location);
                    if (curDistance >= minDistance) continue;
                    minDistance = curDistance;
                    closestSample = item;
                }
                return closestSample;
            }
        }

        public virtual void AddRange(IEnumerable<T> collection) { _list.AddRange(collection); }

        public void TrimToNearestPoints(GeoRect geoRect)
        {
            var southWest = this[geoRect.SouthWest];
            var northEast = this[geoRect.NorthEast];
            var trimRect = GeoRect.InflateWithGeo(new GeoRect(northEast.Latitude, southWest.Latitude, northEast.Longitude, southWest.Longitude), 0.01);
            var pointsToKeep = _list.Where(trimRect.Contains).ToList();
            _list.Clear();
            _list.AddRange(pointsToKeep);
        }

        #region IList members
        public virtual IEnumerator<T> GetEnumerator() { return _list.GetEnumerator(); }
        IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }
        public virtual void Add(T item) { _list.Add(item); }
        public virtual void Clear() { _list.Clear(); }
        public virtual bool Contains(T item) { return _list.Contains(item); }
        public virtual void CopyTo(T[] array, int arrayIndex) { _list.CopyTo(array, arrayIndex); }
        public virtual bool Remove(T item) { return _list.Remove(item); }

        public virtual int Count
        {
            get { return _list.Count; }
        }

        public virtual bool IsReadOnly
        {
            get { return true; }
        }

        public virtual int IndexOf(T item) { return _list.IndexOf(item); }
        public virtual void Insert(int index, T item) { _list.Insert(index, item); }
        public virtual void RemoveAt(int index) { _list.RemoveAt(index); }

        T IList<T>.this[int index]
        {
            get { return _list[index]; }
            set { _list[index] = value; }
        }
        #endregion

        #region Load/Save/Serialize/Deserialize

        /// <summary>
        /// Load the data from a file without validating against an XML schema
        /// </summary>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <param name="derivedReferencedTypes"></param>
        /// <returns></returns>
        public static EnvironmentData<T> Load(string filename, List<Type> derivedReferencedTypes)
        {
            var allTypes = new List<Type>(ReferencedTypes);
            if (derivedReferencedTypes != null) allTypes.AddRange(derivedReferencedTypes);
            return Load(filename, allTypes.ToArray());
        }

        /// <summary>
        /// Load the data from a file without validating against an XML schema
        /// </summary>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <param name="extraTypes"></param>
        /// <returns></returns>
        protected static EnvironmentData<T> Load(string filename, Type[] extraTypes) { return Load(filename, null, extraTypes); }

        /// <summary>
        /// Load the data from a file, validating against a list of schema resources
        /// </summary>
        /// <param name="filename">The name of the file containing the data to be loaded</param>
        /// <param name="schemaResourceNames">An array of resource names containing the schema(s) expected to be found in this file</param>
        /// <param name="extraTypes"></param>
        /// <returns></returns>
        protected static EnvironmentData<T> Load(string filename, string[] schemaResourceNames, Type[] extraTypes)
        {
            if (!File.Exists(filename)) return null;

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
        /// Saves the data to a specified filename
        /// </summary>
        /// <param name="fileName"></param>
        /// <param name="derivedReferencedTypes"></param>
        public void Save(string fileName, List<Type> derivedReferencedTypes)
        {
            var allTypes = new List<Type>(ReferencedTypes);
            if (derivedReferencedTypes != null) allTypes.AddRange(derivedReferencedTypes);
            Save(fileName, allTypes.ToArray());
        }

        /// <summary>
        /// Saves the data to a specified filename
        /// </summary>
        /// <param name="fileName"></param>
        /// <param name="extraTypes"></param>
        protected void Save(string fileName, Type[] extraTypes)
        {
            var fileWriter = new StreamWriter(fileName, false);
            fileWriter.Write(Serialize(extraTypes));
            fileWriter.Close();
        }

        private string Serialize(Type[] extraTypes)
        {
            var ms = new MemoryStream();
            var serializer = new XmlSerializer(typeof(EnvironmentData<T>), extraTypes);
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

        private static EnvironmentData<T> Deserialize(String xmlData, String xmlSchema, Type[] extraTypes)
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
            var serializer = new XmlSerializer(typeof(EnvironmentData<T>), extraTypes);
            var appSettings = (EnvironmentData<T>)serializer.Deserialize(reader);
            return appSettings;
        }

        private static void ValidationError(object sender, ValidationEventArgs arguments)
        {
            throw new XmlSchemaValidationException(arguments.Message);
        }

        #endregion
    }
}
