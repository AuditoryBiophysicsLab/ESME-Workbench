using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading.Tasks;
using HRC.Collections;
using HRC.Navigation;

namespace ESME.Environment.Descriptors
{
    [Serializable]
    class RangeComplexToken : IDeserializationCallback
    {
        RangeComplexToken()
        {
            GeoRect = null;
            _lastWriteTime = new DateTime(1, 1, 1, 0, 0, 0, 0);  // This should ensure that no files are older than the token's last write time
            EnvironmentDictionary = new ObservableConcurrentDictionary<string, EnvironmentFile>();
            EnvironmentDictionary.CollectionChanged += DictionaryChanged;
        }

        void DictionaryChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (_fileName == null) throw new ApplicationException("_fileName is null");
            if (_dataPath == null) throw new ApplicationException("_dataPath is null");
            if (e.Action == NotifyCollectionChangedAction.Add)
                foreach (EnvironmentFile newItem in e.NewItems) newItem.DataPath = _dataPath;
            Save(_fileName, GeoRect);
        }

        public void Save(string filename, GeoRect geoRect)
        {
            var formatter = new BinaryFormatter();
            GeoRect = geoRect;
            _lastWriteTime = DateTime.Now;
            ExtractionsRequired.Clear();
            using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None)) formatter.Serialize(stream, this);
            _fileName = filename;
        }

        public static RangeComplexToken Load(string filename)
        {
            RangeComplexToken result;
            if (!File.Exists(filename))
            {
                result = new RangeComplexToken();
                result._fileName = filename;
                result._dataPath = Path.GetDirectoryName(result._fileName);
                return result;
            }
            var formatter = new BinaryFormatter();
            using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
                result = (RangeComplexToken)formatter.Deserialize(stream);
            result._lastWriteTime = new FileInfo(filename).LastWriteTime;
            result._fileName = filename;
            result._dataPath = Path.GetDirectoryName(result._fileName);
            foreach (var file in result.EnvironmentDictionary.Values) file.DataPath = result._dataPath;
            return result;
        }

        void IDeserializationCallback.OnDeserialization(Object sender)
        {
            ExtractionsRequired = new List<Tuple<object, string>>();
            EnvironmentDictionary.CollectionChanged += DictionaryChanged;
        }

        public static Task<RangeComplexToken> LoadAsync(string filename) { return TaskEx.Run(() => Load(filename)); }

        [NonSerialized] DateTime _lastWriteTime;
        [NonSerialized] string _fileName;
        [NonSerialized] string _dataPath;

        [NonSerialized] public List<Tuple<object, string>> ExtractionsRequired = new List<Tuple<object, string>>();
        [NonSerialized] public bool ReextractionRequired;

        public ObservableConcurrentDictionary<string, EnvironmentFile> EnvironmentDictionary { get; private set; }
        public GeoRect GeoRect { get; set; }
        public DateTime LastWriteTime { get { return _lastWriteTime; } }
    }
}