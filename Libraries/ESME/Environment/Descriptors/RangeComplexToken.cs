using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Diagnostics;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading.Tasks;
using System.Windows.Threading;
using Cinch;
using HRC.Collections;
using HRC.Navigation;

namespace ESME.Environment.Descriptors
{
    [Serializable]
    public class RangeComplexToken : IDeserializationCallback, IEnumerable<KeyValuePair<string, EnvironmentFile>>, INotifyCollectionChanged
    {
        RangeComplexToken()
        {
            GeoRect = null;
            _lastWriteTime = new DateTime(1, 1, 1, 0, 0, 0, 0);  // This should ensure that no files are older than the token's last write time
            EnvironmentDictionary = new ObservableConcurrentDictionary<string, EnvironmentFile>();
            ((INotifyCollectionChanged)EnvironmentDictionary).CollectionChanged += DictionaryChanged;
        }

        void DictionaryChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (_fileName == null) throw new ApplicationException("_fileName is null");
            if (_dataPath == null) throw new ApplicationException("_dataPath is null");
            if (e.Action == NotifyCollectionChangedAction.Add)
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (KeyValuePair<string, EnvironmentFile> newItem in e.NewItems) newItem.Value.DataPath = _dataPath;
                    break;
                case NotifyCollectionChangedAction.Reset:
                    foreach (var newItem in EnvironmentDictionary) newItem.Value.DataPath = _dataPath;
                    break;
            }
            Save(_fileName, GeoRect);
            OnCollectionChanged(e);
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
            try
            {
                RangeComplexToken result;
                var formatter = new BinaryFormatter();
                using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read)) result = (RangeComplexToken)formatter.Deserialize(stream);
                result._lastWriteTime = new FileInfo(filename).LastWriteTime;
                result._fileName = filename;
                result._dataPath = Path.GetDirectoryName(result._fileName);
                foreach (var file in result.EnvironmentDictionary.Values) file.DataPath = result._dataPath;
                return result;
            }
            catch(Exception)
            {
                return new RangeComplexToken
                {
                    _fileName = filename,
                    _dataPath = Path.GetDirectoryName(filename),
                };                
            }
        }

        public EnvironmentFile this[string key]
        {
            get { return EnvironmentDictionary[key]; }
            set { EnvironmentDictionary[key] = value; }
        }

        void IDeserializationCallback.OnDeserialization(Object sender)
        {
            ExtractionsRequired = new List<Tuple<object, string>>();
            ((INotifyCollectionChanged)EnvironmentDictionary).CollectionChanged += DictionaryChanged;
        }

        public static Task<RangeComplexToken> LoadAsync(string filename) { return TaskEx.Run(() => Load(filename)); }

        [NonSerialized] DateTime _lastWriteTime;
        [NonSerialized] string _fileName;
        [NonSerialized] string _dataPath;

        [NonSerialized] public List<Tuple<object, string>> ExtractionsRequired = new List<Tuple<object, string>>();
        [NonSerialized] public bool ReextractionRequired;

        ObservableConcurrentDictionary<string, EnvironmentFile> EnvironmentDictionary { get; set; }
        public GeoRect GeoRect { get; set; }
        public DateTime LastWriteTime { get { return _lastWriteTime; } }
        IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }
        public IEnumerator<KeyValuePair<string, EnvironmentFile>> GetEnumerator()
        {
            return EnvironmentDictionary.GetEnumerator();
        }

        /// <summary>
        ///   Event raised when the collection changes.
        /// </summary>
        [NonSerialized]
        private NotifyCollectionChangedEventHandler _collectionChanged;
        public event NotifyCollectionChangedEventHandler CollectionChanged
        {
            [MethodImpl(MethodImplOptions.Synchronized)]
            add
            {
                _collectionChanged = (NotifyCollectionChangedEventHandler)Delegate.Combine(_collectionChanged, value);
            }
            [MethodImpl(MethodImplOptions.Synchronized)]
            remove
            {
                _collectionChanged = (NotifyCollectionChangedEventHandler)Delegate.Remove(_collectionChanged, value);
            }
        }
        [MethodImpl(MethodImplOptions.Synchronized)]
        protected virtual void OnCollectionChanged(NotifyCollectionChangedEventArgs e)
        {
            //if (Name != null) Debug.WriteLine("{0} {1} [{2}]", DateTime.Now, Name, e.Action);
            var handlers = _collectionChanged;
            if (handlers == null) return;
            foreach (NotifyCollectionChangedEventHandler handler in handlers.GetInvocationList())
            {
                var localHandler = handler;
                try
                {
                    if (handler.Target is DispatcherObject) ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(this, e));
                    else handler(this, e);
                }
                catch (Exception) {}
            }
        }
    }
}