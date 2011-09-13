using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization;
using System.Threading.Tasks;
using ESME.Environment.NAVO;
using HRC.Collections;
using HRC.Navigation;

namespace ESME.Environment.Descriptors
{
    [Serializable]
    public class EnvironmentFile
    {
        public EnvironmentFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect)
        {
            DataPath = dataPath;
            FileName = fileName;
            var info = new FileInfo(Path.Combine(dataPath, fileName));
            FileSize = info.Length;
            LastWriteTime = info.LastWriteTime;
            SampleCount = sampleCount;
            GeoRect = geoRect;
        }

        public string DataPath
        {
            get { return _dataPath; }
            set { _dataPath = value; }
        }
        [NonSerialized] string _dataPath;

        public bool IsValid
        {
            get
            {
                if (DataPath == null) throw new ApplicationException("DataPath is not set");
                var info = new FileInfo(Path.Combine(DataPath, FileName));
                return (FileSize == info.Length) && (LastWriteTime == info.LastWriteTime);
            }
        }

        public string FileName { get; private set; }
        public long FileSize { get; private set; }
        public DateTime LastWriteTime { get; private set; }
        public uint SampleCount { get; private set; }
        public GeoRect GeoRect { get; private set; }
    }

    public class EnvironmentFileList<T> : Dictionary<string, EnvironmentFile<T>> where T : class {}

    [Serializable]
    public class EnvironmentFile<T> : EnvironmentFile, IDeserializationCallback where T : class
    {
        public EnvironmentFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect) 
            : base(dataPath, fileName, sampleCount, geoRect) 
        { }

        public Func<Task<T>> GetAsyncFunc { get { return _getAsyncFunc; } set { _getAsyncFunc = value; } }
        [NonSerialized] Func<Task<T>> _getAsyncFunc;

        public Task<T> AsyncData
        {
            get
            {
                lock (_lockObject)
                {
                    if (_inFlightFunc != null) return _inFlightFunc;
                    if (GetAsyncFunc == null) throw new ApplicationException("GetAsyncFunc is null");
                    _inFlightFunc = GetAsyncFunc();
                    return _inFlightFunc;
                }
            }
        }
        [NonSerialized] Task<T> _inFlightFunc;
        [NonSerialized] object _lockObject = new object();
        public T Data
        {
            get
            {
                if ((_data == null) && (!AsyncData.IsCompleted)) 
                    AsyncData.ContinueWith(task => _data = task.Result).Wait();
                return _data;
            }
        }
        [NonSerialized] T _data;
        public void OnDeserialization(object sender) { _lockObject = new object(); }
    }

    public class TimePeriodEnvironmentFile<T> : EnvironmentFile<T> where T : class
    {
        public TimePeriodEnvironmentFile(NAVOTimePeriod timePeriod, string dataPath, string fileName, uint sampleCount, GeoRect geoRect)
            : base(dataPath, fileName, sampleCount, geoRect) { TimePeriod = timePeriod; }

        public NAVOTimePeriod TimePeriod { get; private set; }
    }

    public class TimePeriodEnvironmentFileList<T> : ObservableConcurrentDictionary<NAVOTimePeriod, TimePeriodEnvironmentFile<T>> where T : class
    {
    }
}