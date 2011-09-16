using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Threading.Tasks;
using System.Windows.Threading;
using Cinch;
using ESME.Environment.NAVO;
using HRC.Navigation;

namespace ESME.Environment.Descriptors
{
    [Serializable]
    public class EnvironmentFile : INotifyPropertyChanged
    {
        public EnvironmentFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect, EnvironmentDataType dataType, NAVOTimePeriod timePeriod)
        {
            DataPath = dataPath;
            FileName = fileName;
            var info = new FileInfo(Path.Combine(dataPath, fileName));
            FileSize = info.Length;
            LastWriteTime = info.LastWriteTime;
            SampleCount = sampleCount;
            GeoRect = geoRect;
            DataType = dataType;
            TimePeriod = timePeriod;
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
        public EnvironmentDataType DataType { get; private set; }
        public NAVOTimePeriod TimePeriod { get; private set; }

        #region public DataAvailability DataAvailability { get; protected set; }
        public DataAvailability DataAvailability
        {
            get { return _dataAvailability; }
            protected set
            {
                if (_dataAvailability == value) return;
                _dataAvailability = value;
                OnDataAvailabilityChanged(_dataAvailability);
            }
        }
        [NonSerialized]
        DataAvailability _dataAvailability = DataAvailability.NotLoaded;


        [NonSerialized]
        private EventHandler<DataAvailabilityChangedEventArgs> _dataAvailabilityChanged;
        public event EventHandler<DataAvailabilityChangedEventArgs> DataAvailabilityChanged
        {
            [MethodImpl(MethodImplOptions.Synchronized)]
            add
            {
                _dataAvailabilityChanged = (EventHandler<DataAvailabilityChangedEventArgs>)Delegate.Combine(_dataAvailabilityChanged, value);
            }
            [MethodImpl(MethodImplOptions.Synchronized)]
            remove
            {
                _dataAvailabilityChanged = (EventHandler<DataAvailabilityChangedEventArgs>)Delegate.Remove(_dataAvailabilityChanged, value);
            }
        }
        [MethodImpl(MethodImplOptions.Synchronized)]
        protected virtual void OnDataAvailabilityChanged(DataAvailability availability)
        {
            var eventArgs = new DataAvailabilityChangedEventArgs(availability);
            //if (Name != null) Debug.WriteLine("{0} {1} [{2}]", DateTime.Now, Name, e.Action);
            var handlers = _dataAvailabilityChanged;
            if (handlers == null) return;
            foreach (EventHandler<DataAvailabilityChangedEventArgs> handler in handlers.GetInvocationList())
            {
                var localHandler = handler;
                try
                {
                    if (handler.Target is DispatcherObject) ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(this, eventArgs));
                    else handler(this, eventArgs);
                }
                catch (Exception) { }
            }
        }
        #endregion

        #region INotifyPropertyChanged Members

        [NonSerialized]
        private PropertyChangedEventHandler _propertyChanged;
        public event PropertyChangedEventHandler PropertyChanged
        {
            [MethodImpl(MethodImplOptions.Synchronized)]
            add
            {
                _propertyChanged = (PropertyChangedEventHandler)Delegate.Combine(_propertyChanged, value);
            }
            [MethodImpl(MethodImplOptions.Synchronized)]
            remove
            {
                _propertyChanged = (PropertyChangedEventHandler)Delegate.Remove(_propertyChanged, value);
            }
        }
        protected void NotifyPropertyChanged(PropertyChangedEventArgs e)
        {
            var handlers = _propertyChanged;
            if (handlers == null) return;
            foreach (PropertyChangedEventHandler handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => handler(this, e));
                else
                    handler(this, e);
            }
        }

        #endregion
    }

    [Serializable]
    public class EnvironmentFile<T> : EnvironmentFile, IDeserializationCallback where T : class
    {
        public EnvironmentFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect, EnvironmentDataType dataType, NAVOTimePeriod timePeriod)
            : base(dataPath, fileName, sampleCount, geoRect, dataType, timePeriod) { DataAvailability = DataAvailability.NotLoaded; }

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
                    DataAvailability = DataAvailability.Loading;
                    _inFlightFunc.ContinueWith(task => DataAvailability = DataAvailability.Available);
                    return _inFlightFunc;
                }
            }
        }
        public T Data { get { return _data ?? (_data = AsyncData.Result); } }

        [NonSerialized] Task<T> _inFlightFunc;
        [NonSerialized] object _lockObject = new object();
        [NonSerialized] T _data;
        
        public void OnDeserialization(object sender)
        {
            _lockObject = new object();
            DataAvailability = DataAvailability.NotLoaded;
        }

    }

    [Serializable]
    public class TemperatureFile : EnvironmentFile<SoundSpeed>
    {
        public TemperatureFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect, EnvironmentDataType dataType, NAVOTimePeriod timePeriod)
            : base(dataPath, fileName, sampleCount, geoRect, dataType, timePeriod) { }
    }

    [Serializable]
    public class SalinityFile : EnvironmentFile<SoundSpeed>
    {
        public SalinityFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect, EnvironmentDataType dataType, NAVOTimePeriod timePeriod)
            : base(dataPath, fileName, sampleCount, geoRect, dataType, timePeriod) { }
    }

    [Serializable]
    public class SedimentFile : EnvironmentFile<Sediment>
    {
        public SedimentFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect, EnvironmentDataType dataType, NAVOTimePeriod timePeriod)
            : base(dataPath, fileName, sampleCount, geoRect, dataType, timePeriod) { }
    }

    [Serializable]
    public class WindFile : EnvironmentFile<Wind>
    {
        public WindFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect, EnvironmentDataType dataType, NAVOTimePeriod timePeriod)
            : base(dataPath, fileName, sampleCount, geoRect, dataType, timePeriod) { }
    }

    [Serializable]
    public class BathymetryFile : EnvironmentFile<Bathymetry>
    {
        public BathymetryFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect, EnvironmentDataType dataType, NAVOTimePeriod timePeriod, bool isCached)
            : base(dataPath, fileName, sampleCount, geoRect, dataType, timePeriod) { IsCached = isCached; }

        public bool IsCached { get; private set; }
    }

    [Serializable]
    public class BottomLossFile : EnvironmentFile<BottomLoss>
    {
        public BottomLossFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect, EnvironmentDataType dataType, NAVOTimePeriod timePeriod)
            : base(dataPath, fileName, sampleCount, geoRect, dataType, timePeriod) { }
    }

    public enum EnvironmentDataType
    {
        Temperature,
        Salinity,
        Sediment,
        Wind,
        Bathymetry,
        BottomLoss,
    }

    public class DataAvailabilityChangedEventArgs : EventArgs
    {
        public DataAvailabilityChangedEventArgs(DataAvailability dataAvailability) { DataAvailability = dataAvailability; }
        public DataAvailability DataAvailability { get; private set; }
    }

    public enum DataAvailability
    {
        NotLoaded,
        Loading,
        Available
    }
}