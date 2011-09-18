using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Threading.Tasks;
using System.Windows.Threading;
using Cinch;
using ESME.Environment.NAVO;
using HRC.Collections;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    [Serializable]
    public class EnvironmentFile : INotifyPropertyChanged, IDeserializationCallback
    {
        protected EnvironmentFile() { }

        public EnvironmentFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect, EnvironmentDataType dataType, NAVOTimePeriod timePeriod)
        {
            FileName = fileName;
            DataPath = dataPath;
            IsCached = false;
            SampleCount = sampleCount;
            GeoRect = geoRect;
            DataType = dataType;
            TimePeriod = timePeriod;
            RequiredFiles = new List<EnvironmentFile>();
        }

        void IDeserializationCallback.OnDeserialization(object sender)
        {
            RequiredFiles = new List<EnvironmentFile>();
        }

        public void UpdateFileInfo()
        {
            if (string.IsNullOrEmpty(DataPath)) return;
            var filePath = Path.Combine(DataPath, FileName);
            IsCached = false;
            if (!File.Exists(filePath)) return;
            IsCached = true;
            var info = new FileInfo(filePath);
            FileSize = info.Length;
            LastWriteTime = info.LastWriteTime;
        }

        public string DataPath
        {
            get { return _dataPath; }
            set
            {
                _dataPath = value;
                UpdateFileInfo();
            }
        }
        [NonSerialized] string _dataPath;

        public string Name { get { return _name ?? (_name = Path.GetFileNameWithoutExtension(FileName)); } }
        [NonSerialized] string _name;

        public bool IsValid
        {
            get
            {
                if (DataPath == null) throw new ApplicationException("DataPath is not set");
                var info = new FileInfo(Path.Combine(DataPath, FileName));
                return (FileSize == info.Length) && (LastWriteTime == info.LastWriteTime);
            }
        }

        public virtual Task GetDataAsync() { throw new NotImplementedException(); }

        #region public bool IsCached { get; set; }

        public bool IsCached
        {
            get { return _isCached; }
            set
            {
                if (_isCached == value) return;
                _isCached = value;
                NotifyPropertyChanged(IsCachedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsCachedChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentFile>(x => x.IsCached);
        bool _isCached;

        #endregion
        #region public string FileName { get; private set; }

        public string FileName
        {
            get { return _fileName; }
            private set
            {
                if (_fileName == value) return;
                _fileName = value;
                NotifyPropertyChanged(FileNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FileNameChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentFile>(x => x.FileName);
        string _fileName;

        #endregion
        #region public long FileSize { get; private set; }

        public long FileSize
        {
            get { return _fileSize; }
            private set
            {
                if (_fileSize == value) return;
                _fileSize = value;
                NotifyPropertyChanged(FileSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FileSizeChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentFile>(x => x.FileSize);
        long _fileSize;

        #endregion
        #region public DateTime LastWriteTime { get; private set; }

        public DateTime LastWriteTime
        {
            get { return _lastWriteTime; }
            private set
            {
                if (_lastWriteTime == value) return;
                _lastWriteTime = value;
                NotifyPropertyChanged(LastWriteTimeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LastWriteTimeChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentFile>(x => x.LastWriteTime);
        DateTime _lastWriteTime;

        #endregion
        #region public uint SampleCount { get; internal set; }

        public uint SampleCount
        {
            get { return _sampleCount; }
            internal set
            {
                if (_sampleCount == value) return;
                _sampleCount = value;
                NotifyPropertyChanged(SampleCountChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SampleCountChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentFile>(x => x.SampleCount);
        uint _sampleCount;

        #endregion
        #region public GeoRect GeoRect { get; set; }

        public GeoRect GeoRect
        {
            get { return _geoRect; }
            internal set
            {
                if (_geoRect == value) return;
                _geoRect = value;
                NotifyPropertyChanged(GeoRectChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs GeoRectChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentFile>(x => x.GeoRect);
        GeoRect _geoRect;

        #endregion
        #region public DataAvailability DataAvailability { get; protected set; }
        public DataAvailability DataAvailability
        {
            get { return _dataAvailability; }
            protected set
            {
                if (_dataAvailability == value) return;
                _dataAvailability = value;
                Debug.WriteLine("{0} DataAvailability for {1} changed to {2}", DateTime.Now, FileName, _dataAvailability);
                OnDataAvailabilityChanged(_dataAvailability);
                NotifyPropertyChanged(DataAvailabilityChangedEventArgs);
            }
        }
        [NonSerialized]
        DataAvailability _dataAvailability = DataAvailability.NotLoaded;
        static readonly PropertyChangedEventArgs DataAvailabilityChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentFile>(x => x.DataAvailability);

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
        public EnvironmentDataType DataType { get; protected set; }
        public NAVOTimePeriod TimePeriod { get; private set; }
        public List<EnvironmentFile> RequiredFiles { get; internal set; }

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
        protected EnvironmentFile() { Initialize(); }

        public EnvironmentFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect, EnvironmentDataType dataType, NAVOTimePeriod timePeriod)
            : base(dataPath, fileName, sampleCount, geoRect, dataType, timePeriod) { Initialize(); }

        void IDeserializationCallback.OnDeserialization(object sender)
        {
            Initialize();
        }

        public void Initialize()
        {
            DataAvailability = DataAvailability.NotLoaded;
            Months = new ObservableConcurrentDictionary<NAVOTimePeriod, EnvironmentFile<T>>();
            MonthsList = ObservableList<EnvironmentFile<T>>.FromObservableConcurrentDictionary(Months, kvp => kvp.Value, (kvp, ef) => kvp.Key == ef.TimePeriod);
            _sourceTasks = new List<Task>();
        }

        public override Task GetDataAsync() { return GetMyDataAsync(); }
        public Task<T> GetMyDataAsync()
        {
            if (DataTask == null) throw new ApplicationException("DataTask is null");
            if (DataTask.Status != TaskStatus.Created) return DataTask;
            DataTask.Start();
            DataAvailability = DataAvailability.Loading;
            DataTask.ContinueWith(task => DataAvailability = DataAvailability.Available);
            return DataTask;
        }

        async Task GetRequiredFilesAsync()
        {
            if (RequiredFiles.Count == 0) return;
            var tasks = RequiredFiles.Select(file => file.GetDataAsync()).ToList();
            await TaskEx.WhenAll(tasks);
        }

        public void LinkTo<TSource, TData>(TSource sourceMonth)
            where TSource : EnvironmentFile<TData>
            where TData : class
        {
            _sourceTasks.Add(sourceMonth.DataTask);
        }

        [NonSerialized] List<Task> _sourceTasks;

        public Task<T> DataTask
        {
            [MethodImpl(MethodImplOptions.Synchronized)]
            get
            {
                return _dataTask;
            }
            [MethodImpl(MethodImplOptions.Synchronized)]
            set
            {
                if (_dataTask != null) throw new ApplicationException("DataTask is already set");
                _dataTask = value;
            }
        }
        [NonSerialized] Task<T> _dataTask;

        public T Data { get { return _data ?? (_data = DataTask.Result); } }

        [NonSerialized] T _data;

        #region public ObservableConcurrentDictionary<NAVOTimePeriod, EnvironmentFile<T>> Months { get; set; }

        public ObservableConcurrentDictionary<NAVOTimePeriod, EnvironmentFile<T>> Months
        {
            get { return _months; }
            set
            {
                if (_months == value) return;
                _months = value;
                NotifyPropertyChanged(MonthsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MonthsChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentFile<T>>(x => x.Months);
        [NonSerialized] ObservableConcurrentDictionary<NAVOTimePeriod, EnvironmentFile<T>> _months;

        #endregion

        #region public ObservableList<EnvironmentFile<T>> MonthsList { get; set; }

        public ObservableList<EnvironmentFile<T>> MonthsList
        {
            get { return _monthsList; }
            private set
            {
                if (_monthsList == value) return;
                _monthsList = value;
                NotifyPropertyChanged(MonthsListChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MonthsListChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentFile<T>>(x => x.MonthsList);
        [NonSerialized] ObservableList<EnvironmentFile<T>> _monthsList;

        #endregion

    }

    [Serializable]
    public class TemperatureFile : EnvironmentFile<SoundSpeed>
    {
        public TemperatureFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect, EnvironmentDataType dataType, NAVOTimePeriod timePeriod)
            : base(dataPath, fileName, sampleCount, geoRect, dataType, timePeriod) { }

        public TemperatureFile() { DataType = EnvironmentDataType.Temperature; }
    }

    [Serializable]
    public class SalinityFile : EnvironmentFile<SoundSpeed>
    {
        public SalinityFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect, EnvironmentDataType dataType, NAVOTimePeriod timePeriod)
            : base(dataPath, fileName, sampleCount, geoRect, dataType, timePeriod) { }

        public SalinityFile() { DataType = EnvironmentDataType.Salinity; }
    }

    [Serializable]
    public class SoundSpeedFile : EnvironmentFile<SoundSpeed>
    {
        public SoundSpeedFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect, EnvironmentDataType dataType, NAVOTimePeriod timePeriod)
            : base(dataPath, fileName, sampleCount, geoRect, dataType, timePeriod) { }

        public SoundSpeedFile()
        {
            DataType = EnvironmentDataType.SoundSpeed;
            IsCached = false;
        }

        public async Task<SoundSpeed> FooAsync(IDictionary<string, EnvironmentFile> environmentFiles, EnvironmentFile<Bathymetry> bathymetryFile)
        {
            var months = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(TimePeriod).ToList();

            var sources = (from month in months
                                select new
                                {
                                    Month = month,
                                    TemperatureFile = (TemperatureFile)environmentFiles[string.Format("{0}.temperature", TimePeriod)],
                                    TemperatureDataTask = ((TemperatureFile)environmentFiles[string.Format("{0}.temperature", TimePeriod)]).GetMyDataAsync(),
                                    SalinityFile = (TemperatureFile)environmentFiles[string.Format("{0}.salinity", TimePeriod)],
                                    SalinityDataTask = ((TemperatureFile)environmentFiles[string.Format("{0}.salinity", TimePeriod)]).GetMyDataAsync(),
                                }).ToDictionary(item => item.Month);
            var sourceTasks = new List<Task>();
            foreach (var month in months)
            {
                sourceTasks.Add(sources[month].TemperatureDataTask);
                sourceTasks.Add(sources[month].SalinityDataTask);
            }
            sourceTasks.Add(bathymetryFile.GetMyDataAsync());
            await TaskEx.WhenAll(sourceTasks);
            var soundSpeedFields = (from month in months
                                    select new
                                    {
                                        Month = month,
                                        SoundSpeedField = SoundSpeedField.Create(sources[month].TemperatureFile.Data[month],
                                                                                 sources[month].SalinityFile.Data[month],
                                                                                 bathymetryFile.Data.DeepestPoint,
                                                                                 bathymetryFile.GeoRect),
                                    }).ToDictionary(item => item.Month);
            var monthlySoundSpeeds = new SoundSpeed();
            foreach (var month in months) monthlySoundSpeeds.SoundSpeedFields.Add(soundSpeedFields[month].SoundSpeedField);
            return SoundSpeed.Average(monthlySoundSpeeds, new List<NAVOTimePeriod>{TimePeriod});
        }
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
            : base(dataPath, fileName, sampleCount, geoRect, dataType, timePeriod)
        {
            DataTask = new Task<Bathymetry>(() => Bathymetry.Load(Path.Combine(dataPath, fileName)));
        }
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
        SoundSpeed,
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