using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Media;
using System.Windows.Threading;
using Cinch;
using ESME.Environment.NAVO;
using ESME.NEMO;
using HRC.Collections;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    [Serializable]
    public class EnvironmentFile : INotifyPropertyChanged, IDeserializationCallback
    {
        protected EnvironmentFile() { }

        public EnvironmentFile(string dataPath, string fileName, uint sampleCount, GeoRect geoRect, EnvironmentDataType dataType, TimePeriod timePeriod, float resolution)
        {
            Resolution = resolution;
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
        #region public float Resolution { get; private set; }

        public float Resolution
        {
            get { return _resolution; }
            private set
            {
                if (Math.Abs(_resolution - value) < 0.00001f) return;
                _resolution = value;
                NotifyPropertyChanged(ResolutionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ResolutionChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentFile>(x => x.Resolution);
        float _resolution;

        #endregion

        public EnvironmentDataType DataType { get; protected set; }
        public TimePeriod TimePeriod { get; private set; }
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

        public static SoundSpeed CalculateSoundSpeed(RangeComplex rangeComplex, TimePeriod timePeriod, Task<Bathymetry> bathymetryTask, GeoRect bathymetryBounds)
        {
            if (rangeComplex == null) throw new ArgumentException("rangeComplex");
            if (timePeriod == TimePeriod.Invalid) throw new ArgumentException("timePeriod");
            if (bathymetryTask == null) throw new ArgumentNullException("bathymetryTask");
            if (bathymetryBounds == null) throw new ArgumentNullException("bathymetryBounds");

            Debug.WriteLine("{0} SSP: Loading bathymetry", DateTime.Now);
            if (bathymetryTask.Status == TaskStatus.Created) bathymetryTask.Start();
            var months = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod).ToList();
            Debug.WriteLine("{0} SSP: Computing soundspeed for {1}", DateTime.Now, timePeriod);
            var sources = (from month in months
                           select new
                           {
                               Month = month,
                               TemperatureTask = new Task<SoundSpeed>(() => SoundSpeed.Load(Path.Combine(rangeComplex.DataPath, string.Format("{0}.temperature", month)))),
                               SalinityTask = new Task<SoundSpeed>(() => SoundSpeed.Load(Path.Combine(rangeComplex.DataPath, string.Format("{0}.salinity", month)))),
                           }).ToDictionary(item => item.Month);
            var sourceTasks = new List<Task>();
            foreach (var month in months)
            {
                Debug.WriteLine("{0} SSP: Loading temperature and salinity data for {1}", DateTime.Now, month);
                sources[month].TemperatureTask.Start();
                sources[month].SalinityTask.Start();
                sourceTasks.Add(sources[month].TemperatureTask);
                sourceTasks.Add(sources[month].SalinityTask);
            }
            sourceTasks.Add(bathymetryTask);
            var continuation = TaskEx.WhenAll(sourceTasks).ContinueWith(task =>
            {
                Debug.WriteLine("{0} SSP: Required data loaded.  Computing monthly sound speeds fields", DateTime.Now);
                var soundSpeedFields = (from month in months
                                        select new
                                        {
                                            Month = month,
                                            SoundSpeedField = SoundSpeedField.Create(sources[month].TemperatureTask.Result[month],
                                                                                     sources[month].SalinityTask.Result[month],
                                                                                     bathymetryTask.Result.DeepestPoint,
                                                                                     bathymetryBounds),
                                        }).ToDictionary(item => item.Month);
                Debug.WriteLine("{0} SSP: Monthly sound speeds fields computed", DateTime.Now);
                var monthlySoundSpeeds = new SoundSpeed();
                foreach (var month in months)
                {
                    Debug.WriteLine("{0} SSP: Releasing temperature and salinity data for {1}", DateTime.Now, month);
                    sources[month].TemperatureTask.Dispose();
                    sources[month].SalinityTask.Dispose();
                    monthlySoundSpeeds.Add(soundSpeedFields[month].SoundSpeedField);
                }
                Debug.WriteLine("{0} SSP: Computing average sound speed for {1}", DateTime.Now, timePeriod);
                var result = SoundSpeed.Average(monthlySoundSpeeds, new List<TimePeriod> { timePeriod });
                Debug.WriteLine("{0} SSP: Returning average sound speed for {1}", DateTime.Now, timePeriod);
                return result;
            });
            return continuation.Result;            
        }

        public static SoundSpeed SeasonalAverage(RangeComplex rangeComplex, TimePeriod timePeriod, EnvironmentDataType dataType)
        {
            if (rangeComplex == null) throw new ArgumentException("rangeComplex");
            if (timePeriod == TimePeriod.Invalid) throw new ArgumentException("timePeriod");
            string fileType;
            switch (dataType)
            {
                case EnvironmentDataType.Salinity:
                    fileType = "salinity";
                    break;
                case EnvironmentDataType.Temperature:
                    fileType = "temperature";
                    break;
                default:
                    throw new ArgumentException("Must be Salinity or Temperature", "dataType");
            }
            var months = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod).ToList();
            var soundSpeed = new SoundSpeed();
            var sources = (from month in months
                           select new
                           {
                               Month = month,
                               DataTask = new Task<SoundSpeed>(() =>
                               {
                                   var result = SoundSpeed.Load(Path.Combine(rangeComplex.DataPath,
                                                                string.Format("{0}.{1}", month, fileType)));
                                   lock(soundSpeed) soundSpeed.SoundSpeedFields.Add(result[month]);
                                   return result;
                               }),
                           }).ToDictionary(item => item.Month);
            var sourceTasks = new List<Task>();
            foreach (var month in months)
            {
                Debug.WriteLine("{0} AVG: Loading {1} data for {2}", DateTime.Now, fileType, month);
                sources[month].DataTask.Start();
                sourceTasks.Add(sources[month].DataTask);
            }
            var continuation = TaskEx.WhenAll(sourceTasks).ContinueWith(task =>
            {
                if (months.Count == 1)
                {
                    Debug.WriteLine("{0} AVG: Requested data for a single month.  Returning loaded {1} data", DateTime.Now, fileType);
                    return sources[timePeriod].DataTask.Result;
                }
                Debug.WriteLine("{0} AVG: Required data loaded. Computing average {1} field for {2}", DateTime.Now, fileType, timePeriod);
                var result = new SoundSpeed();
                result.SoundSpeedFields.Add(SoundSpeed.Average(soundSpeed, timePeriod));
                Debug.WriteLine("{0} AVG: Returning average {1} field for {2}", DateTime.Now, fileType, timePeriod);
                return result;
            });
            return continuation.Result;
        }
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
}