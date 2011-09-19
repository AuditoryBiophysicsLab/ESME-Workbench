using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using System.Windows.Threading;
using Cinch;
using ESME.Environment.NAVO;
using HRC;
using HRC.Navigation;
using HRC.Collections;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    public class NewRangeComplex : ViewModelBase
    {
        public static NewRangeComplex None = new NewRangeComplex {Name = "None"};
        NewRangeComplex() { }

        NewRangeComplex(string simAreaPath, string rangeComplexName, bool isCreate, Dispatcher dispatcher)
        {
            IsLoading = true;
            _dispatcher = dispatcher;
            var rangeComplexPath = Path.Combine(simAreaPath, rangeComplexName);
            Name = rangeComplexName;
            RangeComplexPath = rangeComplexPath;
            AreasPath = Path.Combine(rangeComplexPath, "Areas");
            BathymetryPath = Path.Combine(rangeComplexPath, "Bathymetry");
            DataPath = Path.Combine(rangeComplexPath, "Data");
            EnvironmentPath = Path.Combine(rangeComplexPath, "Environment");
            ImagesPath = Path.Combine(rangeComplexPath, "Images");
            SpeciesPath = Path.Combine(rangeComplexPath, "Species");
            if (isCreate)
            {
                Directory.CreateDirectory(RangeComplexPath);
                Directory.CreateDirectory(AreasPath);
            }
            else
            {
                if (!Directory.Exists(RangeComplexPath) || !Directory.Exists(AreasPath))
                {
                    RangeComplexes.Singleton.DeleteRangeComplexFromDisk(rangeComplexName);
                    throw new InvalidOperationException(string.Format("The range complex \"{0}\" is missing critical files or directories.\r\nThe range complex has been deleted", Name));
                }
            }
            Directory.CreateDirectory(BathymetryPath);
            Directory.CreateDirectory(DataPath);
            Directory.CreateDirectory(EnvironmentPath);
            Directory.CreateDirectory(ImagesPath);
            Directory.CreateDirectory(SpeciesPath);
            Directory.CreateDirectory(Path.Combine(RangeComplexPath, "GeographicAreas"));

            TemperatureFile = new TemperatureFile();
            SalinityFile = new SalinityFile();
            SoundSpeedFile = new SoundSpeedFile();

            EnvironmentFiles = RangeComplexToken.Load(Path.Combine(DataPath, Name + ".token"));
            EnvironmentList = EnvironmentFiles.GetObservableWrapper<EnvironmentFile>();
            _dispatcher.InvokeIfRequired(() =>
            {
                AreaCollection = new ObservableConcurrentDictionary<string, RangeComplexArea>();
                AreaList = ObservableList<RangeComplexArea>.FromObservableConcurrentDictionary(AreaCollection, kvp => kvp.Value, (kvp, ac) => kvp.Key == ac.Name, kvp => kvp.Value.Name);
            });
            IsLoading = false;
            UpdateAreas();
            if ((EnvironmentFiles.GeoRect == null) || (!EnvironmentFiles.GeoRect.Contains(GeoRect))) EnvironmentFiles.ReextractionRequired = true;

            foreach (var envFile in EnvironmentFiles)
            {
                switch (envFile.Value.DataType)
                {
                    case EnvironmentDataType.Bathymetry:
                        throw new NotImplementedException();
                    case EnvironmentDataType.BottomLoss:
                        BottomLossFile = (BottomLossFile)envFile.Value;
                        break;
                    case EnvironmentDataType.Salinity:
                        var salinityFile = (SalinityFile)envFile.Value;
                        SalinityFile.Months.Add(salinityFile.TimePeriod, salinityFile);
                        break;
                    case EnvironmentDataType.Sediment:
                        SedimentFile = (SedimentFile)envFile.Value;
                        break;
                    case EnvironmentDataType.Temperature:
                        var temperatureFile = (TemperatureFile)envFile.Value;
                        TemperatureFile.Months.Add(temperatureFile.TimePeriod, temperatureFile);
                        break;
                    case EnvironmentDataType.Wind:
                        WindFile = (WindFile)envFile.Value;
                        break;
                }
            }
        }

        [NotNull] readonly Dispatcher _dispatcher;

        [NotNull] public string Name { get; private set; }
        [NotNull] public string RangeComplexPath { get; private set; }
        [NotNull] public string AreasPath { get; private set; }
        [NotNull] public string BathymetryPath { get; private set; }
        [NotNull] public string DataPath { get; private set; }
        [NotNull] public string EnvironmentPath { get; private set; }
        [NotNull] public string ImagesPath { get; private set; }
        [NotNull] public string SpeciesPath { get; private set; }

        [NotNull] public RangeComplexToken EnvironmentFiles { get; private set; }
        [NotNull] public RangeComplexArea OpArea { get; private set; }
        [NotNull] public RangeComplexArea SimArea { get; private set; }

        [NotNull] public ObservableConcurrentDictionary<string, RangeComplexArea> AreaCollection { get; private set; }
        [NotNull] public ObservableList<RangeComplexArea> AreaList { get; private set; }
        [NotNull] public ObservableList<EnvironmentFile> EnvironmentList { get; private set; }

        public WindFile WindFile { get; private set; }
        public BottomLossFile BottomLossFile { get; private set; }
        public SedimentFile SedimentFile { get; private set; }
        public TemperatureFile TemperatureFile { get; private set; }
        public SoundSpeedFile SoundSpeedFile { get; private set; }
        public SalinityFile SalinityFile { get; private set; }

        public RangeComplexArea this[string areaName] { get { return AreaCollection[areaName]; } }

        #region public GeoRect GeoRect { get; private set; }

        /// <summary>
        /// A GeoRect that encompasses all RangeComplexAreas in this range complex
        /// </summary>
        [NotNull]
        public GeoRect GeoRect
        {
            get { return _geoRect; }
            private set
            {
                if (_geoRect == null)
                {
                    _geoRect = value;
                }
                else
                {
                    if (_geoRect.Contains(value)) return;
                    _geoRect = value;
                    OnGeoRectChanged();
                }
            }
        }

        GeoRect _geoRect;
        #endregion

        #region public event EventHandler<EventArgs> GeoRectChanged;
        public event EventHandler<EventArgs> GeoRectChanged;
        readonly EventArgs _eventArgs = new EventArgs();
        protected virtual void OnGeoRectChanged()
        {
            var handlers = GeoRectChanged;
            if (handlers == null) return;
            foreach (EventHandler handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => handler(this, _eventArgs));
                else
                    handler(this, _eventArgs);
            }
        }
        #endregion

        void UpdateAreas()
        {
            AreaCollection.Clear();
            foreach (var areaFile in Directory.EnumerateFiles(AreasPath, "*.ovr"))
            {
                var areaName = Path.GetFileNameWithoutExtension(areaFile);
                _dispatcher.InvokeInBackgroundIfRequired(() => AreaCollection.Add(areaName, RangeComplexArea.Read(this, areaName)));
            }
            GeoRect = GeoRect.Union(AreaCollection.Values.Select(area => area.GeoRect).ToArray());
            EnvironmentFiles.GeoRect = GeoRect;
        }

        #region Validation
        void ValidateEnvironment()
        {
            var result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.Sediment, 5, true);
            if (result != null) QueueImportJob(result.Item2);
            result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.BottomLoss, 15, true);
            if (result != null) QueueImportJob(result.Item2);
            result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.Wind, 60, true);
            if (result != null) QueueImportJob(result.Item2);
            foreach (var month in NAVOConfiguration.AllMonths)
            {
                result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.Temperature, 15, true, month);
                if (result != null) QueueImportJob(result.Item2);
                result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.Salinity, 15, true, month);
                if (result != null) QueueImportJob(result.Item2);
                CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.SoundSpeed, 15, false, month);
                //LinkToSourceMonths(result.Item1, month, EnvironmentDataType.Temperature);
                //LinkToSourceMonths(result.Item1, month, EnvironmentDataType.Salinity);
            }
            foreach (var season in NAVOConfiguration.AllSeasons)
            {
                CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.SoundSpeed, 15, false, season);
                //LinkToSourceMonths(result.Item1, season, EnvironmentDataType.SoundSpeed);
            }
        }
        
        void LinkToSourceMonths<TEnvironment, TData>(TEnvironment envFile, NAVOTimePeriod timePeriod, EnvironmentDataType sourceType) where TEnvironment: EnvironmentFile<TData> where TData : class
        {
            var months = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod).ToList();
            envFile.RequiredFiles.Clear();
            foreach (var month in months)
            {
                string key;
                switch (sourceType)
                {
                    case EnvironmentDataType.Wind:
                    case EnvironmentDataType.Sediment:
                    case EnvironmentDataType.BottomLoss:
                        key = string.Format("data.{0}", sourceType.ToString().ToLower());
                        break;
                    case EnvironmentDataType.Salinity:
                    case EnvironmentDataType.SoundSpeed:
                    case EnvironmentDataType.Temperature:
                        key = string.Format("{0}.{1}", timePeriod, sourceType.ToString().ToLower());
                        break;
                    default:
                        throw new ApplicationException(string.Format("Unknown environment data type: {0}", sourceType));
                }
                envFile.LinkTo<TEnvironment, TData>((TEnvironment)EnvironmentFiles[key]);
                envFile.RequiredFiles.Add(EnvironmentFiles[string.Format("{0}.{1}", month, sourceType.ToString().ToLower())]);
            }
        }

        EnvironmentFile NewEnvironmentFile(string fileName, uint sampleCount, EnvironmentDataType dataType, NAVOTimePeriod timePeriod, float resolution)
        {
            switch (dataType)
            {
                case EnvironmentDataType.BottomLoss:
                    return new BottomLossFile(DataPath, fileName, sampleCount, GeoRect, dataType, timePeriod, resolution);
                case EnvironmentDataType.Salinity:
                    return new SalinityFile(DataPath, fileName, sampleCount, GeoRect, dataType, timePeriod, resolution);
                case EnvironmentDataType.Sediment:
                    return new SedimentFile(DataPath, fileName, sampleCount, GeoRect, dataType, timePeriod, resolution);
                case EnvironmentDataType.SoundSpeed:
                    return new SoundSpeedFile(DataPath, fileName, sampleCount, GeoRect, dataType, timePeriod, resolution);
                case EnvironmentDataType.Temperature:
                    return new TemperatureFile(DataPath, fileName, sampleCount, GeoRect, dataType, timePeriod, resolution);
                case EnvironmentDataType.Wind:
                    return new WindFile(DataPath, fileName, sampleCount, GeoRect, dataType, timePeriod, resolution);
                default:
                    throw new ApplicationException(string.Format("Unknown environment data type: {0}", dataType));
            }
        }

        Tuple<EnvironmentFile, ImportJobDescriptor> CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType dataType, float resolution, bool createJobIfRequired, NAVOTimePeriod timePeriod = NAVOTimePeriod.Invalid)
        {
            var samplesPerDegree = 60.0f / resolution;
            var fileName = string.Format("{0}.{1}", timePeriod == NAVOTimePeriod.Invalid ? "data" : timePeriod.ToString(), dataType.ToString().ToLower());
            var north = Math.Round(GeoRect.North * samplesPerDegree) / samplesPerDegree;
            var south = Math.Round(GeoRect.South * samplesPerDegree) / samplesPerDegree;
            var east = Math.Round(GeoRect.East * samplesPerDegree) / samplesPerDegree;
            var west = Math.Round(GeoRect.West * samplesPerDegree) / samplesPerDegree;
            var width = east - west;
            var height = north - south;
            var localPath = Path.Combine(DataPath, fileName);
            var fileInfo = File.Exists(localPath) ? new FileInfo(localPath) : null;
            var sampleCount = (uint)Math.Round(width * samplesPerDegree * height * samplesPerDegree);

            // If the file does not exist, or it's newer than the token, or it's not in the token's list of files, 
            // or it's length is different from the one stored in the token, or it's last write time is different 
            // from the one sorted in the token, then we MIGHT want to re-extract the file from the database
            if ((fileInfo == null) || (fileInfo.LastWriteTime > EnvironmentFiles.LastWriteTime) ||
                (EnvironmentFiles[fileName] == null) || (fileInfo.Length != EnvironmentFiles[fileName].FileSize) ||
                (fileInfo.LastWriteTime != EnvironmentFiles[fileName].LastWriteTime))
            {
                // Given that we MIGHT want to extract the data from the database, let's create an envFile of the appropriate type
                // to contain the metadata we are going to want to know
                EnvironmentFile envFile;
                switch (dataType)
                {
                    case EnvironmentDataType.BottomLoss:
                        envFile = BottomLossFile = (BottomLossFile)NewEnvironmentFile(fileName, sampleCount, dataType, timePeriod, resolution);
                        break;
                    case EnvironmentDataType.Salinity:
                        envFile = SalinityFile.Months[timePeriod] = (SalinityFile)NewEnvironmentFile(fileName, sampleCount, dataType, timePeriod, resolution);
                        break;
                    case EnvironmentDataType.Sediment:
                        envFile = SedimentFile = (SedimentFile)NewEnvironmentFile(fileName, sampleCount, dataType, timePeriod, resolution);
                        break;
                    case EnvironmentDataType.Temperature:
                        envFile = TemperatureFile.Months[timePeriod] = (TemperatureFile)NewEnvironmentFile(fileName, sampleCount, dataType, timePeriod, resolution);
                        break;
                    case EnvironmentDataType.SoundSpeed:
                        envFile = SoundSpeedFile.Months[timePeriod] = (SoundSpeedFile)NewEnvironmentFile(fileName, sampleCount, dataType, timePeriod, resolution);
                        break;
                    case EnvironmentDataType.Wind:
                        envFile = WindFile = (WindFile)NewEnvironmentFile(fileName, sampleCount, dataType, timePeriod, resolution);
                        break;
                    default:
                        throw new ApplicationException(string.Format("Unknown environment data type: {0}", dataType));
                }
                EnvironmentFiles[fileName] = envFile;
                // OK so let's actually go ahead and create a job if we DO want to extract this data from the database
                if (createJobIfRequired)
                {
                    var jobDescriptor = new ImportJobDescriptor
                    {
                        DataType = dataType,
                        GeoRect = GeoRect,
                        DestinationFilename = localPath,
                        Resolution = resolution,
                        TimePeriod = timePeriod,
                        CompletionFunction = arg =>
                        {
                            var job = (ImportJobDescriptor)arg;
                            var key = Path.GetFileName(job.DestinationFilename);
                            envFile.IsCached = true;
                            envFile.SampleCount = job.SampleCount;
                            envFile.GeoRect = job.GeoRect;
                            EnvironmentFiles[key] = envFile;
                            envFile.UpdateFileInfo();
                            return job;
                        },
                    };
                    jobDescriptor.CompletionTask = new Task<ImportJobDescriptor>(jobDescriptor.CompletionFunction, jobDescriptor);
                    
                    return Tuple.Create(envFile, jobDescriptor);
                }
                // So we really DIDN'T want to extract the data from the database, but only wanted to create the metadata for later on
                return Tuple.Create(envFile, (ImportJobDescriptor)null);
            }
            
            return null;
        }

        #endregion

        #region public string ToolTip { get; set; }

        public string ToolTip
        {
            get { return _toolTip; }
            set
            {
                if (_toolTip == value) return;
                _toolTip = value;
                NotifyPropertyChanged(ToolTipChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ToolTipChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplex>(x => x.ToolTip);
        string _toolTip;

        #endregion

        internal static NewRangeComplex Create(string simAreaPath, string rangeComplexName, IEnumerable<Geo> opAreaLimits, IEnumerable<Geo> simAreaLimits, Dispatcher dispatcher)
        {
            var result = new NewRangeComplex(simAreaPath, rangeComplexName, true, dispatcher);
            result.OpArea = result.CreateAreaPrivate(String.Format("{0}_OpArea", rangeComplexName), opAreaLimits);
            result.SimArea = result.CreateAreaPrivate(String.Format("{0}_SimArea", rangeComplexName), simAreaLimits);
            result.UpdateAreas();
            result.ValidateEnvironment();
            return result;
        }

        internal static NewRangeComplex Load(string simAreaPath, Tuple<string, double, double, double, double, string, string> rangeComplexInfo, Dispatcher dispatcher)
        {
            var result = new NewRangeComplex(simAreaPath, rangeComplexInfo.Item1, false, dispatcher);
            try
            {
                result.OpArea = result.AreaCollection[Path.GetFileNameWithoutExtension(rangeComplexInfo.Item6)];
                result.SimArea = result.AreaCollection[Path.GetFileNameWithoutExtension(rangeComplexInfo.Item7)];
            }
            catch (Exception)
            {
                RangeComplexes.Singleton.DeleteRangeComplexFromDisk(result.Name);
                throw new InvalidOperationException(string.Format("The range complex \"{0}\" is missing critical files or directories.\r\nThe range complex has been deleted", result.Name));
            }
            result.UpdateAreas();
            result.ValidateEnvironment();
            return result;
        }

        #region Import job queueing
        readonly object _queueLock = new object();

        public void QueueImportJob(ImportJobDescriptor job)
        {
            lock (_queueLock)
            {
                job.CompletionTask.ContinueWith(task =>
                {
                    JobCompleted();
                    task.Dispose();
                });
                NAVOImporter.Import(job);
                QueuedJobCount++;
                IsLoading = true;
            }
        }

        #region public int QueuedJobCount { get; set; }

        public int QueuedJobCount
        {
            get { return _queuedJobCount; }
            set
            {
                if (_queuedJobCount == value) return;
                _queuedJobCount = value;
                NotifyPropertyChanged(LoadTaskTotalChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LoadTaskTotalChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplex>(x => x.QueuedJobCount);
        int _queuedJobCount;

        #endregion

        #region public int CompletedJobCount { get; set; }
        public int CompletedJobCount { get; private set; }

        [MethodImpl(MethodImplOptions.Synchronized)]
        void JobCompleted()
        {
            var isLoading = _isLoading;
            lock (_queueLock)
            {
                CompletedJobCount++;
                ImportProgressPercent = (int)(((float)CompletedJobCount / QueuedJobCount) * 100);
                if (CompletedJobCount == QueuedJobCount)
                {
                    CompletedJobCount = 0;
                    QueuedJobCount = 0;
                    isLoading = false;
                }
            }
            NotifyPropertyChanged(LoadTasksCompletedChangedEventArgs);
            IsLoading = isLoading;
        }

        static readonly PropertyChangedEventArgs LoadTasksCompletedChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplex>(x => x.CompletedJobCount);
        #endregion

        #region public int ImportProgressPercent { get; set; }

        public int ImportProgressPercent
        {
            get { return _importProgressPercent; }
            set
            {
                if (_importProgressPercent == value) return;
                _importProgressPercent = value;
                NotifyPropertyChanged(LoadProgressChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LoadProgressChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplex>(x => x.ImportProgressPercent);
        int _importProgressPercent;

        #endregion

        #region public bool IsLoading { get; private set; }

        public bool IsLoading
        {
            get { return _isLoading; }
            private set
            {
                if (_isLoading == value) return;
                _isLoading = value;
                NotifyPropertyChanged(IsEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsEnabledChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplex>(x => x.IsLoading);
        bool _isLoading;

        #endregion

        #endregion

        public RangeComplexArea CreateArea(string areaName, IEnumerable<Geo> areaLimits)
        {
            if (IsLoading) throw new InvalidOperationException(string.Format("The range complex {0} cannot be modified at the moment. Please try again shortly.", Name));
            return CreateAreaPrivate(areaName, areaLimits);
        }

        RangeComplexArea CreateAreaPrivate(string areaName, IEnumerable<Geo> areaLimits)
        {
            if (areaName == null) throw new ArgumentNullException("areaName");
            if (AreaCollection.ContainsKey(areaName)) throw new ArgumentException(string.Format("Area {0} already exists", areaName), "areaName");
            var newArea = RangeComplexArea.Create(this, areaName, areaLimits);
            AreaCollection.Add(newArea.Name, newArea); 
            return newArea;
        }

        public bool TryRemoveArea(string areaName, out string message)
        {
            if (string.IsNullOrEmpty(areaName))
            {
                message = "areaName is null or empty!";
                return false;
            }
            if (!AreaCollection.ContainsKey(areaName))
            {
                message = string.Format("Area {0} does not exist", areaName);
                return false;
            }
            if (IsLoading)
            {
                message = string.Format("The range complex {0} cannot be modified at the moment. Please try again shortly.", Name);
                return false;
            }
            if (OpArea.Name == areaName)
            {
                message= string.Format("The area {0} is the operational area of this range complex and may not be removed.", areaName);
                return false;
            }
            if (SimArea.Name == areaName)
            {
                message = string.Format("The area {0} is the simulation area of this range complex and may not be removed.", areaName);
                return false;
            }
            message = null;
            return true;
        }

        public void RemoveArea(string areaName)
        {
            string error;
            if (!TryRemoveArea(areaName, out error)) throw new InvalidOperationException(error);
            AreaCollection[areaName].Remove();
            AreaCollection.Remove(areaName);
        }

        public void Dump()
        {
            Debug.WriteLine("{0} Dump of range complex {1}", DateTime.Now, Name);
            Debug.WriteLine("{0}   Environment Files", DateTime.Now);
            foreach (var item in EnvironmentFiles) Debug.WriteLine("{0}     [{1}]  size: {2}", DateTime.Now, item.Key, item.Value.FileSize);
            Debug.WriteLine("{0}   Areas", DateTime.Now);
            foreach (var item in AreaCollection)
            {
                Debug.WriteLine("{0}     [{1}]  size: {2:0.##}km x {3:0.##}km", DateTime.Now, item.Key, item.Value.GeoRect.AverageWidthKm, item.Value.GeoRect.HeightKm);
                Debug.WriteLine("{0}       Bathymetry", DateTime.Now);
                foreach (var area in item.Value.BathymetryFiles) 
                    Debug.WriteLine("{0}         [{1}]  size: {2}", DateTime.Now, area.Key, area.Value.FileSize);
            }
        }
    }
}