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
        NewRangeComplex(string simAreaPath, string rangeComplexName, bool isCreate, Dispatcher dispatcher)
        {
            IsLoading = false;
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
#if false
        IEnumerable<ImportJobDescriptor> CheckForMissingEnviromentFiles()
        {
            //var test = new Task<EnvironmentFile<SoundSpeed>>()

            var jobs = ValidateMonthlyData("temperature").Select(missingItem => new ImportJobDescriptor
            {
                DataType = EnvironmentDataType.Temperature,
                GeoRect = GeoRect,
                TimePeriod = missingItem,
                DestinationFilename = Path.Combine(DataPath, string.Format("{0}.temperature", missingItem.ToString())),
                CompletionAction = tempJob =>
                {
                    var key = Path.GetFileName(tempJob.DestinationFilename);
                    var envFile = new TemperatureFile(DataPath, key, tempJob.SampleCount, tempJob.GeoRect, EnvironmentDataType.Temperature, tempJob.TimePeriod);
                    EnvironmentFiles[key] = envFile;
                    TemperatureFile.Months[tempJob.TimePeriod] = envFile;
                }
            }).ToList();
            jobs.AddRange(ValidateMonthlyData("salinity").Select(missingItem => new ImportJobDescriptor
            {
                DataType = EnvironmentDataType.Salinity,
                GeoRect = GeoRect,
                TimePeriod = missingItem,
                DestinationFilename = Path.Combine(DataPath, string.Format("{0}.salinity", missingItem.ToString())),
                CompletionAction = tempJob =>
                {
                    var key = Path.GetFileName(tempJob.DestinationFilename);
                    var envFile = new SalinityFile(DataPath, key, tempJob.SampleCount, tempJob.GeoRect, EnvironmentDataType.Salinity, tempJob.TimePeriod);
                    EnvironmentFiles[key] = envFile;
                    SalinityFile.Months[tempJob.TimePeriod] = envFile;
                }
            }));

            foreach (var month in NAVOConfiguration.AllMonths)
            {
                var fileName = string.Format("{0}.soundspeed", month);
                EnvironmentFiles[fileName] = new SoundSpeedFile(DataPath, fileName, 0, GeoRect, EnvironmentDataType.SoundSpeed, month);
            }
            var windFilename = Path.Combine(DataPath, "data.wind");
            if (EnvironmentFiles.ReextractionRequired || (!File.Exists(windFilename)) || (new FileInfo(windFilename).LastWriteTime > EnvironmentFiles.LastWriteTime))
                jobs.Add(new ImportJobDescriptor
                {
                    DataType = EnvironmentDataType.Wind,
                    GeoRect = GeoRect,
                    DestinationFilename = windFilename,
                    CompletionAction = tempJob =>
                    {
                        var key = Path.GetFileName(tempJob.DestinationFilename);
                        var envFile = new WindFile(DataPath, key, tempJob.SampleCount, tempJob.GeoRect, EnvironmentDataType.Wind, NAVOTimePeriod.Invalid);
                        EnvironmentFiles[key] = envFile;
                        WindFile = envFile;
                    },
                });

            var sedimentFilename = Path.Combine(DataPath, "data.sediment");
            if (EnvironmentFiles.ReextractionRequired || (!File.Exists(sedimentFilename)) || (new FileInfo(sedimentFilename).LastWriteTime > EnvironmentFiles.LastWriteTime))
                jobs.Add(new ImportJobDescriptor
                {
                    DataType = EnvironmentDataType.Sediment,
                    GeoRect = GeoRect,
                    DestinationFilename = sedimentFilename,
                    CompletionAction = tempJob =>
                    {
                        var key = Path.GetFileName(tempJob.DestinationFilename);
                        var envFile = new SedimentFile(DataPath, key, tempJob.SampleCount, tempJob.GeoRect, EnvironmentDataType.Sediment, NAVOTimePeriod.Invalid);
                        EnvironmentFiles[key] = envFile;
                        SedimentFile = envFile;
                    },
                });

            if (Globals.AppSettings.IsNavyVersion)
            {
                var bottomLossFilename = Path.Combine(DataPath, "data.bottomloss");
                if (EnvironmentFiles.ReextractionRequired || (!File.Exists(bottomLossFilename)) || (new FileInfo(bottomLossFilename).LastWriteTime > EnvironmentFiles.LastWriteTime))
                    jobs.Add(new ImportJobDescriptor
                    {
                        DataType = EnvironmentDataType.BottomLoss,
                        GeoRect = GeoRect,
                        DestinationFilename = bottomLossFilename,
                        CompletionAction = tempJob =>
                        {
                            var key = Path.GetFileName(tempJob.DestinationFilename);
                            var envFile = new BottomLossFile(DataPath, key, tempJob.SampleCount, tempJob.GeoRect, EnvironmentDataType.BottomLoss, NAVOTimePeriod.Invalid);
                            EnvironmentFiles[key] = envFile;
                            BottomLossFile = envFile;
                        },
                    });
            }
            return jobs;
        }

        IEnumerable<NAVOTimePeriod> ValidateMonthlyData(string dataFileExtension)
        {
            var availableMonths = Directory.EnumerateFiles(DataPath, "*." + dataFileExtension).Select(item => (NAVOTimePeriod)Enum.Parse(typeof(NAVOTimePeriod), Path.GetFileNameWithoutExtension(item), true)).ToList();
            var missingMonths = new List<NAVOTimePeriod>();
            if (EnvironmentFiles.ReextractionRequired) return NAVOConfiguration.AllMonths;
            foreach (var month in NAVOConfiguration.AllMonths)
            {
                var fileName = Path.Combine(DataPath, string.Format("{0}.{1}", month.ToString(), dataFileExtension));
                if (!availableMonths.Contains(month)) missingMonths.Add(month);
                else if ((!File.Exists(fileName)) || (new FileInfo(fileName).LastWriteTime > EnvironmentFiles.LastWriteTime))
                    missingMonths.Add(month);
            }
            return missingMonths.Distinct();
        }
#endif

        List<ImportJobDescriptor> ValidateEnvironment()
        {
            var jobs = new List<ImportJobDescriptor>();
            var result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.Sediment, true);
            if (result != null) jobs.Add(result.Item2);
            result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.BottomLoss, true);
            if (result != null) jobs.Add(result.Item2);
            result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.Wind, true);
            if (result != null) jobs.Add(result.Item2);
            foreach (var month in NAVOConfiguration.AllMonths)
            {
                result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.Temperature, true, month);
                if (result != null) jobs.Add(result.Item2);
                result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.Salinity, true, month);
                if (result != null) jobs.Add(result.Item2);
                result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.SoundSpeed, false, month);
                LinkToSourceMonths(result.Item1, month, EnvironmentDataType.Temperature);
                LinkToSourceMonths(result.Item1, month, EnvironmentDataType.Salinity);
            }
            foreach (var season in NAVOConfiguration.AllSeasons)
            {
                result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.Temperature, false, season);
                LinkToSourceMonths(result.Item1, season, EnvironmentDataType.Temperature);
                result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.Salinity, false, season);
                LinkToSourceMonths(result.Item1, season, EnvironmentDataType.Salinity);
                result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.SoundSpeed, false, season);
                LinkToSourceMonths(result.Item1, season, EnvironmentDataType.SoundSpeed);
            }
            return jobs;
        }
        
        void LinkToSourceMonths(EnvironmentFile envFile, NAVOTimePeriod timePeriod, EnvironmentDataType sourceType)
        {
            var months = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod).ToList();
            envFile.RequiredFiles.Clear();
            foreach (var month in months)
                envFile.RequiredFiles.Add(EnvironmentFiles[string.Format("{0}.{1}", month, sourceType.ToString().ToLower())]);
        }

        EnvironmentFile NewEnvironmentFile(string fileName, uint sampleCount, EnvironmentDataType dataType, NAVOTimePeriod timePeriod)
        {
            switch (dataType)
            {
                case EnvironmentDataType.BottomLoss:
                    return new BottomLossFile(DataPath, fileName, sampleCount, GeoRect, dataType, timePeriod);
                case EnvironmentDataType.Salinity:
                    return new SalinityFile(DataPath, fileName, sampleCount, GeoRect, dataType, timePeriod);
                case EnvironmentDataType.Sediment:
                    return new SedimentFile(DataPath, fileName, sampleCount, GeoRect, dataType, timePeriod);
                case EnvironmentDataType.SoundSpeed:
                    return new SoundSpeedFile(DataPath, fileName, sampleCount, GeoRect, dataType, timePeriod);
                case EnvironmentDataType.Temperature:
                    return new TemperatureFile(DataPath, fileName, sampleCount, GeoRect, dataType, timePeriod);
                case EnvironmentDataType.Wind:
                    return new WindFile(DataPath, fileName, sampleCount, GeoRect, dataType, timePeriod);
                default:
                    throw new ApplicationException(string.Format("Unknown environment data type: {0}", dataType));
            }
        }

        Tuple<EnvironmentFile, ImportJobDescriptor> CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType dataType, bool createJobIfRequired, NAVOTimePeriod timePeriod = NAVOTimePeriod.Invalid)
        {
            const float samplesPerDegree = 15.0f;
            const float resolution = 60.0f / samplesPerDegree;
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
                        envFile = BottomLossFile = (BottomLossFile)NewEnvironmentFile(fileName, sampleCount, dataType, timePeriod);
                        break;
                    case EnvironmentDataType.Salinity:
                        envFile = SalinityFile.Months[timePeriod] = (SalinityFile)NewEnvironmentFile(fileName, sampleCount, dataType, timePeriod);
                        break;
                    case EnvironmentDataType.Sediment:
                        envFile = SedimentFile = (SedimentFile)NewEnvironmentFile(fileName, sampleCount, dataType, timePeriod);
                        break;
                    case EnvironmentDataType.Temperature:
                        envFile = TemperatureFile.Months[timePeriod] = (TemperatureFile)NewEnvironmentFile(fileName, sampleCount, dataType, timePeriod);
                        break;
                    case EnvironmentDataType.SoundSpeed:
                        envFile = SoundSpeedFile.Months[timePeriod] = (SoundSpeedFile)NewEnvironmentFile(fileName, sampleCount, dataType, timePeriod);
                        break;
                    case EnvironmentDataType.Wind:
                        envFile = WindFile = (WindFile)NewEnvironmentFile(fileName, sampleCount, dataType, timePeriod);
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

        #region public bool IsEnabled { get; private set; }

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
            var importJobs = result.ValidateEnvironment().ToList();
            foreach (var area in result.AreaCollection.Values) importJobs.AddRange(area.ImportJobs);
            NAVOImporter.Import(importJobs);
            result.IsLoading = true;
            //Tuple.Create(rangeComplexName, height, latitude, longitude, geoid, opsLimitFile, simLimitFile)
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
            catch (Exception e)
            {
                RangeComplexes.Singleton.DeleteRangeComplexFromDisk(result.Name);
                throw new InvalidOperationException(string.Format("The range complex \"{0}\" is missing critical files or directories.\r\nThe range complex has been deleted", result.Name));
            }
            result.UpdateAreas();
            var importJobs = result.ValidateEnvironment();
            foreach (var area in result.AreaCollection.Values) importJobs.AddRange(area.ImportJobs);
            NAVOImporter.Import(importJobs);
            var completionTasks = (from job in importJobs
                                   select job.CompletionTask).ToList();
            result.LoadTaskTotal = completionTasks.Count();
            foreach (var task in completionTasks) task.ContinueWith(t => result.IncrementCompletedTasks());
            TaskEx.WhenAll(completionTasks).ContinueWith(task => result.IsLoading = true);
            return result;
        }

        #region public int LoadTaskTotal { get; set; }

        public int LoadTaskTotal
        {
            get { return _loadTaskTotal; }
            set
            {
                if (_loadTaskTotal == value) return;
                _loadTaskTotal = value;
                NotifyPropertyChanged(LoadTaskTotalChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LoadTaskTotalChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplex>(x => x.LoadTaskTotal);
        int _loadTaskTotal;

        #endregion

        #region public int LoadTasksCompleted { get; set; }
        public int LoadTasksCompleted { get; private set; }

        [MethodImpl(MethodImplOptions.Synchronized)]
        void IncrementCompletedTasks()
        {
            LoadTasksCompleted++;
            NotifyPropertyChanged(LoadTasksCompletedChangedEventArgs);
            LoadProgress = (int)(((float)LoadTasksCompleted / LoadTaskTotal) * 100);
        }

        static readonly PropertyChangedEventArgs LoadTasksCompletedChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplex>(x => x.LoadTasksCompleted);
        #endregion

        #region public int LoadProgress { get; set; }

        public int LoadProgress
        {
            get { return _loadProgress; }
            set
            {
                if (_loadProgress == value) return;
                _loadProgress = value;
                NotifyPropertyChanged(LoadProgressChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LoadProgressChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplex>(x => x.LoadProgress);
        int _loadProgress;

        #endregion

        public RangeComplexArea CreateArea(string areaName, IEnumerable<Geo> areaLimits)
        {
            if (!IsLoading) throw new InvalidOperationException(string.Format("The range complex {0} cannot be modified at the moment. Please try again shortly.", Name));
            return CreateAreaPrivate(areaName, areaLimits);
        }

        RangeComplexArea CreateAreaPrivate(string areaName, IEnumerable<Geo> areaLimits)
        {
            if (areaName == null) throw new ArgumentNullException("areaName");
            if (AreaCollection.ContainsKey(areaName)) throw new ArgumentException(string.Format("Area {0} already exists", areaName), "areaName");
            var newArea = RangeComplexArea.Create(this, areaName, areaLimits);
            AreaCollection.Add(newArea.Name, newArea); 
            NAVOImporter.Import(newArea.ImportJobs);
            return newArea;
        }

        public void RemoveArea(string areaName)
        {
            if (!IsLoading) throw new InvalidOperationException(string.Format("The range complex {0} cannot be modified at the moment. Please try again shortly.", Name));
            RemoveAreaPrivate(areaName);
        }

        public void RemoveAreaPrivate(string areaName)
        {
            if (areaName == null) throw new ArgumentNullException("areaName");
            if (!AreaCollection.ContainsKey(areaName)) throw new ArgumentException(string.Format("Area {0} does not exist", areaName), "areaName");
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