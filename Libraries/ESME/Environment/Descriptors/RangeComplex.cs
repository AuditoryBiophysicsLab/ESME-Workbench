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
    public class RangeComplex : ViewModelBase
    {
        RangeComplex(string simAreaPath, RangeComplexMetadata rangeComplexMetadata, bool isCreate, Dispatcher dispatcher)
        {
            IsLoading = true;
            _dispatcher = dispatcher;
            RangeComplexMetadata = rangeComplexMetadata;
            Name = RangeComplexMetadata.Name;
            RangeComplexPath = Path.Combine(simAreaPath, Name);
            AreasPath = Path.Combine(RangeComplexPath, "Areas");
            BathymetryPath = Path.Combine(RangeComplexPath, "Bathymetry");
            DataPath = Path.Combine(RangeComplexPath, "Data");
            EnvironmentPath = Path.Combine(RangeComplexPath, "Environment");
            ImagesPath = Path.Combine(RangeComplexPath, "Images");
            SpeciesPath = Path.Combine(RangeComplexPath, "Species");
            if (isCreate)
            {
                Directory.CreateDirectory(RangeComplexPath);
                Directory.CreateDirectory(AreasPath);
            }
            else
            {
                if (!Directory.Exists(RangeComplexPath) || !Directory.Exists(AreasPath))
                {
                    RangeComplexes.Singleton.DeleteRangeComplexFromDisk(Name);
                    throw new InvalidOperationException(string.Format("The range complex \"{0}\" is missing critical files or directories.\r\nThe range complex has been deleted", Name));
                }
            }
            Directory.CreateDirectory(BathymetryPath);
            Directory.CreateDirectory(DataPath);
            Directory.CreateDirectory(EnvironmentPath);
            Directory.CreateDirectory(ImagesPath);
            Directory.CreateDirectory(SpeciesPath);
            Directory.CreateDirectory(Path.Combine(RangeComplexPath, "GeographicAreas"));

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
        }

        [NotNull] readonly Dispatcher _dispatcher;

        [NotNull] public RangeComplexMetadata RangeComplexMetadata { get; private set; }

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
                var area = RangeComplexArea.Read(this, areaName);
                _dispatcher.InvokeInBackgroundIfRequired(() => AreaCollection.Add(areaName, area));
            }
            GeoRect = GeoRect.Union(AreaCollection.Values.Select(area => area.GeoRect).ToArray());
            EnvironmentFiles.GeoRect = GeoRect;
        }

        #region Validation
        void ValidateEnvironment()
        {
            var result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.Sediment, 5, true);
            if (result != null) QueueImportJob(result.Item2);
            result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.Wind, 60, true);
            if (result != null) QueueImportJob(result.Item2);
            foreach (var month in NAVOConfiguration.AllMonths)
            {
                //result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.Temperature, 15, true, month);
                //if (result != null) QueueImportJob(result.Item2);
                //result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.Salinity, 15, true, month);
                //if (result != null) QueueImportJob(result.Item2);
                result = CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.SoundSpeed, 15, true, month);
                if (result != null) QueueImportJob(result.Item2);
                //LinkToSourceMonths(result.Item1, month, EnvironmentDataType.Temperature);
                //LinkToSourceMonths(result.Item1, month, EnvironmentDataType.Salinity);
                //LinkToSourceMonths(result.Item1, month, EnvironmentDataType.Salinity);
            }
            foreach (var season in NAVOConfiguration.AllSeasons)
            {
                CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType.SoundSpeed, 15, false, season);
                //LinkToSourceMonths(result.Item1, season, EnvironmentDataType.SoundSpeed);
            }
        }
        
        Tuple<EnvironmentFile, ImportJobDescriptor> CreateEnvironmentFileMetadataIfNeeded(EnvironmentDataType dataType, float resolution, bool createJobIfRequired, TimePeriod timePeriod = TimePeriod.Invalid)
        {
            var samplesPerDegree = 60.0f / resolution;
            var fileName = string.Format("{0}.{1}", timePeriod == TimePeriod.Invalid ? "data" : timePeriod.ToString(), dataType.ToString().ToLower());
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
                var envFile = new EnvironmentFile(DataPath, fileName, sampleCount, GeoRect, dataType, timePeriod, resolution);
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

        static readonly PropertyChangedEventArgs ToolTipChangedEventArgs = ObservableHelper.CreateArgs<RangeComplex>(x => x.ToolTip);
        string _toolTip;

        #endregion

        public static RangeComplex Create(string simAreaPath, RangeComplexMetadata rangeComplexMetadata, IEnumerable<Geo> opAreaLimits, IEnumerable<Geo> simAreaLimits, Dispatcher dispatcher)
        {
            var result = new RangeComplex(simAreaPath, rangeComplexMetadata, true, dispatcher);
            result.OpArea = result.CreateAreaPrivate(String.Format("{0}_OpArea", rangeComplexMetadata.Name), opAreaLimits);
            result.SimArea = result.CreateAreaPrivate(String.Format("{0}_SimArea", rangeComplexMetadata.Name), simAreaLimits);
            result.UpdateAreas();
            result.ValidateEnvironment();
            return result;
        }

        public static RangeComplex Load(string simAreaPath, RangeComplexMetadata rangeComplexMetadata, Dispatcher dispatcher)
        {
            var result = new RangeComplex(simAreaPath, rangeComplexMetadata, false, dispatcher);
            try
            {
                result.OpArea = result.AreaCollection[Path.GetFileNameWithoutExtension(rangeComplexMetadata.OpsLimitFile)];
                result.SimArea = result.AreaCollection[Path.GetFileNameWithoutExtension(rangeComplexMetadata.SimLimitFile)];
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

        static readonly PropertyChangedEventArgs LoadTaskTotalChangedEventArgs = ObservableHelper.CreateArgs<RangeComplex>(x => x.QueuedJobCount);
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

        static readonly PropertyChangedEventArgs LoadTasksCompletedChangedEventArgs = ObservableHelper.CreateArgs<RangeComplex>(x => x.CompletedJobCount);
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

        static readonly PropertyChangedEventArgs LoadProgressChangedEventArgs = ObservableHelper.CreateArgs<RangeComplex>(x => x.ImportProgressPercent);
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

        static readonly PropertyChangedEventArgs IsEnabledChangedEventArgs = ObservableHelper.CreateArgs<RangeComplex>(x => x.IsLoading);
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

    public class RangeComplexMetadata
    {
        public RangeComplexMetadata(string rangeComplexName, double height, double latitude, double longitude, double geoid, string opsLimitFile, string simLimitFile)
        {
            Name = rangeComplexName;
            Height = height;
            Latitude = latitude;
            Longitude = longitude;
            GeoidSeparation = geoid;
            OpsLimitFile = opsLimitFile;
            SimLimitFile = simLimitFile;
        }

        public string Name { get; private set; }
        public double Height { get; private set; }
        public double Latitude { get; private set; }
        public double Longitude { get; private set; }
        public double GeoidSeparation { get; private set; }
        public string OpsLimitFile { get; private set; }
        public string SimLimitFile { get; private set; }
    }
}