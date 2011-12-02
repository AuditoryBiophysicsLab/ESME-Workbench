using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using System.Windows.Threading;
using Cinch;
using ESME.Environment.NAVO;
using HRC;
using HRC.Navigation;
using HRC.Collections;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    public class RangeComplexes : ViewModelBase, IEnumerable<KeyValuePair<string, NewRangeComplex>>, INotifyCollectionChanged
    {
        RangeComplexes()
        {
            _dispatcher = Dispatcher.CurrentDispatcher;
            RangeComplexCollection = new ObservableConcurrentDictionary<string, NewRangeComplex>();
            RangeComplexList = ObservableList<NewRangeComplex>.FromObservableConcurrentDictionary(RangeComplexCollection, kvp => kvp.Value, (kvp, rc) => kvp.Value == rc);
        }

        #region Private fields
        readonly object _lockObject = new object();
        #endregion

        #region public Dispatcher Dispatcher { get; set; }

        public Dispatcher Dispatcher
        {
            get { return _dispatcher; }
            set
            {
                if (_dispatcher == value) return;
                _dispatcher = value;
                NotifyPropertyChanged(DispatcherChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DispatcherChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.Dispatcher);
        Dispatcher _dispatcher;

        #endregion


        [NotNull]
        public ObservableConcurrentDictionary<string, NewRangeComplex> RangeComplexCollection { get; private set; }

        public ObservableList<NewRangeComplex> RangeComplexList { get; private set; }

        public Task<bool> ReadRangeComplexFileAsync(string fileName)
        {
            IsEnabled = false;
            _rangeComplexFile = fileName;
            return InitializeAsync(_rangeComplexFile);
        }

        #region public bool IsEnabled { get; set; }

        public bool IsEnabled
        {
            get { return _isEnabled; }
            set
            {
                if (_isEnabled == value) return;
                _isEnabled = value;
                NotifyPropertyChanged(IsEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsEnabledChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.IsEnabled);
        bool _isEnabled;

        #endregion

        string _rangeComplexFile;

        async Task<bool> InitializeAsync(string simAreaFile)
        {
            if (!File.Exists(simAreaFile)) throw new FileNotFoundException("Error reading sim area file", simAreaFile);
            SimAreaPath = Path.GetDirectoryName(simAreaFile);
            var actionBlock = new ActionBlock<RangeComplexMetadata>(
		        info =>
		        {
		            try
		            {
                        var rangeComplex = NewRangeComplex.Load(SimAreaPath, info, _dispatcher);
                        _dispatcher.InvokeInBackgroundIfRequired(() => RangeComplexCollection.Add(rangeComplex.Name, rangeComplex));
		            }
		            catch (Exception e)
		            {
		                throw new ApplicationException(string.Format("Error loading range complex {0}", info.Name), e);
		            }
		        },
		        new ExecutionDataflowBlockOptions
		        {
		            TaskScheduler = TaskScheduler.Default,
		            MaxDegreeOfParallelism = 4,
		        });
            var lines = File.ReadAllLines(simAreaFile);
            foreach (var line in lines)
            {
                if (line == null) throw new ApplicationException("line is null");
                var curLine = line.Trim();
                if ((curLine.Trim() == "") || curLine.StartsWith("!") || curLine.StartsWith("#")) continue;
                var fields = curLine.Split(',');
                var rangeComplexName = fields[0].Trim();
                var latString = fields[1].Trim();
                var lonString = fields[2].Trim();
                var heightString = fields[3].Trim();
                var geoidString = fields[4].Trim();
                var opsLimitFile = fields[5].Trim();
                var simLimitFile = fields[6].Trim();
                double latitude;
                double longitude;
                double height;
                double geoid;
                if (string.IsNullOrEmpty(rangeComplexName)) throw new FormatException(string.Format("RangeComplexes: Error reading sim area file \"{0}\"\nError: Invalid sim area name", simAreaFile));
                if (string.IsNullOrEmpty(opsLimitFile)) throw new FormatException(string.Format("RangeComplexes: Error reading sim area file \"{0}\"\nError: Invalid OpsLimit filename", simAreaFile));
                if (string.IsNullOrEmpty(simLimitFile)) throw new FormatException(string.Format("RangeComplexes: Error reading sim area file \"{0}\"\nError: Invalid SimLimit filename", simAreaFile));
                if (!double.TryParse(latString, out latitude)) throw new FormatException(string.Format("RangeComplexes: Error reading sim area file \"{0}\"\nError: Invalid latitude", simAreaFile));
                if (!double.TryParse(lonString, out longitude)) throw new FormatException(string.Format("RangeComplexes: Error reading sim area file \"{0}\"\nError: Invalid longitude", simAreaFile));
                if (!double.TryParse(heightString, out height)) throw new FormatException(string.Format("RangeComplexes: Error reading sim area file \"{0}\"\nError: Invalid height", simAreaFile));
                if (!double.TryParse(geoidString, out geoid)) throw new FormatException(string.Format("RangeComplexes: Error reading sim area file \"{0}\"\nError: Invalid geoid separation value", simAreaFile));
                actionBlock.Post(new RangeComplexMetadata(rangeComplexName, height, latitude, longitude, geoid, opsLimitFile, simLimitFile));
            }
            actionBlock.Complete();
            try
            {
                await actionBlock.Completion;
                IsEnabled = true;
                return true;
            }
            catch
            {
                var sb = new StringBuilder();
                foreach (var ex in actionBlock.Completion.Exception.InnerExceptions) sb.AppendLine(FormatExceptionMessage(ex, 0) + "\r\n");
                throw new ApplicationException("Error loading range complexes:\r\n" + sb);
            }
        }

        public string FormatExceptionMessage(Exception exception, int indentLevel)
        {
            return new string(' ', 2 * indentLevel) + ((exception.InnerException == null)
                                                           ? exception.Message
                                                           : exception.Message + "\r\n" + FormatExceptionMessage(exception.InnerException, indentLevel + 1));
        }

        public string SimAreaPath { get; private set; }
        static RangeComplexes _instance;
        public static RangeComplexes Singleton { get { return _instance ?? (_instance = new RangeComplexes()); } }

        public NewRangeComplex this[string rangeComplexName]
        {
            get { return RangeComplexCollection[rangeComplexName]; }
        }

        public NewRangeComplex CreateRangeComplex(string rangeComplexName, double height, double latitude, double longitude, double geoid, ICollection<Geo> opAreaLimits, List<Geo> simAreaLimits)
        {
            if (opAreaLimits == null) throw new ArgumentNullException("opAreaLimits");
            if (simAreaLimits == null) throw new ArgumentNullException("simAreaLimits");
            if (opAreaLimits.Count < 4) throw new ArgumentException("Must have at least four coordinates", "opAreaLimits");
            if (simAreaLimits.Count < 4) throw new ArgumentException("Must have at least four coordinates", "simAreaLimits");

            if (RangeComplexCollection.ContainsKey(rangeComplexName)) throw new ApplicationException(string.Format("Range complex {0} already exists", rangeComplexName));
            var rangeComplexPath = Path.Combine(SimAreaPath, rangeComplexName);

            if (Directory.Exists(rangeComplexPath)) Directory.Delete(rangeComplexPath, true);
            var rangeComplexMetadata = new RangeComplexMetadata(rangeComplexName, height, latitude, longitude, geoid, rangeComplexName + "_OpArea.ovr", rangeComplexName + "_SimArea.ovr");
            var result = NewRangeComplex.Create(SimAreaPath, rangeComplexMetadata, opAreaLimits, simAreaLimits, _dispatcher);

            lock (_lockObject)
            {
                var needsExtraNewline = !File.ReadAllText(_rangeComplexFile).EndsWith("\n");
                using (var writer = new StreamWriter(_rangeComplexFile, true))
                {
                    if (needsExtraNewline) writer.WriteLine();
                    writer.WriteLine("{0},{1:0.0###},{2:0.0###},{3:0.0###},{4:0.0###},{5},{6}", rangeComplexName.Trim(),
                                     latitude, longitude, height, geoid,
                                     Path.GetFileName(result.OpArea.Name).Trim() + ".ovr", Path.GetFileName(result.SimArea.Name).Trim() + ".ovr");
                }
            }
            RangeComplexCollection.Add(result.Name, result);
            return result;
        }

        public void RemoveRangeComplex(string rangeComplexName)
        {
            if (rangeComplexName == null) throw new ArgumentNullException("rangeComplexName");
            if (!RangeComplexCollection.ContainsKey(rangeComplexName)) throw new ArgumentException(string.Format("Range complex {0} does not exist", rangeComplexName), "rangeComplexName");
            DeleteRangeComplexFromDisk(rangeComplexName);
        }

        internal void DeleteRangeComplexFromDisk(string rangeComplexName)
        {
            if (rangeComplexName == null) throw new ArgumentNullException("rangeComplexName");
            lock (_lockObject)
            {
                var directoryPath = Path.Combine(Path.GetDirectoryName(_rangeComplexFile), rangeComplexName);
                if (Directory.Exists(directoryPath)) Directory.Delete(directoryPath, true);
                var simAreaCSVFileContents = File.ReadAllLines(_rangeComplexFile);
                var oldCSVFileName = _rangeComplexFile;
                var newCSVFileName = _rangeComplexFile + ".new";
                using (var streamWriter = new StreamWriter(newCSVFileName))
                    foreach (var curLine in simAreaCSVFileContents.Where(curLine => !curLine.StartsWith(rangeComplexName + ",")))
                        streamWriter.WriteLine(curLine);
                File.Delete(oldCSVFileName);
                File.Move(newCSVFileName, oldCSVFileName);
                if (RangeComplexCollection.ContainsKey(rangeComplexName)) RangeComplexCollection.Remove(rangeComplexName);
            }
        }

        public void Dump()
        {
            foreach (var rangeComplex in RangeComplexCollection) rangeComplex.Value.Dump();
        }

		//public async Task<RangeComplex> AddAsync(string rangeComplexName, List<Geo> opAreaLimits, List<Geo> simAreaLimits){}
        IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }
        public IEnumerator<KeyValuePair<string, NewRangeComplex>> GetEnumerator()
        {
            return RangeComplexCollection.GetEnumerator();
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
                catch (Exception) { }
            }
        }

        #region public NewRangeComplex SelectedRangeComplex { get; set; }

        public NewRangeComplex SelectedRangeComplex
        {
            get { return _selectedRangeComplex; }
            set
            {
                if (_selectedRangeComplex == value) return;
                _selectedRangeComplex = value;
                NotifyPropertyChanged(SelectedRangeComplexChangedEventArgs);
                ClearEnvironment();
            }
        }

        static readonly PropertyChangedEventArgs SelectedRangeComplexChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.SelectedRangeComplex);
        NewRangeComplex _selectedRangeComplex;

        #endregion

        #region public int SelectedRangeComplexIndex { get; set; }

        public int SelectedRangeComplexIndex
        {
            get { return _selectedRangeComplexIndex; }
            set
            {
                if (_selectedRangeComplexIndex == value) return;
                _selectedRangeComplexIndex = value;
                NotifyPropertyChanged(SelectedRangeComplexIndexChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedRangeComplexIndexChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.SelectedRangeComplexIndex);
        int _selectedRangeComplexIndex = -1;

        #endregion

        #region public NAVOTimePeriod SelectedTimePeriod { get; set; }

        public NAVOTimePeriod SelectedTimePeriod
        {
            get { return _selectedTimePeriod; }
            set
            {
                if (_selectedTimePeriod == value) return;
                _selectedTimePeriod = value;
                NotifyPropertyChanged(SelectedTimePeriodChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedTimePeriodChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.SelectedTimePeriod);
        NAVOTimePeriod _selectedTimePeriod = NAVOTimePeriod.Invalid;

        #endregion

        #region public int SelectedTimePeriodIndex { get; set; }

        public int SelectedTimePeriodIndex
        {
            get { return _selectedTimePeriodIndex; }
            set
            {
                if (_selectedTimePeriodIndex == value) return;
                _selectedTimePeriodIndex = value;
                NotifyPropertyChanged(SelectedTimePeriodIndexChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedTimePeriodIndexChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.SelectedTimePeriodIndex);
        int _selectedTimePeriodIndex = -1;

        #endregion

        #region public RangeComplexArea SelectedArea { get; set; }

        public RangeComplexArea SelectedArea
        {
            get { return _selectedArea; }
            set
            {
                if (_selectedArea == value) return;
                _selectedArea = value;
                SelectedBathymetry = null;
                NotifyPropertyChanged(SelectedAreaChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedAreaChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.SelectedArea);
        RangeComplexArea _selectedArea;

        #endregion

        #region public int SelectedAreaIndex { get; set; }

        public int SelectedAreaIndex
        {
            get { return _selectedAreaIndex; }
            set
            {
                if (_selectedAreaIndex == value) return;
                _selectedAreaIndex = value;
                NotifyPropertyChanged(SelectedAreaIndexChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedAreaIndexChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.SelectedAreaIndex);
        int _selectedAreaIndex = -1;

        #endregion

        #region public int SelectedBathymetryIndex { get; set; }

        public int SelectedBathymetryIndex
        {
            get { return _selectedBathymetryIndex; }
            set
            {
                if (_selectedBathymetryIndex == value) return;
                _selectedBathymetryIndex = value;
                NotifyPropertyChanged(SelectedBathymetryIndexChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedBathymetryIndexChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.SelectedBathymetryIndex);
        int _selectedBathymetryIndex = -1;

        #endregion

        #region public BathymetryFile SelectedBathymetry { get; set; }

        public EnvironmentFile SelectedBathymetry
        {
            get { return _selectedBathymetry; }
            set
            {
                if (_selectedBathymetry == value) return;
                _selectedBathymetry = value;
                NotifyPropertyChanged(SelectedBathymetryChangedEventArgs);
                CheckEnvironment();
            }
        }

        static readonly PropertyChangedEventArgs SelectedBathymetryChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.SelectedBathymetry);
        EnvironmentFile _selectedBathymetry;

        #endregion

        #region public ObservableConcurrentDictionary<EnvironmentDataType, EnvironmentFile> SelectedEnvironment { get; set; }

        public ObservableConcurrentDictionary<EnvironmentDataType, EnvironmentFile> SelectedEnvironment
        {
            get { return _selectedEnvironment; }
            set
            {
                if (_selectedEnvironment == value) return;
                _selectedEnvironment = value;
                NotifyPropertyChanged(SelectedEnvironmentChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedEnvironmentChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.SelectedEnvironment);
        ObservableConcurrentDictionary<EnvironmentDataType, EnvironmentFile> _selectedEnvironment = new ObservableConcurrentDictionary<EnvironmentDataType, EnvironmentFile>
        {
            {EnvironmentDataType.Bathymetry, null},
            {EnvironmentDataType.BottomLoss, null},
            {EnvironmentDataType.Sediment, null},
            {EnvironmentDataType.Salinity, null},
            {EnvironmentDataType.SoundSpeed, null},
            {EnvironmentDataType.Temperature, null},
            {EnvironmentDataType.Wind, null},
        };

        #endregion

        #region public ObservableConcurrentDictionary<EnvironmentDataType, Task> EnvironmentData { get; set; }

        public ObservableConcurrentDictionary<EnvironmentDataType, Task> EnvironmentData
        {
            get { return _environmentData; }
            set
            {
                if (_environmentData == value) return;
                _environmentData = value;
                NotifyPropertyChanged(EnvironmentDataChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EnvironmentDataChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.EnvironmentData);
        ObservableConcurrentDictionary<EnvironmentDataType, Task> _environmentData = new ObservableConcurrentDictionary<EnvironmentDataType, Task>
        {
            {EnvironmentDataType.Bathymetry, null},
            {EnvironmentDataType.BottomLoss, null},
            {EnvironmentDataType.Sediment, null},
            {EnvironmentDataType.Salinity, null},
            {EnvironmentDataType.SoundSpeed, null},
            {EnvironmentDataType.Temperature, null},
            {EnvironmentDataType.Wind, null},
        };

        #endregion

        #region public bool IsEnvironmentFullySpecified { get; private set; }

        public bool IsEnvironmentFullySpecified
        {
            get { return _isEnvironmentFullySpecified; }
            private set
            {
                _isEnvironmentFullySpecified = value;
                NotifyPropertyChanged(IsEnvironmentFullySpecifiedChangedEventArgs);
                Debug.WriteLine("{0} Environment {1} fully specified", DateTime.Now, _isEnvironmentFullySpecified ? "IS" : "IS NOT");
            }
        }

        static readonly PropertyChangedEventArgs IsEnvironmentFullySpecifiedChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.IsEnvironmentFullySpecified);
        bool _isEnvironmentFullySpecified;

        #endregion

        #region public bool IsEnvironmentLoading { get; set; }

        public bool IsEnvironmentLoading
        {
            get { return _isEnvironmentLoading; }
            set
            {
                _isEnvironmentLoading = value;
                NotifyPropertyChanged(IsEnvironmentLoadingChangedEventArgs);
                Debug.WriteLine("{0} Environment {1} loading", DateTime.Now, _isEnvironmentLoading ? "IS" : "IS NOT");
            }
        }

        static readonly PropertyChangedEventArgs IsEnvironmentLoadingChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.IsEnvironmentLoading);
        bool _isEnvironmentLoading;

        #endregion

        #region public bool IsEnvironmentLoaded { get; set; }

        public bool IsEnvironmentLoaded
        {
            get { return _isEnvironmentLoaded; }
            set
            {
                _isEnvironmentLoaded = value;
                NotifyPropertyChanged(IsEnvironmentLoadedChangedEventArgs);
                Debug.WriteLine("{0} Environment {1} loaded", DateTime.Now, _isEnvironmentLoaded ? "IS" : "IS NOT");
            }
        }

        static readonly PropertyChangedEventArgs IsEnvironmentLoadedChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.IsEnvironmentLoaded);
        bool _isEnvironmentLoaded;

        #endregion


        public void CheckEnvironment()
        {
            const string bottomLossFilename = "data.bottomloss";
            const string sedimentFilename = "data.sediment";
            const string windFilename = "data.wind";
            if (SelectedRangeComplex != null)
            {
                SelectedEnvironment[EnvironmentDataType.BottomLoss] = SelectedRangeComplex.EnvironmentFiles[bottomLossFilename];
                EnvironmentData[EnvironmentDataType.BottomLoss] = new Task<BottomLoss>(() => BottomLoss.Load(Path.Combine(SelectedRangeComplex.DataPath, bottomLossFilename)));
                SelectedEnvironment[EnvironmentDataType.Sediment] = SelectedRangeComplex.EnvironmentFiles[sedimentFilename];
                EnvironmentData[EnvironmentDataType.Sediment] = new Task<Sediment>(() => Sediment.Load(Path.Combine(SelectedRangeComplex.DataPath, sedimentFilename)));
                if (SelectedTimePeriod != NAVOTimePeriod.Invalid)
                {
                    SelectedEnvironment[EnvironmentDataType.Wind] = SelectedRangeComplex.EnvironmentFiles[windFilename];
                    EnvironmentData[EnvironmentDataType.Wind] =
                            new Task<Wind>(() => Wind.Load(Path.Combine(SelectedRangeComplex.DataPath, windFilename)));
                    EnvironmentData[EnvironmentDataType.Salinity] =
                            new Task<SoundSpeed>(
                                    () =>
                                    EnvironmentFile.SeasonalAverage(SelectedRangeComplex, SelectedTimePeriod,
                                                                    EnvironmentDataType.Salinity));
                    EnvironmentData[EnvironmentDataType.Temperature] =
                            new Task<SoundSpeed>(
                                    () => EnvironmentFile.SeasonalAverage(SelectedRangeComplex, SelectedTimePeriod,
                                                                          EnvironmentDataType.Temperature));
                    if (SelectedArea != null)
                    {
                        if (SelectedBathymetry != null && SelectedBathymetry.IsCached)
                        {
                            SelectedEnvironment[EnvironmentDataType.Bathymetry] = SelectedBathymetry;
                            Task<Bathymetry> bathyTask;
                            EnvironmentData[EnvironmentDataType.Bathymetry] = bathyTask = new Task<Bathymetry>(() => Bathymetry.Load(Path.Combine(SelectedArea.BathymetryPath, SelectedBathymetry.FileName)));
                            SelectedEnvironment[EnvironmentDataType.SoundSpeed] = SelectedRangeComplex.EnvironmentFiles[string.Format("{0}.soundspeed", SelectedTimePeriod)];
                            EnvironmentData[EnvironmentDataType.SoundSpeed] =
                                new Task<SoundSpeed>(() => EnvironmentFile.CalculateSoundSpeed(SelectedRangeComplex, SelectedTimePeriod, bathyTask, SelectedBathymetry.GeoRect));
                            IsEnvironmentFullySpecified = true;
                            LoadEnvironment();
                            return;
                        }
                    }
                }
            }
            IsEnvironmentFullySpecified = false;
        }

        void LoadEnvironment()
        {
            LoadTask = null;
            if (!IsEnvironmentFullySpecified) return;
            LoadTask = new Task(async () =>
            {
                IsEnvironmentLoading = true;
                var tasks = new List<Task>();
                EnvironmentData[EnvironmentDataType.BottomLoss].Start();
                tasks.Add(EnvironmentData[EnvironmentDataType.BottomLoss]);
                EnvironmentData[EnvironmentDataType.Sediment].Start();
                tasks.Add(EnvironmentData[EnvironmentDataType.Sediment]);
                EnvironmentData[EnvironmentDataType.Wind].Start();
                tasks.Add(EnvironmentData[EnvironmentDataType.Wind]);
                EnvironmentData[EnvironmentDataType.Bathymetry].Start();
                tasks.Add(EnvironmentData[EnvironmentDataType.Bathymetry]);
                EnvironmentData[EnvironmentDataType.Salinity].Start();
                tasks.Add(EnvironmentData[EnvironmentDataType.Salinity]);
                EnvironmentData[EnvironmentDataType.Temperature].Start();
                tasks.Add(EnvironmentData[EnvironmentDataType.Temperature]);
                EnvironmentData[EnvironmentDataType.SoundSpeed].Start();
                tasks.Add(EnvironmentData[EnvironmentDataType.SoundSpeed]);
                await TaskEx.WhenAll(tasks).ContinueWith(task =>
                {
                    IsEnvironmentLoading = false;
                    IsEnvironmentLoaded = true;
                });
            });
            LoadTask.Start();
        }

        public Task LoadTask { get; private set; }

        public void ClearEnvironment()
        {
            IsEnvironmentLoaded = false;
            SelectedTimePeriod = NAVOTimePeriod.Invalid;
            SelectedArea = null;
            SelectedBathymetry = null;
            ClearEnvironment(EnvironmentDataType.BottomLoss);
            ClearEnvironment(EnvironmentDataType.Sediment);
            ClearEnvironment(EnvironmentDataType.Wind);
            ClearEnvironment(EnvironmentDataType.Bathymetry);
            ClearEnvironment(EnvironmentDataType.Salinity);
            ClearEnvironment(EnvironmentDataType.Temperature);
            ClearEnvironment(EnvironmentDataType.SoundSpeed);
        }

        void ClearEnvironment(EnvironmentDataType dataType)
        {
            if ((EnvironmentData[dataType] != null) && (EnvironmentData[dataType].IsCompleted)) EnvironmentData[dataType].Dispose();
            EnvironmentData[dataType] = null;
            SelectedEnvironment[dataType] = null;
        }

        public void HookEnvironment<T>(EnvironmentDataType dataType, Action<T> action)
        {
            try
            {
                if (EnvironmentData[dataType] != null) ((Task<T>)EnvironmentData[dataType]).ContinueWith(task => action(task.Result));
            }
            catch (AggregateException ae)
            {
                foreach (var e in ae.InnerExceptions) Debug.WriteLine("{0}: Error hooking {0}: {1}", DateTime.Now, dataType, e.Message);
            }
        }
    }
}
