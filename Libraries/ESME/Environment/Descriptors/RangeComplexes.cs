using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using System.Windows.Threading;
using Cinch;
using ESME.Environment.NAVO;
using ESME.NEMO.Overlay;
using HRC;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    public class RangeComplexes : ViewModelBase
    {
        RangeComplexes()
        {
            _rangeComplexes = new ObservableCollection<NewRangeComplex>();
            _dispatcher = Dispatcher.CurrentDispatcher;
            RangeComplexCollection = new ReadOnlyObservableCollection<NewRangeComplex>(_rangeComplexes);
        }

        #region Private fields
        readonly object _lockObject = new object();
        readonly ObservableCollection<NewRangeComplex> _rangeComplexes;
        readonly Dispatcher _dispatcher;
        #endregion

        #region public ReadOnlyObservableCollection<NewRangeComplex> RangeComplexCollection { get; private set; }

        public ReadOnlyObservableCollection<NewRangeComplex> RangeComplexCollection
        {
            get { return _rangeComplexCollection; }
            private set
            {
                if (_rangeComplexCollection == value) return;
                _rangeComplexCollection = value;
                NotifyPropertyChanged(RangeComplexListChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RangeComplexListChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.RangeComplexCollection);
        ReadOnlyObservableCollection<NewRangeComplex> _rangeComplexCollection;

        #endregion

        #region public string SimAreaCSVFile { get; set; }

        public string SimAreaCSVFile
        {
            get { return _simAreaCSVFile; }
            set
            {
                if (_simAreaCSVFile == value) return;
                _simAreaCSVFile = value;
                _rangeComplexes.Clear();
                if (_simAreaCSVFile != null) InitializeAsync(_simAreaCSVFile);
                NotifyPropertyChanged(SimAreaCSVFileChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SimAreaCSVFileChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexes>(x => x.SimAreaCSVFile);
        string _simAreaCSVFile;

        #endregion

        async Task InitializeAsync(string simAreaFile)
        {
            var ranges = new List<NewRangeComplex>();
            if (!File.Exists(simAreaFile)) throw new FileNotFoundException("Error reading sim area file", simAreaFile);
            SimAreaPath = Path.GetDirectoryName(simAreaFile);
		    var transformBlock = new TransformBlock<Tuple<string, double, double, double, double, string, string>, NewRangeComplex>(
		        async info => await NewRangeComplex.ReadAsync(SimAreaPath, info),
		        new ExecutionDataflowBlockOptions
		        {
		            TaskScheduler = TaskScheduler.Default,
		            MaxDegreeOfParallelism = 4,
		        });
            var actionBlock = new ActionBlock<NewRangeComplex>(item => { if (item != null) ranges.Add(item); });
            transformBlock.LinkTo(actionBlock);
            var lines = File.ReadAllLines(simAreaFile);
            foreach (var line in lines)
            {
                if (line == null) return;
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
                transformBlock.Post(new Tuple<string, double, double, double, double, string, string>(rangeComplexName, height, latitude, longitude, geoid, opsLimitFile, simLimitFile));
            }
		    transformBlock.Complete();
            transformBlock.Completion.ContinueWith(t => actionBlock.Complete());
            actionBlock.Completion.ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() =>
            {
                _rangeComplexes.Clear();
                foreach (var range in ranges) _rangeComplexes.Add(range);
            }));
        }

        public string SimAreaPath { get; private set; }
        static RangeComplexes _instance;
        public static RangeComplexes Singleton { get { return _instance ?? (_instance = new RangeComplexes()); } }
		public ReadOnlyCollection<string> RangeComplexNames { get; private set; }

        public NewRangeComplex this[string rangeComplexName]
        {
            get
            {
                var matches = _rangeComplexes.Where(complex => complex.Name == rangeComplexName).ToList();
                if (matches.Count == 0) throw new IndexOutOfRangeException(string.Format("Range complex {0} not found", rangeComplexName));
                if (matches.Count > 1) throw new ApplicationException(string.Format("Multiple matches found for range complex {0}", rangeComplexName));
                return matches.First();
            }
        }

        public async Task<NewRangeComplex> CreateAsync(string rangeComplexName, double height, double latitude, double longitude, double geoid, IList<Geo> opAreaLimits, IList<Geo> simAreaLimits)
        {
            if (opAreaLimits == null) throw new ArgumentNullException("opAreaLimits");
            if (simAreaLimits == null) throw new ArgumentNullException("simAreaLimits");
            if (opAreaLimits.Count < 4) throw new ArgumentException("Must have at least four coordinates", "opAreaLimits");
            if (simAreaLimits.Count < 4) throw new ArgumentException("Must have at least four coordinates", "simAreaLimits");

            var rangeComplexPath = Path.Combine(SimAreaPath, rangeComplexName);

            if (Directory.Exists(rangeComplexPath) || (_rangeComplexes.Where(complex => complex.Name == rangeComplexName).Count() != 0)) throw new ApplicationException(string.Format("Range complex {0} already exists", rangeComplexName));

            var result = await NewRangeComplex.CreateAsync(SimAreaPath, rangeComplexName, opAreaLimits, simAreaLimits);

            lock (_lockObject)
            {
                var needsExtraNewline = !File.ReadAllText(SimAreaCSVFile).EndsWith("\n");
                using (var writer = new StreamWriter(SimAreaCSVFile, true))
                {
                    if (needsExtraNewline) writer.WriteLine();
                    writer.WriteLine("{0},{1:0.0###},{2:0.0###},{3:0.0###},{4:0.0###},{5},{6}", rangeComplexName.Trim(),
                                     latitude, longitude, height, geoid,
                                     Path.GetFileName(result.OpArea.Name).Trim(), Path.GetFileName(result.SimArea.Name).Trim());
                }
            }
		    _rangeComplexes.Add(result);
            return result;
        }

		//public async Task<RangeComplex> AddAsync(string rangeComplexName, List<Geo> opAreaLimits, List<Geo> simAreaLimits){}
    }
	
	public class NewRangeComplex : ViewModelBase
	{
        NewRangeComplex(string simAreaPath, string rangeComplexName)
        {
            var rangeComplexPath = Path.Combine(simAreaPath, rangeComplexName);
            Name = rangeComplexName;
            RangeComplexPath = rangeComplexPath;
            AreasPath = Path.Combine(rangeComplexPath, "Areas");
            BathymetryPath = Path.Combine(rangeComplexPath, "Bathymetry");
            DataPath = Path.Combine(rangeComplexPath, "Data");
            EnvironmentPath = Path.Combine(rangeComplexPath, "Environment");
            ImagesPath = Path.Combine(rangeComplexPath, "Images");
            SpeciesPath = Path.Combine(rangeComplexPath, "Species");
            Directory.CreateDirectory(RangeComplexPath);
            Directory.CreateDirectory(AreasPath);
            Directory.CreateDirectory(BathymetryPath);
            Directory.CreateDirectory(DataPath);
            Directory.CreateDirectory(EnvironmentPath);
            Directory.CreateDirectory(ImagesPath);
            Directory.CreateDirectory(SpeciesPath);
            Directory.CreateDirectory(Path.Combine(RangeComplexPath, "GeographicAreas"));
            _areas = new ObservableCollection<RangeComplexArea>();
            UpdateAreas();
            _temperatureArgs = new AsyncArgs<SoundSpeed>(SoundSpeed.LoadAsync, () => GDEM.ReadTemperatureAsync(NAVOConfiguration.AllMonths.ToList(), GeoRect), task => Temperature = task.Result);
            _salinityArgs = new AsyncArgs<SoundSpeed>(SoundSpeed.LoadAsync, () => GDEM.ReadSalinityAsync(NAVOConfiguration.AllMonths.ToList(), GeoRect), task => Salinity = task.Result);
        }
        readonly AsyncArgs<SoundSpeed> _temperatureArgs, _salinityArgs, _soundspeedArgs;
        readonly ObservableCollection<RangeComplexArea> _areas;

	    #region public ReadOnlyObservableCollection<RangeComplexArea> AreaCollection { get; private set; }

	    public ReadOnlyObservableCollection<RangeComplexArea> AreaCollection
	    {
	        get { return _areaCollection; }
            private set
	        {
	            if (_areaCollection == value) return;
	            _areaCollection = value;
	            NotifyPropertyChanged(AreaCollectionChangedEventArgs);
	        }
	    }

	    static readonly PropertyChangedEventArgs AreaCollectionChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplex>(x => x.AreaCollection);
	    ReadOnlyObservableCollection<RangeComplexArea> _areaCollection;

	    #endregion

	    #region public bool IsInitializing { get; private set; }

	    public bool IsInitializing
	    {
	        get { return _isInitializing; }
	        private set
	        {
	            if (_isInitializing == value) return;
	            _isInitializing = value;
	            NotifyPropertyChanged(IsInitializingChangedEventArgs);
	        }
	    }

	    static readonly PropertyChangedEventArgs IsInitializingChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplex>(x => x.IsInitializing);
	    bool _isInitializing = true;

	    #endregion

	    #region public string InitializationState { get; private set; }

	    public string InitializationState
	    {
	        get { return _initializationState; }
	        private set
	        {
	            if (_initializationState == value) return;
	            _initializationState = value;
	            NotifyPropertyChanged(InitializationStateChangedEventArgs);
	        }
	    }

	    static readonly PropertyChangedEventArgs InitializationStateChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplex>(x => x.InitializationState);
	    string _initializationState;

	    #endregion

        #region public IEnumerable<Tuple<object, string>> Environment { get; }

        public IEnumerable<Tuple<object, string>> Environment
	    {
	        get
	        {
                yield return new Tuple<object, string>(Temperature, "Temperature");
                yield return new Tuple<object, string>(Salinity, "Salinity");
                yield return new Tuple<object, string>(SoundSpeed, "Soundspeed");
            }
	    }

	    #endregion


	    #region public int InitializationProgress { get; set; }

	    public int InitializationProgress
	    {
	        get { return _initializationProgress; }
	        set
	        {
	            if (_initializationProgress == value) return;
	            _initializationProgress = value;
	            NotifyPropertyChanged(InitializationProgressChangedEventArgs);
	        }
	    }

	    static readonly PropertyChangedEventArgs InitializationProgressChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplex>(x => x.InitializationProgress);
	    int _initializationProgress;

	    #endregion


        internal async static Task<NewRangeComplex> CreateAsync(string simAreaPath, string rangeComplexName, IEnumerable<Geo> opAreaLimits, IEnumerable<Geo> simAreaLimits)
        {
            var result = new NewRangeComplex(simAreaPath, rangeComplexName);
            result.OpArea = RangeComplexArea.Create(result.AreasPath, Path.Combine(result.AreasPath, String.Format("{0}_OpArea", rangeComplexName)), opAreaLimits);
            result.SimArea = RangeComplexArea.Create(result.AreasPath, Path.Combine(result.AreasPath, String.Format("{0}_SimArea", rangeComplexName)), simAreaLimits);
            result.UpdateAreas();
            result.InitializeAsync();
            return result;
        }

        internal async static Task<NewRangeComplex> ReadAsync(string simAreaPath, Tuple<string, double, double, double, double, string, string> rangeComplexInfo)
        {
            var result = new NewRangeComplex(simAreaPath, rangeComplexInfo.Item1);
            result.OpArea = result._areas.Where(area => area.Name == Path.GetFileNameWithoutExtension(rangeComplexInfo.Item6)).First();
            result.SimArea = result._areas.Where(area => area.Name == Path.GetFileNameWithoutExtension(rangeComplexInfo.Item7)).First();
            result.InitializeAsync();
            return result;
        }

        async Task InitializeAsync()
        {
            Debug.WriteLine("{0}: Range complex {1} initializing", DateTime.Now, Name);
            IsInitializing = true;
            InitializationState = "initializing";
            var months = NAVOConfiguration.AllMonths.ToList();
            //var temperaturePath = Path.Combine(DataPath, "data.temperature");
            //Task<SoundSpeed> temperatureTask = null, salinityTask = null;
            var currentState = new Progress<string>(p => InitializationState = p);
            var progress = new Progress<float>(p => InitializationProgress = (int)p);
#if true
            var continuations = new List<Task>
            {
                GDEM.ImportByMonthAsync(DataPath, true, true, months, GeoRect, currentState, progress),
                //ReadDataAsync(Path.Combine(DataPath, "data.temperature"), SoundSpeed.LoadAsync, () => GDEM.ReadTemperatureAsync(NAVOConfiguration.AllMonths.ToList(), GeoRect), task => Temperature = task.Result, out temperatureTask), 
                //ReadDataAsync(Path.Combine(DataPath, "data.salinity"), SoundSpeed.LoadAsync, () => GDEM.ReadSalinityAsync(NAVOConfiguration.AllMonths.ToList(), GeoRect), task => Salinity = task.Result, out salinityTask)
            };
#else
            if (File.Exists(temperaturePath)) continuations.Add((temperatureTask = SoundSpeed.LoadAsync(temperaturePath)).ContinueWith(task => Temperature = task.Result));
            else
            {
                temperatureTask = GDEM.ReadTemperatureAsync(months, GeoRect);
                continuations.Add(temperatureTask.ContinueWith(task => task.Result.Save(temperaturePath)).ContinueWith(task => Temperature = temperatureTask.Result));
            }
            var salinityPath = Path.Combine(DataPath, "data.salinity");
            if (File.Exists(salinityPath)) continuations.Add((salinityTask = SoundSpeed.LoadAsync(salinityPath)).ContinueWith(task => Salinity = task.Result));
            else
            {
                salinityTask = GDEM.ReadSalinityAsync(months, GeoRect);
                continuations.Add(salinityTask.ContinueWith(task => task.Result.Save(salinityPath)));
                continuations.Add(salinityTask.ContinueWith(task => Salinity = task.Result));
            }
#endif

            //await TaskEx.WhenAll(temperatureTask, salinityTask);
            //var soundSpeedTask = GDEM.CalculateSoundSpeedAsync(temperatureTask.Result, salinityTask.Result, null);
            //continuations.Add(soundSpeedTask.ContinueWith(task => SoundSpeed = task.Result));

            await TaskEx.WhenAll(continuations);
            Temperature = Salinity = SoundSpeed = null;
            IsInitializing = false;
            Debug.WriteLine("{0}: Range complex {1} initialization complete", DateTime.Now, Name);
        }

        public SoundSpeed Temperature { get; private set; }
        public SoundSpeed Salinity { get; private set; }
        public SoundSpeed SoundSpeed { get; private set; }

        void UpdateAreas()
        {
            _areas.Clear();
            foreach (var areaFile in Directory.EnumerateFiles(AreasPath, "*.ovr")) _areas.Add(RangeComplexArea.Read(areaFile));
            GeoRect = GeoRect.Union(_areas.Select(area => area.GeoRect).ToArray());
            AreaCollection = new ReadOnlyObservableCollection<RangeComplexArea>(_areas);
        }

        [NotNull] public string Name { get; private set; }
        [NotNull] public string RangeComplexPath { get; private set; }
        [NotNull] public string AreasPath { get; private set; }
        [NotNull] public string BathymetryPath { get; private set; }
        [NotNull] public string DataPath { get; private set; }
        [NotNull] public string EnvironmentPath { get; private set; }
        [NotNull] public string ImagesPath { get; private set; }
        [NotNull] public string SpeciesPath { get; private set; }
        [NotNull] public RangeComplexArea OpArea { get; private set; }
        [NotNull] public RangeComplexArea SimArea { get; private set; }

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
                    NotifyPropertyChanged(GeoRectChangedEventArgs);
                }
                else
                {
                    if (_geoRect.Contains(value)) return;
                    _geoRect = value;
                    NotifyPropertyChanged(GeoRectChangedEventArgs);
                    OnGeoRectChanged();
                }
            }
	    }

	    static readonly PropertyChangedEventArgs GeoRectChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplex>(x => x.GeoRect);
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

        class AsyncArgs<T> where T : ICanSave
        {
            public AsyncArgs(Func<string, Task<T>> loadFunc, Func<Task<T>> readFunc, Action<Task<T>> completionAction)
            {
                LoadFunction = loadFunc;
                ReadFunction = readFunc;
                CompletionAction = completionAction;
            }

            public Func<string, Task<T>> LoadFunction { get; private set; }
            public Func<Task<T>> ReadFunction { get; private set; }
            public Action<Task<T>> CompletionAction { get; private set; }
        }

        Task ReadDataAsync<T>(string dataFilename, Func<string, Task<T>> loadFunc, Func<Task<T>> readFunc, Action<Task<T>> completionAction, out Task<T> mainTask) where T : ICanSave
        {
            // args.Item1 = mainTask
            // args.Item2 = continuationTask
            // args.Item3 = loadFunc
            // args.Item4 = readFunc
            // args.Item5 = completionAction
            var dataFile = Path.Combine(DataPath, dataFilename);
            Task<T> main;
            var result = File.Exists(dataFile) ? (main = loadFunc(dataFile)) : (main = readFunc()).ContinueWith(task => task.Result.Save(dataFile));
            result.ContinueWith(task => completionAction(main));
            mainTask = main;
            return result;
        }

        Task ReadDataAsync<T>(string dataFilename, AsyncArgs<T> args, out Task<T> mainTask) where T : ICanSave
        {
            // args.Item1 = mainTask
            // args.Item2 = continuationTask
            // args.Item3 = loadFunc
            // args.Item4 = readFunc
            // args.Item5 = completionAction
            var dataFile = Path.Combine(DataPath, dataFilename);
            Task<T> main;
            var result = File.Exists(dataFile)
                ? (main = args.LoadFunction(dataFile)).ContinueWith(task => args.CompletionAction(main))
                : (main = args.ReadFunction()).ContinueWith(task => task.Result.Save(dataFile)).ContinueWith(task2 => args.CompletionAction(main));
            mainTask = main;
            return result;
        }
    }

    public class RangeComplexArea : ViewModelBase
    {
        internal static RangeComplexArea Create(string areasPath, string areaName, IEnumerable<Geo> limits)
        {
            var areaPath = Path.Combine(areasPath, areaName + ".ovr");
            OverlayFile.Create(areaPath, limits);
            var overlay = new OverlayFile(areaPath);
            return new RangeComplexArea
            {
                Name = areaName,
                OverlayShape = overlay.Shapes[0],
                GeoRect = new GeoRect(overlay.Shapes[0].BoundingBox),
            };
        }

        internal static RangeComplexArea Read(string areaPath)
        {
            var overlay = new OverlayFile(areaPath);
            return new RangeComplexArea
            {
                Name = Path.GetFileNameWithoutExtension(areaPath),
                OverlayShape = overlay.Shapes[0],
                GeoRect = new GeoRect(overlay.Shapes[0].BoundingBox),
            };
        }

        [NotNull] public string Name { get; private set; }
        [NotNull] public GeoRect GeoRect { get; private set; }
        [NotNull] public OverlayShape OverlayShape { get; private set; }
    }
}
