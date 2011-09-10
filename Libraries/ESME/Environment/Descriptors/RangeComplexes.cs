using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using System.Windows.Threading;
using Cinch;
using ESME.Environment.NAVO;
using ESME.NEMO.Overlay;
using HRC;
using HRC.Navigation;

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
		            MaxDegreeOfParallelism = 1,
		        });
            var actionBlock = new ActionBlock<NewRangeComplex>(item => { if (item != null) _dispatcher.InvokeInBackgroundIfRequired(() => _rangeComplexes.Add(item)); });
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
            await actionBlock.Completion;
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

        public async Task<NewRangeComplex> CreateAsync(string rangeComplexName, double height, double latitude, double longitude, double geoid, ICollection<Geo> opAreaLimits, List<Geo> simAreaLimits)
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

    public abstract class TreeItem : ViewModelBase
    {
        #region public string Name { get; set; }

        public string Name
        {
            get { return _name; }
            set
            {
                if (_name == value) return;
                _name = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<TreeItem>(x => x.Name);
        string _name;

        #endregion

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

        static readonly PropertyChangedEventArgs IsEnabledChangedEventArgs = ObservableHelper.CreateArgs<TreeItem>(x => x.IsEnabled);
        bool _isEnabled;

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

        static readonly PropertyChangedEventArgs ToolTipChangedEventArgs = ObservableHelper.CreateArgs<TreeItem>(x => x.ToolTip);
        string _toolTip;

        #endregion

        #region public Progress<string> Status { get; set; }

        public Progress<string> Status
        {
            get { return _status; }
            set
            {
                if (_status == value) return;
                _status = value;
                NotifyPropertyChanged(StatusChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs StatusChangedEventArgs = ObservableHelper.CreateArgs<TreeItem>(x => x.Status);
        Progress<string> _status;

        #endregion

        #region public Progress<float> Progress { get; set; }

        public Progress<float> Progress
        {
            get { return _progress; }
            set
            {
                if (_progress == value) return;
                _progress = value;
                NotifyPropertyChanged(ProgressChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ProgressChangedEventArgs = ObservableHelper.CreateArgs<TreeItem>(x => x.Progress);
        Progress<float> _progress;

        #endregion
    }

    public abstract class TreeItem<T> : TreeItem
    {
        #region public T Data { get; set; }

        public T Data
        {
            get { return _data; }
            set
            {
                _data = value;
                NotifyPropertyChanged(DataChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DataChangedEventArgs = ObservableHelper.CreateArgs<TreeItem<T>>(x => x.Data);
        T _data;

        #endregion
    }

    public class PlainTreeItem : TreeItem<bool>
    {
        #region public List<PlainTreeItem> Children { get; set; }

        public List<PlainTreeItem> Children
        {
            get { return _children; }
            set
            {
                if (_children == value) return;
                _children = value;
                NotifyPropertyChanged(ChildrenChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ChildrenChangedEventArgs = ObservableHelper.CreateArgs<PlainTreeItem>(x => x.Children);
        List<PlainTreeItem> _children;

        #endregion

        public PlainTreeItem this[string childName] { get { return Children.Where(item => item.Name == childName).First(); } }
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
            _token = RangeComplexToken.Load(Path.Combine(DataPath, "token"));
            if ((_token.GeoRect == null ) || (!_token.GeoRect.Contains(GeoRect))) _token.ReextractionRequired = true;
            var fields = new List<PlainTreeItem>
            {
                new PlainTreeItem{Name = "January", IsEnabled = false},
                new PlainTreeItem{Name = "February", IsEnabled = false},
                new PlainTreeItem{Name = "March", IsEnabled = false},
                new PlainTreeItem{Name = "April", IsEnabled = false},
                new PlainTreeItem{Name = "May", IsEnabled = false},
                new PlainTreeItem{Name = "June", IsEnabled = false},
            };
            Environment = new PlainTreeItem
            {
                Name = "Environment",
                IsEnabled = true,
                Children = new List<PlainTreeItem>
                {
                    new PlainTreeItem {Name = "Sediment", IsEnabled = false},
                    new PlainTreeItem {Name = "Bottom Loss", IsEnabled = false},
                    new PlainTreeItem {Name = "Temperature", IsEnabled = true, Children = fields},
                    new PlainTreeItem {Name = "Salinity", IsEnabled = true, Children = fields},
                    new PlainTreeItem {Name = "Sound Speed", IsEnabled = true, Children = fields},
                    new PlainTreeItem {Name = "Wind", IsEnabled = true, Children = fields},
                }
            };
        }
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

	    public RangeComplexArea this[string areaName] { get { return _areaCollection.Where(item => item.Name == areaName).First(); } }

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

        public PlainTreeItem Environment { get; private set; }

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

        #region Validation
	    readonly RangeComplexToken _token;

        Task ValidateAsync() { return TaskEx.Run(Validate); }

        void Validate()
        {
            _token.ExtractionsRequired.AddRange(ValidateMonthlyData("temperature").Select(month => new Tuple<NAVOTimePeriod, string>(month, "temperature")));
            _token.ExtractionsRequired.AddRange(ValidateMonthlyData("salinity").Select(month => new Tuple<NAVOTimePeriod, string>(month, "salinity")));
            
            var windFilename = Path.Combine(DataPath, "wind.sediment");
            if (_token.ReextractionRequired || (!File.Exists(windFilename)) || (new FileInfo(windFilename).LastWriteTime > _token.LastWriteTime))
                _token.ExtractionsRequired.Add(new Tuple<NAVOTimePeriod, string>(0, "wind"));

            var sedimentFilename = Path.Combine(DataPath, "data.sediment");
            if (_token.ReextractionRequired || (!File.Exists(sedimentFilename)) || (new FileInfo(sedimentFilename).LastWriteTime > _token.LastWriteTime))
                _token.ExtractionsRequired.Add(new Tuple<NAVOTimePeriod, string>(0, "sediment"));
            
            if (Globals.AppSettings.IsNavyVersion)
            {
                var bottomLossFilename = Path.Combine(DataPath, "data.bottomloss");
                if (_token.ReextractionRequired || (!File.Exists(bottomLossFilename)) || (new FileInfo(bottomLossFilename).LastWriteTime > _token.LastWriteTime)) 
                    _token.ExtractionsRequired.Add(new Tuple<NAVOTimePeriod, string>(0, "bottomloss"));
            }
        }

        IEnumerable<NAVOTimePeriod> ValidateMonthlyData(string dataFileExtension)
        {
            var availableMonths = Directory.EnumerateFiles(DataPath, "*." + dataFileExtension).Select(item => (NAVOTimePeriod)Enum.Parse(typeof(NAVOTimePeriod), Path.GetFileNameWithoutExtension(item), true)).ToList();
            var missingMonths = new List<NAVOTimePeriod>();
            if (_token.ReextractionRequired) return NAVOConfiguration.AllMonths;
            foreach (var month in NAVOConfiguration.AllMonths)
            {
                var fileName = Path.Combine(DataPath, month.ToString().ToLower() + dataFileExtension);
                if (!availableMonths.Contains(month)) missingMonths.Add(month);
                else if ((!File.Exists(fileName)) || (new FileInfo(fileName).LastWriteTime > _token.LastWriteTime))
                    missingMonths.Add(month);
            }
            return missingMonths.Distinct();
        }

        [Serializable]
        class RangeComplexToken
        {
            RangeComplexToken()
            {
                GeoRect = null;
                _lastWriteTime = new DateTime(1, 1, 1, 0, 0, 0, 0);  // This should ensure that no files are older than the token's last write time
            }

            public void Save(string filename, GeoRect geoRect)
            {
                var formatter = new BinaryFormatter();
                GeoRect = geoRect;
                _lastWriteTime = DateTime.Now;
                ExtractionsRequired.Clear();
                using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None)) formatter.Serialize(stream, this);
            }

            public static RangeComplexToken Load(string filename)
            {
                if (!File.Exists(filename)) return new RangeComplexToken();
                RangeComplexToken result;
                var formatter = new BinaryFormatter();
                using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
                    result = (RangeComplexToken)formatter.Deserialize(stream);
                result._lastWriteTime = new FileInfo(filename).LastWriteTime;
                return result;
            }

            public static Task<RangeComplexToken> LoadAsync(string filename) { return TaskEx.Run(() => Load(filename)); }

            [NonSerialized] DateTime _lastWriteTime;
            [NonSerialized] public readonly List<Tuple<NAVOTimePeriod, string>> ExtractionsRequired = new List<Tuple<NAVOTimePeriod,string>>();
            [NonSerialized] public bool ReextractionRequired = false;
            public GeoRect GeoRect { get; private set; }

            public DateTime LastWriteTime { get { return _lastWriteTime; } }
        }

#endregion

        internal async static Task<NewRangeComplex> CreateAsync(string simAreaPath, string rangeComplexName, IEnumerable<Geo> opAreaLimits, List<Geo> simAreaLimits)
        {
            var result = new NewRangeComplex(simAreaPath, rangeComplexName);
            result.OpArea = RangeComplexArea.Create(result.AreasPath, Path.Combine(result.AreasPath, String.Format("{0}_OpArea", rangeComplexName)), opAreaLimits);
            result.SimArea = RangeComplexArea.Create(result.AreasPath, Path.Combine(result.AreasPath, String.Format("{0}_SimArea", rangeComplexName)), simAreaLimits);
            result.UpdateAreas();
            await result.ValidateAsync();
            // expand simArea by 200km for the initial environment extraction
            var extractionArea = new GeoRect(new Limits(ConvexHull.Create(simAreaLimits, true)).CreateExpandedLimit(200f).Geos);
            //result.InitializeAsync();
            return result;
        }

        internal async static Task<NewRangeComplex> ReadAsync(string simAreaPath, Tuple<string, double, double, double, double, string, string> rangeComplexInfo)
        {
            var result = new NewRangeComplex(simAreaPath, rangeComplexInfo.Item1);
            result.OpArea = result._areas.Where(area => area.Name == Path.GetFileNameWithoutExtension(rangeComplexInfo.Item6)).First();
            result.SimArea = result._areas.Where(area => area.Name == Path.GetFileNameWithoutExtension(rangeComplexInfo.Item7)).First();
            var simAreaLimits = result.SimArea.OverlayShape.Geos;
            await result.ValidateAsync();
            // expand simArea by 100km if we need to re-extract all data
            var extractionArea = new GeoRect(new Limits(ConvexHull.Create(simAreaLimits, true)).CreateExpandedLimit(200f).Geos);
            //result.InitializeAsync();
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
            IsInitializing = false;
            Debug.WriteLine("{0}: Range complex {1} initialization complete", DateTime.Now, Name);
        }

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

#if false
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
            var dataFile = Path.Combine(DataPath, dataFilename);
            Task<T> main;
            var result = File.Exists(dataFile) ? (main = loadFunc(dataFile)) : (main = readFunc()).ContinueWith(task => task.Result.Save(dataFile));
            result.ContinueWith(task => completionAction(main));
            mainTask = main;
            return result;
        }

        Task ReadDataAsync<T>(string dataFilename, AsyncArgs<T> args, out Task<T> mainTask) where T : ICanSave
        {
            var dataFile = Path.Combine(DataPath, dataFilename);
            Task<T> main;
            var result = File.Exists(dataFile)
                ? (main = args.LoadFunction(dataFile)).ContinueWith(task => args.CompletionAction(main))
                : (main = args.ReadFunction()).ContinueWith(task => task.Result.Save(dataFile)).ContinueWith(task2 => args.CompletionAction(main));
            mainTask = main;
            return result;
        }

	    public void OnDeserialization(object sender) { throw new NotImplementedException(); }
#endif
    }

    public class ResolutionTreeItem : TreeItem<uint> {}

    public class RangeComplexArea : ViewModelBase
    {
        private RangeComplexArea(string rangeComplexPath, string areaName, OverlayShape overlayShape)
        {
            _rangeComplexPath = rangeComplexPath;
            Name = areaName;
            OverlayShape = overlayShape;
            GeoRect = new GeoRect(overlayShape.BoundingBox);
            BathymetryPath = Path.Combine(_rangeComplexPath, "Data", Name);
            _availableResolutions = new ObservableCollection<ResolutionTreeItem>();
            AvailableResolutions = new ReadOnlyObservableCollection<ResolutionTreeItem>(_availableResolutions);
            UpdateAvailableBathymetry();
        }

        public void UpdateAvailableBathymetry()
        {
            _availableResolutions.Clear();
            foreach (var samplesPerDegree in AvailableSampleCountsPerDegree)
            {
                var resolutionString = string.Format("{0:0.00}min", 60.0 / samplesPerDegree);
                var north = Math.Round(GeoRect.North * samplesPerDegree) / samplesPerDegree;
                var south = Math.Round(GeoRect.South * samplesPerDegree) / samplesPerDegree;
                var east = Math.Round(GeoRect.East * samplesPerDegree) / samplesPerDegree;
                var west = Math.Round(GeoRect.West * samplesPerDegree) / samplesPerDegree;
                var width = (uint)(east - west);
                var height = (uint)(north - south);
                _availableResolutions.Add(new ResolutionTreeItem
                {
                    Name = resolutionString,
                    IsEnabled = File.Exists(Path.Combine(BathymetryPath, resolutionString + ".bathymetry")),
                    Data = width * samplesPerDegree * height * samplesPerDegree,
                });
            }
        }

        internal static RangeComplexArea Create(string rangeComplexPath, string areaName, IEnumerable<Geo> limits)
        {
            var areaPath = Path.Combine(rangeComplexPath, "Areas", areaName + ".ovr");
            OverlayFile.Create(areaPath, limits);
            var overlay = new OverlayFile(areaPath);
            return new RangeComplexArea(rangeComplexPath, areaName, overlay.Shapes[0]);
        }

        internal static RangeComplexArea Read(string areaPath)
        {
            var overlay = new OverlayFile(areaPath);
            return new RangeComplexArea(Path.GetDirectoryName(Path.GetDirectoryName(areaPath)), Path.GetFileNameWithoutExtension(areaPath), overlay.Shapes[0]);
        }

        [NotNull] public ReadOnlyObservableCollection<ResolutionTreeItem> AvailableResolutions { get; private set; }
        [NotNull] public string Name { get; private set; }
        [NotNull] public string BathymetryPath { get; private set; }
        [NotNull] public GeoRect GeoRect { get; private set; }
        [NotNull] public OverlayShape OverlayShape { get; private set; }

        [NotNull] readonly ObservableCollection<ResolutionTreeItem> _availableResolutions;
        [NotNull] readonly string _rangeComplexPath;

        static readonly List<uint> AvailableSampleCountsPerDegree = new List<uint> { 30, 60, 120, 600, 1200 };
    }
}
