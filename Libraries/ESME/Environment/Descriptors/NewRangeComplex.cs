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
using HRC;
using HRC.Navigation;

namespace ESME.Environment.Descriptors
{
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
            _token = RangeComplexToken.Load(Path.Combine(DataPath, "data.token"));
            if ((_token.GeoRect == null ) || (!_token.GeoRect.Contains(GeoRect))) _token.ReextractionRequired = true;
            EnvironmentTree = new EnvironmentDataTree
            {
                Name = "Environment",
                IsEnabled = true,
                Children = new List<EnvironmentTreeItem>
                {
                    new SampleCountTreeItem {Name = "Sediment", IsEnabled = false},
                    new SampleCountTreeItem {Name = "Bottom Loss", IsEnabled = false},
                    new EnvironmentTreeItem {Name = "Temperature", IsEnabled = true},
                    new EnvironmentTreeItem {Name = "Salinity", IsEnabled = true},
                    new EnvironmentTreeItem {Name = "Wind", IsEnabled = true},
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

        public EnvironmentDataTree EnvironmentTree { get; private set; }

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

        async Task ValidateAsync()
        {
            _token.ExtractionsRequired.Add(new Tuple<object, string>(ValidateMonthlyData("temperature").ToList(), "temperature"));
            _token.ExtractionsRequired.Add(new Tuple<object, string>(ValidateMonthlyData("salinity").ToList(), "salinity"));
            
            var windFilename = Path.Combine(DataPath, "data.wind");
            if (_token.ReextractionRequired || (!File.Exists(windFilename)) || (new FileInfo(windFilename).LastWriteTime > _token.LastWriteTime))
                _token.ExtractionsRequired.Add(new Tuple<object, string>(null, "wind"));

            var sedimentFilename = Path.Combine(DataPath, "data.sediment");
            if (_token.ReextractionRequired || (!File.Exists(sedimentFilename)) || (new FileInfo(sedimentFilename).LastWriteTime > _token.LastWriteTime))
                _token.ExtractionsRequired.Add(new Tuple<object, string>(null, "sediment"));
            
            if (Globals.AppSettings.IsNavyVersion)
            {
                var bottomLossFilename = Path.Combine(DataPath, "data.bottomloss");
                if (_token.ReextractionRequired || (!File.Exists(bottomLossFilename)) || (new FileInfo(bottomLossFilename).LastWriteTime > _token.LastWriteTime))
                    _token.ExtractionsRequired.Add(new Tuple<object, string>(null, "bottomloss"));
            }
            if (_token.ExtractionsRequired == null) return;
            await TaskEx.WhenAll(ImportEnvironmentAsync(_token.ExtractionsRequired));
            _token.Save(Path.Combine(DataPath, "data.token"), GeoRect);
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

        IEnumerable<Task> ImportEnvironmentAsync(IEnumerable<Tuple<object, string>> itemsToExtract)
        {
            var result = new List<Task>();
            foreach (var item in itemsToExtract)
            {
                switch (item.Item2.ToLower().Trim())
                {
                    case "temperature":
                        var temperatureMonths = (List<NAVOTimePeriod>)item.Item1;
                        var temperature = EnvironmentTree["Temperature"];
                        temperature.IsInitializing = true;
                        temperature.CreateProgressTrackers();
                        result.Add(GDEM.ImportByMonthAsync(DataPath, true, false, temperatureMonths, GeoRect, temperature.Status, temperature.Progress).ContinueWith(t => temperature.IsInitializing = false));
                        break;
                    case "salinity":
                        var salinityMonths = (List<NAVOTimePeriod>)item.Item1;
                        var salinity = EnvironmentTree["Salinity"];
                        salinity.IsInitializing = true;
                        salinity.CreateProgressTrackers();
                        result.Add(GDEM.ImportByMonthAsync(DataPath, false, true, salinityMonths, GeoRect, salinity.Status, salinity.Progress).ContinueWith(t => salinity.IsInitializing = false));
                        break;
                    case "wind":
                        var wind = EnvironmentTree["Wind"];
                        wind.IsInitializing = true;
                        wind.CreateProgressTrackers();
                        result.Add(SMGC.ImportAsync(DataPath, GeoRect, wind.Status, wind.Progress).ContinueWith(t => wind.IsInitializing = false));
                        break;
                    case "sediment":
                        var sediment = EnvironmentTree["Sediment"];
                        sediment.IsInitializing = true;
                        sediment.CreateProgressTrackers();
                        // BST is fire and forget, for now
                        BST.ImportAsync(DataPath, GeoRect, () => sediment.IsInitializing = false, sediment.Status, sediment.Progress);
                        break;
                    case "bottomloss":
                        var bottomLoss = EnvironmentTree["Bottom Loss"];
                        bottomLoss.IsInitializing = true;
                        bottomLoss.CreateProgressTrackers();
                        result.Add(BottomLossDatabase.ImportAsync(DataPath, GeoRect, bottomLoss.Status, bottomLoss.Progress).ContinueWith(t => bottomLoss.IsInitializing = false));
                        break;
                    default:
                        break;
                }
            }
            return result;
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
                result.ExtractionsRequired = new List<Tuple<object, string>>();
                return result;
            }

            public static Task<RangeComplexToken> LoadAsync(string filename) { return TaskEx.Run(() => Load(filename)); }

            [NonSerialized] DateTime _lastWriteTime;
            [NonSerialized] public List<Tuple<object, string>> ExtractionsRequired = new List<Tuple<object, string>>();
            [NonSerialized] public bool ReextractionRequired;
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

        internal async static Task<NewRangeComplex> ReadAsync(string simAreaPath, Tuple<string, double, double, double, double, string, string> rangeComplexInfo, Action<NewRangeComplex> action)
        {
            var result = new NewRangeComplex(simAreaPath, rangeComplexInfo.Item1);
            action(result);
            result.OpArea = result._areas.Where(area => area.Name == Path.GetFileNameWithoutExtension(rangeComplexInfo.Item6)).First();
            result.SimArea = result._areas.Where(area => area.Name == Path.GetFileNameWithoutExtension(rangeComplexInfo.Item7)).First();
            var simAreaLimits = result.SimArea.OverlayShape.Geos;
            await result.ValidateAsync();
            await result.GetEnvironmentMetadataAsync();
            // expand simArea by 100km if we need to re-extract all data
            var extractionArea = new GeoRect(new Limits(ConvexHull.Create(simAreaLimits, true)).CreateExpandedLimit(200f).Geos);
            //result.InitializeAsync();
            return result;
        }

        Task GetEnvironmentMetadataAsync()
        {
            var soundSpeedAction = new ActionBlock<Tuple<NAVOTimePeriod, string, SampleCountTreeItem>>(
                async item =>
                {
                    SoundSpeed data = await SoundSpeed.LoadAsync(Path.Combine(DataPath, string.Format("{0}.{1}", item.Item1.ToString().ToLower(), item.Item2)));
                    var curField = data[item.Item1];
                    item.Item3.SampleCount = (uint)curField.EnvironmentData.Count;
                    item.Item3.GeoRect = curField.EnvironmentData.GeoRect;
                    item.Item3.Name = curField.TimePeriod.ToString();
                    item.Item3.IsInitializing = false;
                }, new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    MaxDegreeOfParallelism = 2,
                });
            foreach (var month in NAVOConfiguration.AllMonths)
            {
                var monthName = month.ToString();
                EnvironmentTree["Temperature"].Children.Add(new SoundSpeedTreeItem { Name = monthName, IsInitializing = true });
                soundSpeedAction.Post(new Tuple<NAVOTimePeriod, string, SampleCountTreeItem>(month, "temperature", (SampleCountTreeItem)EnvironmentTree["Temperature"][monthName]));
                EnvironmentTree["Salinity"].Children.Add(new SoundSpeedTreeItem { Name = monthName });
                soundSpeedAction.Post(new Tuple<NAVOTimePeriod, string, SampleCountTreeItem>(month, "salinity", (SampleCountTreeItem)EnvironmentTree["Salinity"][monthName]));
            }
            soundSpeedAction.Complete();
            var windTask = Wind.LoadAsync(Path.Combine(DataPath, "data.wind")).ContinueWith(wind =>
            {
                var windData = wind.Result;
                foreach (var timePeriod in windData.TimePeriods)
                {
                    var periodName = timePeriod.TimePeriod.ToString();
                    EnvironmentTree["Wind"].Children.Add(new TimePeriodEnvironmentTreeItem
                    {
                        Name = periodName, 
                        SampleCount = (uint)timePeriod.EnvironmentData.Count, 
                        GeoRect = timePeriod.EnvironmentData.GeoRect, 
                        IsInitializing = false
                    });
                }
            });

            var sedimentTask = Sediment.LoadAsync(Path.Combine(DataPath, "data.sediment")).ContinueWith(sediment =>
            {
                var sedimentData = sediment.Result;
                var sedimentRoot = (SampleCountTreeItem)EnvironmentTree["Sediment"];
                sedimentRoot.SampleCount = (uint)sedimentData.Samples.Count;
                sedimentRoot.GeoRect = sedimentData.Samples.GeoRect;
                sedimentRoot.IsInitializing = false;
            });

            var bottomLossTask = BottomLoss.LoadAsync(Path.Combine(DataPath, "data.bottomLoss")).ContinueWith(bottomLoss =>
            {
                var bottomLossData = bottomLoss.Result;
                var bottomLossRoot = (SampleCountTreeItem)EnvironmentTree["Bottom Loss"];
                bottomLossRoot.SampleCount = (uint)bottomLossData.Samples.Count;
                bottomLossRoot.GeoRect = bottomLossData.Samples.GeoRect;
                bottomLossRoot.IsInitializing = false;
            });

            return TaskEx.WhenAll(soundSpeedAction.Completion, windTask, sedimentTask, bottomLossTask);
        }

        async Task InitializeAsync()
        {
            Debug.WriteLine("{0}: Range complex {1} initializing", DateTime.Now, Name);
            var months = NAVOConfiguration.AllMonths.ToList();
            //var temperaturePath = Path.Combine(DataPath, "data.temperature");
            //Task<SoundSpeed> temperatureTask = null, salinityTask = null;
#if true
            var continuations = new List<Task>
            {
                //GDEM.ImportByMonthAsync(DataPath, true, true, months, GeoRect, currentState, progress),
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

#if true
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

        Task<T> ReadAsync<T>(string dataFilename, Func<string, Task<T>> readFunc)
        {
            var dataFile = Path.Combine(DataPath, dataFilename);
            return readFunc(dataFile);
        }

        Task ExtractAsync<T>(string dataFilename, Func<Task<T>> extractFunc) where T : ICanSave
        {
            var dataFile = Path.Combine(DataPath, dataFilename);
            return extractFunc().ContinueWith(task => task.Result.Save(dataFile));
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
}