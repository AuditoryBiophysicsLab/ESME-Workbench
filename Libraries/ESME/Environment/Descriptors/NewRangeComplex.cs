using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
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
            TemperatureFiles = new TimePeriodEnvironmentFileList<SoundSpeed>();
            TemperatureFiles.CollectionChanged += (s, e) => UpdateTree(s, e, EnvironmentTree["Temperature"]);
            SalinityFiles = new TimePeriodEnvironmentFileList<SoundSpeed>();
            TemperatureFiles.CollectionChanged += (s, e) => UpdateTree(s, e, EnvironmentTree["Salinity"]);
            WindFiles = new TimePeriodEnvironmentFileList<Wind>();
            TemperatureFiles.CollectionChanged += (s, e) => UpdateTree(s, e, EnvironmentTree["Wind"]);
            _areas = new ObservableCollection<RangeComplexArea>();
            _token = RangeComplexToken.Load(Path.Combine(DataPath, "data.token"));
            UpdateAreas();
            if ((_token.GeoRect == null) || (!_token.GeoRect.Contains(GeoRect))) _token.ReextractionRequired = true;
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

        void UpdateTree(object sender, NotifyCollectionChangedEventArgs args, EnvironmentTreeItem tree)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (TimePeriodEnvironmentFile<SoundSpeed> newItem in args.NewItems)
                        tree.Children.Add(new SampleCountTreeItem
                        {
                            Name = newItem.TimePeriod.ToString(),
                            SampleCount = newItem.SampleCount,
                            GeoRect = newItem.GeoRect,
                        });
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (TimePeriodEnvironmentFile<SoundSpeed> oldItem in args.OldItems)
                        tree.Children.RemoveAll(child => child.Name == oldItem.TimePeriod.ToString());
                    break;
            }
        }

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
            _token.ExtractionsRequired.Add(new Tuple<object, string>(ValidateMonthlyData("wind").ToList(), "wind"));

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

        IEnumerable<ImportJobDescriptor> CreateImportJobs()
        {
            var jobs = ValidateMonthlyData("temperature").Select(missingItem => new ImportJobDescriptor
            {
                DataType = EnvironmentDataType.Temperature,
                GeoRect = GeoRect,
                TimePeriod = missingItem,
                DestinationFilename = Path.Combine(DataPath, string.Format("{0}.temperature", missingItem.ToString().ToLower())),
                CompletionAction = tempJob =>
                {
                    var key = Path.GetFileName(tempJob.DestinationFilename);
                    var envFile = new EnvironmentFile<SoundSpeed>(DataPath, key, tempJob.SampleCount, tempJob.GeoRect);
                    _token.EnvironmentDictionary[key] = envFile;
                    EnvironmentTree["Temperature"].Children.Add(new SampleCountTreeItem
                    {
                        Name = tempJob.TimePeriod.ToString(),
                        SampleCount = tempJob.SampleCount,
                        GeoRect = tempJob.GeoRect,
                        IsDataAvailable = true
                    });
                }
            }).ToList();
            jobs.AddRange(ValidateMonthlyData("salinity").Select(missingItem => new ImportJobDescriptor
            {
                DataType = EnvironmentDataType.Salinity,
                GeoRect = GeoRect,
                TimePeriod = missingItem,
                DestinationFilename = Path.Combine(DataPath, string.Format("{0}.salinity", missingItem.ToString().ToLower())),
                CompletionAction = tempJob =>
                {
                    var key = Path.GetFileName(tempJob.DestinationFilename);
                    var envFile = new EnvironmentFile<SoundSpeed>(DataPath, key, tempJob.SampleCount, tempJob.GeoRect);
                    _token.EnvironmentDictionary[key] = envFile;
                    EnvironmentTree["Salinity"].Children.Add(new SampleCountTreeItem
                    {
                        Name = tempJob.TimePeriod.ToString(),
                        SampleCount = tempJob.SampleCount,
                        GeoRect = tempJob.GeoRect,
                        IsDataAvailable = true,
                    });
                }
            }));

            var windFilename = Path.Combine(DataPath, "data.wind");
            if (_token.ReextractionRequired || (!File.Exists(windFilename)) || (new FileInfo(windFilename).LastWriteTime > _token.LastWriteTime))
                jobs.Add(new ImportJobDescriptor
                {
                    DataType = EnvironmentDataType.Wind,
                    GeoRect = GeoRect,
                    DestinationFilename = windFilename,
                    CompletionAction = tempJob =>
                    {
                        var key = Path.GetFileName(tempJob.DestinationFilename);
                        var envFile = new EnvironmentFile<SoundSpeed>(DataPath, key, tempJob.SampleCount, tempJob.GeoRect);
                        _token.EnvironmentDictionary[key] = envFile;
                        var treeItem = EnvironmentTree["Wind"];
                        //treeItem.SampleCount = tempJob.SampleCount;
                        //treeItem.GeoRect = tempJob.GeoRect;
                        //treeItem.IsDataAvailable = true;
                    },
                });

            var sedimentFilename = Path.Combine(DataPath, "data.sediment");
            if (_token.ReextractionRequired || (!File.Exists(sedimentFilename)) || (new FileInfo(sedimentFilename).LastWriteTime > _token.LastWriteTime))
                jobs.Add(new ImportJobDescriptor
                {
                    DataType = EnvironmentDataType.Sediment,
                    GeoRect = GeoRect,
                    DestinationFilename = sedimentFilename,
                    CompletionAction = tempJob =>
                    {
                        var key = Path.GetFileName(tempJob.DestinationFilename);
                        var envFile = new EnvironmentFile<SoundSpeed>(DataPath, key, tempJob.SampleCount, tempJob.GeoRect);
                        _token.EnvironmentDictionary[key] = envFile;
                        var treeItem = (SampleCountTreeItem)EnvironmentTree["Sediment"];
                        treeItem.SampleCount = tempJob.SampleCount;
                        treeItem.GeoRect = tempJob.GeoRect;
                        treeItem.IsDataAvailable = true;
                    },
                });

            if (Globals.AppSettings.IsNavyVersion)
            {
                var bottomLossFilename = Path.Combine(DataPath, "data.bottomloss");
                if (_token.ReextractionRequired || (!File.Exists(bottomLossFilename)) || (new FileInfo(bottomLossFilename).LastWriteTime > _token.LastWriteTime))
                    jobs.Add(new ImportJobDescriptor
                    {
                        DataType = EnvironmentDataType.BottomLoss,
                        GeoRect = GeoRect,
                        DestinationFilename = bottomLossFilename,
                        CompletionAction = tempJob =>
                        {
                            var key = Path.GetFileName(tempJob.DestinationFilename);
                            var envFile = new EnvironmentFile<SoundSpeed>(DataPath, key, tempJob.SampleCount, tempJob.GeoRect);
                            _token.EnvironmentDictionary[key] = envFile;
                            var treeItem = (SampleCountTreeItem)EnvironmentTree["Bottom Loss"];
                            treeItem.SampleCount = tempJob.SampleCount;
                            treeItem.GeoRect = tempJob.GeoRect;
                            treeItem.IsDataAvailable = true;
                        },
                    });
            }
            return jobs;
        }

        IEnumerable<NAVOTimePeriod> ValidateMonthlyData(string dataFileExtension)
        {
            var availableMonths = Directory.EnumerateFiles(DataPath, "*." + dataFileExtension).Select(item => (NAVOTimePeriod)Enum.Parse(typeof(NAVOTimePeriod), Path.GetFileNameWithoutExtension(item), true)).ToList();
            var missingMonths = new List<NAVOTimePeriod>();
            if (_token.ReextractionRequired) return NAVOConfiguration.AllMonths;
            foreach (var month in NAVOConfiguration.AllMonths)
            {
                var fileName = Path.Combine(DataPath, string.Format("{0}.{1}", month.ToString().ToLower(), dataFileExtension));
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

        #endregion

        internal async static Task<NewRangeComplex> CreateAsync(string simAreaPath, string rangeComplexName, IEnumerable<Geo> opAreaLimits, List<Geo> simAreaLimits)
        {
            var result = new NewRangeComplex(simAreaPath, rangeComplexName);
            result.OpArea = RangeComplexArea.Create(result.AreasPath, Path.Combine(result.AreasPath, String.Format("{0}_OpArea", rangeComplexName)), opAreaLimits, result._token);
            result.SimArea = RangeComplexArea.Create(result.AreasPath, Path.Combine(result.AreasPath, String.Format("{0}_SimArea", rangeComplexName)), simAreaLimits, result._token);
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
            var importJobs = result.CreateImportJobs().ToList();
            foreach (var area in result.AreaCollection) importJobs.AddRange(area.ImportJobs);
            NAVOImporter.Import(importJobs);
            //await result.ValidateAsync();
            //await result.GetEnvironmentMetadataAsync();
            // expand simArea by 100km if we need to re-extract all data
            var extractionArea = new GeoRect(new Limits(ConvexHull.Create(simAreaLimits, true)).CreateExpandedLimit(200f).Geos);
            //result.InitializeAsync();
            return result;
        }

        Task GetEnvironmentMetadataAsync()
        {
#if false
		    var soundSpeedAction = new ActionBlock<Tuple<NAVOTimePeriod, string, SampleCountTreeItem, 
                                                Func<NAVOTimePeriod, string, string, uint, GeoRect, TimePeriodEnvironmentFile<SoundSpeed>>, 
                                                Action<TimePeriodEnvironmentFile<SoundSpeed>>>>(
            async item =>
            {
                var itemName = string.Format("{0}.{1}", item.Item1.ToString().ToLower(), item.Item2);
                SoundSpeed data = await SoundSpeed.LoadAsync(Path.Combine(DataPath, itemName));
                var curField = data[item.Item1];
                item.Item3.SampleCount = (uint)curField.EnvironmentData.Count;
                item.Item3.GeoRect = curField.EnvironmentData.GeoRect;
                item.Item3.Name = curField.TimePeriod.ToString();
                item.Item3.IsInitializing = false;
                item.Item4(item.Item1, DataPath, itemName, (uint)curField.EnvironmentData.Count, curField.EnvironmentData.GeoRect);
            }, new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                MaxDegreeOfParallelism = 2,
            });
#endif
            var temperatureAction = new ActionBlock<NAVOTimePeriod>(async timePeriod =>
            {
                var fileName = string.Format("{0}.temperature", timePeriod.ToString().ToLower());
                SoundSpeed data = await SoundSpeed.LoadAsync(Path.Combine(DataPath, fileName));
                var curField = data[timePeriod];
                var item = new TimePeriodEnvironmentFile<SoundSpeed>(timePeriod, DataPath, fileName, (uint)curField.EnvironmentData.Count, curField.EnvironmentData.GeoRect);
                TemperatureFiles.Add(timePeriod, item);
            });
            var salinityAction = new ActionBlock<NAVOTimePeriod>(async timePeriod =>
            {
                var fileName = string.Format("{0}.salinity", timePeriod.ToString().ToLower());
                SoundSpeed data = await SoundSpeed.LoadAsync(Path.Combine(DataPath, fileName));
                var curField = data[timePeriod];
                var item = new TimePeriodEnvironmentFile<SoundSpeed>(timePeriod, DataPath, fileName, (uint)curField.EnvironmentData.Count, curField.EnvironmentData.GeoRect);
                SalinityFiles.Add(timePeriod, item);
            });
            var windAction = new ActionBlock<NAVOTimePeriod>(async timePeriod =>
            {
                var fileName = string.Format("{0}.wind", timePeriod.ToString().ToLower());
                Wind data = await Wind.LoadAsync(Path.Combine(DataPath, fileName));
                var curField = data[timePeriod];
                var item = new TimePeriodEnvironmentFile<Wind>(timePeriod, DataPath, fileName, (uint)curField.EnvironmentData.Count, curField.EnvironmentData.GeoRect);
                WindFiles.Add(timePeriod, item);
            });
            foreach (var month in NAVOConfiguration.AllMonths)
            {
                temperatureAction.Post(month);
                salinityAction.Post(month);
                windAction.Post(month);
            }
            temperatureAction.Complete();
            salinityAction.Complete();
            windAction.Complete();

            var sedimentTask = Sediment.LoadAsync(Path.Combine(DataPath, "data.sediment")).ContinueWith(sediment =>
            {
                var sedimentData = sediment.Result;
                SedimentFile = new EnvironmentFile<Sediment>(DataPath, "data.sediment", (uint)sedimentData.Samples.Count, sedimentData.Samples.GeoRect);
                _token.EnvironmentDictionary[SedimentFile.FileName] = SedimentFile;
                var sedimentRoot = (SampleCountTreeItem)EnvironmentTree["Sediment"];
                sedimentRoot.SampleCount = (uint)sedimentData.Samples.Count;
                sedimentRoot.GeoRect = sedimentData.Samples.GeoRect;
                sedimentRoot.IsInitializing = false;
            });

            var bottomLossTask = BottomLoss.LoadAsync(Path.Combine(DataPath, "data.bottomLoss")).ContinueWith(bottomLoss =>
            {
                var bottomLossData = bottomLoss.Result;
                BottomLossFile = new EnvironmentFile<BottomLoss>(DataPath, "data.bottomLoss", (uint)bottomLossData.Samples.Count, bottomLossData.Samples.GeoRect);
                _token.EnvironmentDictionary[BottomLossFile.FileName] = BottomLossFile;
                var bottomLossRoot = (SampleCountTreeItem)EnvironmentTree["Bottom Loss"];
                bottomLossRoot.SampleCount = (uint)bottomLossData.Samples.Count;
                bottomLossRoot.GeoRect = bottomLossData.Samples.GeoRect;
                bottomLossRoot.IsInitializing = false;
            });

            return TaskEx.WhenAll(temperatureAction.Completion, salinityAction.Completion, windAction.Completion, sedimentTask, bottomLossTask);
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
            foreach (var areaFile in Directory.EnumerateFiles(AreasPath, "*.ovr"))
            {
                var keyStartsWith = Path.GetFileNameWithoutExtension(areaFile) + "\\";
                var fileList = new EnvironmentFileList<Bathymetry>();
                var items = _token.EnvironmentDictionary.Where(entry => entry.Key.StartsWith(keyStartsWith));
                foreach (var item in items) fileList.Add(item.Key, (EnvironmentFile<Bathymetry>)item.Value);
                _areas.Add(RangeComplexArea.Read(areaFile, fileList, _token));
            }
            GeoRect = GeoRect.Union(_areas.Select(area => area.GeoRect).ToArray());
            _token.GeoRect = GeoRect;
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

        public EnvironmentFile<BottomLoss> BottomLossFile { get; private set; }
        public EnvironmentFile<Sediment> SedimentFile { get; private set; }
        [NotNull] public TimePeriodEnvironmentFileList<SoundSpeed> TemperatureFiles { get; private set; }
        [NotNull] public TimePeriodEnvironmentFileList<SoundSpeed> SalinityFiles { get; private set; }
        [NotNull] public TimePeriodEnvironmentFileList<Wind> WindFiles { get; private set; }
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