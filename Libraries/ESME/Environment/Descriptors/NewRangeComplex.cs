using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Windows.Threading;
using Cinch;
using ESME.Environment.NAVO;
using HRC;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    public class NewRangeComplex
    {
        NewRangeComplex(string simAreaPath, string rangeComplexName, Dispatcher dispatcher)
        {
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
            Directory.CreateDirectory(RangeComplexPath);
            Directory.CreateDirectory(AreasPath);
            Directory.CreateDirectory(BathymetryPath);
            Directory.CreateDirectory(DataPath);
            Directory.CreateDirectory(EnvironmentPath);
            Directory.CreateDirectory(ImagesPath);
            Directory.CreateDirectory(SpeciesPath);
            Directory.CreateDirectory(Path.Combine(RangeComplexPath, "GeographicAreas"));
            TemperatureFiles = new EnvironmentFileList<SoundSpeed>();
            SalinityFiles = new EnvironmentFileList<SoundSpeed>();
            WindFiles = new EnvironmentFileList<Wind>();
            _dispatcher.InvokeIfRequired(() =>
            {
                AreaCollection = new ObservableList<RangeComplexArea>();
                EnvironmentFileCollection = new ObservableList<EnvironmentFile>();
            });
            Token = RangeComplexToken.Load(Path.Combine(DataPath, "data.token"));
            UpdateAreas();
            if ((Token.GeoRect == null) || (!Token.GeoRect.Contains(GeoRect))) Token.ReextractionRequired = true;
            else EnvironmentFileCollection.AddRange(Token.EnvironmentDictionary.Values);
            Token.EnvironmentDictionary.CollectionChanged += (s, e) =>
            {
                switch (e.Action)
                {
                    case NotifyCollectionChangedAction.Add:
                        foreach (EnvironmentFile envFile in e.NewItems)
                            EnvironmentFileCollection.Add(envFile);
                        break;
                    case NotifyCollectionChangedAction.Remove:
                        foreach (EnvironmentFile envFile in e.OldItems)
                            EnvironmentFileCollection.Remove(envFile);
                        break;
                    case NotifyCollectionChangedAction.Replace:
                        throw new NotImplementedException();
                        break;
                }
            };
        }
        [NotNull] readonly Dispatcher _dispatcher;

        [NotNull] internal RangeComplexToken Token { get; private set; }

        [NotNull] public ObservableList<RangeComplexArea> AreaCollection { get; private set; }
        [NotNull] public ObservableList<EnvironmentFile> EnvironmentFileCollection { get; private set; }

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
        [NotNull] public EnvironmentFileList<SoundSpeed> TemperatureFiles { get; private set; }
        [NotNull] public EnvironmentFileList<SoundSpeed> SalinityFiles { get; private set; }
        [NotNull] public EnvironmentFileList<Wind> WindFiles { get; private set; }

        public EnvironmentFile<BottomLoss> BottomLossFile { get; private set; }
        public EnvironmentFile<Sediment> SedimentFile { get; private set; }

        public RangeComplexArea this[string areaName] { get { return AreaCollection.Where(item => item.Name == areaName).Single(); } }

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
                var keyStartsWith = Path.GetFileNameWithoutExtension(areaFile) + "\\";
                var fileList = new EnvironmentFileList<Bathymetry>();
                var items = Token.EnvironmentDictionary.Where(entry => entry.Key.StartsWith(keyStartsWith));
                foreach (var item in items) fileList.Add(item.Key, (EnvironmentFile<Bathymetry>)item.Value);
                _dispatcher.InvokeInBackgroundIfRequired(() => AreaCollection.Add(RangeComplexArea.Read(this, Path.GetFileNameWithoutExtension(areaFile), fileList, Token)));
            }
            GeoRect = GeoRect.Union(AreaCollection.Select(area => area.GeoRect).ToArray());
            Token.GeoRect = GeoRect;
        }

        #region Validation

        IEnumerable<ImportJobDescriptor> CheckForMissingEnviromentFiles()
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
                    var envFile = new EnvironmentFile<SoundSpeed>(DataPath, key, tempJob.SampleCount, tempJob.GeoRect, EnvironmentDataType.Temperature, tempJob.TimePeriod);
                    Token.EnvironmentDictionary[key] = envFile;
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
                    var envFile = new EnvironmentFile<SoundSpeed>(DataPath, key, tempJob.SampleCount, tempJob.GeoRect, EnvironmentDataType.Salinity, tempJob.TimePeriod);
                    Token.EnvironmentDictionary[key] = envFile;
                }
            }));

            var windFilename = Path.Combine(DataPath, "data.wind");
            if (Token.ReextractionRequired || (!File.Exists(windFilename)) || (new FileInfo(windFilename).LastWriteTime > Token.LastWriteTime))
                jobs.Add(new ImportJobDescriptor
                {
                    DataType = EnvironmentDataType.Wind,
                    GeoRect = GeoRect,
                    DestinationFilename = windFilename,
                    CompletionAction = tempJob =>
                    {
                        var key = Path.GetFileName(tempJob.DestinationFilename);
                        var envFile = new EnvironmentFile<SoundSpeed>(DataPath, key, tempJob.SampleCount, tempJob.GeoRect, EnvironmentDataType.Wind, NAVOTimePeriod.Invalid);
                        Token.EnvironmentDictionary[key] = envFile;
                    },
                });

            var sedimentFilename = Path.Combine(DataPath, "data.sediment");
            if (Token.ReextractionRequired || (!File.Exists(sedimentFilename)) || (new FileInfo(sedimentFilename).LastWriteTime > Token.LastWriteTime))
                jobs.Add(new ImportJobDescriptor
                {
                    DataType = EnvironmentDataType.Sediment,
                    GeoRect = GeoRect,
                    DestinationFilename = sedimentFilename,
                    CompletionAction = tempJob =>
                    {
                        var key = Path.GetFileName(tempJob.DestinationFilename);
                        var envFile = new EnvironmentFile<SoundSpeed>(DataPath, key, tempJob.SampleCount, tempJob.GeoRect, EnvironmentDataType.Sediment, NAVOTimePeriod.Invalid);
                        Token.EnvironmentDictionary[key] = envFile;
                    },
                });

            if (Globals.AppSettings.IsNavyVersion)
            {
                var bottomLossFilename = Path.Combine(DataPath, "data.bottomloss");
                if (Token.ReextractionRequired || (!File.Exists(bottomLossFilename)) || (new FileInfo(bottomLossFilename).LastWriteTime > Token.LastWriteTime))
                    jobs.Add(new ImportJobDescriptor
                    {
                        DataType = EnvironmentDataType.BottomLoss,
                        GeoRect = GeoRect,
                        DestinationFilename = bottomLossFilename,
                        CompletionAction = tempJob =>
                        {
                            var key = Path.GetFileName(tempJob.DestinationFilename);
                            var envFile = new EnvironmentFile<SoundSpeed>(DataPath, key, tempJob.SampleCount, tempJob.GeoRect, EnvironmentDataType.BottomLoss, NAVOTimePeriod.Invalid);
                            Token.EnvironmentDictionary[key] = envFile;
                        },
                    });
            }
            return jobs;
        }

        IEnumerable<NAVOTimePeriod> ValidateMonthlyData(string dataFileExtension)
        {
            var availableMonths = Directory.EnumerateFiles(DataPath, "*." + dataFileExtension).Select(item => (NAVOTimePeriod)Enum.Parse(typeof(NAVOTimePeriod), Path.GetFileNameWithoutExtension(item), true)).ToList();
            var missingMonths = new List<NAVOTimePeriod>();
            if (Token.ReextractionRequired) return NAVOConfiguration.AllMonths;
            foreach (var month in NAVOConfiguration.AllMonths)
            {
                var fileName = Path.Combine(DataPath, string.Format("{0}.{1}", month.ToString().ToLower(), dataFileExtension));
                if (!availableMonths.Contains(month)) missingMonths.Add(month);
                else if ((!File.Exists(fileName)) || (new FileInfo(fileName).LastWriteTime > Token.LastWriteTime))
                    missingMonths.Add(month);
            }
            return missingMonths.Distinct();
        }
        #endregion

        internal async static Task<NewRangeComplex> CreateAsync(string simAreaPath, string rangeComplexName, IEnumerable<Geo> opAreaLimits, List<Geo> simAreaLimits, Dispatcher dispatcher)
        {
            var result = new NewRangeComplex(simAreaPath, rangeComplexName, dispatcher);
            result.OpArea = RangeComplexArea.Create(result, Path.Combine(result.AreasPath, String.Format("{0}_OpArea", rangeComplexName)), opAreaLimits, result.Token);
            result.SimArea = RangeComplexArea.Create(result, Path.Combine(result.AreasPath, String.Format("{0}_SimArea", rangeComplexName)), simAreaLimits, result.Token);
            result.UpdateAreas();
            var importJobs = result.CheckForMissingEnviromentFiles().ToList();
            foreach (var area in result.AreaCollection) importJobs.AddRange(area.ImportJobs);
            NAVOImporter.Import(importJobs);
            return result;
        }

        internal async static Task<NewRangeComplex> ReadAsync(string simAreaPath, Tuple<string, double, double, double, double, string, string> rangeComplexInfo, Action<NewRangeComplex> action, Dispatcher dispatcher)
        {
            var result = new NewRangeComplex(simAreaPath, rangeComplexInfo.Item1, dispatcher);
            action(result);
            result.OpArea = result.AreaCollection.Where(area => area.Name == Path.GetFileNameWithoutExtension(rangeComplexInfo.Item6)).First();
            result.SimArea = result.AreaCollection.Where(area => area.Name == Path.GetFileNameWithoutExtension(rangeComplexInfo.Item7)).First();
            result.UpdateAreas();
            var importJobs = result.CheckForMissingEnviromentFiles().ToList();
            foreach (var area in result.AreaCollection) importJobs.AddRange(area.ImportJobs);
            NAVOImporter.Import(importJobs);
            return result;
        }
    }
}