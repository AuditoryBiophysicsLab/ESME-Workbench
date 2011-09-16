using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
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
            IsEnabled = false;
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

            EnvironmentFiles = RangeComplexToken.Load(Path.Combine(DataPath, Name + ".token"));
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
            EnvironmentList = new ObservableList<EnvironmentFile>
            {
                WindFile,
                SedimentFile,
                BottomLossFile,
                TemperatureFile,
                SalinityFile
            };
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

        IEnumerable<ImportJobDescriptor> CheckForMissingEnviromentFiles()
        {
            //var test = new Task<EnvironmentFile<SoundSpeed>>()

            var jobs = ValidateMonthlyData("temperature").Select(missingItem => new ImportJobDescriptor
            {
                DataType = EnvironmentDataType.Temperature,
                GeoRect = GeoRect,
                TimePeriod = missingItem,
                DestinationFilename = Path.Combine(DataPath, string.Format("{0}.temperature", missingItem.ToString().ToLower())),
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
                DestinationFilename = Path.Combine(DataPath, string.Format("{0}.salinity", missingItem.ToString().ToLower())),
                CompletionAction = tempJob =>
                {
                    var key = Path.GetFileName(tempJob.DestinationFilename);
                    var envFile = new SalinityFile(DataPath, key, tempJob.SampleCount, tempJob.GeoRect, EnvironmentDataType.Salinity, tempJob.TimePeriod);
                    EnvironmentFiles[key] = envFile;
                    SalinityFile.Months[tempJob.TimePeriod] = envFile;
                }
            }));

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
                var fileName = Path.Combine(DataPath, string.Format("{0}.{1}", month.ToString().ToLower(), dataFileExtension));
                if (!availableMonths.Contains(month)) missingMonths.Add(month);
                else if ((!File.Exists(fileName)) || (new FileInfo(fileName).LastWriteTime > EnvironmentFiles.LastWriteTime))
                    missingMonths.Add(month);
            }
            return missingMonths.Distinct();
        }
        #endregion

        #region public bool IsEnabled { get; private set; }

        public bool IsEnabled
        {
            get { return _isEnabled; }
            private set
            {
                if (_isEnabled == value) return;
                _isEnabled = value;
                NotifyPropertyChanged(IsEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsEnabledChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplex>(x => x.IsEnabled);
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

        static readonly PropertyChangedEventArgs ToolTipChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplex>(x => x.ToolTip);
        string _toolTip;

        #endregion

        internal static NewRangeComplex Create(string simAreaPath, string rangeComplexName, IEnumerable<Geo> opAreaLimits, IEnumerable<Geo> simAreaLimits, Dispatcher dispatcher)
        {
            var result = new NewRangeComplex(simAreaPath, rangeComplexName, true, dispatcher);
            result.OpArea = result.CreateAreaPrivate(String.Format("{0}_OpArea", rangeComplexName), opAreaLimits);
            result.SimArea = result.CreateAreaPrivate(String.Format("{0}_SimArea", rangeComplexName), simAreaLimits);
            result.UpdateAreas();
            var importJobs = result.CheckForMissingEnviromentFiles().ToList();
            foreach (var area in result.AreaCollection.Values) importJobs.AddRange(area.ImportJobs);
            NAVOImporter.Import(importJobs);
            result.IsEnabled = true;
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
            NAVOImporter.Import(result.CheckForMissingEnviromentFiles());
            foreach (var area in result.AreaCollection.Values) NAVOImporter.Import(area.ImportJobs);
            result.IsEnabled = true;
            return result;
        }

        public RangeComplexArea CreateArea(string areaName, IEnumerable<Geo> areaLimits)
        {
            if (!IsEnabled) throw new InvalidOperationException(string.Format("The range complex {0} cannot be modified at the moment. Please try again shortly.", Name));
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
            if (!IsEnabled) throw new InvalidOperationException(string.Format("The range complex {0} cannot be modified at the moment. Please try again shortly.", Name));
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