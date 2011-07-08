using System;
using System.Collections.ObjectModel;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Threading;
using Cinch;
using ESME.Data;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.TransmissionLoss.CASS;
using ESMEWorkBench.Data;
using HRC.Navigation;
using HRC.Utility;

namespace ESMEWorkBench.ViewModels.NAVO
{
    public class EnvironmentBuilderViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        Dispatcher _dispatcher;
        IViewAwareStatus _viewAwareStatus;
        readonly IMessageBoxService _messageBoxService;
        readonly Experiment _experiment;
        bool _extractionCanceled;

        public EnvironmentBuilderViewModel(IMessageBoxService messageBoxService, AppSettings appSettings, Experiment experiment)
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nEnvironmentBuilderViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
            _messageBoxService = messageBoxService;
            AppSettings = appSettings;
            _experiment = experiment;
            ExtractButtonText = "Initializing...";
        }

        #region public int BufferZoneSize { get; set; }

        public int BufferZoneSize
        {
            get { return _bufferZoneSize; }
            set
            {
                if (_bufferZoneSize == value) return;
                if (value < 0)
                {
                    _messageBoxService.ShowError("Buffer zone size may not be negative");
                    NotifyPropertyChanged(BufferZoneSizeChangedEventArgs);
                    return;
                }
                _bufferZoneSize = value;
                NotifyPropertyChanged(BufferZoneSizeChangedEventArgs);
                //NAVODataSources.ExtractionArea = GeoRect.Inflate(_experiment.OpArea, BufferZoneSize, BufferZoneSize);
                //NAVODataSources.ExtractionArea = GeoRect.InflateWithGeo(_experiment.OpArea, BufferZoneSize / 1000.0);
                var limits = (Limits)_experiment.OpArea;
                var expandedLimits = limits.CreateExpandedLimit(BufferZoneSize / 1000.0);
                var northWest = expandedLimits.GetNorthWestPoint();
                var southEast = expandedLimits.GetSouthEastPoint();
                NAVODataSources.ExtractionArea = new GeoRect(northWest.Latitude, southEast.Latitude, southEast.Longitude, northWest.Longitude);
            }
        }

        static readonly PropertyChangedEventArgs BufferZoneSizeChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.BufferZoneSize);
        int _bufferZoneSize;

        #endregion

        #region public bool UseExpandedExtractionArea { get; set; }

        public bool UseExpandedExtractionArea
        {
            get { return _useExpandedExtractionArea; }
            set
            {
                if (_useExpandedExtractionArea == value) return;
                _useExpandedExtractionArea = value;
                NotifyPropertyChanged(UseExpandedExtractionAreaChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs UseExpandedExtractionAreaChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.UseExpandedExtractionArea);
        bool _useExpandedExtractionArea;

        #endregion
        
        #region SelectAllMonthsCommand

        SimpleCommand<object, object> _selectAllMonths;

        public SimpleCommand<object, object> SelectAllMonthsCommand
        {
            get { return _selectAllMonths ?? (_selectAllMonths = new SimpleCommand<object, object>(delegate { foreach (var month in MonthCheckboxes) month.IsChecked = true; })); }
        }

        #endregion

        #region UnselectAllMonthsCommand

        SimpleCommand<object, object> _unselectAllMonths;

        public SimpleCommand<object, object> UnselectAllMonthsCommand
        {
            get { return _unselectAllMonths ?? (_unselectAllMonths = new SimpleCommand<object, object>(delegate { foreach (var month in MonthCheckboxes) month.IsChecked = false; })); }
        }

        #endregion

        #region SelectAllSeasonsCommand

        SimpleCommand<object, object> _selectAllSeasons;

        public SimpleCommand<object, object> SelectAllSeasonsCommand
        {
            get { return _selectAllSeasons ?? (_selectAllSeasons = new SimpleCommand<object, object>(delegate { foreach (var month in SeasonCheckboxes) month.IsChecked = true; })); }
        }

        #endregion

        #region UnselectAllSeasonsCommand

        SimpleCommand<object, object> _unselectAllSeasons;

        public SimpleCommand<object, object> UnselectAllSeasonsCommand
        {
            get { return _unselectAllSeasons ?? (_unselectAllSeasons = new SimpleCommand<object, object>(delegate { foreach (var month in SeasonCheckboxes) month.IsChecked = false; })); }
        }

        #endregion

        #region public ObservableCollection<NAVOTimePeriod> SelectedPeriods { get; set; }

        static readonly PropertyChangedEventArgs SelectedPeriodsChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.SelectedPeriods);
        ObservableCollection<NAVOTimePeriod> _selectedPeriods;

        public ObservableCollection<NAVOTimePeriod> SelectedPeriods
        {
            get { return _selectedPeriods; }
            set
            {
                if (_selectedPeriods == value) return;
                if (_selectedPeriods != null) _selectedPeriods.CollectionChanged -= SelectedPeriodsCollectionChanged;
                _selectedPeriods = value;
                if (_selectedPeriods != null) _selectedPeriods.CollectionChanged += SelectedPeriodsCollectionChanged;
                NotifyPropertyChanged(SelectedPeriodsChangedEventArgs);
            }
        }

        void SelectedPeriodsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(SelectedPeriodsChangedEventArgs); }

        #endregion

        #region public CheckboxSettings MonthCheckboxes { get; set; }

        public CheckboxSettings MonthCheckboxes
        {
            get { return _monthCheckboxes; }
            set
            {
                if (_monthCheckboxes == value) return;
                if (_monthCheckboxes != null) _monthCheckboxes.CollectionChanged -= MonthCheckboxesCollectionChanged;
                _monthCheckboxes = value;
                if (_monthCheckboxes != null) _monthCheckboxes.CollectionChanged += MonthCheckboxesCollectionChanged;
                NotifyPropertyChanged(MonthCheckboxesChangedEventArgs);
            }
        }

        void MonthCheckboxesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(MonthCheckboxesChangedEventArgs); }
        static readonly PropertyChangedEventArgs MonthCheckboxesChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.MonthCheckboxes);
        static CheckboxSettings _monthCheckboxes = new CheckboxSettings
                                                   {
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.January
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.February
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.March
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.April
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.May
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.June
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.July
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.August
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.September
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.October
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.November
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.December
                                                       },
                                                   };

        #endregion

        #region public CheckboxSettings SeasonCheckboxes { get; set; }

        public CheckboxSettings SeasonCheckboxes
        {
            get { return _seasonCheckboxes; }
            set
            {
                if (_seasonCheckboxes == value) return;
                if (_seasonCheckboxes != null) _seasonCheckboxes.CollectionChanged -= SeasonCheckboxesCollectionChanged;
                _seasonCheckboxes = value;
                if (_seasonCheckboxes != null) _seasonCheckboxes.CollectionChanged += SeasonCheckboxesCollectionChanged;
                NotifyPropertyChanged(SeasonCheckboxesChangedEventArgs);
            }
        }

        void SeasonCheckboxesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(SeasonCheckboxesChangedEventArgs); }
        static readonly PropertyChangedEventArgs SeasonCheckboxesChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.SeasonCheckboxes);

        CheckboxSettings _seasonCheckboxes = new CheckboxSettings
                                             {
                                                 new CheckboxSetting
                                                 {
                                                     TimePeriod = NAVOTimePeriod.Spring
                                                 },
                                                 new CheckboxSetting
                                                 {
                                                     TimePeriod = NAVOTimePeriod.Summer
                                                 },
                                                 new CheckboxSetting
                                                 {
                                                     TimePeriod = NAVOTimePeriod.Fall
                                                 },
                                                 new CheckboxSetting
                                                 {
                                                     TimePeriod = NAVOTimePeriod.Winter
                                                 },
                                                 new CheckboxSetting
                                                 {
                                                     TimePeriod = NAVOTimePeriod.Warm
                                                 },
                                                 new CheckboxSetting
                                                 {
                                                     TimePeriod = NAVOTimePeriod.Cold
                                                 },
                                             };

        #endregion
        
        #region public AppSettings AppSettings { get; set; }

        static readonly PropertyChangedEventArgs AppSettingsChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.AppSettings);
        AppSettings _appSettings;

        public AppSettings AppSettings
        {
            get { return _appSettings; }
            set
            {
                if (_appSettings == value) return;
                _appSettings = value;
                NotifyPropertyChanged(AppSettingsChangedEventArgs);
            }
        }

        #endregion

        #region public NAVODataSources NAVODataSources { get; set; }

        public NAVODataSources NAVODataSources
        {
            get { return _navoDataSources; }
            set
            {
                if (_navoDataSources == value) return;
                _navoDataSources = value;
                NotifyPropertyChanged(NAVODataSourcesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NAVODataSourcesChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.NAVODataSources);
        NAVODataSources _navoDataSources;

        #endregion

        #region public string ExtractButtonText { get; set; }

        public string ExtractButtonText
        {
            get { return _extractButtonText; }
            set
            {
                if (_extractButtonText == value) return;
                _extractButtonText = value;
                NotifyPropertyChanged(ExtractButtonTextChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExtractButtonTextChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.ExtractButtonText);
        string _extractButtonText;

        #endregion

        #region public bool NotExtractingData { get; set; }

        public bool NotExtractingData
        {
            get { return _notExtractingData; }
            set
            {
                if (_notExtractingData == value) return;
                _notExtractingData = value;
                NotifyPropertyChanged(ExtractingDataChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExtractingDataChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.NotExtractingData);
        bool _notExtractingData = true;

        #endregion

        #region public bool ExportCASSData { get; set; }

        public bool ExportCASSData
        {
            get { return _exportCASSData; }
            set
            {
                if (_exportCASSData == value) return;
                _exportCASSData = value;
                NotifyPropertyChanged(ExportCASSDataChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExportCASSDataChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.ExportCASSData);
        bool _exportCASSData;

        #endregion

        #region public BackgroundTaskAggregator BackgroundTaskAggregator { get; set; }

        public BackgroundTaskAggregator BackgroundTaskAggregator
        {
            get { return _backgroundTaskAggregator; }
            set
            {
                if (_backgroundTaskAggregator == value) return;
                _backgroundTaskAggregator = value;
                NotifyPropertyChanged(BackgroundTaskAggregatorChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BackgroundTaskAggregatorChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.BackgroundTaskAggregator);
        BackgroundTaskAggregator _backgroundTaskAggregator = new BackgroundTaskAggregator();

        #endregion

        #region BufferZoneSizeTextChangedCommand

        public SimpleCommand<object, object> BufferZoneSizeTextChangedCommand
        {
            get
            {
                return _bufferZoneSizeTextChanged ?? (_bufferZoneSizeTextChanged = new SimpleCommand<object, object>(delegate(object cinchArgs)
                {
                    var sender = (TextBox)((EventToCommandArgs)cinchArgs).Sender;
                    //var args = (TextChangedEventArgs)((EventToCommandArgs)cinchArgs).EventArgs;
                    int tempSize;
                    if (sender != null && !string.IsNullOrEmpty(sender.Text) && (int.TryParse(sender.Text, out tempSize)))
                    {
                        BufferZoneSize = tempSize;
                        _bufferZoneSizeOk = true;
                    }
                    else
                    {
                        _bufferZoneSizeOk = false;
                    }
                }));
            }
        }

        SimpleCommand<object, object> _bufferZoneSizeTextChanged;

        #endregion

        bool _bufferZoneSizeOk = true;

        #region ExtractAllCommand
        void ExtractInBackground(List<NAVOTimePeriod> selectedTimePeriods, float desiredResolution, GeoRect extractionArea, AppSettings appSettings, string environmentRoot, string simAreaPath, bool useExpandedExtractionArea, BackgroundTaskAggregator aggregator, bool exportToNAEMO)
        {
            var tempPath = Path.GetTempPath().Remove(Path.GetTempPath().Length - 1);
            if (!Directory.Exists(tempPath)) Directory.CreateDirectory(tempPath);
            if (!Directory.Exists(environmentRoot)) Directory.CreateDirectory(environmentRoot);
            if (!Directory.Exists(simAreaPath)) Directory.CreateDirectory(simAreaPath);

            var assemblyLocation = Assembly.GetCallingAssembly().Location;
            if (assemblyLocation == null) throw new ApplicationException("Assembly can't be null!");
            var extractionPath = Path.GetDirectoryName(assemblyLocation);
            if (extractionPath == null) throw new ApplicationException("Extraction path can't be null!");

            var gdemExtractionProgramPath = Path.Combine(extractionPath, "ImportGDEM.exe");
            var gdemRequiredSupportFiles = new List<string>
                                               {
                                                   Path.Combine(extractionPath, "netcdf.dll"),
                                                   Path.Combine(extractionPath, "NetCDF_Wrapper.dll")
                                               };

            var requiredMonths = selectedTimePeriods.Select(Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod).ToList();
            var allMonths = new List<NAVOTimePeriod>();
            foreach (var curPeriod in requiredMonths) allMonths.AddRange(curPeriod);
            var uniqueMonths = allMonths.Distinct().ToList();
            uniqueMonths.Sort();
            var extendedMonthlySoundSpeeds = new SoundSpeed();
            var extendedAndAveragedSoundSpeeds = new SoundSpeed();
            var monthlyTemperature = new SoundSpeed();
            var monthlySalinity = new SoundSpeed();
            var soundSpeedExtractors = new List<GDEMBackgroundExtractor>();

            // Create a NAEMO exporter if we've been asked to export to NAEMO
            var naemoEnvironmentExporters = !exportToNAEMO
                                                    ? null
                                                    : selectedTimePeriods.Select(t => new CASSBackgroundExporter
                                                    {
                                                        WorkerSupportsCancellation = false,
                                                        TimePeriod = t,
                                                        ExtractionArea = extractionArea,
                                                        NAVOConfiguration = appSettings.NAVOConfiguration,
                                                        DestinationPath = simAreaPath,
                                                        UseExpandedExtractionArea = useExpandedExtractionArea,
                                                        TaskName = "Export NAEMO environment for " + t,
                                                    }).ToList();
            var naemoBathymetryExporter = !exportToNAEMO
                                                  ? null
                                                  : new CASSBackgroundExporter
                                                  {
                                                        WorkerSupportsCancellation = false,
                                                        ExtractionArea = extractionArea,
                                                        NAVOConfiguration = appSettings.NAVOConfiguration,
                                                        DestinationPath = simAreaPath,
                                                        UseExpandedExtractionArea = useExpandedExtractionArea,
                                                        TaskName = "Export NAEMO bathymetry",
                                                  };

            // Create a wind extractor
            var windExtractor = new SMGCBackgroundExtractor
            {
                WorkerSupportsCancellation = false,
                ExtractionArea = extractionArea,
                SelectedTimePeriods = selectedTimePeriods,
                NAVOConfiguration = appSettings.NAVOConfiguration,
                UseExpandedExtractionArea = useExpandedExtractionArea,
                SaveAsFilename = (Path.Combine(environmentRoot, "wind.xml")),
                TaskName = "Wind data extraction",
            };
            aggregator.BackgroundTasks.Add(windExtractor);
            windExtractor.RunWorkerCompleted += (s, e) =>
            { if (naemoEnvironmentExporters != null) foreach (var naemo in naemoEnvironmentExporters) naemo.Wind = ((SMGCBackgroundExtractor)s).Wind; };

            // Create a sediment extractor
            var sedimentExtractor = new BSTBackgroundExtractor
            {
                WorkerSupportsCancellation = false,
                ExtractionArea = extractionArea,
                NAVOConfiguration = appSettings.NAVOConfiguration,
                UseExpandedExtractionArea = useExpandedExtractionArea,
                SaveAsFilename = (Path.Combine(environmentRoot, "sediment.xml")),
                TaskName = "Sediment data extraction",
            };
            aggregator.BackgroundTasks.Add(sedimentExtractor);
            sedimentExtractor.RunWorkerCompleted += (s, e) =>
            { if (naemoEnvironmentExporters != null) foreach (var naemo in naemoEnvironmentExporters) naemo.Sediment = ((BSTBackgroundExtractor)s).Sediment; };

            // Create a bathymetry extractor
            var bathymetryExtractor = new DBDBBackgroundExtractor
            {
                WorkerSupportsCancellation = false,
                ExtractionArea = extractionArea,
                NAVOConfiguration = appSettings.NAVOConfiguration,
                DestinationPath = tempPath,
                UseExpandedExtractionArea = useExpandedExtractionArea,
                SelectedResolution = desiredResolution,
                SaveAsFilename = (Path.Combine(environmentRoot, "bathymetry.yxz")),
                TaskName = "Bathymetry data extraction",
            };
            aggregator.BackgroundTasks.Add(bathymetryExtractor);
            bathymetryExtractor.RunWorkerCompleted += (s, e) =>
            {
                var bathymetry = ((DBDBBackgroundExtractor)s).Bathymetry;
                if (naemoBathymetryExporter != null) naemoBathymetryExporter.Bathymetry = bathymetry;
            };
            // Create a bathymetry extractor
            var temperatureAndSalinityFileWriter = new TemperatureAndSalinityFileWriter
            {
                WorkerSupportsCancellation = false,
                DestinationPath = environmentRoot,
                TaskName = "Save soundspeed data"
            };

            // Create a soundspeed averager/extender for each selected time period.  These averagers need the max bathymetry depth
            // and the monthly sound speed fields.  The averagers will block until that data becomes available
            var averagers = !exportToNAEMO
                                    ? null
                                    : selectedTimePeriods.Select(timePeriod => new SoundSpeedBackgroundAverager
                                    {
                                            WorkerSupportsCancellation = false,
                                            TimePeriod = timePeriod,
                                            ExtractionArea = extractionArea,
                                            NAVOConfiguration = appSettings.NAVOConfiguration,
                                            UseExpandedExtractionArea = useExpandedExtractionArea,
                                            TaskName = "Calculate extended sound speeds for " + timePeriod,
                                    }).ToList();

            foreach (var month in uniqueMonths)
            {
                var soundSpeedExtractor = new GDEMBackgroundExtractor
                {
                    WorkerSupportsCancellation = false,
                    TimePeriod = month,
                    ExtractionArea = extractionArea,
                    NAVOConfiguration = appSettings.NAVOConfiguration,
                    DestinationPath = tempPath,
                    UseExpandedExtractionArea = useExpandedExtractionArea,
                    ExtractionProgramPath = gdemExtractionProgramPath,
                    RequiredSupportFiles = gdemRequiredSupportFiles,
                };
                soundSpeedExtractor.RunWorkerCompleted += (sender, e) =>
                {
                    var extractor = (GDEMBackgroundExtractor)sender;
                    monthlyTemperature.SoundSpeedFields.Add(extractor.TemperatureField);
                    monthlySalinity.SoundSpeedFields.Add(extractor.SalinityField);
                    extendedMonthlySoundSpeeds.SoundSpeedFields.Add(extractor.ExtendedSoundSpeedField);
                    //Console.WriteLine("soundspeed extractor for {0} complete. {1} extractors are still busy", extractor.TimePeriod, soundSpeedExtractors.Where(s => s.IsBusy).Count());
                    if (soundSpeedExtractors.Any(ssfExtractor => ssfExtractor.IsBusy)) return;
                    temperatureAndSalinityFileWriter.Temperature = monthlyTemperature;
                    temperatureAndSalinityFileWriter.Salinity = monthlySalinity;
                    if (averagers != null) foreach (var averager in averagers) averager.ExtendedMonthlySoundSpeeds = extendedMonthlySoundSpeeds;
                };
                soundSpeedExtractors.Add(soundSpeedExtractor);
                aggregator.BackgroundTasks.Add(soundSpeedExtractor);
            }
            aggregator.BackgroundTasks.Add(temperatureAndSalinityFileWriter);

            bathymetryExtractor.RunWorkerCompleted += (sender, args) =>
            {
                // When the bathymetry extractor has completed, provide the max depth to all the averagers
                var extractor = (DBDBBackgroundExtractor)sender;
                var maxDepth = new EarthCoordinate<float>(extractor.Bathymetry.Minimum, Math.Abs(bathymetryExtractor.Bathymetry.Minimum.Data));
                // The averagers block until the monthly soundspeed dataset is available, AND the bathymetry is available so we know the max depth
                foreach (var soundSpeedExtractor in soundSpeedExtractors) soundSpeedExtractor.MaxDepth = maxDepth;
            };

            if (averagers != null)
            {
                foreach (var averager in averagers)
                {
                    aggregator.BackgroundTasks.Add(averager);
                    averager.RunWorkerCompleted += (sender, e) =>
                    {
                        var avg = (SoundSpeedBackgroundAverager)sender;
                        extendedAndAveragedSoundSpeeds.SoundSpeedFields.Add(avg.ExtendedAverageSoundSpeedField);
                        if (averagers.Any(a => a.IsBusy)) return;
                        foreach (var naemo in naemoEnvironmentExporters) naemo.ExtendedAndAveragedSoundSpeeds = extendedAndAveragedSoundSpeeds;
                    };
                }
            }

            if (naemoBathymetryExporter != null) aggregator.BackgroundTasks.Add(naemoBathymetryExporter);
            if (naemoEnvironmentExporters != null) foreach (var naemo in naemoEnvironmentExporters) aggregator.BackgroundTasks.Add(naemo);

            aggregator.TaskName = "Environmental data extraction";
            aggregator.Start();
        }

        SimpleCommand<object, object> _extractAll;

        public SimpleCommand<object, object> ExtractAllCommand
        {
            get
            {
                return _extractAll ?? (_extractAll = new SimpleCommand<object, object>(
                    delegate
                    {
                        return  ((NAVODataSources != null) && (NotExtractingData) && (_bufferZoneSizeOk) && (NAVODataSources.DigitalBathymetricDatabase.SelectedResolution != null) &&  
                                 ((MonthCheckboxes.SelectedTimePeriods.Count() > 0) || (SeasonCheckboxes.SelectedTimePeriods.Count() > 0))); }, 
                    delegate
                    {
                        var selectedTimePeriods = new List<NAVOTimePeriod>();
                        if (MonthCheckboxes != null)
                            selectedTimePeriods.AddRange(MonthCheckboxes.SelectedTimePeriods);
                        if (SeasonCheckboxes != null)
                            selectedTimePeriods.AddRange(SeasonCheckboxes.SelectedTimePeriods);
                        if (selectedTimePeriods.Count < 1) return;
                        var selectedResolution = NAVODataSources.DigitalBathymetricDatabase.SelectedResolution;
                        var resTemp = selectedResolution.EndsWith("min") ? selectedResolution.Remove(selectedResolution.Length - 3) : selectedResolution;
                        double desiredResolution;
                        if (!double.TryParse(resTemp, out desiredResolution)) throw new FormatException("Illegal number format for selectedResolution: " + selectedResolution);
#if true
                        BackgroundTaskAggregator = new BackgroundTaskAggregator();
                        BackgroundTaskAggregator.RunWorkerCompleted += (s, e) =>
                        {
                            var tempDirectory = Path.Combine(_experiment.LocalStorageRoot, "NAVOTemp");
                            // At this point, the user can no longer cancel the operation.)
                            if (!Directory.Exists(_experiment.EnvironmentRoot)) Directory.CreateDirectory(_experiment.EnvironmentRoot);
                            var files = Directory.GetFiles(_experiment.EnvironmentRoot);
                            foreach (var file in files) File.Delete(file);
                            files = Directory.GetFiles(tempDirectory);
                            foreach (var sourceFile in files)
                            {
                                var destFile = Path.Combine(_experiment.EnvironmentRoot, Path.GetFileName(sourceFile));
                                File.Move(sourceFile, destFile);
                            }
                            Directory.Delete(tempDirectory, true);

                            NotExtractingData = true;
                            ExtractButtonText = "Extract";
                            CommandManager.InvalidateRequerySuggested();
                            if (_extractionCanceled) return;
                            if (selectedTimePeriods.Count > 0)
                            {
                                _experiment.WindSpeedFileName = Path.Combine(_experiment.EnvironmentRoot, "wind.xml");
                                _experiment.TemperatureFilename = Path.Combine(_experiment.EnvironmentRoot, "temperature.xml");
                                _experiment.SalinityFilename = Path.Combine(_experiment.EnvironmentRoot, "salinity.xml");
                                _experiment.SoundSpeedFileName = null;
                                _experiment.SedimentFileName = Path.Combine(_experiment.EnvironmentRoot, "sediment.xml");
                                _experiment.BathymetryFileName = Path.Combine(_experiment.EnvironmentRoot, "bathymetry.yxz");
                                _experiment.SimArea = NAVODataSources.ExtractionArea;
                                _experiment.AvailableTimePeriods = new List<NAVOTimePeriod>(selectedTimePeriods);
                            }
                            AppSettings.Save(); //remember the new values. 
                            CloseActivePopUpCommand.Execute(true);
                        };
                        NotExtractingData = false;
                        ExtractButtonText = "Extracting...";
                        _extractionCanceled = false;

                        ExtractInBackground(selectedTimePeriods, (float)desiredResolution, NAVODataSources.ExtractionArea, Globals.AppSettings, Path.Combine(_experiment.LocalStorageRoot, "NAVOTemp"), Path.Combine(Globals.AppSettings.ScenarioDataDirectory, _experiment.NemoFile.Scenario.SimAreaName), UseExpandedExtractionArea, BackgroundTaskAggregator, ExportCASSData);
#else
                        NAVODataSources.SelectedTimePeriods = selectedTimePeriods;
                        NAVODataSources.ExportCASSData = ExportCASSData;
                        NAVODataSources.UseExpandedExtractionArea = UseExpandedExtractionArea;
                        NAVODataSources.ExtractDataInBackground(delegate
                                                                {
                                                                    NotExtractingData = true;
                                                                    ExtractButtonText = "Extract";
                                                                    CommandManager.InvalidateRequerySuggested();
                                                                    if (_extractionCanceled) return;
                                                                    if (selectedTimePeriods.Count > 0)
                                                                    {
                                                                        _experiment.WindSpeedFileName = Path.Combine(_experiment.EnvironmentRoot, "wind.xml");
                                                                        _experiment.SoundSpeedFileName = Path.Combine(_experiment.EnvironmentRoot, "soundspeed.xml");
                                                                        _experiment.SedimentFileName = Path.Combine(_experiment.EnvironmentRoot, "sediment.xml");
                                                                        _experiment.BathymetryFileName = NAVODataSources.BathymetryFilename;
                                                                        _experiment.SimArea = NAVODataSources.ExtractionArea;
                                                                        _experiment.AvailableTimePeriods = new List<NAVOTimePeriod>(selectedTimePeriods);
                                                                    }
                                                                    AppSettings.Save(); //remember the new values. 
                                                                    CloseActivePopUpCommand.Execute(true);
                                                                });
                        NotExtractingData = false;
                        ExtractButtonText = "Extracting...";
                        _extractionCanceled = false;
                        //close the view.
#endif
                    }));
            }
        }


        #endregion

        #region CancelCommand

        SimpleCommand<object, object> _cancel;

        public SimpleCommand<object, object> CancelCommand
        {
            get
            {
                return _cancel ?? (_cancel = new SimpleCommand<object, object>(delegate
                                                                               {
                                                                                   if (!NotExtractingData)
                                                                                   {
                                                                                       NAVODataSources.CancelExtraction();
                                                                                       _extractionCanceled = true;
                                                                                   }
                                                                                   else
                                                                                   {
                                                                                       AppSettings = AppSettings.Load(); //invalidate all changes.
                                                                                       CloseActivePopUpCommand.Execute(false);
                                                                                   }
                                                                               }));
            }
        }

        #endregion
        
        #region IViewStatusAwareInjectionAware Members

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
            NAVODataSources = new NAVODataSources(_experiment.OpArea, Globals.AppSettings.NAVOConfiguration, _dispatcher, _experiment.LocalStorageRoot, Path.Combine(Globals.AppSettings.ScenarioDataDirectory, _experiment.NemoFile.Scenario.SimAreaName));
            ExtractButtonText = "Extract";
        }

        #endregion
    }

    public class CheckboxSettings : ObservableCollection<CheckboxSetting>
    {
        public CheckboxSetting this[string caption]
        {
            get
            {
                foreach (var setting in this.Where(setting => setting.Caption == caption))
                    return setting;
                throw new IndexOutOfRangeException("CheckboxSettings: Specified setting \"" + caption + "\" not found");
            }
        }

        public bool IsAtLeastOneChecked { get { return this.Aggregate(false, (current, setting) => current | setting.IsChecked); } }

        public IEnumerable<NAVOTimePeriod> SelectedTimePeriods
        {
            get { return this.Where(setting => setting.IsChecked).Select(setting => setting.TimePeriod); }
        }
    }

    public class CheckboxSetting : ViewModelBase
    {
        public CheckboxSetting() { IsChecked = false; }

        #region public string Caption { get; set; }

        public string Caption
        {
            get { return TimePeriod.ToString(); }
        }

        #endregion

        public NAVOTimePeriod TimePeriod { get; set; }

        #region public bool IsChecked { get; set; }

        public bool IsChecked
        {
            get { return _isChecked; }
            set
            {
                if (_isChecked == value) return;
                _isChecked = value;
                NotifyPropertyChanged(IsCheckedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsCheckedChangedEventArgs = ObservableHelper.CreateArgs<CheckboxSetting>(x => x.IsChecked);
        bool _isChecked;

        #endregion
    }
}