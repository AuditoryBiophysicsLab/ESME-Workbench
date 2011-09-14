using System;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.IO;
using System.Windows.Input;
using Cinch;
using ESME.Data;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Views.EnvironmentBuilder;
using HRC.Services;
using HRC.Utility;
using MEFedMVVM.ViewModelLocator;
using Environment = System.Environment;

namespace DavesWPFTester
{
    [ExportViewModel("MainWindowViewModel")]
    public class MainWindowViewModel : ViewModelBase
    {
        readonly IMessageBoxService _messageBoxService;
        readonly IHRCOpenFileService _openFileService;
        readonly IHRCSaveFileService _saveFileService;
        readonly IViewAwareStatus _viewAwareStatus;
        readonly IUIVisualizerService _visualizerService;

        [ImportingConstructor]
        public MainWindowViewModel(IViewAwareStatus viewAwareStatus, IMessageBoxService messageBoxService, IHRCOpenFileService openFileService, IHRCSaveFileService saveFileService, IUIVisualizerService visualizerService) 
        {
            _viewAwareStatus = viewAwareStatus;
            _messageBoxService = messageBoxService;
            _openFileService = openFileService;
            _saveFileService = saveFileService;
            _visualizerService = visualizerService;
            var settings = Path.Combine(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME WorkBench"), "settings.xml");
            ESME.Globals.AppSettings = AppSettings.Load(settings);
            _rangeComplexes = RangeComplexes.Singleton;
            RangeComplexCollection = new ObservableList<RangeComplexViewModel>();
            ImportProgressCollection = new ObservableList<ImportProgressViewModel>
            {
                NAVOImporter.TemperatureProgress, 
                NAVOImporter.SalinityProgress,
                NAVOImporter.BathymetryProgress,
                NAVOImporter.BottomLossProgress,
                NAVOImporter.SedimentProgress,
                NAVOImporter.WindProgress,
            };
            _rangeComplexes.RangeComplexCollection.CollectionChanged += (s, e) =>
            {
                if (e.Action == NotifyCollectionChangedAction.Add)
                    foreach (NewRangeComplex item in e.NewItems)
                        RangeComplexCollection.Add(new RangeComplexViewModel(item) { Name = item.Name });
            };
            _rangeComplexes.SimAreaCSVFile = Path.Combine(ESME.Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv");
        }

        readonly RangeComplexes _rangeComplexes;

        #region StartCommand
        public SimpleCommand<object, object> StartCommand
        {
            get { return _start ?? (_start = new SimpleCommand<object, object>(delegate { return IsStartCommandEnabled; }, delegate { StartHandler(); })); }
        }

        SimpleCommand<object, object> _start;

        bool IsStartCommandEnabled
        {
            get { return _isStartCommandEnabled; }
            set
            {
                _isStartCommandEnabled = value;
                CommandManager.InvalidateRequerySuggested();
            }
        }

        bool _isStartCommandEnabled = true;

        async void StartHandler()
        {
#if false
            LogText = string.Format("{0} Started\r\n", DateTime.Now);
            IsStartCommandEnabled = false;
            const double north = 32.964529899922404f;
            const double south = 27.555799630786112f;
            const double east = -77.1238998087263f;
            const double west = -83.37449801842213f;
            var region = new GeoRect(north, south, east, west);
            const string where = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas\Jacksonville\Data";
            const string overlayName = "Jax_Ops_Area_200km";
            var outputPath = Path.Combine(where, overlayName);
            var months = new List<NAVOTimePeriod> { NAVOTimePeriod.January, NAVOTimePeriod.February, NAVOTimePeriod.March, NAVOTimePeriod.April, NAVOTimePeriod.May, NAVOTimePeriod.June, NAVOTimePeriod.July, NAVOTimePeriod.August, NAVOTimePeriod.September, NAVOTimePeriod.October, NAVOTimePeriod.November, NAVOTimePeriod.December };
            var seasons = new List<NAVOTimePeriod> { NAVOTimePeriod.Spring, NAVOTimePeriod.Summer, NAVOTimePeriod.Fall, NAVOTimePeriod.Winter, NAVOTimePeriod.Warm, NAVOTimePeriod.Cold };
            Directory.CreateDirectory(outputPath);

            var temperatureProgress = new Progress<float>(p => LogText += string.Format("{0} Temperature extraction progress: {1:0.0}%\r\n", DateTime.Now, p));
            var salinityProgress = new Progress<float>(p => LogText += string.Format("{0} Salinity extraction progress: {1:0.0}%\r\n", DateTime.Now, p));
            var soundSpeedProgress = new Progress<float>(p => LogText += string.Format("{0} Soundspeed calculation progress: {1:0.0}%\r\n", DateTime.Now, p));
            var windProgress = new Progress<float>(p => LogText += string.Format("{0} Wind extraction progress: {1:0.0}%\r\n", DateTime.Now, p));
            var sedimentProgress = new Progress<float>(p => LogText += string.Format("{0} Sediment extraction progress: {1:0.0}%\r\n", DateTime.Now, p));
            var bottomLossProgress = new Progress<float>(p => LogText += string.Format("{0} Bottom Loss extraction progress: {1:0.0}%\r\n", DateTime.Now, p));
            var bathymetryProgress = new Progress<float>(p => LogText += string.Format("{0} Bathymetry extraction progress: {1:0.0}%\r\n", DateTime.Now, p));

            var temperature = GDEM.ReadTemperatureAsync(months, region, temperatureProgress);
            var salinity = GDEM.ReadSalinityAsync(months, region, salinityProgress);
            var sediment = BST.ExtractAsync(region, sedimentProgress);
            var wind = SMGC.ExtractAsync(months, region, windProgress);
            var bottomLoss = BottomLossDatabase.ExtractAsync(false, region, bottomLossProgress);
            var bathymetry = DBDB.ExtractAsync(0.50f, region, bathymetryProgress);
            LogText += string.Format("{0} Waiting for data extraction...\r\n", DateTime.Now);
            await TaskEx.WhenAll(temperature, salinity);
            var soundSpeed = GDEM.CalculateSoundSpeedAsync(temperature.Result, salinity.Result, null, soundSpeedProgress);
            await TaskEx.WhenAll(sediment, wind, soundSpeed, bottomLoss, bathymetry);
            LogText += string.Format("{0} Data extraction complete!\r\n", DateTime.Now);
            IsStartCommandEnabled = true;
            await TaskEx.WhenAll(temperature.ContinueWith(task => task.Result.Save(Path.Combine(outputPath, "data.temperature"))),
                                 salinity.ContinueWith(task => task.Result.Save(Path.Combine(outputPath, "data.salinity"))),
                                 soundSpeed.ContinueWith(task => task.Result.Save(Path.Combine(outputPath, "data.soundspeed"))),
                                 sediment.ContinueWith(task => task.Result.Save(Path.Combine(outputPath, "data.sediment"))),
                                 bottomLoss.ContinueWith(task => task.Result.Save(Path.Combine(outputPath, "data.bottomloss"))),
                                 bathymetry.ContinueWith(task => task.Result.Save(Path.Combine(outputPath, "data.0.50min.bathymetry"))),
                                 wind.ContinueWith(task => task.Result.Save(Path.Combine(outputPath, "data.wind"))));

#endif
        }
        #endregion

        #region StopCommand
        public SimpleCommand<object, object> StopCommand
        {
            get { return _stop ?? (_stop = new SimpleCommand<object, object>(delegate { return !IsStartCommandEnabled; }, delegate { StopHandler(); })); }
        }

        SimpleCommand<object, object> _stop;

        void StopHandler()
        {
            LogText += string.Format("{0} Stopped\r\n", DateTime.Now);
            IsStartCommandEnabled = true;
        }
        #endregion

        #region public string LogText { get; set; }

        public string LogText
        {
            get { return _logText; }
            set
            {
                if (_logText == value) return;
                _logText = value;
                NotifyPropertyChanged(LogTextChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LogTextChangedEventArgs = ObservableHelper.CreateArgs<MainWindowViewModel>(x => x.LogText);
        string _logText;

        #endregion

        #region public ObservableList<RangeComplexViewModel> RangeComplexCollection { get; set; }

        public ObservableList<RangeComplexViewModel> RangeComplexCollection
        {
            get { return _rangeComplexCollection; }
            set
            {
                if (_rangeComplexCollection == value) return;
                _rangeComplexCollection = value;
                NotifyPropertyChanged(RangeComplexCollectionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RangeComplexCollectionChangedEventArgs = ObservableHelper.CreateArgs<MainWindowViewModel>(x => x.RangeComplexCollection);
        ObservableList<RangeComplexViewModel> _rangeComplexCollection;

        #endregion

        #region public ObservableList<ImportProgressViewModel> ImportProgressCollection { get; set; }

        public ObservableList<ImportProgressViewModel> ImportProgressCollection
        {
            get { return _importProgressCollection; }
            set
            {
                if (_importProgressCollection == value) return;
                _importProgressCollection = value;
                NotifyPropertyChanged(ImportProgressCollectionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ImportProgressCollectionChangedEventArgs = ObservableHelper.CreateArgs<MainWindowViewModel>(x => x.ImportProgressCollection);
        ObservableList<ImportProgressViewModel> _importProgressCollection;

        #endregion

    }
}
