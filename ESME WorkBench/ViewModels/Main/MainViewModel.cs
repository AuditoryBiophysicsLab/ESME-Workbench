using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Input;
using System.Windows.Threading;
using System.Xml.Serialization;
using Cinch;
using ESME;
using ESME.Environment;
using ESME.Mapping;
using ESME.Plugins;
using ESMEWorkbench.Data;
using ESMEWorkbench.Properties;
using ESMEWorkbench.ViewModels.RecentFiles;
using HRC.Navigation;
using HRC.Services;
using HRC.Utility;
using MEFedMVVM.Common;
using MEFedMVVM.ViewModelLocator;
using ESME.Views.AcousticBuilder;
using ESME.Environment.Descriptors;

namespace ESMEWorkbench.ViewModels.Main
{
    [ExportViewModel("MainViewModel")]
    public partial class MainViewModel : ViewModelBase
    {
        #region Private fields

        readonly IMessageBoxService _messageBoxService;
        readonly IHRCOpenFileService _openFileService;
        readonly IHRCSaveFileService _saveFileService;
        readonly IViewAwareStatus _viewAwareStatus;
        readonly IUIVisualizerService _visualizerService;
        readonly IPluginManagerService _pluginManagerService;
#if EXPERIMENTS_SUPPORTED
        Experiment _experiment;
#endif
        //TransmissionLossQueueCalculatorViewModel _bellhopQueueCalculatorViewModel;
        Dispatcher _dispatcher;
        public const bool ExperimentsCurrentlySupported = false;

        readonly PleaseWaitViewModel _pleaseWait;
        #endregion

        #region Constructor
        [ImportingConstructor]
        public MainViewModel(IViewAwareStatus viewAwareStatus, IMessageBoxService messageBoxService, IHRCOpenFileService openFileService, IHRCSaveFileService saveFileService, IUIVisualizerService visualizerService, IPluginManagerService pluginManagerService)
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nMainViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }

            AnalysisPointPropertiesViewModel.MessageBoxService = messageBoxService;
            Experiment.MessageBoxService = messageBoxService;
            Experiment.VisualizerService = visualizerService;
            _viewAwareStatus = viewAwareStatus;
            _messageBoxService = messageBoxService;
            _openFileService = openFileService;
            _saveFileService = saveFileService;
            _visualizerService = visualizerService;
            _pluginManagerService = pluginManagerService;
            _pleaseWait = new PleaseWaitViewModel((Window)_viewAwareStatus.View, _visualizerService);
            if (Designer.IsInDesignMode) return;
            _viewAwareStatus.ViewUnloaded += () =>
            {
                if (Designer.IsInDesignMode) return;
                MediatorMessage.Send(MediatorMessage.ApplicationClosing);
            };
            
            _viewAwareStatus.ViewLoaded += () =>
            {
                if (Designer.IsInDesignMode) return;
                _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
                MediatorMessage.Send(MediatorMessage.MainViewModelInitialized, _dispatcher);
                Globals.AppSettings.PluginManagerService = _pluginManagerService;
                NAVOImporter.PluginManagerService = _pluginManagerService;
            };

            IsLatLonGridVisible = Settings.Default.ShowGrid;
            IsScaleBarVisible = Settings.Default.ShowScaleBar;
            IsPanZoomVisible = Settings.Default.ShowPanZoom;
            IsEnvironmentTabSelected = Settings.Default.SelectedRibbonTabIndex == 1;

        }

        protected override void OnDispose()
        {
            base.OnDispose();
            Mediator.Instance.Unregister(this);
        }

        #endregion

        #region ViewModel properties

        #region public string DecoratedExperimentName { get; set; }

        static readonly PropertyChangedEventArgs DecoratedExperimentNameChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.DecoratedExperimentName);
        string _decoratedExperimentName;

        public string DecoratedExperimentName
        {
            get { return _decoratedExperimentName; }
            set
            {
                if (_decoratedExperimentName == value) return;
                _decoratedExperimentName = value;
                NotifyPropertyChanged(DecoratedExperimentNameChangedEventArgs);
            }
        }

        #endregion

        #region public Geo MouseGeo { get; set; }

        public Geo MouseGeo
        {
            get { return _mouseGeo; }
            set
            {
                if (_mouseGeo == value) return;
                _mouseGeo = value;
                NotifyPropertyChanged(MouseEarthCoordinateChangedEventArgs);
                NotifyPropertyChanged(MouseLocationInfoChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MouseEarthCoordinateChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.MouseGeo);
        Geo _mouseGeo;

        #endregion

        #region public string MouseLocationInfo { get; set; }

        public string MouseLocationInfo
        {
            get
            {
                if (MouseGeo == null) return null;
                var lat = MouseGeo.Latitude;
                var lon = MouseGeo.Longitude;
                if (-90 > lat || lat > 90) return null;
                if (-180 > lon || lon > 180) return null;
                var northSouth = lat >= 0 ? "N" : "S";
                var eastWest = lon >= 0 ? "E" : "W";
                if (RangeComplexes != null && RangeComplexes.SelectedBathymetry != null)
                {
                    if ((_bathymetry == null || _bathymetry.Target == null) && RangeComplexes.IsEnvironmentLoaded)
                    {
                        _bathymetry = new WeakReference<Bathymetry>(((Task<Bathymetry>)RangeComplexes.EnvironmentData[EnvironmentDataType.Bathymetry]).Result);
                    }
                    if (_bathymetry != null && _bathymetry.Target != null && _bathymetry.Target.Samples.GeoRect.Contains(MouseGeo)) return string.Format("Lat: {0:0.0000}{1} Lon: {2:0.0000}{3} Elevation: {4:0.#}m", Math.Abs(lat), northSouth, Math.Abs(lon), eastWest, _bathymetry.Target.Samples.GetNearestPoint(MouseGeo).Data);
                }
                return string.Format("Lat: {0:0.0000}{1} Lon: {2:0.0000}{3}", Math.Abs(lat), northSouth, Math.Abs(lon), eastWest);
            }
        }

        static readonly PropertyChangedEventArgs MouseLocationInfoChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.MouseLocationInfo);
        WeakReference<Bathymetry> _bathymetry;

        #endregion

        #region public float? MouseDepth { get; set; }

        public float? MouseDepth
        {
            get { return _mouseDepth; }
            set
            {
                if (_mouseDepth == value) return;
                _mouseDepth = value;
                NotifyPropertyChanged(MouseDepthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MouseDepthChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.MouseDepth);
        float? _mouseDepth;

        #endregion

        #region public bool IsDebugMode { get; set; }

        public bool IsDebugMode
        {
            get
            {
#if DEBUG
                return true;
#else
                return false;
#endif
            }
        }

        #endregion

        #endregion

        async void InitializeEnvironmentManager()
        {
            RangeComplexes = RangeComplexes.Singleton;
            ImportProgressCollection = ImportProgressCollection.Singleton;
            try
            {
                try
                {
                    await RangeComplexes.ReadRangeComplexFile(Path.Combine(ESME.Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"));
                }
                catch (AggregateException ae)
                {
                    var sb = new StringBuilder();
                    foreach (var e in ae.InnerExceptions) sb.AppendLine(e.Message);
                    _messageBoxService.ShowError("Error loading range complexes:\n" + sb);
                }
                catch (Exception e)
                {
                    _messageBoxService.ShowError(e.Message);
                }
                RangeComplexes.PropertyChanged += (s, e) =>
                {
                    switch (e.PropertyName)
                    {
                        case "SelectedRangeComplex":
                            DisplayRangeComplex();
                            break;
                        case "SelectedArea":
                            DisplaySelectedArea();
                            break;
                        case "SelectedBathymetry":
                            DisplayBathymetry();
                            break;
                    }
                };
            }
            catch (Exception e)
            {
                _messageBoxService.ShowError(e.Message);
            }
        }

        public ImportProgressCollection ImportProgressCollection
        {
            get { return _importProgressCollection; }
            private set
            {
                if (_importProgressCollection == value) return;
                _importProgressCollection = value;
                NotifyPropertyChanged(ImportProgressCollectionChangedEventArgs);
            }
        }
        static readonly PropertyChangedEventArgs ImportProgressCollectionChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.ImportProgressCollection);
        ImportProgressCollection _importProgressCollection;

        [XmlIgnore]
        public RangeComplexes RangeComplexes
        {
            get { return _rangeComplexes; }
            private set
            {
                if (_rangeComplexes == value) return;
                _rangeComplexes = value;
                NotifyPropertyChanged(RangeComplexesChangedEventArgs);

                if (_rangeComplexes != null) _rangeComplexes.PropertyChanged += (s, e) =>
                {
                    switch (e.PropertyName)
                    {
                        case "IsEnvironmentFullySpecified":
                            if (RangeComplexes != null && RangeComplexes.IsEnvironmentFullySpecified) HookLayerData();
                            else ClearLayerData();
                            break;
                    }
                };
            }
        }
        static readonly PropertyChangedEventArgs RangeComplexesChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.RangeComplexes);
        RangeComplexes _rangeComplexes;

        public MapLayerCollection CurrentMapLayers { get; set; }

        #region public int SelectedRibbonTabIndex { get; set; }

        public int SelectedRibbonTabIndex
        {
            get { return Settings.Default.SelectedRibbonTabIndex; }
            set
            {
                if (Settings.Default.SelectedRibbonTabIndex == value) return;
                Settings.Default.SelectedRibbonTabIndex = value;
                IsEnvironmentTabSelected = Settings.Default.SelectedRibbonTabIndex == 1;
                if (!IsEnvironmentTabSelected) SelectedSidebarTabIndex = IsScenarioLoaded ? 1 : 0;

                NotifyPropertyChanged(SelectedRibbonTabIndexChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedRibbonTabIndexChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedRibbonTabIndex);

        #endregion

        #region public bool IsEnvironmentTabSelected { get; set; }

        public bool IsEnvironmentTabSelected
        {
            get { return _isEnvironmentTabSelected; }
            set
            {
                if (_isEnvironmentTabSelected == value) return;
                _isEnvironmentTabSelected = value;
                NotifyPropertyChanged(IsEnvironmentTabSelectedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsEnvironmentTabSelectedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsEnvironmentTabSelected);
        bool _isEnvironmentTabSelected;

        #endregion

        #region public int SelectedSidebarTabIndex { get; set; }

        public int SelectedSidebarTabIndex
        {
            get { return _selectedSidebarTabIndex; }
            set
            {
                if (_selectedSidebarTabIndex == value) return;
                _selectedSidebarTabIndex = value;
                NotifyPropertyChanged(SelectedSidebarTabIndexChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedSidebarTabIndexChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedSidebarTabIndex);
        int _selectedSidebarTabIndex;

        #endregion

        void ShowAboutView()
        {
            var aboutViewModel = new AboutViewModel();
            _visualizerService.ShowDialog("AboutView", aboutViewModel);
        }

        #region public bool IsLatLonGridVisible { get; set; }

        public bool IsLatLonGridVisible
        {
            get { return _isLatLonGridVisible; }
            set
            {
                _isLatLonGridVisible = value;
                Settings.Default.ShowGrid = value;
                MediatorMessage.Send(MediatorMessage.SetGridOverlayDisplay, value);
                NotifyPropertyChanged(IsLatLonGridVisibleChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsLatLonGridVisibleChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsLatLonGridVisible);
        bool _isLatLonGridVisible;

        #endregion

        #region public bool IsPanZoomVisible { get; set; }

        public bool IsPanZoomVisible
        {
            get { return _isPanZoomVisible; }
            set
            {
                _isPanZoomVisible = value;
                Settings.Default.ShowPanZoom = value;
                MediatorMessage.Send(MediatorMessage.SetPanZoomDisplay, value);
                NotifyPropertyChanged(IsPanZoomVisibleChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsPanZoomVisibleChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsPanZoomVisible);
        bool _isPanZoomVisible;

        #endregion

        #region public bool IsScaleBarVisible { get; set; }

        public bool IsScaleBarVisible
        {
            get { return _isScaleBarVisible; }
            set
            {
                _isScaleBarVisible = value;
                Settings.Default.ShowScaleBar = value;
                MediatorMessage.Send(MediatorMessage.SetScaleBarDisplay, value);
                NotifyPropertyChanged(IsScaleBarVisibleChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsScaleBarVisibleChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsScaleBarVisible);
        bool _isScaleBarVisible;

        #endregion

        #region public RecentFileDescriptor RecentFilesSelectedItem { get; set; }

        public RecentFileDescriptor RecentFilesSelectedItem
        {
            get { return null; }
            set
            {
                if (_recentFilesSelectedItem == value) return;
                _recentFilesSelectedItem = value;
                if (_recentFilesSelectedItem != null)
                {
                    if (!_recentFilesSelectedItem.LongName.EndsWith(".nemo"))
                    {
                        RecentFiles.RemoveFile(_recentFilesSelectedItem.LongName);
                        _recentFilesSelectedItem = null;
                        return;
                    }
                    try
                    {
                        OpenScenarioHandler(_recentFilesSelectedItem.LongName);
                        NotifyPropertyChanged(RecentFilesSelectedItemChangedEventArgs);
                        return;
                    }
                    catch (Exception e)
                    {
                        _messageBoxService.ShowError("Error opening scenario: " + e.Message);
                        RecentFiles.RemoveFile(_recentFilesSelectedItem.LongName);
                    }
                }
                NotifyPropertyChanged(RecentFilesSelectedItemChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RecentFilesSelectedItemChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.RecentFilesSelectedItem);
        RecentFileDescriptor _recentFilesSelectedItem;

        #endregion

        #region public RecentFileList RecentFiles { get; set; }

        public RecentFileList RecentFiles
        {
            get { return _recentFiles; }
            set
            {
                if (_recentFiles == value) return;
                _recentFiles = value;
                NotifyPropertyChanged(RecentFilesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RecentFilesChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.RecentFiles);
        RecentFileList _recentFiles = new RecentFileList();

        #endregion

        #region public bool IsInAnalysisPointMode { get; set; }

        public bool IsInAnalysisPointMode
        {
            get { return _isInAnalysisPointMode; }
            set
            {
                if (_isInAnalysisPointMode == value) return;
                _isInAnalysisPointMode = value;
                NotifyPropertyChanged(IsInAnalysisPointModeChangedEventArgs);
                MediatorMessage.Send(MediatorMessage.SetAnalysisPointMode, _isInAnalysisPointMode);
            }
        }

        static readonly PropertyChangedEventArgs IsInAnalysisPointModeChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsInAnalysisPointMode);
        bool _isInAnalysisPointMode;

        [MediatorMessageSink(MediatorMessage.SetAnalysisPointMode)]
        void SetAnalysisPointMode(bool mode)
        {
            IsInAnalysisPointMode = mode;
        }

        #endregion

        #region PreviewKeyDownCommand

        public SimpleCommand<object, EventToCommandArgs> PreviewKeyDownCommand
        {
            get
            {
                return _previewKeyDown ?? (_previewKeyDown = new SimpleCommand<object, EventToCommandArgs>(delegate(EventToCommandArgs args)
                {
                    // var commandRan = args.CommandRan;

                    // get command parameter
                    // var o = args.CommandParameter; 

                    // get KeyEventArgs
                    var keyEventArgs = (KeyEventArgs)args.EventArgs;
                    
                    // get the orginal event sender
                    // var sender = args.Sender; 
                    switch (keyEventArgs.Key)
                    {
                        case Key.Escape:
                            // Anything else that is to be canceled by the user hitting the ESC key must be put here
                            IsInAnalysisPointMode = false;
                            break;
                        default:
                            break;
                    }
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _previewKeyDown;

        #endregion

        #region public MapLayerCollections MapLayerCollections { get; set; }

        public Dictionary<string, MapLayerCollection> MapLayerCollections
        {
            get { return _mapLayerCollections ?? (_mapLayerCollections = new Dictionary<string, MapLayerCollection>()); }
            set
            {
                if (_mapLayerCollections == value) return;
                _mapLayerCollections = value;
                NotifyPropertyChanged(MapLayerCollectionsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MapLayerCollectionsChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.MapLayerCollections);
        Dictionary<string, MapLayerCollection> _mapLayerCollections;

        #endregion

        [MediatorMessageSink(MediatorMessage.MainViewModelInitialized)]
        void MainViewModelInitialized(Dispatcher dispatcher)
        {
            _mainViewModelInitialized = true;
            AllViewModelsAreReady();
        }
        static bool _mainViewModelInitialized;

        [MediatorMessageSink(MediatorMessage.MapViewModelInitialized)]
        void MapViewModelInitialized(bool dummy)
        {
            _mapViewModelInitialized = true;
            AllViewModelsAreReady();
        }
        static bool _mapViewModelInitialized;

        [MediatorMessageSink(MediatorMessage.LayerListViewModelInitialized)]
        void LayerListViewModelInitialized(bool dummy)
        {
            _layerListViewModelInitialized = true;
            AllViewModelsAreReady();
        }
        static bool _layerListViewModelInitialized;

        static void AllViewModelsAreReady()
        {
            if (_layerListViewModelInitialized && _mapViewModelInitialized && _mainViewModelInitialized)
            {
                _allViewModelsAreReady = true;
                MediatorMessage.Send(MediatorMessage.AllViewModelsAreReady, true);
            }
        }
        static bool _allViewModelsAreReady;

    }
}