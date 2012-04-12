using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Windows;
using System.Windows.Input;
using System.Windows.Threading;
using Cinch;
using ESME;
using ESME.Environment;
using ESME.Locations;
using ESME.Mapping;
using ESME.Plugins;
using ESME.TransmissionLoss;
using ESMEWorkbench.Data;
using ESMEWorkbench.Properties;
using ESMEWorkbench.ViewModels.Map;
using HRC;
using HRC.Navigation;
using HRC.Services;
using MEFedMVVM.Common;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkbench.ViewModels.Main
{
    [ExportViewModel("MainViewModel")]
    public partial class MainViewModel : ViewModelBase
    {
        #region Private fields

        [Import, UsedImplicitly] IHRCOpenFileService _openFile;
        [Import, UsedImplicitly] IHRCSaveFileService _saveFile;
        [Import, UsedImplicitly] IUIVisualizerService _visualizer;
        [Import, UsedImplicitly] IPluginManagerService _plugins;
        [Import, UsedImplicitly] EnvironmentalCacheService _cache;
        [Import, UsedImplicitly] TransmissionLossCalculatorService _transmissionLoss;
        readonly IViewAwareStatus _viewAwareStatus;
        readonly IMessageBoxService _messageBox;
#if EXPERIMENTS_SUPPORTED
        Experiment _experiment;
#endif
        //TransmissionLossQueueCalculatorViewModel _bellhopQueueCalculatorViewModel;
        Dispatcher _dispatcher;
        public const bool ExperimentsCurrentlySupported = false;
        #endregion

        #region Constructor
        [ImportingConstructor]
        public MainViewModel(IViewAwareStatus viewAwareStatus, MasterDatabaseService database, IMessageBoxService messageBox)
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
            _viewAwareStatus = viewAwareStatus;
            _messageBox = messageBox;
            Database = database;
            MapViewModel = new MapViewModel(_viewAwareStatus, _messageBox, this);
            if (Designer.IsInDesignMode) return;
            _viewAwareStatus.ViewUnloaded += () =>
            {
                if (Designer.IsInDesignMode) return;
                MediatorMessage.Send(MediatorMessage.ApplicationClosing);
            };
            
            _viewAwareStatus.ViewLoaded += () =>
            {
                if (Designer.IsInDesignMode) return;
#if DEBUG
                _plugins.PluginDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
#else
                _pluginManagerService.PluginDirectory = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "Plugins");
#endif

                _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
                Globals.AppSettings.PluginManagerService = _plugins;
                if (!Directory.Exists(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database")))
                    Directory.CreateDirectory(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database"));
                Database.MasterDatabaseDirectory = Globals.AppSettings.DatabaseDirectory ?? Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database");
                NAVOImporter.PluginManagerService = _plugins;
            };
        }

        public MasterDatabaseService Database { get; private set; }

        protected override void OnDispose()
        {
            base.OnDispose();
            Mediator.Instance.Unregister(this);
        }

        #endregion

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
                if (Scenario != null && Scenario.Bathymetry != null && ((Bathymetry)_cache[Scenario.Bathymetry]).Samples.GeoRect.Contains(MouseGeo))
                {
                    MouseDepth = ((Bathymetry)_cache[Scenario.Bathymetry]).Samples.GetNearestPoint(MouseGeo).Data;
                    return string.Format("Lat: {0:0.0000}{1} Lon: {2:0.0000}{3} Elevation: {4:0.#}m",
                                         Math.Abs(lat),
                                         northSouth,
                                         Math.Abs(lon),
                                         eastWest,
                                         MouseDepth);
                }
                MouseDepth = null;
                return string.Format("Lat: {0:0.0000}{1} Lon: {2:0.0000}{3}", Math.Abs(lat), northSouth, Math.Abs(lon), eastWest);
            }
        }

        static readonly PropertyChangedEventArgs MouseLocationInfoChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.MouseLocationInfo);

        #endregion

        public float? MouseDepth { get; set; }

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

        public MapLayerCollection CurrentMapLayers { get; set; }

        public int SelectedRibbonTabIndex
        {
            get { return Settings.Default.SelectedRibbonTabIndex; }
            set { Settings.Default.SelectedRibbonTabIndex = value; }
        }

        void ShowAboutView()
        {
            var aboutViewModel = new AboutViewModel();
            _visualizer.ShowDialog("AboutView", aboutViewModel);
        }

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

        [MediatorMessageSink(MediatorMessage.SetAnalysisPointMode), UsedImplicitly]
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
                    }
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _previewKeyDown;

        #endregion
    }
}