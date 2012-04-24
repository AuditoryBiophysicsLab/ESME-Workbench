using System;
using System.Collections.ObjectModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Windows;
using System.Windows.Input;
using System.Windows.Threading;
using ESME;
using ESME.Environment;
using ESME.Locations;
using ESME.Mapping;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using ESMEWorkbench.Properties;
using ESMEWorkbench.ViewModels.Map;
using HRC;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Services;
using HRC.ViewModels;
using HRC.WPF;
using MEFedMVVM.Common;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkbench.ViewModels.Main
{
    [ExportViewModel("MainViewModel")]
    public partial class MainViewModel : ViewModelBase, IPartImportsSatisfiedNotification
    {
        #region Private fields

        [Import, UsedImplicitly] IHRCOpenFileService _openFile;
        readonly IHRCSaveFileService _saveFile;
        [Import, UsedImplicitly] IPluginManagerService _plugins;
        [Import, UsedImplicitly] EnvironmentalCacheService _cache;
        [Import, UsedImplicitly] TransmissionLossCalculatorService _transmissionLoss;
        readonly IViewAwareStatus _viewAwareStatus;
        readonly IMessageBoxService _messageBox;
        readonly IUIVisualizerService _visualizer;
        //TransmissionLossQueueCalculatorViewModel _bellhopQueueCalculatorViewModel;
        public const bool ExperimentsCurrentlySupported = false;
        Dispatcher _dispatcher;
        #endregion

        #region Constructor
        [ImportingConstructor]
        public MainViewModel(IViewAwareStatus viewAwareStatus, MasterDatabaseService database, IMessageBoxService messageBox, IUIVisualizerService visualizer, IHRCSaveFileService saveFile)
        {
            MainWindowTitle = "ESME Workbench: <No scenario loaded>";
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
            _visualizer = visualizer;
            _saveFile = saveFile;
            MapViewModel = new MapViewModel(_viewAwareStatus, _messageBox, this, _visualizer, _saveFile);
            if (!Directory.Exists(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database")))
                Directory.CreateDirectory(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database"));
            Database.MasterDatabaseDirectory = Globals.AppSettings.DatabaseDirectory ?? Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database");
            Locations = Database.Context.Locations.Local;
            Scenarios = Database.Context.Scenarios.Local;

            if (Designer.IsInDesignMode) return;

            _viewAwareStatus.ViewLoaded += () =>
            {
                if (Designer.IsInDesignMode) return;
                _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
                _plugins.PluginDirectory = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location);

                Globals.AppSettings.PluginManagerService = _plugins;

                if (_importsSatisfied) _transmissionLoss.Start();
                NAVOImporter.PluginManagerService = _plugins;
            };
        }

        bool _importsSatisfied;
        public void OnImportsSatisfied()
        {
            _transmissionLoss.RangeCellSize = Globals.AppSettings.BellhopSettings.RangeCellSize;
            _transmissionLoss.DepthCellSize = Globals.AppSettings.BellhopSettings.DepthCellSize;
            _importsSatisfied = true;
            _cache.PropertyChanged += (s, e) =>
            {
                if (e.PropertyName == "BusyCount") CacheActivity = _cache.BusyCount > 0 ? string.Format("Cache: {0} importing", _cache.BusyCount) : "Cache: idle";
            };
            _transmissionLoss.WorkQueue.PropertyChanged += (s, e) =>
            {
                if (e.PropertyName == "Count") TransmissionLossActivity = _transmissionLoss.WorkQueue.Keys.Count > 0 ? string.Format("TL: {0} queued", _transmissionLoss.WorkQueue.Keys.Count) : "TL: idle";
            };
            if (Database.MasterDatabaseDirectory != null) _transmissionLoss.Start();
        }

        public ObservableCollection<Location> Locations { get; private set; }
        public ObservableCollection<Scenario> Scenarios { get; private set; }
        public MasterDatabaseService Database { get; private set; }

        protected override void OnDispose()
        {
            base.OnDispose();
            Mediator.Instance.Unregister(this);
        }

        #endregion

        [Initialize("Cache: idle")]
        public string CacheActivity { get; set; }
        [Initialize("TL: idle")]
        public string TransmissionLossActivity { get; set; }
        public string DecoratedExperimentName { get; set; }

        [Affects("MouseLocationInfo")]
        public Geo MouseGeo { get; set; }

        [MediatorMessageSink(MediatorMessage.SetMouseEarthCoordinate), UsedImplicitly]
        void SetMouseEarthCoordinate(Geo mouseEarthCoordinate)
        {
            MouseGeo = mouseEarthCoordinate;
            var foo = MouseSoundSpeed;
        }

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

        public SoundSpeedProfile MouseSoundSpeed
        {
            get
            {
                if (MouseGeo == null) return null;
                var lat = MouseGeo.Latitude;
                var lon = MouseGeo.Longitude;
                if (-90 > lat || lat > 90) return null;
                if (-180 > lon || lon > 180) return null;
                SoundSpeedProfile mouseSoundSpeedProfile = null;
                if (Scenario != null && Scenario.SoundSpeed != null && ((SoundSpeed)_cache[Scenario.SoundSpeed])[Scenario.TimePeriod].EnvironmentData.GeoRect.Contains(MouseGeo))
                    mouseSoundSpeedProfile = ((SoundSpeed)_cache[Scenario.SoundSpeed])[Scenario.TimePeriod].EnvironmentData.GetNearestPoint(MouseGeo);
                MapViewModel.MouseSoundSpeedProfile = mouseSoundSpeedProfile;
                return mouseSoundSpeedProfile;
            }
        }

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
                _isInAnalysisPointMode = value;
                MediatorMessage.Send(MediatorMessage.SetAnalysisPointMode, _isInAnalysisPointMode);
            }
        }

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

        [MediatorMessageSink(MediatorMessage.ShowTransmissionLossQueueView), UsedImplicitly]
        void ShowTransmissionLossQueueView(bool isVisible)
        {
            if (_dispatcher == null) return;
            lock (LockObject)
            {
                if (isVisible)
                {
                    if (_queueView == null)
                    {
                        _dispatcher.InvokeIfRequired(() => _queueView = _visualizer.ShowWindow("TransmissionLossQueueView",
                                                                                               _transmissionLoss.WorkQueue,
                                                                                               false,
                                                                                               (sender, args) =>
                                                                                               {
                                                                                                   _queueView.Close();
                                                                                                   _queueView = null;
                                                                                               }));
                    }
                }
                else
                {
                    if (_queueView != null)
                    {
                        _dispatcher.InvokeIfRequired(() =>
                        {
                            _queueView.Close();
                            _queueView = null;
                        });
                    }
                }
            }
        }
        readonly static object LockObject = new object();
        Window _queueView;

        [MediatorMessageSink(MediatorMessage.MapClick), UsedImplicitly]
        void MapClick(Geo geo)
        {
            Debug.WriteLine("Map click at {0}", geo);
        }

        [MediatorMessageSink(MediatorMessage.MapDoubleClick), UsedImplicitly]
        void MapDoubleClick(Geo geo)
        {
            Debug.WriteLine("Map double click at {0}", geo);
        }
    }
}