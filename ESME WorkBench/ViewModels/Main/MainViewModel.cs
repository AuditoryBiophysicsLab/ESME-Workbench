using System;
using System.Collections.ObjectModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Input;
using System.Windows.Threading;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Locations;
using ESME.Mapping;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using ESME.Views.TransmissionLossViewer;
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
    public partial class MainViewModel : ViewModelBase
    {
        #region Private fields
        readonly IHRCSaveFileService _saveFile;
        readonly IPluginManagerService _plugins;
        readonly EnvironmentalCacheService _cache;
        readonly TransmissionLossCalculatorService _transmissionLoss;
        readonly IViewAwareStatus _viewAwareStatus;
        readonly IMessageBoxService _messageBox;
        readonly IUIVisualizerService _visualizer;
        public const bool ExperimentsCurrentlySupported = false;
        Dispatcher _dispatcher;
        #endregion

        #region Constructor
        [ImportingConstructor]
        public MainViewModel(IViewAwareStatus viewAwareStatus,
                             IMasterDatabaseService database,
                             IMessageBoxService messageBox,
                             IUIVisualizerService visualizer,
                             IHRCSaveFileService saveFile,
                             TransmissionLossCalculatorService transmissionLoss,
                             IPluginManagerService plugins,
                             EnvironmentalCacheService cache)
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
            _transmissionLoss = transmissionLoss;
            _plugins = plugins;
            _cache = cache;
            MapViewModel = new MapViewModel(_viewAwareStatus, _messageBox, this, _visualizer, _saveFile);
            if (!Directory.Exists(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database"))) Directory.CreateDirectory(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database"));
            Database.MasterDatabaseDirectory = Globals.AppSettings.DatabaseDirectory ?? Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database");
            Locations = Database.Context.Locations.Local;
            Scenarios = Database.Context.Scenarios.Local;
            Cursor = Cursors.Arrow;
            _transmissionLoss.RangeCellSize = Globals.AppSettings.BellhopSettings.RangeCellSize;
            _transmissionLoss.DepthCellSize = Globals.AppSettings.BellhopSettings.DepthCellSize;
            _cache.PropertyChanged += (s, e) => { if (e.PropertyName == "BusyCount") CacheActivity = _cache.BusyCount > 0 ? string.Format("Cache: {0} importing", _cache.BusyCount) : "Cache: idle"; };
            _transmissionLoss.WorkQueue.PropertyChanged +=
                (s, e) => { if (e.PropertyName == "Count") TransmissionLossActivity = _transmissionLoss.WorkQueue.Keys.Count > 0 ? string.Format("TL: {0} queued", _transmissionLoss.WorkQueue.Keys.Count) : "TL: idle"; };

            if (Designer.IsInDesignMode) return;

            _viewAwareStatus.ViewLoaded += () =>
            {
                if (Designer.IsInDesignMode) return;
                _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
                _plugins.PluginDirectory = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location);

                Globals.AppSettings.PluginManagerService = _plugins;

                _transmissionLoss.Start();
                NAVOImporter.PluginManagerService = _plugins;
            };
        }

        public ObservableCollection<Location> Locations { get; private set; }
        public ObservableCollection<Scenario> Scenarios { get; private set; }
        public IMasterDatabaseService Database { get; private set; }

        protected override void OnDispose()
        {
            base.OnDispose();
            Mediator.Instance.Unregister(this);
        }
        #endregion

        public Geo MouseGeo { get; set; }

        [MediatorMessageSink(MediatorMessage.SetMouseEarthCoordinate), UsedImplicitly]
        void SetMouseEarthCoordinate(Geo mouseEarthCoordinate)
        {
            MouseGeo = mouseEarthCoordinate;
            if (mouseEarthCoordinate != null)
            {
                var lat = mouseEarthCoordinate.Latitude;
                var lon = mouseEarthCoordinate.Longitude;
                var latInfo = Math.Abs(lat) <= 90 ? string.Format("Lat: {0:0.0000}{1}", Math.Abs(lat), lat >= 0 ? "N" : "S") : "Lat: ??";
                var lonInfo = Math.Abs(lon) <= 180 ? string.Format("Lon: {0:0.0000}{1}", Math.Abs(lon), lon >= 0 ? "E" : "W") : "Lon: ??";
                MouseLocationInfo = latInfo + " " + lonInfo;
            }
            else MouseLocationInfo = "Lat: N/A Lon: N/A";
            if (IsInAnalysisPointMode)
            {
                if (Scenario != null) Cursor = Scenario.GeoRect.Contains(MouseGeo) ? Cursors.Cross : Cursors.No;
                else
                {
                    IsInAnalysisPointMode = false;
                    Cursor = Cursors.Arrow;
                }
            }
            if (Scenario != null && Scenario.Bathymetry != null && _cache.IsCached(Scenario.Bathymetry))
            {
                if (_bathymetryTask == null)
                {
                    _bathymetryTask = Scenario.BathymetryData.Samples.GetNearestPointAsync(MouseGeo);
                    if (!_bathymetryTask.IsCompleted)
                    {
                        MouseDepthInfo = string.Format("Depth: <loading>");
                        _bathymetryTask.ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() =>
                        {
                            MouseDepthInfo = string.Format("Depth: <loaded>");
                            _bathymetryTask = null;
                        }));
                    } else _bathymetryTask.ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() =>
                    {
                        MouseDepth = t.Result.Data;
                        MouseDepthInfo = string.Format("Depth: {0:0.#}m", -MouseDepth);
                        _bathymetryTask = null;
                    }));
                }
            }
            else
            {
                MouseDepth = null;
                MouseDepthInfo = string.Format("Depth: N/A");
            }
            if (Scenario != null && Scenario.Wind != null && _cache.IsCached(Scenario.Wind))
            {
                if (_windTask == null)
                {
                    _windTask = Scenario.WindData[Scenario.TimePeriod].EnvironmentData.GetNearestPointAsync(MouseGeo);
                    if (!_windTask.IsCompleted)
                    {
                        MouseWindSpeedInfo = string.Format("Wind Speed: <loading>");
                        _windTask.ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() =>
                        {
                            MouseWindSpeedInfo = string.Format("Wind Speed: <loaded>");
                            _windTask = null;
                        }));
                    } else _windTask.ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() =>
                    {
                        MouseWindSpeed = t.Result.Data;
                        MouseWindSpeedInfo = string.Format("Wind Speed: {0:0.#}m/s", MouseWindSpeed);
                        _windTask = null;
                    }));
                }
            }
            else
            {
                MouseWindSpeed = null;
                MouseWindSpeedInfo = string.Format("Wind Speed: N/A");
            }
            if (Scenario != null && Scenario.SoundSpeed != null && _cache.IsCached(Scenario.SoundSpeed))
            {
                if (_soundSpeedTask == null)
                {
                    _soundSpeedTask = Scenario.SoundSpeedData[Scenario.TimePeriod].EnvironmentData.GetNearestPointAsync(MouseGeo);
                    if (!_soundSpeedTask.IsCompleted)
                    {
                        MouseSoundSpeedInfo = string.Format("Sound Speed: <loading>");
                        _soundSpeedTask.ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() =>
                        {
                            MouseSoundSpeedInfo = string.Format("Sound Speed: <loaded>");
                            _soundSpeedTask = null;
                        }));
                    } else _soundSpeedTask.ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() =>
                    {
                        MouseSoundSpeed = t.Result;
                        MouseSoundSpeedInfo = string.Format("Sound Speed: {0} samples", MouseSoundSpeed.Data.Count);
                        MapViewModel.MouseSoundSpeedProfile = MouseSoundSpeed;
                        _soundSpeedTask = null;
                    }));
                }
            }
            else
            {
                MouseSoundSpeed = null;
                MouseSoundSpeedInfo = string.Format("Sound Speed: N/A");
                MapViewModel.MouseSoundSpeedProfile = MouseSoundSpeed;
            }

            if (Scenario != null && Scenario.Sediment != null && _cache.IsCached(Scenario.Sediment))
            {
                if (_sedimentTask == null)
                {
                    _sedimentTask = Scenario.SedimentData.Samples.GetNearestPointAsync(MouseGeo);
                    if (!_sedimentTask.IsCompleted)
                    {
                        MouseSedimentInfo = string.Format("Sediment: <loading>");
                        _sedimentTask.ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() =>
                        {
                            MouseSedimentInfo = string.Format("Sediment: <loaded>");
                            _sedimentTask = null;
                        }));
                    } else _sedimentTask.ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() =>
                    {
                        MouseSediment = t.Result;
                        MouseSedimentInfo = string.Format("Sediment: {0}", BottomSedimentTypeTable.SedimentNames[MouseSediment.Data.SampleValue]);
                        _sedimentTask = null;
                    }));
                }
            }
            else
            {
                MouseSediment = null;
                MouseSedimentInfo = string.Format("Sediment: N/A");
            }
        }

        Task<Geo<float>> _bathymetryTask;
        Task<WindSample> _windTask;
        Task<SoundSpeedProfile> _soundSpeedTask;
        Task<SedimentSample> _sedimentTask;

        [Initialize("Cache: idle")] public string CacheActivity { get; set; }
        [Initialize("TL: idle")] public string TransmissionLossActivity { get; set; }

        [Initialize("Lat: N/A Lon: N/A")] public string MouseLocationInfo { get; private set; }

        public float? MouseDepth { get; private set; }
        [Initialize("Depth: N/A")] public string MouseDepthInfo { get; private set; }

        public SedimentSample MouseSediment { get; private set; }
        [Initialize("Sediment: N/A")] public string MouseSedimentInfo { get; private set; }

        public float? MouseWindSpeed { get; private set; }
        [Initialize("Wind Speed: N/A")] public string MouseWindSpeedInfo { get; private set; }

        public SoundSpeedProfile MouseSoundSpeed { get; private set; }
        [Initialize("Sound Speed: N/A")] public string MouseSoundSpeedInfo { get; private set; }

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

        public int SelectedRibbonTabIndex { get { return Settings.Default.SelectedRibbonTabIndex; } set { Settings.Default.SelectedRibbonTabIndex = value; } }

        void ShowAboutView()
        {
            var aboutViewModel = new AboutViewModel();
            _visualizer.ShowDialog("AboutView", aboutViewModel);
        }

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

        static readonly object LockObject = new object();
        Window _queueView;

        public Cursor Cursor { get; set; }

        [MediatorMessageSink(MediatorMessage.MapClick), UsedImplicitly]
        void MapClick(Geo geo)
        {
            Debug.WriteLine("Map click at {0}", geo);
            if (IsInAnalysisPointMode)
            {
                if (Scenario != null && Scenario.GeoRect.Contains(MouseGeo))
                {
                    var analysisPoint = new AnalysisPoint {Geo = MouseGeo, Scenario = Scenario};
                    Scenario.AnalysisPoints.Add(analysisPoint);
                    Database.Add(analysisPoint, (Bathymetry)_cache[Scenario.Bathymetry].Result, true);
                }
                IsInAnalysisPointMode = false;
                Cursor = Cursors.Arrow;
            }
        }

        [MediatorMessageSink(MediatorMessage.MapDoubleClick), UsedImplicitly]
        void MapDoubleClick(Geo geo) { Debug.WriteLine("Map double click at {0}", geo); }

        [MediatorMessageSink(MediatorMessage.ViewTransmissionLoss), UsedImplicitly]
        void ViewTransmissionLoss(ESME.Scenarios.TransmissionLoss transmissionLoss)
        {
            var transmissionLossViewModel = new TransmissionLossViewModel {TransmissionLoss = transmissionLoss,};

            var window = _visualizer.ShowWindow("TransmissionLossWindowView", transmissionLossViewModel);
            transmissionLossViewModel.Window = window;
            transmissionLossViewModel.SaveFileService = _saveFile;
            transmissionLossViewModel.SelectedRadialIndex = 0;
            transmissionLossViewModel.RadialViewModel.WaitToRenderText = "Please Wait ...";
        }

        [MediatorMessageSink(MediatorMessage.ViewAnalysisPoint), UsedImplicitly]
        void ViewAnalysisPoint(AnalysisPoint analysisPoint)
        {
            var analysisPointViewModel = new AnalysisPointViewModel(analysisPoint);
            var window = _visualizer.ShowWindow("AnalysisPointWindowView", analysisPointViewModel);
            analysisPointViewModel.TransmissionLossViewModel.Window = window;
            analysisPointViewModel.TransmissionLossViewModel.SaveFileService = _saveFile;
            analysisPointViewModel.TransmissionLossViewModel.SelectedRadialIndex = 0;
            analysisPointViewModel.TransmissionLossViewModel.RadialViewModel.WaitToRenderText = "Please Wait ...";
        }
    }
}