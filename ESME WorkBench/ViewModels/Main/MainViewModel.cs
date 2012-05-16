﻿using System;
using System.Collections.Generic;
using System.Linq;
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
using ESME.Database;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Locations;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using ESME.Views.TransmissionLossViewer;
using ESMEWorkbench.Properties;
using ESMEWorkbench.ViewModels.Map;
using ESMEWorkbench.ViewModels.Tree;
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

        List<Window> _openPopups;
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
            _openPopups = new List<Window>();
            MapViewModel = new MapViewModel(_viewAwareStatus, _messageBox, this, _visualizer, _saveFile);
            if (!Directory.Exists(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database"))) Directory.CreateDirectory(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database"));
            Database.MasterDatabaseDirectory = Globals.AppSettings.DatabaseDirectory ?? Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database");
            Scenarios = Database.Context.Scenarios.Local;
            LocationsTreeViewModel = new LocationsTreeViewModel(Database.Context.Locations.Local);
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
                _transmissionLoss.Dispatcher = _dispatcher;
                _transmissionLoss.Start();
                NAVOImporter.PluginManagerService = _plugins;
                GeoRect locationsExtent = null;
                if (!Database.Context.Locations.Any()) _dispatcher.InvokeInBackgroundIfRequired(async () =>
                {
                    var wind = (EnvironmentalDataSourcePluginBase)_plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Wind];
                    var soundSpeed = (EnvironmentalDataSourcePluginBase)_plugins[PluginType.EnvironmentalDataSource, PluginSubtype.SoundSpeed];
                    var bathymetry = (EnvironmentalDataSourcePluginBase)_plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry];
                    var sediment = (EnvironmentalDataSourcePluginBase)_plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Sediment];
                    foreach (var locations in Database.Context.Locations.Local) locations.CreateMapLayers();
                    if (wind == null || soundSpeed == null || bathymetry == null || sediment == null) return;
                    var result = _visualizer.ShowDialog("FirstRunQuestionView", new FirstRunQuestionViewModel());
                    if (!result.HasValue || !result.Value) return;
                    var progress = new FirstRunProgressViewModel { ItemCount = 20, CurrentItem = 0 };
                    var window = _visualizer.ShowWindow("FirstRunProgressView", progress, true);
                    _openPopups.Add(window);
                    await TaskEx.Delay(10);
                    var windData = new EnvironmentalDataSet { SourcePlugin = new DbPluginIdentifier(wind.PluginIdentifier), Resolution = wind.AvailableResolutions.Max() };
                    var soundSpeedData = new EnvironmentalDataSet { SourcePlugin = new DbPluginIdentifier(soundSpeed.PluginIdentifier), Resolution = soundSpeed.AvailableResolutions.Max() };
                    var bathymetryData = new EnvironmentalDataSet { SourcePlugin = new DbPluginIdentifier(bathymetry.PluginIdentifier), Resolution = bathymetry.AvailableResolutions.Max() };
                    var sedimentData = new EnvironmentalDataSet { SourcePlugin = new DbPluginIdentifier(sediment.PluginIdentifier), Resolution = sediment.AvailableResolutions.Max() };
                    MediatorMessage.Send(MediatorMessage.SetMapExtent, new GeoRect(45, 22, -70, -120));
                    await CreateSample("Gulf of Maine", "Maine Sample", new GeoRect(44, 41, -65, -71), (TimePeriod)DateTime.Today.Month, progress, windData, soundSpeedData, bathymetryData, sedimentData);
                    if (progress.IsCanceled) { window.Close(); return; }

                    await CreateSample("Carolina Coast", "Carolina Sample", new GeoRect(36, 33, -75, -78), (TimePeriod)DateTime.Today.Month, progress, windData, soundSpeedData, bathymetryData, sedimentData);
                    if (progress.IsCanceled) { window.Close(); return; }

                    await CreateSample("Florida Atlantic Coast", "Florida Atlantic Sample", new GeoRect(32, 27, -76, -81.5), (TimePeriod)DateTime.Today.Month, progress, windData, soundSpeedData, bathymetryData, sedimentData);
                    if (progress.IsCanceled) { window.Close(); return; }

                    await CreateSample("Florida Gulf Coast", "Florida Gulf Sample", new GeoRect(30.5, 25, -81, -87), (TimePeriod)DateTime.Today.Month, progress, windData, soundSpeedData, bathymetryData, sedimentData);
                    if (progress.IsCanceled) { window.Close(); return; }

                    await CreateSample("Southern California", "Southern California Sample", new GeoRect(34, 32, -117.5, -120), (TimePeriod)DateTime.Today.Month, progress, windData, soundSpeedData, bathymetryData, sedimentData);
                    if (progress.IsCanceled) { window.Close(); return; }

                    await CreateSample("Bahamas", "Bahamas Sample", new GeoRect(27, 22, -73.5, -79), (TimePeriod)DateTime.Today.Month, progress, windData, soundSpeedData, bathymetryData, sedimentData);
                    if (progress.IsCanceled) { window.Close(); return; }

                    progress.ProgressMessage = string.Format("Updating database...");
                    progress.CurrentItem++;
                    //Database.SaveChanges();
                    window.Close();
                });
                else foreach (var location in Database.Context.Locations.Local)
                {
                    locationsExtent = locationsExtent == null ? new GeoRect(location.GeoRect) : GeoRect.Union(locationsExtent, (GeoRect)location.GeoRect);
                    location.CreateMapLayers();
                }
                if (Database.Context.Locations.Local.Count > 0) MediatorMessage.Send(MediatorMessage.SetMapExtent, locationsExtent);
            };
        }

        async Task CreateSample(string locationName, string scenarioName, GeoRect locationGeoRect, TimePeriod timePeriod, FirstRunProgressViewModel progress, EnvironmentalDataSet windData, EnvironmentalDataSet soundSpeedData, EnvironmentalDataSet bathymetryData, EnvironmentalDataSet sedimentData)
        {
            progress.ProgressMessage = string.Format("Creating sample location \"{0}\"", locationName);
            progress.CurrentItem++;
            await TaskEx.Delay(10);
            var location = CreateLocation(locationName, "Created as a sample location", locationGeoRect);
            location.CreateMapLayers();
            progress.ProgressMessage = string.Format("Creating sample scenario \"{0}\"", scenarioName);
            progress.CurrentItem++;
            await TaskEx.Delay(10);
            var scenario = CreateScenario(location, scenarioName, "Created as a sample scenario", timePeriod, windData, soundSpeedData, bathymetryData, sedimentData);
            AddMode(AddSource(AddPlatform(scenario, "Sample Platform", false), "Sample Source", false), "1 KHz mode", false);
            //Database.SaveChanges();
            await TaskEx.WhenAll(_cache[scenario.Wind], _cache[scenario.SoundSpeed], _cache[scenario.Bathymetry], _cache[scenario.Sediment]);
            _dispatcher.InvokeInBackgroundIfRequired(() =>
            {
                progress.ProgressMessage = string.Format("Adding analysis point to scenario \"{0}\"", scenarioName);
                progress.CurrentItem++;
                scenario.ShowAllAnalysisPoints = true;
                var analysisPoint = new AnalysisPoint
                {
                    Geo = new Geo(((GeoRect)location.GeoRect).Center),
                    Scenario = scenario
                };
                scenario.AnalysisPoints.Add(analysisPoint);
                Database.Add(analysisPoint, (Bathymetry)_cache[scenario.Bathymetry].Result);
            });
        }

        public ObservableCollection<Scenario> Scenarios { get; private set; }
        public IMasterDatabaseService Database { get; private set; }

        protected override void OnDispose()
        {
            base.OnDispose();
            Mediator.Instance.Unregister(this);
        }
        #endregion

        public Geo MouseGeo { get; set; }
        AnalysisPoint _fakeAnalysisPoint;

        [MediatorMessageSink(MediatorMessage.SetMouseGeo), UsedImplicitly]
        void SetMouseEarthCoordinate(Geo mouseGeo)
        {
            MouseGeo = mouseGeo;
#if false
            if (IsInAnalysisPointMode)
            {
                if (_fakeAnalysisPoint == null)
                {
                    _fakeAnalysisPoint = new AnalysisPoint { Geo = MouseGeo, Scenario = Scenario };
                    Database.AddFakeMapLayer(_fakeAnalysisPoint);
                    _fakeAnalysisPoint.CreateMapLayers();
                    _fakeAnalysisPoint.LayerSettings.IsChecked = true;
                }
                else
                {
                    _fakeAnalysisPoint.Geo = MouseGeo;
                    MediatorMessage.Send(MediatorMessage.RemoveMapLayer, _fakeAnalysisPoint.LayerSettings.MapLayerViewModel);
                    ((OverlayShapeMapLayer)_fakeAnalysisPoint.LayerSettings.MapLayerViewModel).DrawAction();
                    MediatorMessage.Send(MediatorMessage.AddMapLayer, _fakeAnalysisPoint.LayerSettings.MapLayerViewModel);
                }
            }
#endif
            if (MouseGeo != null)
            {
                var lat = mouseGeo.Latitude;
                var lon = mouseGeo.Longitude;
                MouseLatitude = Math.Abs(lat) <= 90 ? string.Format("Lat: {0:0.0000}{1}", Math.Abs(lat), lat >= 0 ? "N" : "S") : "Lat: N/A";
                MouseLongitude = Math.Abs(lon) <= 180 ? string.Format("Lon: {0:0.0000}{1}", Math.Abs(lon), lon >= 0 ? "E" : "W") : "Lon: N/A";
            }
            else
            {
                MouseLatitude = "Lat: N/A";
                MouseLongitude = "Lon: N/A";
            }
            if (Scenario == null || MouseGeo == null || !Scenario.GeoRect.Contains(MouseGeo))
            {
                MouseDepth = null;
                MouseDepthInfo = string.Format("Depth: N/A");
                MouseWindSpeed = null;
                MouseWindSpeedInfo = string.Format("Wind: N/A");
                MouseSoundSpeed = null;
                MouseSoundSpeedInfo = string.Format("Sound Speed: N/A");
                MapViewModel.MouseSoundSpeedProfile = MouseSoundSpeed;
                MouseSediment = null;
                MouseSedimentInfo = string.Format("Sediment: N/A");
                return;
            }
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
                        MouseDepthInfo = string.Format("Depth: {0}m", -MouseDepth);
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
                        MouseWindSpeedInfo = string.Format("Wind: <loading>");
                        _windTask.ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() =>
                        {
                            MouseWindSpeedInfo = string.Format("Wind: <loaded>");
                            _windTask = null;
                        }));
                    } else _windTask.ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() =>
                    {
                        MouseWindSpeed = t.Result.Data;
                        MouseWindSpeedInfo = string.Format("Wind: {0:0.0}m/s", MouseWindSpeed);
                        _windTask = null;
                    }));
                }
            }
            else
            {
                MouseWindSpeed = null;
                MouseWindSpeedInfo = string.Format("Wind: N/A");
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
                        MouseSoundSpeedInfo = string.Format("Sound Speed: {0:0} samples", MouseSoundSpeed.Data.Count);
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

        public bool IsModified { get { return Database.Context.IsModified; } }


        [Initialize("Cache: idle")] public string CacheActivity { get; set; }
        [Initialize("TL: idle")] public string TransmissionLossActivity { get; set; }

        [Initialize("Lat: N/A")] public string MouseLatitude { get; private set; }

        [Initialize("Lon: N/A")] public string MouseLongitude { get; private set; }

        public float? MouseDepth { get; private set; }
        [Initialize("Depth: N/A")] public string MouseDepthInfo { get; private set; }

        public SedimentSample MouseSediment { get; private set; }
        [Initialize("Sediment: N/A")] public string MouseSedimentInfo { get; private set; }

        public float? MouseWindSpeed { get; private set; }
        [Initialize("Wind: N/A")] public string MouseWindSpeedInfo { get; private set; }

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
                            Cursor = Cursors.Arrow;
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
                if (Scenario != null && geo != null && Scenario.GeoRect.Contains(geo))
                {
                    var analysisPoint = new AnalysisPoint { Geo = new Geo(geo), Scenario = Scenario };
                    Scenario.AnalysisPoints.Add(analysisPoint);
                    TaskEx.Run(() =>
                    {
                        Database.Add(analysisPoint, (Bathymetry)_cache[Scenario.Bathymetry].Result);
                        _dispatcher.InvokeInBackgroundIfRequired(analysisPoint.CreateMapLayers);
                    });
                }
                IsInAnalysisPointMode = false;
                Cursor = Cursors.Arrow;
            }
            if (_fakeAnalysisPoint != null)
            {
                _fakeAnalysisPoint.RemoveMapLayers();
                _fakeAnalysisPoint = null;
            }
        }

        [MediatorMessageSink(MediatorMessage.MapDoubleClick), UsedImplicitly]
        void MapDoubleClick(Geo geo) { Debug.WriteLine("Map double click at {0}", geo); }

        [MediatorMessageSink(MediatorMessage.ViewTransmissionLoss), UsedImplicitly]
        void ViewTransmissionLoss(ESME.Scenarios.TransmissionLoss transmissionLoss)
        {
            var transmissionLossViewModel = new TransmissionLossViewModel {TransmissionLoss = transmissionLoss,};

            var window = _visualizer.ShowWindow("TransmissionLossWindowView", transmissionLossViewModel);
            _openPopups.Add(window);
            transmissionLossViewModel.Window = window;
            transmissionLossViewModel.SaveFileService = _saveFile;
            transmissionLossViewModel.SelectedRadialIndex = 0;
            transmissionLossViewModel.RadialViewModel.WaitToRenderText = "Please wait...";
        }

        [MediatorMessageSink(MediatorMessage.ViewAnalysisPoint), UsedImplicitly]
        void ViewAnalysisPoint(AnalysisPoint analysisPoint)
        {
            var analysisPointViewModel = new AnalysisPointViewModel(analysisPoint);
            var window = _visualizer.ShowWindow("AnalysisPointWindowView", analysisPointViewModel);
            _openPopups.Add(window);
            analysisPointViewModel.TransmissionLossViewModel.Window = window;
            analysisPointViewModel.TransmissionLossViewModel.SaveFileService = _saveFile;
            analysisPointViewModel.TransmissionLossViewModel.SelectedRadialIndex = 0;
            analysisPointViewModel.TransmissionLossViewModel.RadialViewModel.WaitToRenderText = "Please wait...";
        }
    }
}