using System;
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
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Locations;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using ESME.Views.TransmissionLossViewer;
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
        public const bool ExperimentsCurrentlySupported = false;

        readonly List<Window> _openPopups = new List<Window>();
        #endregion

        #region Constructor
        [ImportingConstructor]
        public MainViewModel(IViewAwareStatus viewAwareStatus,
                             IMasterDatabaseService database,
                             IMessageBoxService messageBox,
                             IUIVisualizerService visualizer,
                             IHRCSaveFileService saveFile,
                             IHRCOpenFileService openFile,
                             TransmissionLossCalculatorService transmissionLoss,
                             IPluginManagerService plugins,
                             EnvironmentalCacheService cache)
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
            Cursor = Cursors.Arrow;

            ESME.Data.AppSettings.ApplicationName = App.Name;
            Globals.AppSettings = ESME.Data.AppSettings.Load(ESME.Data.AppSettings.AppSettingsFile);
            Globals.AppSettings.Save();

            Globals.PluginManagerService = plugins;
            Globals.TransmissionLossCalculatorService = transmissionLoss;
            Globals.MasterDatabaseService = database;
            Globals.VisualizerService = visualizer;
            Globals.SaveFileService = saveFile;
            Globals.OpenFileService = openFile;
            Globals.EnvironmentalCacheService = cache;
            Globals.ViewAwareStatusService = viewAwareStatus;
            Globals.MessageBoxService = messageBox;

            MapViewModel = new MapViewModel(this);

            Globals.TransmissionLossCalculatorService.WorkQueue.PropertyChanged +=
                (s, e) =>
                {
                    if (e.PropertyName == "Count")
                    {
                        TransmissionLossActivity = Globals.TransmissionLossCalculatorService.WorkQueue.Keys.Count > 0
                            ? string.Format("Acoustic Simulator: {0} items", Globals.TransmissionLossCalculatorService.WorkQueue.Keys.Count)
                            : "Acoustic Simulator: idle";
                        // Debug.WriteLine(string.Format("TransmissionLossActivity: {0}", TransmissionLossActivity));
                        var isBusy = Globals.TransmissionLossCalculatorService.WorkQueue.Keys.Count > 0;
                        IsTransmissionLossBusy = isBusy;
                        if (!isBusy && IsSaveSampleDataRequested)
                        {
                            Globals.MasterDatabaseService.SaveChanges();
                            IsSaveSampleDataRequested = false;
                        }

                    }
                };

            if (Designer.IsInDesignMode) return;

            Globals.ViewAwareStatusService.ViewLoaded += ViewLoaded;
            //_appTracker = new ApplicationTracker("UA-44329261-1", "TestWPFApp");
            //_appTracker.StartSession();
            //_appTracker.TrackEvent(ApplicationTrackerCategories.Command, "ApplicationStartup");
        }

        void ViewLoaded()
        {
            if (Designer.IsInDesignMode) return;

            MapViewModel.MouseGeo.Subscribe(SetMouseEarthCoordinate);
            MapViewModel.Click.Subscribe(MapClick);

            var databaseFile = Path.Combine(Globals.MasterDatabaseService.MasterDatabaseDirectory, "esme.db.sdf");
            if (Directory.Exists(Globals.MasterDatabaseService.MasterDatabaseDirectory) && File.Exists(databaseFile))
            {
                var fileInfo = new FileInfo(databaseFile);
                var cutoffTime = new DateTime(2013, 5, 1, 00, 00, 00);
                if (fileInfo.CreationTime < cutoffTime) Directory.Delete(Globals.MasterDatabaseService.MasterDatabaseDirectory, true);
            }

            Scenarios = Globals.MasterDatabaseService.Context.Scenarios.Local;
            LocationsTreeViewModel = new LocationsTreeViewModel(Globals.MasterDatabaseService.Context.Locations.Local);

            Globals.Dispatcher = ((Window)Globals.ViewAwareStatusService.View).Dispatcher;
            Globals.PluginManagerService.PluginDirectory = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location);
            Globals.AppSettings.PluginManagerService = Globals.PluginManagerService;
            Globals.TransmissionLossCalculatorService.Start();
            GeoRect locationsExtent = null;
            if (!Globals.MasterDatabaseService.Context.Locations.Any()) Globals.Dispatcher.InvokeInBackgroundIfRequired(CreateSampleScenariosIfRequested);
            else foreach (var location in Globals.MasterDatabaseService.Context.Locations.Local)
                {
                    locationsExtent = locationsExtent == null ? new GeoRect(location.GeoRect) : GeoRect.Union(locationsExtent, (GeoRect)location.GeoRect);
                    location.UpdateMapLayers();
                }
            if (Globals.MasterDatabaseService.Context.Locations.Local.Count > 0) MediatorMessage.Send(MediatorMessage.SetMapExtent, locationsExtent);
            
        }
        public ObservableCollection<Scenario> Scenarios { get; private set; }
        [Affects("IsRunSimulationCommandEnabled")] public bool IsTransmissionLossBusy { get; set; }
        protected override void OnDispose()
        {
            base.OnDispose();
            Mediator.Instance.Unregister(this);
        }
        #endregion

        public Geo MouseGeo { get; set; }
        AnalysisPoint _fakeAnalysisPoint;

        void SetMouseEarthCoordinate(Geo mouseGeo)
        {
            MouseGeo = mouseGeo;
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
            if (Scenario != null && Scenario.Bathymetry != null && Globals.EnvironmentalCacheService.IsCached(Scenario.Bathymetry) && Scenario.Bathymetry.SampleCount > 0)
            {
                if (_bathymetryTask == null)
                {
                    _bathymetryTask = Scenario.BathymetryData.Samples.GetNearestPointAsync(MouseGeo);
                    if (!_bathymetryTask.IsCompleted)
                    {
                        MouseDepthInfo = string.Format("Depth: <loading>");
                        _bathymetryTask.ContinueWith(t => Globals.Dispatcher.InvokeInBackgroundIfRequired(() =>
                        {
                            MouseDepthInfo = string.Format("Depth: <loaded>");
                            _bathymetryTask = null;
                        }));
                    }
                    else _bathymetryTask.ContinueWith(t => Globals.Dispatcher.InvokeInBackgroundIfRequired(() =>
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
            if (Scenario != null && Scenario.Wind != null && Globals.EnvironmentalCacheService.IsCached(Scenario.Wind) && Scenario.Wind.SampleCount > 0)
            {
                if (_windTask == null)
                {
                    _windTask = Scenario.WindData[Scenario.TimePeriod].EnvironmentData.GetNearestPointAsync(MouseGeo);
                    if (!_windTask.IsCompleted)
                    {
                        MouseWindSpeedInfo = string.Format("Wind: <loading>");
                        _windTask.ContinueWith(t => Globals.Dispatcher.InvokeInBackgroundIfRequired(() =>
                        {
                            MouseWindSpeedInfo = string.Format("Wind: <loaded>");
                            _windTask = null;
                        }));
                    }
                    else _windTask.ContinueWith(t => Globals.Dispatcher.InvokeInBackgroundIfRequired(() =>
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
            if (Scenario != null && Scenario.SoundSpeed != null && Globals.EnvironmentalCacheService.IsCached(Scenario.SoundSpeed) && Scenario.SoundSpeed.SampleCount > 0)
            {
                if (_soundSpeedTask == null)
                {
                    _soundSpeedTask = Scenario.SoundSpeedData[Scenario.TimePeriod].EnvironmentData.GetNearestPointAsync(MouseGeo);
                    if (!_soundSpeedTask.IsCompleted)
                    {
                        MouseSoundSpeedInfo = string.Format("Sound Speed: <loading>");
                        _soundSpeedTask.ContinueWith(t => Globals.Dispatcher.InvokeInBackgroundIfRequired(() =>
                        {
                            MouseSoundSpeedInfo = string.Format("Sound Speed: <loaded>");
                            _soundSpeedTask = null;
                        }));
                    }
                    else _soundSpeedTask.ContinueWith(t => Globals.Dispatcher.InvokeInBackgroundIfRequired(() =>
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
            if (Scenario != null && Scenario.Sediment != null && Globals.EnvironmentalCacheService.IsCached(Scenario.Sediment) && Scenario.Sediment.SampleCount > 0)
            {
                if (_sedimentTask == null)
                {
                    _sedimentTask = Scenario.SedimentData.Samples.GetNearestPointAsync(MouseGeo);
                    if (!_sedimentTask.IsCompleted)
                    {
                        MouseSedimentInfo = string.Format("Sediment: <loading>");
                        _sedimentTask.ContinueWith(t => Globals.Dispatcher.InvokeInBackgroundIfRequired(() =>
                        {
                            MouseSedimentInfo = string.Format("Sediment: <loaded>");
                            _sedimentTask = null;
                        }));
                    }
                    else _sedimentTask.ContinueWith(t => Globals.Dispatcher.InvokeInBackgroundIfRequired(() =>
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

        public bool IsModified { get { return Globals.MasterDatabaseService.Context.IsModified; } }



        [Initialize("Acoustic Simulator: idle")]
        public string TransmissionLossActivity { get; set; }

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

        void ShowAboutView()
        {
            var aboutViewModel = new AboutViewModel();
            Globals.VisualizerService.ShowDialog("AboutView", aboutViewModel);
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
            if (Globals.Dispatcher == null) return;
            lock (LockObject)
            {
                if (isVisible)
                {
                    if (_queueView == null)
                    {
                        Globals.Dispatcher.InvokeIfRequired(() => _queueView = Globals.VisualizerService.ShowWindow("TransmissionLossQueueView",
                                                                                               Globals.TransmissionLossCalculatorService.WorkQueue,
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
                        Globals.Dispatcher.InvokeIfRequired(() =>
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

        void MapClick(Geo geo)
        {
            if (IsInAnalysisPointMode)
            {
                if (Scenario != null && geo != null && Scenario.GeoRect.Contains(geo))
                {
                    Task.Run(() =>
                    {
                        var analysisPoint = new AnalysisPoint { Geo = new Geo(geo), Scenario = Scenario };
                        Scenario.AnalysisPoints.Add(analysisPoint);
                        Scenario.UpdateAnalysisPoints();
                        analysisPoint.CheckForErrors();
                        var analysisPointsNode = (AnalysisPointsNode)LayerTreeViewModel.RootNodes.FirstOrDefault(n => n.GetType() == typeof(AnalysisPointsNode));
                        if (analysisPointsNode != null) analysisPointsNode.CheckForErrors();
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

        [MediatorMessageSink(MediatorMessage.ViewTransmissionLoss), UsedImplicitly]
        void ViewTransmissionLoss(TransmissionLoss transmissionLoss)
        {
            try
            {
                var transmissionLossViewModel = new TransmissionLossViewModel { TransmissionLoss = transmissionLoss };
                var window = Globals.VisualizerService.ShowWindow("TransmissionLossWindowView", transmissionLossViewModel);
                _openPopups.Add(window);
                transmissionLossViewModel.Window = window;
                transmissionLossViewModel.SaveFileService = Globals.SaveFileService;
            }
            catch (InvalidOperationException ex)
            {
                Globals.MessageBoxService.ShowError(string.Format("Error displaying transmission loss: {0}", ex.Message));
            }
        }

        [MediatorMessageSink(MediatorMessage.ViewAnalysisPoint), UsedImplicitly]
        void ViewAnalysisPoint(AnalysisPoint analysisPoint)
        {
            try
            {
                var analysisPointViewModel = new AnalysisPointViewModel(analysisPoint);
                var window = Globals.VisualizerService.ShowWindow("AnalysisPointWindowView", analysisPointViewModel);
                _openPopups.Add(window);
                analysisPointViewModel.TransmissionLossViewModel.Window = window;
                analysisPointViewModel.TransmissionLossViewModel.SaveFileService = Globals.SaveFileService;
            }
            catch (InvalidOperationException ex)
            {
                Globals.MessageBoxService.ShowError(string.Format("Error displaying analysis point: {0}", ex.Message));
            }
        }
    }
}