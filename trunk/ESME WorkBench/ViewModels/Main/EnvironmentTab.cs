using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Threading;
using Cinch;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Metadata;
using ESME.Overlay;
using ESME.TransmissionLoss.CASS;
using ESME.Views.EnvironmentBuilder;
using ESME.Views.Locations;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Map;
using HRC.Navigation;
using HRC.Utility;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        void InitializeEnvironmentTab()
        {
            if (ESME.Globals.AppSettings.ScenarioDataDirectory == null) return;
            SimAreaCSV = SimAreaCSV.ReadCSV(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"));
            //if (_simAreaCSVWatcher != null) _simAreaCSVWatcher.Dispose();
            //_simAreaCSVWatcher = new FileSystemWatcher(Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv");
            //_simAreaCSVWatcher.Changed += (s, e) => SimAreaCSV.ReadCSV(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"));
            //_simAreaCSVWatcher.EnableRaisingEvents = true;
        }

        //FileSystemWatcher _simAreaCSVWatcher;

        [MediatorMessageSink(MediatorMessage.AllViewModelsAreReady)]
        void AllViewModelsAreReady(bool allViewModelsAreReady)
        {
            Console.WriteLine("All view models are ready!");
            _dispatcher.InvokeIfRequired(DisplayRangeComplex, DispatcherPriority.Normal);
            _dispatcher.InvokeIfRequired(DisplayBathymetry, DispatcherPriority.Normal);
            _dispatcher.InvokeIfRequired(DisplayOverlay, DispatcherPriority.Normal);
        }

        T FindMapLayer<T>(LayerType layerType, string layerName) where T : class
        {
            return MapLayers.Where(layer => layer.LayerType == layerType).Where(layer => layer.Name == layerName).FirstOrDefault() as T;
        }

        #region ViewActivatedCommand
        public SimpleCommand<object, object> ViewActivatedCommand
        {
            get { return _viewActivated ?? (_viewActivated = new SimpleCommand<object, object>(delegate { ViewActivatedHandler(); })); }
        }

        SimpleCommand<object, object> _viewActivated;

        void ViewActivatedHandler()
        {
            if (_viewIsActivated) return;
            Console.WriteLine("The window has been activated!");
            _viewIsActivated = true;
            _dispatcher.InvokeIfRequired(DisplayRangeComplex, DispatcherPriority.Normal);
            _dispatcher.InvokeIfRequired(DisplayBathymetry, DispatcherPriority.Normal);
            _dispatcher.InvokeIfRequired(DisplayOverlay, DispatcherPriority.Normal);
        }
        bool _viewIsActivated;
        #endregion

        #region public bool EnvironmentTabIsActive { get; set; }

        public bool EnvironmentTabIsActive
        {
            get { return _environmentTabIsActive; }
            set
            {
                if (_environmentTabIsActive == value) return;
                _environmentTabIsActive = value;
                NotifyPropertyChanged(EnvironmentTabIsActiveChangedEventArgs);
                if (_environmentTabIsActive)
                {
                    MapLayers = new ObservableCollection<MapLayerViewModel>();
                }
                else
                {
                    MapLayers = null;
                }
            }
        }

        static readonly PropertyChangedEventArgs EnvironmentTabIsActiveChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.EnvironmentTabIsActive);
        bool _environmentTabIsActive;

        #endregion

        #region public ObservableCollection<MapLayerViewModel> MapLayers { get; set; }

        static readonly PropertyChangedEventArgs MapLayersChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.MapLayers);
        ObservableCollection<MapLayerViewModel> _mapLayers;

        public ObservableCollection<MapLayerViewModel> MapLayers
        {
            get { return _mapLayers; }
            set
            {
                if (_mapLayers == value) return;
                if (_mapLayers != null) _mapLayers.CollectionChanged -= MapLayersCollectionChanged;
                _mapLayers = value;
                if (_mapLayers != null) _mapLayers.CollectionChanged += MapLayersCollectionChanged;
                NotifyPropertyChanged(MapLayersChangedEventArgs);
                MapLayerViewModel.Layers = _mapLayers;
                MediatorMessage.Send(MediatorMessage.SetMapLayers, _mapLayers);
            }
        }

        void MapLayersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null) { }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    break;
            }
            NotifyPropertyChanged(MapLayersChangedEventArgs);
        }

        #endregion

        #region Range Complex ribbon group

        #region public SimAreaCSV SimAreaCSV { get; set; }

        public SimAreaCSV SimAreaCSV
        {
            get { return _simAreaCSV; }
            set
            {
                if (_simAreaCSV == value) return;
                _simAreaCSV = value;
                NotifyPropertyChanged(SimAreaCSVChangedEventArgs);
                SelectedRangeComplex = _simAreaCSV[0];
            }
        }

        static readonly PropertyChangedEventArgs SimAreaCSVChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SimAreaCSV);
        SimAreaCSV _simAreaCSV;

        #endregion

        #region public SimAreaDescriptor SelectedRangeComplex { get; set; }

        public SimAreaDescriptor SelectedRangeComplex
        {
            get { return _selectedRangeComplex; }
            set
            {
                if ((_selectedRangeComplex == value) || (!IsRangeComplexListReady)) return;
                _selectedRangeComplex = value;
                IsRangeComplexSelected = _selectedRangeComplex != null;
                if ((_selectedRangeComplex != null) && (_selectedRangeComplex != _lastNonNullSimArea))
                {
                    _lastNonNullSimArea = _selectedRangeComplex;
                    SelectedRangeComplexInfo = string.Format("Name: {0}\nReference Point: ({1}, {2})\nHeight: {3}\nGeoid Separation: {4}\nOps Limit: {5}\nSim Limit: {6}",
                                                             _selectedRangeComplex.Name, Math.Round(_selectedRangeComplex.Latitude, 5), Math.Round(_selectedRangeComplex.Longitude, 5),
                                                             _selectedRangeComplex.Height, _selectedRangeComplex.GeoidSeparation, SelectedRangeComplex.OpsLimitFile,
                                                             SelectedRangeComplex.SimLimitFile);
                    Console.WriteLine("Range complex {0} is selected!", _selectedRangeComplex.Name);
                    //if (_selectedRangeComplex.Name == "Jacksonville") System.Diagnostics.Debugger.Break();

                    if ((_bw1 != null) && (_bw1.IsBusy)) _bw1.CancelAsync();
                    _bw1 = new BackgroundWorker {WorkerSupportsCancellation = true};
                    _bw1.DoWork += (s, e) =>
                    {
                        NotifyPropertyChanged(IsRangeComplexListReadyChangedEventArgs);
                        NAEMOOverlayDescriptors = new NAEMOOverlayDescriptors(_selectedRangeComplex.Name, _bw1);
                    };
                    _bw1.RunWorkerCompleted += (s, e) =>
                    {
                        _bw1 = null;
                        CommandManager.InvalidateRequerySuggested();
                        NotifyPropertyChanged(IsRangeComplexListReadyChangedEventArgs);
                    };
                    _bw1.RunWorkerAsync();

                    if ((_bw2 != null) && (_bw2.IsBusy)) _bw2.CancelAsync();
                    _bw2 = new BackgroundWorker {WorkerSupportsCancellation = true};
                    _bw2.DoWork += (s, e) => NAEMOBathymetryDescriptors = new NAEMOBathymetryDescriptors(_selectedRangeComplex.Name, _bw2);
                    _bw2.RunWorkerCompleted += (s, e) =>
                    {
                        _bw2 = null;
                        CommandManager.InvalidateRequerySuggested();
                        NotifyPropertyChanged(IsRangeComplexListReadyChangedEventArgs);
                    };
                    _bw2.RunWorkerAsync();

                    if ((_bw3 != null) && (_bw3.IsBusy)) _bw3.CancelAsync();
                    _bw3 = new BackgroundWorker {WorkerSupportsCancellation = true};
                    _bw3.DoWork += (s, e) => NAEMOEnvironmentDescriptors = new NAEMOEnvironmentDescriptors(_selectedRangeComplex.Name, _bw3);
                    _bw3.RunWorkerCompleted += (s, e) =>
                    {
                        _bw3 = null;
                        CommandManager.InvalidateRequerySuggested();
                        NotifyPropertyChanged(IsRangeComplexListReadyChangedEventArgs);
                    };
                    _bw3.RunWorkerAsync();
                }
                else
                {
                    NAEMOOverlayDescriptors = null;
                    NAEMOBathymetryDescriptors = null;
                    NAEMOEnvironmentDescriptors = null;
                }
                _dispatcher.InvokeIfRequired(DisplayRangeComplex, DispatcherPriority.Normal);
            }
        }

        static readonly PropertyChangedEventArgs SelectedSimAreaChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedRangeComplex);
        SimAreaDescriptor _selectedRangeComplex;
        SimAreaDescriptor _lastNonNullSimArea;

        void DisplayWorldMap()
        {
            if (MapLayers != null) return;
            var appPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
            MapLayers = new ObservableCollection<MapLayerViewModel>
            {
                    new ShapefileMapLayer
                    {
                            LineColor = Colors.Beige,
                            AreaStyle = AreaStyles.Country2,
                            CanBeRemoved = false,
                            CanBeReordered = true,
                            CanChangeAreaColor = true,
                            CanChangeLineColor = true,
                            ShapefileName = Path.Combine(appPath, @"Sample GIS Data\Countries02.shp"),
                            Name = "Base Map",
                            LayerType = LayerType.BaseMap,
                    },
            };
        }

        void DisplayRangeComplex()
        {
            if ((_selectedRangeComplex == null) || (!_allViewModelsAreReady) || (!_viewIsActivated)) return;
            _dispatcher.InvokeIfRequired(DisplayWorldMap, DispatcherPriority.Normal);
            var opAreaOverlayFilename = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, _selectedRangeComplex.Name, "Areas", _selectedRangeComplex.OpsLimitFile);
            var simAreaOverlayFilename = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, _selectedRangeComplex.Name, "Areas", _selectedRangeComplex.SimLimitFile);
            var opsLimit = new OverlayFile(opAreaOverlayFilename);
            var north = (float)opsLimit.Shapes[0].BoundingBox.Bottom + 3;
            var west = (float)opsLimit.Shapes[0].BoundingBox.Left - 3;
            var south = (float)opsLimit.Shapes[0].BoundingBox.Top - 3;
            var east = (float)opsLimit.Shapes[0].BoundingBox.Right + 3;
            var mapExtent = new RectangleShape(west, north, east, south);
            MediatorMessage.Send(MediatorMessage.SetCurrentExtent, mapExtent);
            var opAreaLayer = FindMapLayer<OverlayShapeMapLayer>(LayerType.OverlayFile, "Op Area") ?? new OverlayShapeMapLayer
                {
                        Name = "Op Area",
                        CanBeRemoved = true,
                        CanBeReordered = true,
                        CanChangeAreaColor = false,
                        CanChangeLineColor = true,
                        LineWidth = 1,
                        LayerType = LayerType.OverlayFile,
                };
            opAreaLayer.Clear();
            var opAreaOverlay = new OverlayFile(opAreaOverlayFilename);
            foreach (var shape in opAreaOverlay.Shapes)
                opAreaLayer.Add(shape);
            opAreaLayer.Done();
            if (MapLayers.IndexOf(opAreaLayer) == -1) MapLayers.Add(opAreaLayer);

            if (simAreaOverlayFilename != opAreaOverlayFilename)
            {
                var simAreaLayer = FindMapLayer<OverlayShapeMapLayer>(LayerType.OverlayFile, "Sim Area") ?? new OverlayShapeMapLayer
                {
                    Name = "Sim Area",
                    CanBeRemoved = true,
                    CanBeReordered = true,
                    CanChangeAreaColor = false,
                    CanChangeLineColor = true,
                    LineWidth = 1,
                    LayerType = LayerType.OverlayFile,
                };
                simAreaLayer.Clear();
                var simAreaOverlay = new OverlayFile(opAreaOverlayFilename);
                foreach (var shape in simAreaOverlay.Shapes)
                    simAreaLayer.Add(shape);
                simAreaLayer.Done();
                if (MapLayers.IndexOf(simAreaLayer) == -1) MapLayers.Add(simAreaLayer);
            }
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }

        #endregion

        #region public string SelectedRangeComplexInfo { get; set; }

        public string SelectedRangeComplexInfo
        {
            get { return _selectedRangeComplexInfo; }
            set
            {
                if (_selectedRangeComplexInfo == value) return;
                _selectedRangeComplexInfo = value;
                NotifyPropertyChanged(SelectedRangeComplexInfoChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedRangeComplexInfoChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedRangeComplexInfo);
        string _selectedRangeComplexInfo;

        #endregion

        #region public bool IsRangeComplexListReady { get; set; }

        public bool IsRangeComplexListReady
        {
            get
            {
                var result = ((_bw1 == null) && (_bw2 == null) && (_bw3 == null));
                if (_isRangeComplexListReady != result)
                {
                    _isRangeComplexListReady = result;
                    if (_isRangeComplexListReady) NotifyPropertyChanged(SelectedSimAreaChangedEventArgs);
                    NotifyPropertyChanged(IsOverlayListReadyChangedEventArgs);
                    NotifyPropertyChanged(IsBathymetryListReadyChangedEventArgs);
                    NotifyPropertyChanged(IsEnvironmentListReadyChangedEventArgs);
                }
                return result;
            }
        }

        bool _isRangeComplexListReady;
        static readonly PropertyChangedEventArgs IsRangeComplexListReadyChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsRangeComplexListReady);

        #endregion

        #region public bool IsRangeComplexSelected { get; set; }

        public bool IsRangeComplexSelected
        {
            get { return _isRangeComplexSelected; }
            set
            {
                if (_isRangeComplexSelected == value) return;
                _isRangeComplexSelected = value;

                NotifyPropertyChanged(RangeComplexIsSelectedChangedEventArgs);
            }
        }

        BackgroundWorker _bw1, _bw2, _bw3;
        static readonly PropertyChangedEventArgs RangeComplexIsSelectedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsRangeComplexSelected);
        bool _isRangeComplexSelected;

        #endregion

        #region NewRangeComplexCommand

        public SimpleCommand<object, object> NewRangeComplexCommand
        {
            get
            {
                return _newLocation ??
                       (_newLocation =
                        new SimpleCommand<object, object>(delegate { NewLocationHandler(); }));
            }
        }

        private SimpleCommand<object, object> _newLocation;

        void NewLocationHandler()
        {
            var vm = new NewRangeComplexViewModel(Globals.AppSettings);
            var result = _visualizerService.ShowDialog("NewRangeComplexView", vm);
            if ((result.HasValue) && (result.Value))
            {
                SimAreaCSV = SimAreaCSV.ReadCSV(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"));
                SelectedRangeComplex = SimAreaCSV[vm.LocationName];
            }
        }
        #endregion

        #endregion

        #region Overlay ribbon group

        #region public NAEMOOverlayDescriptors NAEMOOverlayDescriptors { get; set; }

        public NAEMOOverlayDescriptors NAEMOOverlayDescriptors
        {
            get { return _naemoOverlayDescriptors; }
            set
            {
                if (_naemoOverlayDescriptors == value) return;
                _naemoOverlayDescriptors = value;
                NotifyPropertyChanged(OverlayFilesChangedEventArgs);
                NotifyPropertyChanged(IsOverlayListReadyChangedEventArgs);
                SelectedOverlayDescriptor = NAEMOOverlayDescriptors != null && NAEMOOverlayDescriptors.Count > 0 ? NAEMOOverlayDescriptors[0].Value : null;
            }
        }

        static readonly PropertyChangedEventArgs OverlayFilesChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.NAEMOOverlayDescriptors);
        NAEMOOverlayDescriptors _naemoOverlayDescriptors;

        #endregion

        #region public NAEMOOverlayDescriptor SelectedOverlayDescriptor { get; set; }

        public NAEMOOverlayDescriptor SelectedOverlayDescriptor
        {
            get { return _selectedOverlayDescriptor; }
            set
            {
                _selectedOverlayDescriptor = value;
                NotifyPropertyChanged(SelectedOverlayDescriptorChangedEventArgs);
                if (_selectedOverlayDescriptor != null) SelectedOverlayInfo = string.Format("Name: {0}\nBuffer: {1}\nSource Overlay: {2}", Path.GetFileNameWithoutExtension(_selectedOverlayDescriptor.DataFilename), _selectedOverlayDescriptor.Metadata.BufferZoneSize > 0 ? _selectedOverlayDescriptor.Metadata.BufferZoneSize + " km" : "[N/A]", _selectedOverlayDescriptor.Metadata.OverlayFilename ?? "[Unknown]");
                IsOverlayFileSelected = _selectedOverlayDescriptor != null;
                _dispatcher.InvokeIfRequired(DisplayOverlay, DispatcherPriority.Normal);
            }
        }

        static readonly PropertyChangedEventArgs SelectedOverlayDescriptorChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedOverlayDescriptor);
        NAEMOOverlayDescriptor _selectedOverlayDescriptor;

        void DisplayOverlay()
        {
            if ((_selectedOverlayDescriptor == null) || (!_allViewModelsAreReady) || (!_viewIsActivated)) return;
            _dispatcher.InvokeIfRequired(DisplayWorldMap, DispatcherPriority.Normal);
            var overlayFilename = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, _selectedRangeComplex.Name, "Areas", Path.GetFileNameWithoutExtension(_selectedOverlayDescriptor.DataFilename) + ".ovr");
            var overlayLayer = FindMapLayer<OverlayShapeMapLayer>(LayerType.OverlayFile, "Overlay") ?? new OverlayShapeMapLayer
            {
                Name = "Overlay",
                CanBeRemoved = true,
                CanBeReordered = true,
                CanChangeAreaColor = false,
                CanChangeLineColor = true,
                LineWidth = 1,
                LayerType = LayerType.OverlayFile,
            };
            overlayLayer.Clear();
            var overlay = new OverlayFile(overlayFilename);
            foreach (var shape in overlay.Shapes)
                overlayLayer.Add(shape);
            overlayLayer.Done();
            if (MapLayers.IndexOf(overlayLayer) == -1) MapLayers.Add(overlayLayer);
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion

        #region public string SelectedOverlayInfo { get; set; }

        public string SelectedOverlayInfo
        {
            get { return _selectedOverlayInfo; }
            set
            {
                if (_selectedOverlayInfo == value) return;
                _selectedOverlayInfo = value;
                NotifyPropertyChanged(SelectedOverlayNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedOverlayNameChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedOverlayInfo);
        string _selectedOverlayInfo;

        #endregion

        #region public bool IsOverlayListReady { get; set; }

        public bool IsOverlayListReady
        {
            get { return (_naemoOverlayDescriptors != null) && IsRangeComplexListReady; }
        }

        static readonly PropertyChangedEventArgs IsOverlayListReadyChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsOverlayListReady);

        #endregion

        #region public bool IsOverlayFileSelected { get; set; }

        public bool IsOverlayFileSelected
        {
            get { return _isOverlayFileSelected; }
            set
            {
                if (_isOverlayFileSelected == value) return;
                _isOverlayFileSelected = value;
                NotifyPropertyChanged(IsOverlayFileSelectedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsOverlayFileSelectedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsOverlayFileSelected);
        bool _isOverlayFileSelected;

        #endregion

        #region NewOverlayCommand

        public SimpleCommand<object, object> NewOverlayCommand
        {
            get
            {
                return _newOverlay ?? (_newOverlay = new SimpleCommand<object, object>(delegate { return IsRangeComplexSelected; }, delegate { NewOverlayHandler(); }));
            }
        }

        private SimpleCommand<object, object> _newOverlay;

        void NewOverlayHandler()
        {
            var vm = new NewOverlayViewModel(Globals.AppSettings, SelectedRangeComplex.Name);
            var result = _visualizerService.ShowDialog("NewOverlayView", vm);
            if ((result.HasValue) && (result.Value))
            {
                NAEMOOverlayDescriptors = new NAEMOOverlayDescriptors(SelectedRangeComplex.Name);
            }
        }

        #endregion

        #region ExpandOverlayCommand
        public SimpleCommand<object, object> ExpandOverlayCommand
        {
            get { return _expandOverlay ?? (_expandOverlay = new SimpleCommand<object, object>(delegate { return IsExpandOverlayCommandEnabled; }, delegate { ExpandOverlayHandler(); })); }
        }

        SimpleCommand<object, object> _expandOverlay;

        bool IsExpandOverlayCommandEnabled
        {
            get { return IsOverlayFileSelected; }
        }

        void ExpandOverlayHandler()
        {
            var vm = new OverlayExpandViewModel(SelectedOverlayDescriptor.Metadata);
            var result = _visualizerService.ShowDialog("OverlayExpandView", vm);
            if ((result.HasValue) && (result.Value))
            {
                //vm.BufferZoneSize
                var curOverlay = SelectedOverlayDescriptor.Data;
                var limits = (Limits)(new GeoRect(curOverlay.Shapes[0].BoundingBox));
                var expandedLimits = limits.CreateExpandedLimit(vm.BufferZoneSize);  //in km.
                var geoRect = new GeoRect(expandedLimits.GeoPointList);
                var overlayFileName = string.Format("{0}_{1}km.ovr", Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename), vm.BufferZoneSize);
                var metadataFileName = string.Format("{0}_{1}km.xml", Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename), vm.BufferZoneSize);
                var overlayPath = Path.Combine(Path.GetDirectoryName(SelectedOverlayDescriptor.DataFilename), overlayFileName);
                var metadataPath = Path.Combine(Path.GetDirectoryName(SelectedOverlayDescriptor.DataFilename), metadataFileName);
                using (var writer = new StreamWriter(overlayPath))
                {
                    writer.WriteLine("navigation");
                    writer.WriteLine("green");
                    writer.WriteLine("solid");
                    writer.WriteLine("move");
                    writer.WriteLine("{0:0.0000}  {1:0.0000}", geoRect.NorthWest.Latitude, geoRect.NorthWest.Longitude);
                    writer.WriteLine("lines");
                    writer.WriteLine("{0:0.0000}  {1:0.0000}", geoRect.NorthEast.Latitude, geoRect.NorthEast.Longitude);
                    writer.WriteLine("{0:0.0000}  {1:0.0000}", geoRect.SouthEast.Latitude, geoRect.SouthEast.Longitude);
                    writer.WriteLine("{0:0.0000}  {1:0.0000}", geoRect.SouthWest.Latitude, geoRect.SouthWest.Longitude);
                    writer.WriteLine("{0:0.0000}  {1:0.0000}", geoRect.NorthWest.Latitude, geoRect.NorthWest.Longitude);
                }

                var metadata = new NAEMOOverlayMetadata
                {
                    Bounds = geoRect,
                    BufferZoneSize = vm.BufferZoneSize,
                    Filename = metadataPath,
                    OverlayFilename = Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename),
                };
                metadata.Save();

                NAEMOOverlayDescriptors = new NAEMOOverlayDescriptors(_selectedRangeComplex.Name);
                SelectedOverlayDescriptor = (NAEMOOverlayDescriptor)NAEMOOverlayDescriptors[Path.GetFileNameWithoutExtension(overlayFileName)];

            }
        }

        #endregion

        #region OverlayPropertiesCommand
        public SimpleCommand<object, object> OverlayPropertiesCommand
        {
            get { return _overlayProperties ?? (_overlayProperties = new SimpleCommand<object, object>(delegate { return IsOverlayFileSelected; }, delegate { OverlayPropertiesHandler(); })); }
        }

        SimpleCommand<object, object> _overlayProperties;

        void OverlayPropertiesHandler()
        {
            var vm = new MetadataPropertiesViewModel(SelectedOverlayDescriptor.Metadata);
            var result = _visualizerService.ShowDialog("MetadataPropertiesView", vm);
            if ((result.HasValue) && (result.Value))
            {
                SelectedOverlayDescriptor = _selectedOverlayDescriptor;
            }
        }
        #endregion

        #endregion

        #region Bathymetry ribbon group

        #region public NAEMOBathymetryDescriptors NAEMOBathymetryDescriptors { get; set; }

        public NAEMOBathymetryDescriptors NAEMOBathymetryDescriptors
        {
            get { return _naemoBathymetryDescriptors; }
            set
            {
                if (_naemoBathymetryDescriptors == value) return;
                _naemoBathymetryDescriptors = value;
                NotifyPropertyChanged(NAEMOBathymetryDescriptorsChangedEventArgs);
                NotifyPropertyChanged(IsBathymetryListReadyChangedEventArgs);
                SelectedBathymetryDescriptor = NAEMOBathymetryDescriptors != null && NAEMOBathymetryDescriptors.Count > 0 ? NAEMOBathymetryDescriptors[0].Value : null;
            }
        }

        static readonly PropertyChangedEventArgs NAEMOBathymetryDescriptorsChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.NAEMOBathymetryDescriptors);
        NAEMOBathymetryDescriptors _naemoBathymetryDescriptors;

        #endregion

        #region public NAEMOBathymetryDescriptor SelectedBathymetryDescriptor { get; set; }

        public NAEMOBathymetryDescriptor SelectedBathymetryDescriptor
        {
            get { return _selectedBathymetryDescriptor; }
            set
            {
                _selectedBathymetryDescriptor = value;
                NotifyPropertyChanged(SelectedBathymetryDescriptorChangedEventArgs);
                if (_selectedBathymetryDescriptor != null) SelectedBathymetryInfo = string.Format("Name: {0}\nResolution: {1} min\nSource Overlay: {2}", Path.GetFileNameWithoutExtension(_selectedBathymetryDescriptor.DataFilename), _selectedBathymetryDescriptor.Metadata.Resolution, _selectedBathymetryDescriptor.Metadata.OverlayFilename ?? "[Unknown]");
                IsBathymetryFileSelected = _selectedBathymetryDescriptor != null;
                _dispatcher.InvokeIfRequired(DisplayBathymetry, DispatcherPriority.Normal);
            }
        }

        static readonly PropertyChangedEventArgs SelectedBathymetryDescriptorChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedBathymetryDescriptor);
        NAEMOBathymetryDescriptor _selectedBathymetryDescriptor;
        void DisplayBathymetry()
        {
            if ((_selectedBathymetryDescriptor == null) || (!_allViewModelsAreReady) || (!_viewIsActivated)) return;
            _dispatcher.InvokeIfRequired(DisplayWorldMap, DispatcherPriority.Normal);
            var bathyBitmapLayer = FindMapLayer<RasterMapLayer>(LayerType.BathymetryRaster, "Bathymetry") ?? new RasterMapLayer
            {
                Name = "Bathymetry",
                CanBeReordered = true,
                CanChangeLineColor = false,
                CanChangeLineWidth = false,
                CanBeRemoved = false,
                LayerType = LayerType.BathymetryRaster,
            };
            bathyBitmapLayer.North = (float)_selectedBathymetryDescriptor.Metadata.Bounds.North;
            bathyBitmapLayer.South = (float)_selectedBathymetryDescriptor.Metadata.Bounds.South;
            bathyBitmapLayer.East = (float)_selectedBathymetryDescriptor.Metadata.Bounds.East;
            bathyBitmapLayer.West = (float)_selectedBathymetryDescriptor.Metadata.Bounds.West;
            bathyBitmapLayer.RasterFilename = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, SelectedRangeComplex.Name, "Images",
                                                           Path.GetFileNameWithoutExtension(_selectedBathymetryDescriptor.DataFilename) + ".bmp");
            if (MapLayers.IndexOf(bathyBitmapLayer) == -1) MapLayers.Add(bathyBitmapLayer);
            MediatorMessage.Send(MediatorMessage.MoveLayerToBottom, bathyBitmapLayer);
        }

        #endregion

        #region public string SelectedBathymetryInfo { get; set; }

        public string SelectedBathymetryInfo
        {
            get { return _selectedBathymetryInfo; }
            set
            {
                if (_selectedBathymetryInfo == value) return;
                _selectedBathymetryInfo = value;
                NotifyPropertyChanged(SelectedBathymetryNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedBathymetryNameChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedBathymetryInfo);
        string _selectedBathymetryInfo;

        #endregion

        #region public bool IsBathymetryListReady { get; set; }

        public bool IsBathymetryListReady
        {
            get { return (_naemoBathymetryDescriptors != null) && IsRangeComplexListReady; }
        }

        static readonly PropertyChangedEventArgs IsBathymetryListReadyChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsBathymetryListReady);
        
        #endregion

        #region public bool IsBathymetryFileSelected { get; set; }

        public bool IsBathymetryFileSelected
        {
            get { return _isBathymetryFileSelected; }
            set
            {
                if (_isBathymetryFileSelected == value) return;
                _isBathymetryFileSelected = value;
                NotifyPropertyChanged(IsBathymetryFileSelectedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsBathymetryFileSelectedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsBathymetryFileSelected);
        bool _isBathymetryFileSelected;

        #endregion

        #region NewBathymetryCommand
        public SimpleCommand<object, object> NewBathymetryCommand
        {
            get { return _newBathymetry ?? (_newBathymetry = new SimpleCommand<object, object>(delegate { return IsRangeComplexSelected; }, delegate { NewBathymetryHandler(); })); }
        }

        SimpleCommand<object, object> _newBathymetry;

        void NewBathymetryHandler()
        {
            var vm = new BathymetryExtractionViewModel(Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename));
            var result = _visualizerService.ShowDialog("BathymetryExtractionView", vm);
            if ((result.HasValue) && (result.Value))
            {
                NAEMOBathymetryDescriptors = null;
                CommandManager.InvalidateRequerySuggested();
                var extractionArea = new GeoRect(SelectedOverlayDescriptor.Data.Shapes[0].BoundingBox);
                var tempPath = Path.GetTempPath().Remove(Path.GetTempPath().Length - 1);
                if (!Directory.Exists(tempPath)) Directory.CreateDirectory(tempPath);
                var destinationPath = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, SelectedRangeComplex.Name, "Bathymetry", vm.BathymetryName + ".txt");
                var naemoBathymetryExporter = new CASSBackgroundExporter
                {
                    WorkerSupportsCancellation = false,
                    ExtractionArea = extractionArea,
                    NAVOConfiguration = Globals.AppSettings.NAVOConfiguration,
                    DestinationPath = destinationPath,
                    UseExpandedExtractionArea = false,
                    TaskName = "Export NAEMO bathymetry",
                };
                var bathymetryExtractor = new DBDBBackgroundExtractor
                {
                    WorkerSupportsCancellation = false,
                    ExtractionArea = extractionArea,
                    NAVOConfiguration = Globals.AppSettings.NAVOConfiguration,
                    DestinationPath = tempPath,
                    UseExpandedExtractionArea = false,
                    SelectedResolution = vm.SelectedResolution,
                    SaveAsFilename = (Path.Combine(tempPath, vm.BathymetryName + ".txt")),
                    TaskName = "Bathymetry data extraction",
                };
                bathymetryExtractor.RunWorkerCompleted += (s, e) =>
                {
                    var bathymetry = ((DBDBBackgroundExtractor)s).Bathymetry;
                    naemoBathymetryExporter.Bathymetry = bathymetry;
                };
                BackgroundTaskAggregator = new BackgroundTaskAggregator();
                BackgroundTaskAggregator.BackgroundTasks.Add(bathymetryExtractor);
                BackgroundTaskAggregator.BackgroundTasks.Add(naemoBathymetryExporter);
                BackgroundTaskAggregator.TaskName = "Bathymetry data extraction";
                BackgroundTaskAggregator.Start();
                BackgroundTaskAggregator.RunWorkerCompleted += (s, e) =>
                {
                    Bathymetry bathymetry;
                    var metadata = NAEMOBathymetryMetadata.FromBathymetryFile(destinationPath, out bathymetry);
                    metadata.OverlayFilename = Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename);
                    metadata.Save();
                    var bw1 = new BackgroundWorker();
                    bw1.DoWork += (s1, e1) => NAEMOBathymetryDescriptors = new NAEMOBathymetryDescriptors(_selectedRangeComplex.Name);
                    bw1.RunWorkerCompleted += (s1, e1) =>
                    {
                        SelectedBathymetryDescriptor = NAEMOBathymetryDescriptors.Find(item => item.Key == Path.GetFileNameWithoutExtension(destinationPath)).Value;
                        CommandManager.InvalidateRequerySuggested();
                    };
                    bw1.RunWorkerAsync();
                };
            }
        }

        #endregion

        #region BathymetryPropertiesCommand
        public SimpleCommand<object, object> BathymetryPropertiesCommand
        {
            get { return _bathymetryProperties ?? (_bathymetryProperties = new SimpleCommand<object, object>(delegate { return IsBathymetryFileSelected; }, delegate { BathymetryPropertiesHandler(); })); }
        }

        SimpleCommand<object, object> _bathymetryProperties;

        void BathymetryPropertiesHandler()
        {
            var vm = new MetadataPropertiesViewModel(null, SelectedBathymetryDescriptor.Metadata);
            var result = _visualizerService.ShowDialog("MetadataPropertiesView", vm);
            if ((result.HasValue) && (result.Value))
            {
                SelectedBathymetryDescriptor = _selectedBathymetryDescriptor;
            }

        }
        #endregion

        #endregion

        #region Environment ribbon group

        #region public NAEMOEnvironmentDescriptors NAEMOEnvironmentDescriptors { get; set; }

        public NAEMOEnvironmentDescriptors NAEMOEnvironmentDescriptors
        {
            get { return _naemoEnvironmentDescriptors; }
            set
            {
                if (_naemoEnvironmentDescriptors == value) return;
                _naemoEnvironmentDescriptors = value;
                NotifyPropertyChanged(NAEMOEnvironmentDescriptorsChangedEventArgs);
                NotifyPropertyChanged(IsEnvironmentListReadyChangedEventArgs);
                SelectedEnvironmentDescriptor = NAEMOEnvironmentDescriptors != null && NAEMOEnvironmentDescriptors.Count > 0 ? NAEMOEnvironmentDescriptors[0].Value : null;
            }
        }

        static readonly PropertyChangedEventArgs NAEMOEnvironmentDescriptorsChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.NAEMOEnvironmentDescriptors);
        NAEMOEnvironmentDescriptors _naemoEnvironmentDescriptors;

        #endregion

        #region public NAEMOEnvironmentDescriptor SelectedEnvironmentDescriptor { get; set; }

        public NAEMOEnvironmentDescriptor SelectedEnvironmentDescriptor
        {
            get { return _selectedEnvironmentDescriptor; }
            set
            {
                _selectedEnvironmentDescriptor = value;
                NotifyPropertyChanged(SelectedEnvironmentDescriptorChangedEventArgs);
                if (_selectedEnvironmentDescriptor != null) SelectedEnvironmentInfo = string.Format("Name: {0}\nTime Period: {1}\nSource Overlay: {2}", Path.GetFileNameWithoutExtension(_selectedEnvironmentDescriptor.DataFilename), _selectedEnvironmentDescriptor.Metadata.TimePeriod, _selectedEnvironmentDescriptor.Metadata.OverlayFilename ?? "[Unknown]");
                IsEnvironmentFileSelected = _selectedEnvironmentDescriptor != null;
            }
        }

        static readonly PropertyChangedEventArgs SelectedEnvironmentDescriptorChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedEnvironmentDescriptor);
        NAEMOEnvironmentDescriptor _selectedEnvironmentDescriptor;

        #endregion

        #region public string SelectedEnvironmentInfo { get; set; }

        public string SelectedEnvironmentInfo
        {
            get { return _selectedEnvironmentInfo; }
            set
            {
                if (_selectedEnvironmentInfo == value) return;
                _selectedEnvironmentInfo = value;
                NotifyPropertyChanged(SelectedEnvironmentNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedEnvironmentNameChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedEnvironmentInfo);
        string _selectedEnvironmentInfo;

        #endregion

        #region public bool IsEnvironmentFileSelected { get; set; }

        public bool IsEnvironmentFileSelected
        {
            get { return _isEnvironmentFileSelected; }
            set
            {
                if (_isEnvironmentFileSelected == value) return;
                _isEnvironmentFileSelected = value;
                NotifyPropertyChanged(IsEnvironmentFileSelectedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsEnvironmentFileSelectedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsEnvironmentFileSelected);
        bool _isEnvironmentFileSelected;

        #endregion

        #region public bool IsEnvironmentListReady { get; set; }

        public bool IsEnvironmentListReady
        {
            get { return (_naemoEnvironmentDescriptors != null) && IsRangeComplexListReady; }
        }

        static readonly PropertyChangedEventArgs IsEnvironmentListReadyChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsEnvironmentListReady);

        #endregion

        #region NewEnvironmentCommand
        public SimpleCommand<object, object> NewEnvironmentCommand
        {
            get
            {
                return _newEnvironment ??
                       (_newEnvironment =
                        new SimpleCommand<object, object>(delegate { return IsRangeComplexSelected; },
                                                          delegate { NewEnvironmentHandler(); }));
            }
        }

        SimpleCommand<object, object> _newEnvironment;

        void NewEnvironmentHandler()
        {
            var vm = new EnvironmentExtractionViewModel(Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename));
            var result = _visualizerService.ShowDialog("EnvironmentExtractionView", vm);
            if ((result.HasValue) && (result.Value))
            {
            }
        }

        #endregion

        #region EnvironmentPropertiesCommand
        public SimpleCommand<object, object> EnvironmentPropertiesCommand
        {
            get
            {
                return _environmentProperties ??
                       (_environmentProperties =
                        new SimpleCommand<object, object>(delegate { return IsEnvironmentFileSelected; },
                                                          delegate { EnvironmentPropertiesHandler(); }));
            }
        }

        SimpleCommand<object, object> _environmentProperties;

        void EnvironmentPropertiesHandler()
        {
            var vm = new MetadataPropertiesViewModel(null, null, SelectedEnvironmentDescriptor.Metadata);
            var result = _visualizerService.ShowDialog("MetadataPropertiesView", vm);
            if ((result.HasValue) && (result.Value))
            {
                SelectedEnvironmentDescriptor = _selectedEnvironmentDescriptor;
            }
        }
        #endregion

        #endregion
    }
}
