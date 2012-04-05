using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Windows.Media;
using System.Windows.Threading;
using Cinch;
using ESME;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Mapping;
using ESME.Model;
using ESME.NEMO.Overlay;
using ESME.Views.Locations;
using ESMEWorkbench.ViewModels.NAVO;
using HRC.Navigation;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkbench.ViewModels.Main
{
    public partial class MainViewModel
    {
        bool _once = true;
        [MediatorMessageSink(MediatorMessage.AllViewModelsAreReady)]
        public void AllViewModelsAreReady(bool allViewModelsAreReady)
        {
            if (!_once) return;
            _once = false;
            EnvironmentLayers = new Dictionary<EnvironmentDataType, MapLayerViewModel>
            {
                {EnvironmentDataType.Bathymetry, null}, 
                {EnvironmentDataType.SoundSpeed, null}, 
                {EnvironmentDataType.Wind, null}
            };
            Console.WriteLine("All view models are ready!");
            //WizardViewModel.LaunchWizardIfNeeded(_visualizerService);

            if (ESME.Globals.AppSettings != null)
                InitializeEnvironmentManager();
            else
                _messageBox.ShowError("The ESME Workbench is not fully configured, and may not function properly.  Please complete the configuration wizard or fill in the proper configuration details in the Application Options Configuration dialog.");

            _dispatcher.InvokeIfRequired(DisplayWorldMap, DispatcherPriority.Normal);
            AreAllViewModelsReady = true;
            //if (ESME.Globals.AppSettings.ScenarioDataDirectory == null) return;
            if (RangeComplexes != null) RangeComplexes.PropertyChanged += (s, e) =>
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

        public Dictionary<EnvironmentDataType, MapLayerViewModel> EnvironmentLayers { get; private set; }
        readonly List<MapLayerViewModel> _sedimentLayers = new List<MapLayerViewModel>();

        #region NewLocationCommand
        public SimpleCommand<object, object> NewLocationCommand
        {
            get { return _newLocation ?? (_newLocation = new SimpleCommand<object, object>(delegate { return IsNewLocationCommandEnabled; }, delegate { NewLocationHandler(); })); }
        }

        SimpleCommand<object, object> _newLocation;

        static bool IsNewLocationCommandEnabled
        {
            get { return true; }
        }

        void NewLocationHandler()
        {
            try
            {
                var vm = new NewLocationViewModel(_plugins, _database);
                var result = _visualizer.ShowDialog("NewLocationView", vm);
                if ((result.HasValue) && (result.Value))
                {
                    
                }
            }
            catch (Exception e) { _messageBox.ShowError(e.Message); }
        }
        #endregion

        #region public bool AreAllViewModelsReady { get; set; }

        public bool AreAllViewModelsReady
        {
            get { return _areAllViewModelsReady; }
            set
            {
                if (_areAllViewModelsReady == value) return;
                _areAllViewModelsReady = value;
                NotifyPropertyChanged(AreAllViewModelsReadyChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AreAllViewModelsReadyChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.AreAllViewModelsReady);
        bool _areAllViewModelsReady;

        #endregion

        #region ZoomToWorldMapCommand
        public SimpleCommand<object, object> ZoomToWorldMapCommand
        {
            get { return _zoomToWorldMap ?? (_zoomToWorldMap = new SimpleCommand<object, object>(delegate { ZoomToWorldMap(); })); }
        }

        SimpleCommand<object, object> _zoomToWorldMap;

        static void ZoomToWorldMap()
        {
            var mapExtent = new RectangleShape(-180, 90, 180, -90);
            MediatorMessage.Send(MediatorMessage.SetCurrentExtent, mapExtent);
        }
        #endregion

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
            //_dispatcher.InvokeIfRequired(DisplayRangeComplex, DispatcherPriority.Normal);
            //_dispatcher.InvokeIfRequired(DisplayBathymetry, DispatcherPriority.Normal);
            //_dispatcher.InvokeIfRequired(DisplayOverlay, DispatcherPriority.Normal);
            //_dispatcher.InvokeIfRequired(DisplayEnvironment, DispatcherPriority.Normal);
        }
        bool _viewIsActivated;
        #endregion

        public void HookLayerData()
        {
            RangeComplexes.HookEnvironment<Sediment>(EnvironmentDataType.Sediment, data =>
            {
                var result = data.Samples.GroupBy(sample => sample == null ? 0 : sample.Data.SampleValue);
                foreach (var sedimentType in result)
                {
                    if (sedimentType.Key == 0) continue;
                    var samplePoints = sedimentType.Select(samplePoint => new OverlayPoint(samplePoint)).ToList();
                    _dispatcher.InvokeInBackgroundIfRequired(() => _sedimentLayers.Add(CurrentMapLayers.DisplayOverlayShapes(string.Format("Sediment: {0}", SedimentTypes.Find(sedimentType.Key).Name),
                                                                                                                             LayerType.BottomType,
                                                                                                                             Colors.Transparent,
                                                                                                                             samplePoints,
                                                                                                                             0,
                                                                                                                             PointSymbolType.Diamond,
                                                                                                                             false,
                                                                                                                             null,
                                                                                                                             false)));
                }
            });
            RangeComplexes.HookEnvironment<Wind>(EnvironmentDataType.Wind, data =>
            {
                if (RangeComplexes.SelectedTimePeriod == TimePeriod.Invalid) return;
                var samplePoints = data[RangeComplexes.SelectedTimePeriod].EnvironmentData.Select(samplePoint => new OverlayPoint(samplePoint)).ToList();
                _dispatcher.InvokeInBackgroundIfRequired(() => EnvironmentLayers[EnvironmentDataType.Wind] = CurrentMapLayers.DisplayOverlayShapes("Wind", LayerType.WindSpeed, Colors.Transparent, samplePoints, 0, PointSymbolType.Diamond, false, null, false));
            });
            RangeComplexes.HookEnvironment<SoundSpeed>(EnvironmentDataType.SoundSpeed, data =>
            {
                if (RangeComplexes.SelectedTimePeriod == TimePeriod.Invalid) return;
                var samplePoints = data[RangeComplexes.SelectedTimePeriod].EnvironmentData.Select(samplePoint => new OverlayPoint(samplePoint)).ToList();
                _dispatcher.InvokeInBackgroundIfRequired(
                                                         () =>
                                                         EnvironmentLayers[EnvironmentDataType.SoundSpeed] =
                                                         CurrentMapLayers.DisplayOverlayShapes("Sound Speed",
                                                                                               LayerType.SoundSpeed,
                                                                                               Colors.Transparent,
                                                                                               samplePoints,
                                                                                               0,
                                                                                               PointSymbolType.Diamond,
                                                                                               false,
                                                                                               null,
                                                                                               false));
            });
        }

        public void ClearLayerData()
        {
            CurrentMapLayers.RemoveAll(layer => (layer.LayerType == LayerType.BottomType) && (layer.Name.StartsWith("Sediment: ")));
            if (EnvironmentLayers[EnvironmentDataType.Wind] != null) EnvironmentLayers[EnvironmentDataType.Wind].IsEnabled = false;
            if (EnvironmentLayers[EnvironmentDataType.SoundSpeed] != null) EnvironmentLayers[EnvironmentDataType.SoundSpeed].IsEnabled = false;
        }

        #region NewRangeComplexCommand
        public SimpleCommand<object, object> NewRangeComplexCommand
        {
            get
            {
                return _newRangeComplex ??
                       (_newRangeComplex =
                        new SimpleCommand<object, object>(delegate { NewRangeComplexHandler(); }));
            }
        }

        SimpleCommand<object, object> _newRangeComplex;

        void NewRangeComplexHandler()
        {
            try
            {
                var vm = new NewRangeComplexViewModel(ESME.Globals.AppSettings);
                var result = _visualizer.ShowDialog("NewRangeComplexView", vm);
                if ((result.HasValue) && (result.Value))
                {
                    var opAreaCoords = vm.ExistingOpAreaOverlayFilename != null ? new OverlayFile(vm.ExistingOpAreaOverlayFilename).Shapes[0].Geos : vm.NewOpAreaOverlayGeos;
                    var simAreaCoords = vm.ExistingSimAreaOverlayFilename != null ? new OverlayFile(vm.ExistingSimAreaOverlayFilename).Shapes[0].Geos : vm.NewSimAreaOverlayGeos;
                    RangeComplexes.CreateRangeComplex(vm.LocationName, vm.Height, vm.ReferencePointLatitude,
                                                      vm.ReferencePointLongitude, vm.GeoidSeparation,
                                                      opAreaCoords,
                                                      simAreaCoords);
                }
            }
            catch (Exception e) { _messageBox.ShowError(e.Message); }
        }
        #endregion

        #region ZoomToRangeComplexCommand
        public SimpleCommand<object, object> ZoomToRangeComplexCommand
        {
            get { return _zoomToRangeComplex ?? (_zoomToRangeComplex = new SimpleCommand<object, object>(delegate { return RangeComplexes != null && RangeComplexes.SelectedRangeComplex != null; }, delegate { ZoomToRangeComplex(); })); }
        }

        SimpleCommand<object, object> _zoomToRangeComplex;

        void ZoomToRangeComplex()
        {
            var bounds = RangeComplexes.SelectedRangeComplex.OpArea.OverlayShape.GeoRect;
            var north = (float)bounds.North + 3;
            var west = (float)bounds.West - 3;
            var south = (float)bounds.South - 3;
            var east = (float)bounds.East + 3;
            var mapExtent = new RectangleShape(west, north, east, south);
            MediatorMessage.Send(MediatorMessage.SetCurrentExtent, mapExtent);
        }
        #endregion

        #region DeleteRangeComplexCommand
        public SimpleCommand<object, object> DeleteRangeComplexCommand
        {
            get { return _deleteRangeComplex ?? (_deleteRangeComplex = new SimpleCommand<object, object>(delegate { return RangeComplexes != null && RangeComplexes.SelectedRangeComplex != null; }, delegate { DeleteRangeComplexHandler(); })); }
        }

        SimpleCommand<object, object> _deleteRangeComplex;

        void DeleteRangeComplexHandler()
        {
            var result = _messageBox.ShowYesNo(string.Format("Warning: Deleting the range complex \"{0}\" will also delete any overlays, bathymetry and environment data that have previously been created or extracted.\n\nThis operation CANNOT be undone.\n\nProceed with deletion?", RangeComplexes.SelectedRangeComplex.Name),
                                                 CustomDialogIcons.Exclamation);
            if (result == CustomDialogResults.No) return;
            try
            {
                RangeComplexes.RemoveRangeComplex(RangeComplexes.SelectedRangeComplex.Name);
                RangeComplexes.SelectedRangeComplex = null;
            }
            catch (Exception e) { _messageBox.ShowError(e.Message); }
        }
        #endregion

        #region ClearRangeComplexSelectionCommand
        public SimpleCommand<object, object> ClearRangeComplexSelectionCommand
        {
            get
            {
                return _clearRangeComplexSelection ?? (_clearRangeComplexSelection = new SimpleCommand<object, object>(
                    delegate { return RangeComplexes != null && RangeComplexes.SelectedRangeComplex != null; },
                    delegate
                    {
                        RangeComplexes.SelectedRangeComplex = null;
                        RangeComplexes.SelectedRangeComplexIndex = -1;
                    }));
            }
        }

        SimpleCommand<object, object> _clearRangeComplexSelection;
        #endregion

        #region NewOverlayCommand

        public SimpleCommand<object, object> NewOverlayCommand
        {
            get
            {
                return _newOverlay ?? (_newOverlay = new SimpleCommand<object, object>(delegate { return RangeComplexes != null && RangeComplexes.SelectedRangeComplex != null; }, delegate { NewOverlayHandler(); }));
            }
        }

        private SimpleCommand<object, object> _newOverlay;

        void NewOverlayHandler()
        {
            try
            {
                var vm = new NewOverlayViewModel(ESME.Globals.AppSettings, RangeComplexes.SelectedRangeComplex.Name);
                var result = _visualizer.ShowDialog("NewOverlayView", vm);
                if ((!result.HasValue) || (!result.Value)) return;
                RangeComplexes.SelectedArea = RangeComplexes.SelectedRangeComplex.CreateArea(vm.OverlayName, vm.OverlayGeos);
            }
            catch (Exception e) { _messageBox.ShowError(e.Message); }
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
            get
            {
                if (RangeComplexes == null) return false;
                return RangeComplexes.SelectedArea != null;
            }
        }

        void ExpandOverlayHandler()
        {
            try
            {
                var vm = new OverlayExpandViewModel(RangeComplexes.SelectedRangeComplex, RangeComplexes.SelectedArea);
                var result = _visualizer.ShowDialog("OverlayExpandView", vm);
                if ((!result.HasValue) || (!result.Value)) return;

                var curOverlay = RangeComplexes.SelectedArea.OverlayShape;
                var limits = new Limits(ConvexHull.Create(curOverlay.Geos, true));
                var expandedLimits = limits.CreateExpandedLimit(vm.BufferSize);  //in km.
                var coordinateList = expandedLimits.Geos;
                var testShape = new OverlayLineSegments(coordinateList, Colors.Black);

                if (!testShape.IsUsableAsPerimeter) coordinateList = ConvexHull.Create(coordinateList, true);

                RangeComplexes.SelectedArea = RangeComplexes.SelectedRangeComplex.CreateArea(vm.OverlayName, coordinateList);
            }
            catch (Exception e) { _messageBox.ShowError(e.Message); }
        }

        #endregion

        #region DeleteOverlayCommand
        public SimpleCommand<object, object> DeleteOverlayCommand
        {
            get { return _deleteOverlay ?? (_deleteOverlay = new SimpleCommand<object, object>(delegate { DeleteOverlayHandler(); })); }
        }

        SimpleCommand<object, object> _deleteOverlay;

        void DeleteOverlayHandler()
        {
            string error;
            var canDelete = RangeComplexes.SelectedRangeComplex.TryRemoveArea(RangeComplexes.SelectedArea.Name, out error);
            if (!canDelete)
            {
                _messageBox.ShowError(error);
                return;
            }
            var result = _messageBox.ShowYesNo(string.Format("Are you sure you want to delete the overlay \"{0}\"?\r\nThis operation cannot be undone.", RangeComplexes.SelectedArea.Name), CustomDialogIcons.Exclamation);
            if (result == CustomDialogResults.No) return;
            RangeComplexes.SelectedRangeComplex.RemoveArea(RangeComplexes.SelectedArea.Name);
            RangeComplexes.SelectedArea = null;
        }
        #endregion

        #region ClearAreaSelectionCommand
        public SimpleCommand<object, object> ClearAreaSelectionCommand
        {
            get
            {
                return _clearAreaSelectionCommand ?? (_clearAreaSelectionCommand = new SimpleCommand<object, object>(
                    delegate { return RangeComplexes != null && RangeComplexes.SelectedArea != null; },
                    delegate
                    {
                        RangeComplexes.SelectedArea = null;
                        RangeComplexes.SelectedAreaIndex = -1;
                    }));
            }
        }

        SimpleCommand<object, object> _clearAreaSelectionCommand;
        #endregion

        #region AddBathymetryCommand
        public SimpleCommand<object, object> AddBathymetryCommand
        {
            get
            {
                return _addBathymetry ?? (_addBathymetry = new SimpleCommand<object, object>(
                    delegate
                    {
                        return RangeComplexes != null && RangeComplexes.SelectedArea != null && RangeComplexes.SelectedBathymetry != null && !RangeComplexes.SelectedBathymetry.IsCached;
                    },
                    delegate
                    {
                        RangeComplexes.SelectedArea.ImportBathymetry(RangeComplexes.SelectedBathymetry);
                    }));
            }
        }

        SimpleCommand<object, object> _addBathymetry;
        #endregion

        #region RemoveBathymetryCommand
        public SimpleCommand<object, object> RemoveBathymetryCommand
        {
            get
            {
                return _removeBathymetry ?? (_removeBathymetry = new SimpleCommand<object, object>(
                    delegate
                    {
                        return RangeComplexes != null && RangeComplexes.SelectedArea != null && RangeComplexes.SelectedBathymetry != null && RangeComplexes.SelectedArea.CanRemoveBathymetry(RangeComplexes.SelectedBathymetry);
                    },
                    delegate
                    {
                        var selectedIndex = RangeComplexes.SelectedBathymetryIndex;
                        var selectedBathymetry = RangeComplexes.SelectedBathymetry;
                        RangeComplexes.SelectedBathymetryIndex = -1;
                        RangeComplexes.SelectedArea.RemoveBathymetry(selectedBathymetry);
                        RangeComplexes.SelectedBathymetryIndex = selectedIndex;
                    }));
            }
        }

        SimpleCommand<object, object> _removeBathymetry;
        #endregion

        #region ReloadBathymetryCommand
        public SimpleCommand<object, object> ReloadBathymetryCommand
        {
            get { return _reloadBathymetry ?? (_reloadBathymetry = new SimpleCommand<object, object>(
                delegate
                {
                    return RangeComplexes != null && RangeComplexes.SelectedBathymetry != null && RangeComplexes.SelectedBathymetry.IsCached;
                }, 
                delegate
                {
                    RangeComplexes.SelectedArea.ImportBathymetry(RangeComplexes.SelectedBathymetry);
                }));
            }
        }

        SimpleCommand<object, object> _reloadBathymetry;
        #endregion

        #region ClearBathymetrySelectionCommand
        public SimpleCommand<object, object> ClearBathymetrySelectionCommand
        {
            get
            {
                return _clearBathymetrySelectionCommand ?? (_clearBathymetrySelectionCommand = new SimpleCommand<object, object>(
                    delegate { return RangeComplexes != null && RangeComplexes.SelectedBathymetry != null; },
                    delegate
                    {
                        RangeComplexes.SelectedBathymetry = null;
                        RangeComplexes.SelectedBathymetryIndex = -1;
                    }));
            }
        }

        SimpleCommand<object, object> _clearBathymetrySelectionCommand;
        #endregion

        #region ExportAllEnvironmentalDataCommand
        public SimpleCommand<object, object> ExportAllEnvironmentalDataCommand
        {
            get
            {
                return _exportAllEnvironmentalData ??
                       (_exportAllEnvironmentalData =
                        new SimpleCommand<object, object>(delegate { return IsExportAllEnvironmentalDataCommandEnabled; },
                                                          delegate { ExportAllEnvironmentalDataHandler(); }));
            }
        }

        SimpleCommand<object, object> _exportAllEnvironmentalData;

        bool IsExportAllEnvironmentalDataCommandEnabled
        {
            get { return _rangeComplexes != null; }
        }

        void ExportAllEnvironmentalDataHandler()
        {
            var result = _messageBox.ShowYesNo("This operation might take a long time to complete.\r\nReally export ALL environment data to NUWC-format files?", CustomDialogIcons.Question);
            if (result == CustomDialogResults.No) return;
            var vm = new ExportAllEnvironmentalDataProgressViewModel(_rangeComplexes.SelectedRangeComplex, _dispatcher);
            _visualizer.ShowDialog("ExportAllEnvironmentalDataProgressView", vm);
        }

       
        #endregion

        #region public string ScenarioLoadedToolTip { get; set; }

        public string ScenarioLoadedToolTip
        {
            get { return _scenarioLoadedToolTip; }
            set
            {
                if (_scenarioLoadedToolTip == value) return;
                _scenarioLoadedToolTip = value;
                NotifyPropertyChanged(ScenarioLoadedToolTipChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ScenarioLoadedToolTipChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.ScenarioLoadedToolTip);
        string _scenarioLoadedToolTip;

        #endregion

        void DisplayWorldMap()
        {
            if (MapLayerCollections.ContainsKey("Map")) return;
            MapLayerCollections.Add("Map", new MapLayerCollection(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"Sample GIS Data\Countries02.shp")));
            CurrentMapLayers = MapLayerCollections["Map"];
            ZoomToWorldMap();
        }

        void DisplayRangeComplex()
        {
            if ((!_allViewModelsAreReady) || (!_viewIsActivated)) return;
            if ((RangeComplexes == null) || (RangeComplexes.SelectedRangeComplex == null))
            {
                var opAreaLayer = CurrentMapLayers.Find<OverlayShapeMapLayer>(LayerType.OverlayFile, "Op Area");
                if (opAreaLayer != null) opAreaLayer.IsChecked = false;
                var simAreaLayer = CurrentMapLayers.Find<OverlayShapeMapLayer>(LayerType.OverlayFile, "Sim Area");
                if (simAreaLayer != null) simAreaLayer.IsChecked = false;
                return;
            }

            ZoomToRangeComplex();

            CurrentMapLayers.DisplayOverlayShapes("Op Area", LayerType.OverlayFile, Colors.Transparent, new List<OverlayShape> { RangeComplexes.SelectedRangeComplex.OpArea.OverlayShape });
            if ((RangeComplexes.SelectedRangeComplex.OpArea != RangeComplexes.SelectedRangeComplex.SimArea) &&
                (RangeComplexes.SelectedRangeComplex.OpArea.GeoRect != RangeComplexes.SelectedRangeComplex.SimArea.GeoRect))
                CurrentMapLayers.DisplayOverlayShapes("Sim Area", LayerType.OverlayFile, Colors.Transparent, new List<OverlayShape> { RangeComplexes.SelectedRangeComplex.SimArea.OverlayShape });
            
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }

        void DisplaySelectedArea()
        {
            if ((!_allViewModelsAreReady) || (!_viewIsActivated)) return;
            OverlayShapeMapLayer overlayLayer;
            if ((RangeComplexes == null) || (RangeComplexes.SelectedArea == null))
            {
                overlayLayer = CurrentMapLayers.Find<OverlayShapeMapLayer>(LayerType.OverlayFile, "Selected Area");
                overlayLayer.IsEnabled = false;
                overlayLayer.IsChecked = false;
                return;
            }
            overlayLayer = CurrentMapLayers.DisplayOverlayShapes("Selected Area", LayerType.OverlayFile, Colors.Transparent, new List<OverlayShape> { RangeComplexes.SelectedArea.OverlayShape });
            overlayLayer.IsEnabled = true;
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }

        void DisplayBathymetry()
        {
            if ((!_allViewModelsAreReady) || (!_viewIsActivated)) return;
            RasterMapLayer bathyBitmapLayer;
            if ((RangeComplexes.SelectedBathymetry == null) || (!RangeComplexes.SelectedBathymetry.IsCached) || (RangeComplexes.SelectedBathymetry.FileName == null))
            {
                bathyBitmapLayer = CurrentMapLayers.Find<RasterMapLayer>(LayerType.BathymetryRaster, "Bathymetry");
                if (bathyBitmapLayer != null)
                {
                    bathyBitmapLayer.IsChecked = false;
                    bathyBitmapLayer.IsEnabled = false;
                }
                return;
            }
            if ((RangeComplexes.SelectedBathymetry != null) && (RangeComplexes.SelectedBathymetry.IsCached) && (RangeComplexes.SelectedBathymetry.FileName != null))
            {
                var bitmapFilename = Path.Combine(RangeComplexes.SelectedArea.BathymetryPath, Path.GetFileNameWithoutExtension(RangeComplexes.SelectedBathymetry.FileName) + ".bmp");
                bathyBitmapLayer = CurrentMapLayers.DisplayBathymetryRaster("Bathymetry", bitmapFilename, true, false, true, RangeComplexes.SelectedBathymetry.GeoRect);
                bathyBitmapLayer.IsEnabled = true;
                MediatorMessage.Send(MediatorMessage.MoveLayerToBottom, bathyBitmapLayer);
            }
        }
    }
}
