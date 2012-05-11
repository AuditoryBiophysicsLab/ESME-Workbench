using System;
using ESME.Locations;
using ESME.Views.Locations;
using ESMEWorkbench.ViewModels.Tree;
using HRC.Navigation;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Main
{
    public partial class MainViewModel
    {
        #region CreateLocationCommand
        public SimpleCommand<object, object> CreateLocationCommand
        {
            get { return _createLocation ?? (_createLocation = new SimpleCommand<object, object>(CreateLocationHandler)); }
        }

        SimpleCommand<object, object> _createLocation;

        void CreateLocationHandler(object o)
        {
            try
            {
                var currentExtent = MapViewModel.CurrentExtent;
                MapViewModel.EditOverlayViewModel.North = (currentExtent.North + currentExtent.Center.Latitude) / 2;
                MapViewModel.EditOverlayViewModel.South = (currentExtent.South + currentExtent.Center.Latitude) / 2;
                MapViewModel.EditOverlayViewModel.East = (currentExtent.East + currentExtent.Center.Longitude) / 2;
                MapViewModel.EditOverlayViewModel.West = (currentExtent.West + currentExtent.Center.Longitude) / 2;
                MapViewModel.EditOverlayViewModel.IsVisible = true;

                _visualizer.ShowWindow("CreateLocationView",
                                       new CreateLocationViewModel { EditOverlayViewModel = MapViewModel.EditOverlayViewModel },
                                       true,
                                       (sender, args) =>
                                       {
                                           MapViewModel.EditOverlayViewModel.IsVisible = false;
                                           var vm = (CreateLocationViewModel)args.State;
                                           if (vm.IsCanceled) return;
                                           Database.Add(new Location
                                           {
                                               Name = vm.LocationName,
                                               Comments = vm.Comments,
                                               GeoRect = MapViewModel.EditOverlayViewModel.GeoRect
                                           });
                                           Database.SaveChanges();
                                       }); 
            }
            catch (Exception e) { _messageBox.ShowError(e.Message); }
        }
        #endregion

        public int SelectedLocationIndex { get; set; }
        public Location SelectedLocation { get; set; }

        #region ZoomToWorldMapCommand
        public SimpleCommand<object, object> ZoomToWorldMapCommand
        {
            get { return _zoomToWorldMap ?? (_zoomToWorldMap = new SimpleCommand<object, object>(o => { MapViewModel.CurrentExtent = new GeoRect(90, -90, 180, -180); })); }
        }

        SimpleCommand<object, object> _zoomToWorldMap;

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
        }
        bool _viewIsActivated;
        #endregion

        public LocationsTreeViewModel LocationsTreeViewModel { get; set; }

#if false
        void DisplayWorldMap()
        {
            if (MapLayerCollections.ContainsKey("Map")) return;
            MapLayerCollections.Add("Map", new MapLayerCollection(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"Sample GIS Data\Countries02.shp")));
            CurrentMapLayers = MapLayerCollections["Map"];
            ZoomToWorldMap();
        }

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
            var result = _messageBox.ShowYesNo(string.Format("Are you sure you want to delete the overlay \"{0}\"?\r\nThis operation cannot be undone.", RangeComplexes.SelectedArea.Name), MessageBoxImage.Exclamation);
            if (result == MessageBoxResult.No) return;
            RangeComplexes.SelectedRangeComplex.RemoveArea(RangeComplexes.SelectedArea.Name);
            RangeComplexes.SelectedArea = null;
        }
        #endregion

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
#endif
    }
}
