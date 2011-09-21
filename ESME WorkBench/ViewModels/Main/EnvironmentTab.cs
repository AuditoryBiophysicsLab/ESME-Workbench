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
using ESME.Environment.Descriptors;
using ESME.Environment.NAVO;
using ESME.Mapping;
using ESME.Model;
using ESME.NEMO.Overlay;
using ESME.Views.Locations;
using ESME.Views.InstallationWizard;
using HRC.Navigation;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Main
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
                {EnvironmentDataType.BottomLoss, null}, 
                {EnvironmentDataType.SoundSpeed, null}, 
                {EnvironmentDataType.Wind, null}
            };
            Console.WriteLine("All view models are ready!");
            WizardViewModel.LaunchWizardIfNeeded(_visualizerService);
            _dispatcher.InvokeIfRequired(DisplayWorldMap, DispatcherPriority.Normal);
            AreAllViewModelsReady = true;
            if (ESME.Globals.AppSettings.ScenarioDataDirectory == null) return;
            SelectedRangeComplexIndex = -1;
            SelectedTimePeriodIndex = 0;
            SelectedAreaIndex = -1;
            SelectedBathymetryIndex = -1;
        }

        public Dictionary<EnvironmentDataType, MapLayerViewModel> EnvironmentLayers { get; private set; }
        readonly List<MapLayerViewModel> _sedimentLayers = new List<MapLayerViewModel>();

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

        #region public NewRangeComplex SelectedRangeComplex { get; set; }

        public NewRangeComplex SelectedRangeComplex
        {
            get { return _selectedRangeComplex; }
            set
            {
                if (_selectedRangeComplex == value) return;
                _selectedRangeComplex = value;
                NotifyPropertyChanged(SelectedRangeComplexChangedEventArgs);
                IsRangeComplexSelected = _selectedRangeComplex != NewRangeComplex.None;
                NotifyPropertyChanged(IsRangeComplexSelectedChangedEventArgs);
                NotifyPropertyChanged(IsTimePeriodSelectionEnabledChangedEventArgs);
                DisplayRangeComplex();
                if (IsRangeComplexSelected)
                {
                    RangeComplexes.SelectedSediment.GetDataAsync();
                    RangeComplexes.SelectedSediment.DataTask.ContinueWith(task =>
                    {
                        var result = task.Result.Samples.GroupBy(sample => sample.Data.SampleValue);
                        foreach (var sedimentType in result)
                        {
                            if (sedimentType.Key == 0) continue;
                            var samplePoints = sedimentType.Select(samplePoint => new OverlayPoint(samplePoint)).ToList();
                            _dispatcher.InvokeInBackgroundIfRequired(
                                                                     () =>
                                                                     _sedimentLayers.Add(CurrentMapLayers.DisplayOverlayShapes(
                                                                                                                               string.Format("Sediment: {0}", SedimentTypes.Find(sedimentType.Key).Name),
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
                    RangeComplexes.SelectedBottomLoss.GetDataAsync();
                    RangeComplexes.SelectedBottomLoss.DataTask.ContinueWith(task =>
                    {
                        var samplePoints = task.Result.Samples.Select(samplePoint => new OverlayPoint(samplePoint)).ToList();
                        _dispatcher.InvokeInBackgroundIfRequired(
                                                                 () =>
                                                                 EnvironmentLayers[EnvironmentDataType.BottomLoss] =
                                                                 CurrentMapLayers.DisplayOverlayShapes("Bottom Loss",
                                                                                                       LayerType.BottomType,
                                                                                                       Colors.Transparent,
                                                                                                       samplePoints,
                                                                                                       0,
                                                                                                       PointSymbolType.Diamond,
                                                                                                       false,
                                                                                                       null,
                                                                                                       false));
                    });
                }
                else
                {
                    foreach (var layer in _sedimentLayers) CurrentMapLayers.Remove(layer);
                    _sedimentLayers.Clear();
                    if (EnvironmentLayers[EnvironmentDataType.BottomLoss] != null) EnvironmentLayers[EnvironmentDataType.BottomLoss].IsEnabled = false;
                }
                UpdateTimePeriodData();
            }
        }

        #region public int SelectedRangeComplexIndex { get; set; }

        public int SelectedRangeComplexIndex
        {
            get { return _selectedRangeComplexIndex; }
            set
            {
                if (_selectedRangeComplexIndex == value) return;
                _selectedRangeComplexIndex = value;
                NotifyPropertyChanged(SelectedRangeComplexIndexChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedRangeComplexIndexChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedRangeComplexIndex);
        int _selectedRangeComplexIndex;

        #endregion

        public bool IsRangeComplexSelected { get; private set; }
        static readonly PropertyChangedEventArgs IsRangeComplexSelectedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsRangeComplexSelected);

        static readonly PropertyChangedEventArgs SelectedRangeComplexChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedRangeComplex);
        NewRangeComplex _selectedRangeComplex;

        #endregion

        #region public NAVOTimePeriod SelectedTimePeriod { get; set; }

        public NAVOTimePeriod SelectedTimePeriod
        {
            get { return _selectedTimePeriod; }
            set
            {
                if (_selectedTimePeriod == value) return;
                _selectedTimePeriod = value;
                NotifyPropertyChanged(SelectedTimePeriodChangedEventArgs);
                UpdateTimePeriodData();
            }
        }

        void UpdateTimePeriodData()
        {
            if (IsRangeComplexSelected && RangeComplexes.SelectedTimePeriod != NAVOTimePeriod.Invalid)
            {
                RangeComplexes.SelectedWind.GetDataAsync();
                RangeComplexes.SelectedWind.DataTask.ContinueWith(task =>
                {
                    var samplePoints = task.Result[RangeComplexes.SelectedTimePeriod].EnvironmentData.Select(samplePoint => new OverlayPoint(samplePoint)).ToList();
                    _dispatcher.InvokeInBackgroundIfRequired(() => EnvironmentLayers[EnvironmentDataType.Wind] = CurrentMapLayers.DisplayOverlayShapes("Wind", LayerType.WindSpeed, Colors.Transparent, samplePoints, 0, PointSymbolType.Diamond, false, null, false));
                });
            }
            else if (EnvironmentLayers[EnvironmentDataType.Wind] != null) EnvironmentLayers[EnvironmentDataType.Wind].IsEnabled = false;
            UpdateSoundSpeedData();
        }

        #region public bool IsTimePeriodSelectionEnabled { get; set; }

        public bool IsTimePeriodSelectionEnabled
        {
            get { return IsScenarioLoaded && IsScenarioNotLoaded; }
        }

        static readonly PropertyChangedEventArgs IsTimePeriodSelectionEnabledChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsTimePeriodSelectionEnabled);

        #endregion


        void UpdateSoundSpeedData()
        {
            if (IsRangeComplexSelected && IsBathymetrySelected && RangeComplexes.SelectedBathymetry.IsCached)
            {
                if (RangeComplexes.SelectedSoundSpeed.DataTask == null)
                {
                    RangeComplexes.SelectedSoundSpeed.SelectedBathymetry = RangeComplexes.SelectedBathymetry;
                    RangeComplexes.SelectedSoundSpeed.RangeComplexToken = RangeComplexes.SelectedRangeComplex.EnvironmentFiles;
                    RangeComplexes.SelectedSoundSpeed.Reset();
                }
                RangeComplexes.SelectedSoundSpeed.GetDataAsync();
                RangeComplexes.SelectedSoundSpeed.DataTask.ContinueWith(task =>
                {
                    if (task.Result == null) return;
                    var samplePoints = task.Result[RangeComplexes.SelectedTimePeriod].EnvironmentData.Select(samplePoint => new OverlayPoint(samplePoint)).ToList();
                    _dispatcher.InvokeInBackgroundIfRequired(
                                                             () =>
                                                             EnvironmentLayers[EnvironmentDataType.SoundSpeed] = CurrentMapLayers.DisplayOverlayShapes("Sound Speed", LayerType.SoundSpeed, Colors.Transparent, samplePoints, 0, PointSymbolType.Diamond,
                                                                                                   false, null, false));
                });
            }
            else if (EnvironmentLayers[EnvironmentDataType.SoundSpeed] != null) EnvironmentLayers[EnvironmentDataType.SoundSpeed].IsEnabled = false;
        }

        #region public int SelectedTimePeriodIndex { get; set; }

        public int SelectedTimePeriodIndex
        {
            get { return _selectedTimePeriodIndex; }
            set
            {
                if (_selectedTimePeriodIndex == value) return;
                _selectedTimePeriodIndex = value;
                NotifyPropertyChanged(SelectedTimePeriodIndexChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedTimePeriodIndexChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedTimePeriodIndex);
        int _selectedTimePeriodIndex;
        #endregion

        static readonly PropertyChangedEventArgs SelectedTimePeriodChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedTimePeriod);
        NAVOTimePeriod _selectedTimePeriod;

        #endregion

        #region public RangeComplexArea SelectedArea { get; set; }

        public RangeComplexArea SelectedArea
        {
            get { return _selectedArea; }
            set
            {
                if (_selectedArea == value) return;
                _selectedArea = value;
                NotifyPropertyChanged(SelectedAreaChangedEventArgs);
                IsAreaSelected = _selectedArea != RangeComplexArea.None;
                NotifyPropertyChanged(IsAreaSelectedChangedEventArgs);
                DisplaySelectedArea();
            }
        }

        #region public int SelectedAreaIndex { get; set; }

        public int SelectedAreaIndex
        {
            get { return _selectedAreaIndex; }
            set
            {
                if (_selectedAreaIndex == value) return;
                _selectedAreaIndex = value;
                NotifyPropertyChanged(SelectedAreaIndexChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedAreaIndexChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedAreaIndex);
        int _selectedAreaIndex;

        #endregion

        public bool IsAreaSelected { get; private set; }
        static readonly PropertyChangedEventArgs IsAreaSelectedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsAreaSelected);

        static readonly PropertyChangedEventArgs SelectedAreaChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedArea);
        RangeComplexArea _selectedArea;

        #endregion

        #region public BathymetryFile SelectedBathymetry { get; set; }

        public BathymetryFile SelectedBathymetry
        {
            get { return _selectedBathymetry; }
            set
            {
                if (_selectedBathymetry == value) return;
                _selectedBathymetry = value;
                NotifyPropertyChanged(SelectedBathymetryChangedEventArgs);
                IsBathymetrySelected = _selectedBathymetry != BathymetryFile.None;
                NotifyPropertyChanged(IsBathymetrySelectedChangedEventArgs);
                DisplayBathymetry();
                if (IsBathymetrySelected && RangeComplexes.SelectedBathymetry.IsCached) RangeComplexes.SelectedBathymetry.GetDataAsync();

                UpdateSoundSpeedData();
            }
        }

        #region public int SelectedBathymetryIndex { get; set; }

        public int SelectedBathymetryIndex
        {
            get { return _selectedBathymetryIndex; }
            set
            {
                if (_selectedBathymetryIndex == value) return;
                _selectedBathymetryIndex = value;
                NotifyPropertyChanged(SelectedBathymetryIndexChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedBathymetryIndexChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedBathymetryIndex);
        int _selectedBathymetryIndex;

        #endregion

        public bool IsBathymetrySelected { get; private set; }
        static readonly PropertyChangedEventArgs IsBathymetrySelectedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsBathymetrySelected);

        static readonly PropertyChangedEventArgs SelectedBathymetryChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedBathymetry);
        BathymetryFile _selectedBathymetry;

        #endregion

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
                var vm = new NewRangeComplexViewModel(Globals.AppSettings);
                var result = _visualizerService.ShowDialog("NewRangeComplexView", vm);
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
            catch (Exception e) { _messageBoxService.ShowError(e.Message); }
        }
        #endregion

        #region ZoomToRangeComplexCommand
        public SimpleCommand<object, object> ZoomToRangeComplexCommand
        {
            get { return _zoomToRangeComplex ?? (_zoomToRangeComplex = new SimpleCommand<object, object>(delegate { return IsRangeComplexSelected; }, delegate { ZoomToRangeComplex(); })); }
        }

        SimpleCommand<object, object> _zoomToRangeComplex;

        void ZoomToRangeComplex()
        {
            var bounds = RangeComplexes.SelectedRangeComplex.OpArea.OverlayShape.BoundingBox;
            var north = (float)bounds.Bottom + 3;
            var west = (float)bounds.Left - 3;
            var south = (float)bounds.Top - 3;
            var east = (float)bounds.Right + 3;
            var mapExtent = new RectangleShape(west, north, east, south);
            MediatorMessage.Send(MediatorMessage.SetCurrentExtent, mapExtent);
        }
        #endregion

        #region DeleteRangeComplexCommand
        public SimpleCommand<object, object> DeleteRangeComplexCommand
        {
            get { return _deleteRangeComplex ?? (_deleteRangeComplex = new SimpleCommand<object, object>(delegate { return IsRangeComplexSelected; }, delegate { DeleteRangeComplexHandler(); })); }
        }

        SimpleCommand<object, object> _deleteRangeComplex;

        void DeleteRangeComplexHandler()
        {
            var result = _messageBoxService.ShowYesNo(string.Format("Warning: Deleting the range complex \"{0}\" will also delete any overlays, bathymetry and environment data that have previously been created or extracted.\n\nThis operation CANNOT be undone.\n\nProceed with deletion?", RangeComplexes.SelectedRangeComplex.Name),
                                                 CustomDialogIcons.Exclamation);
            if (result == CustomDialogResults.No) return;
            try
            {
                RangeComplexes.RemoveRangeComplex(RangeComplexes.SelectedRangeComplex.Name);
                RangeComplexes.SelectedRangeComplex = NewRangeComplex.None;
            }
            catch (Exception e) { _messageBoxService.ShowError(e.Message); }
        }
        #endregion

        #region ClearRangeComplexSelectionCommand
        public SimpleCommand<object, object> ClearRangeComplexSelectionCommand
        {
            get
            {
                return _clearRangeComplexSelection ?? (_clearRangeComplexSelection = new SimpleCommand<object, object>(
                    delegate { return IsRangeComplexSelected; },
                    delegate
                    {
                        RangeComplexes.SelectedRangeComplex = NewRangeComplex.None;
                        SelectedRangeComplexIndex = -1;
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
                return _newOverlay ?? (_newOverlay = new SimpleCommand<object, object>(delegate { return IsRangeComplexSelected; }, delegate { NewOverlayHandler(); }));
            }
        }

        private SimpleCommand<object, object> _newOverlay;

        void NewOverlayHandler()
        {
            try
            {
                var vm = new NewOverlayViewModel(Globals.AppSettings, RangeComplexes.SelectedRangeComplex.Name);
                var result = _visualizerService.ShowDialog("NewOverlayView", vm);
                if ((!result.HasValue) || (!result.Value)) return;
                RangeComplexes.SelectedArea = RangeComplexes.SelectedRangeComplex.CreateArea(vm.OverlayName, vm.OverlayGeos);
            }
            catch (Exception e) { _messageBoxService.ShowError(e.Message); }
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
                return RangeComplexes.SelectedArea != RangeComplexArea.None;
            }
        }

        void ExpandOverlayHandler()
        {
            try
            {
                var vm = new OverlayExpandViewModel(SelectedRangeComplex, SelectedArea);
                var result = _visualizerService.ShowDialog("OverlayExpandView", vm);
                if ((!result.HasValue) || (!result.Value)) return;

                var curOverlay = RangeComplexes.SelectedArea.OverlayShape;
                var limits = new Limits(ConvexHull.Create(curOverlay.EarthCoordinates.Cast<Geo>().ToList(), true));
                var expandedLimits = limits.CreateExpandedLimit(vm.BufferSize);  //in km.
                var coordinateList = expandedLimits.Geos;
                var testShape = new OverlayLineSegments(coordinateList, Colors.Black);

                if (!testShape.IsUsableAsPerimeter) coordinateList = ConvexHull.Create(coordinateList, true);

                RangeComplexes.SelectedArea = RangeComplexes.SelectedRangeComplex.CreateArea(vm.OverlayName, coordinateList);
            }
            catch (Exception e) { _messageBoxService.ShowError(e.Message); }
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
            var canDelete = SelectedRangeComplex.TryRemoveArea(SelectedArea.Name, out error);
            if (!canDelete)
            {
                _messageBoxService.ShowError(error);
                return;
            }
            var result = _messageBoxService.ShowYesNo(string.Format("Are you sure you want to delete the overlay \"{0}\"?\r\nThis operation cannot be undone.", RangeComplexes.SelectedArea.Name), CustomDialogIcons.Exclamation);
            if (result == CustomDialogResults.No) return;
            RangeComplexes.SelectedRangeComplex.RemoveArea(RangeComplexes.SelectedArea.Name);
            RangeComplexes.SelectedArea = RangeComplexArea.None;
        }
        #endregion

        #region ClearAreaSelectionCommand
        public SimpleCommand<object, object> ClearAreaSelectionCommand
        {
            get
            {
                return _clearAreaSelectionCommand ?? (_clearAreaSelectionCommand = new SimpleCommand<object, object>(
                    delegate { return IsAreaSelected; },
                    delegate
                    {
                        RangeComplexes.SelectedArea = RangeComplexArea.None;
                        SelectedAreaIndex = -1;
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
                        return IsAreaSelected && IsBathymetrySelected && !SelectedBathymetry.IsCached;
                    },
                    delegate
                    {
                        SelectedArea.ImportBathymetry(SelectedBathymetry);
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
                        return IsAreaSelected && IsBathymetrySelected && SelectedBathymetry.IsDeleteable && SelectedArea.CanRemoveBathymetry(SelectedBathymetry);
                    },
                    delegate
                    {
                        var selectedIndex = SelectedBathymetryIndex;
                        var selectedBathymetry = RangeComplexes.SelectedBathymetry;
                        SelectedBathymetryIndex = -1;
                        SelectedArea.RemoveBathymetry(selectedBathymetry);
                        SelectedBathymetryIndex = selectedIndex;
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
                    return IsBathymetrySelected && SelectedBathymetry.IsCached;
                }, 
                delegate
                {
                    SelectedArea.ImportBathymetry(SelectedBathymetry);
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
                    delegate { return IsBathymetrySelected; },
                    delegate
                    {
                        RangeComplexes.SelectedBathymetry = BathymetryFile.None;
                        SelectedBathymetryIndex = -1;
                    }));
            }
        }

        SimpleCommand<object, object> _clearBathymetrySelectionCommand;
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
            if ((RangeComplexes == null) || (RangeComplexes.SelectedRangeComplex == NewRangeComplex.None))
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
            if ((RangeComplexes == null) || (RangeComplexes.SelectedArea == RangeComplexArea.None))
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
            if ((SelectedBathymetry == null) || (SelectedBathymetry == BathymetryFile.None) || (SelectedBathymetry.BitmapFilename == null))
            {
                bathyBitmapLayer = CurrentMapLayers.Find<RasterMapLayer>(LayerType.BathymetryRaster, "Bathymetry");
                if (bathyBitmapLayer != null)
                {
                    bathyBitmapLayer.IsChecked = false;
                    bathyBitmapLayer.IsEnabled = false;
                }
                return;
            }
            bathyBitmapLayer = CurrentMapLayers.DisplayBathymetryRaster("Bathymetry", SelectedBathymetry.BitmapFilename, true, false, true, SelectedBathymetry.GeoRect);
            bathyBitmapLayer.IsEnabled = true;
            MediatorMessage.Send(MediatorMessage.MoveLayerToBottom, bathyBitmapLayer);
        }
#if false
        #region Range Complex ribbon group

        #region public RangeComplexDescriptors RangeComplexDescriptors { get; set; }

        public RangeComplexDescriptors RangeComplexDescriptors
        {
            get { return _rangeComplexDescriptors; }
            set
            {
                if (_rangeComplexDescriptors == value) return;
                _rangeComplexDescriptors = value;
                Debug.WriteLine("{0}: RangeComplexDescriptors has changed.  New value {1}", DateTime.Now,
                                _rangeComplexDescriptors == null ? "is NULL" : string.Format("has {0} entries", _rangeComplexDescriptors.Count));
                NotifyPropertyChanged(RangeComplexesChangedEventArgs);
                NotifyPropertyChanged(AreRangeComplexesLoadedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RangeComplexesChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.RangeComplexDescriptors);
        RangeComplexDescriptors _rangeComplexDescriptors;

        #endregion

        #region public bool AreRangeComplexesLoaded { get; set; }

        public bool AreRangeComplexesLoaded
        {
            get { return RangeComplexDescriptors != null; }
        }

        static readonly PropertyChangedEventArgs AreRangeComplexesLoadedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.AreRangeComplexesLoaded);

        #endregion

        #region public RangeComplexDescriptor SelectedRangeComplexDescriptor { get; set; }

        public RangeComplexDescriptor SelectedRangeComplexDescriptor
        {
            get { return _selectedRangeComplexDescriptor; }
            set
            {
                if (_selectedRangeComplexDescriptor == value) return;
                _selectedRangeComplexDescriptor = value;
                IsRangeComplexSelected = _selectedRangeComplexDescriptor != null;
                if (_selectedRangeComplexDescriptor != null)
                {
                    SelectedRangeComplexInfo = string.Format("Name: {0}\nReference Point: ({1}, {2})\nHeight: {3}\nGeoid Separation: {4}\nOps Limit: {5}\nSim Limit: {6}",
                                                             _selectedRangeComplexDescriptor.Data.Name, Math.Round(_selectedRangeComplexDescriptor.Data.Latitude, 5), Math.Round(_selectedRangeComplexDescriptor.Data.Longitude, 5),
                                                             _selectedRangeComplexDescriptor.Data.Height, _selectedRangeComplexDescriptor.Data.GeoidSeparation, SelectedRangeComplexDescriptor.Data.OpsLimitFile,
                                                             SelectedRangeComplexDescriptor.Data.SimLimitFile);
                    //Console.WriteLine("Range complex {0} is selected!", _selectedRangeComplexDescriptor.Data.Name);

                    NAEMOOverlayDescriptors = _selectedRangeComplexDescriptor.NAEMOOverlayDescriptors;
                    NAEMOBathymetryDescriptors = _selectedRangeComplexDescriptor.NAEMOBathymetryDescriptors;
                    NAEMOEnvironmentDescriptors = _selectedRangeComplexDescriptor.NAEMOEnvironmentDescriptors;
                }
                else
                {
                    NAEMOOverlayDescriptors = null;
                    NAEMOBathymetryDescriptors = null;
                    NAEMOEnvironmentDescriptors = null;
                    if (MapLayerCollections.ActiveLayer.Name == "Environment") ZoomToWorldMap();
                }
                NotifyPropertyChanged(SelectedRangeComplexDescriptorChangedEventArgs);
                try
                {
                    _dispatcher.InvokeIfRequired(DisplayRangeComplex, DispatcherPriority.Normal);
                }
                catch (Exception e)
                {
                    if (_selectedRangeComplexDescriptor != null) _messageBoxService.ShowError("Error loading range complex \"" + _selectedRangeComplexDescriptor.Data.Name + "\": " + e.Message);
                }
            }
        }

        static readonly PropertyChangedEventArgs SelectedRangeComplexDescriptorChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedRangeComplexDescriptor);
        RangeComplexDescriptor _selectedRangeComplexDescriptor;


        #endregion

        #region public RibbonToolTip RangeComplexToolTip { get; set; }

        public RibbonToolTip RangeComplexToolTip
        {
            get { return _rangeComplexToolTip; }
            set
            {
                if (_rangeComplexToolTip == value) return;
                _rangeComplexToolTip = value;
                NotifyPropertyChanged(RangeComplexToolTipChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RangeComplexToolTipChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.RangeComplexToolTip);
        RibbonToolTip _rangeComplexToolTip;

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
                if (SelectedBathymetryOverlayName != SelectedOverlayName)
                    Task.Factory.StartNew(() =>
                    {
                        if (NAEMOBathymetryDescriptors != null) SelectedBathymetryDescriptor = NAEMOBathymetryDescriptors[0].Value;
                    });
                _dispatcher.InvokeIfRequired(DisplayOverlay, DispatcherPriority.Normal);
            }
        }

        static readonly PropertyChangedEventArgs SelectedOverlayDescriptorChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedOverlayDescriptor);
        NAEMOOverlayDescriptor _selectedOverlayDescriptor;

        public string SelectedOverlayName
        {
            get
            {
                return (_selectedOverlayDescriptor != null) && (_selectedOverlayDescriptor.Metadata != null)
                    ? Path.GetFileNameWithoutExtension(_selectedOverlayDescriptor.Metadata.Filename) : null;
            }
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



        #region DeleteOverlayCommand
        public SimpleCommand<object, object> DeleteOverlayCommand
        {
            get { return _deleteOverlay ?? (_deleteOverlay = new SimpleCommand<object, object>(delegate { return IsDeleteOverlayCommandEnabled; }, delegate { DeleteOverlayHandler(); })); }
        }

        SimpleCommand<object, object> _deleteOverlay;

        bool IsDeleteOverlayCommandEnabled
        {
            get
            {
                if (SelectedOverlayDescriptor == null) return false;
                var selectedOverlayFilename = Path.GetFileName(SelectedOverlayDescriptor.DataFilename);
                return IsOverlayFileSelected && (selectedOverlayFilename != SelectedRangeComplexDescriptor.Data.OpsLimitFile) && (selectedOverlayFilename != SelectedRangeComplexDescriptor.Data.SimLimitFile);
            }
        }

        void DeleteOverlayHandler()
        {
            var dependentOverlays = NAEMOOverlayDescriptors.GetDependentOverlays(SelectedOverlayDescriptor).ToList();
            var dependentOverlayCount = dependentOverlays.Count;
            var dependentOverlayList = new StringBuilder();
            foreach (var overlay in dependentOverlays) 
                dependentOverlayList.Append(Path.GetFileNameWithoutExtension(overlay.DataFilename) + ", ");
            if (dependentOverlayList.Length > 0) dependentOverlayList.Remove(dependentOverlayList.Length - 2, 2);

            dependentOverlays.Add(SelectedOverlayDescriptor);
            dependentOverlays.Sort();

            var dependentBathymetries = NAEMOBathymetryDescriptors.GetDependentBathymetries(dependentOverlays).ToList();
            var dependentBathymetryList = new StringBuilder();
            foreach (var bathymetry in dependentBathymetries)
                dependentBathymetryList.Append(Path.GetFileNameWithoutExtension(bathymetry.DataFilename) + ", ");
            if (dependentBathymetryList.Length > 0) dependentBathymetryList.Remove(dependentBathymetryList.Length - 2, 2);

            var dependentEnvironments = NAEMOEnvironmentDescriptors.GetDependentEnvironments(dependentOverlays, dependentBathymetries).ToList();
            var dependentEnvironmentList = new StringBuilder();
            foreach (var environment in dependentEnvironments)
                dependentEnvironmentList.Append(Path.GetFileNameWithoutExtension(environment.DataFilename) + ", ");
            if (dependentEnvironmentList.Length > 0) dependentEnvironmentList.Remove(dependentEnvironmentList.Length - 2, 2);

            var message = new StringBuilder();
            message.Append("Warning: You are about to delete the overlay \"" + Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.Data.FileName) + "\"");
            if (dependentOverlayCount == 0)
            {
                if ((dependentBathymetries.Count > 0) && (dependentEnvironments.Count > 0))
                {
                    message.Append(". This will also delete the dependent bathymetry data:\n\n" + dependentBathymetryList + "\n\n");
                    message.Append("and the dependent environment data:\n\n" + dependentEnvironmentList);
                }
                else
                {
                    if (dependentBathymetries.Count > 0) message.Append(". This will also delete the dependent bathymetry data:\n\n" + dependentBathymetryList);
                    if (dependentEnvironments.Count > 0) message.Append(". This will also delete the dependent environment data:\n\n" + dependentEnvironmentList);
                }
            }
            else
            {
                message.Append(". This will also delete the dependent overlays:\n\n" + dependentOverlayList);
                if ((dependentBathymetries.Count > 0) && (dependentEnvironments.Count > 0))
                {
                    message.Append("\n\nthe dependent bathymetry data: " + dependentBathymetryList + "\n\n");
                    message.Append("and the dependent environment data: " + dependentEnvironmentList);
                }
                else
                {
                    if (dependentBathymetries.Count > 0) message.Append("\n\nand the dependent bathymetry data: " + dependentBathymetryList);
                    if (dependentEnvironments.Count > 0) message.Append("\n\nand the dependent environment data: " + dependentEnvironmentList);
                }
            }

            message.Append("\n\nThis operation CANNOT be undone.\n\nProceed with deletion?");

            var result = _messageBoxService.ShowYesNo(message.ToString(), CustomDialogIcons.Exclamation);
            if (result == CustomDialogResults.No) return;
            NAEMOOverlayDescriptors.DeleteOverlays(dependentOverlays);
            NAEMOBathymetryDescriptors.DeleteBathymetry(dependentBathymetries);
            NAEMOEnvironmentDescriptors.DeleteEnvironment(dependentEnvironments);
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
                //SelectedBathymetryDescriptor = NAEMOBathymetryDescriptors != null && NAEMOBathymetryDescriptors.Count > 0 ? NAEMOBathymetryDescriptors[0].Value : null;
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
                if (_selectedBathymetryDescriptor != null) SelectedBathymetryInfo = string.Format("Name: {0}\nResolution: {1} min\nSource Overlay: {2}\nNumber of Points: {3:n}", Path.GetFileNameWithoutExtension(_selectedBathymetryDescriptor.DataFilename), _selectedBathymetryDescriptor.Metadata.Resolution, _selectedBathymetryDescriptor.Metadata.OverlayFilename ?? "[Unknown]",_selectedBathymetryDescriptor.Metadata.PointCount);
                IsBathymetryFileSelected = _selectedBathymetryDescriptor != null;
                if (SelectedBathymetryOverlayName != null) Task.Factory.StartNew(() => SelectedOverlayDescriptor = (NAEMOOverlayDescriptor)NAEMOOverlayDescriptors[SelectedBathymetryOverlayName]);
                if (SelectedEnvironmentBathymetryName != SelectedBathymetryName) Task.Factory.StartNew(() =>
                {
                    if (NAEMOEnvironmentDescriptors != null) SelectedEnvironmentDescriptor = NAEMOEnvironmentDescriptors[0].Value;
                });
                Task.Factory.StartNew(() => _dispatcher.InvokeIfRequired(DisplayBathymetry, DispatcherPriority.Normal));

            }
        }

        static readonly PropertyChangedEventArgs SelectedBathymetryDescriptorChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedBathymetryDescriptor);
        NAEMOBathymetryDescriptor _selectedBathymetryDescriptor;

        public string SelectedBathymetryOverlayName
        {
            get
            {
                return (_selectedBathymetryDescriptor != null) && (_selectedBathymetryDescriptor.Metadata != null)
                               ? _selectedBathymetryDescriptor.Metadata.OverlayFilename : null;
            }
        }

        public string SelectedBathymetryName
        {
            get
            {
                return (_selectedBathymetryDescriptor != null) && (_selectedBathymetryDescriptor.Metadata != null)
                    ? Path.GetFileNameWithoutExtension(_selectedBathymetryDescriptor.Metadata.Filename) : null;
            }
        }

        void DisplayBathymetry()
        {
            if ((!_allViewModelsAreReady) || (!_viewIsActivated)) return;
            RasterMapLayer bathyBitmapLayer;
            if (_selectedBathymetryDescriptor == null)
            {
                bathyBitmapLayer = EnvironmentTabMapLayers.Find<RasterMapLayer>(LayerType.BathymetryRaster, "Bathymetry");
                if (bathyBitmapLayer != null) bathyBitmapLayer.IsChecked = false;
                return;
            }
            bathyBitmapLayer = EnvironmentTabMapLayers.DisplayBathymetryRaster("Bathymetry",
                                                                                   Path.Combine(Globals.AppSettings.ScenarioDataDirectory, SelectedRangeComplexDescriptor.Data.Name, "Images",
                                                                                                Path.GetFileNameWithoutExtension(_selectedBathymetryDescriptor.DataFilename) + ".bmp"), true, false, true, _selectedBathymetryDescriptor.Metadata.Bounds);
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
            get { return _newBathymetry ?? (_newBathymetry = new SimpleCommand<object, object>(delegate { return IsRangeComplexSelected && SelectedOverlayDescriptor != null; }, delegate { NewBathymetryHandler(); })); }
        }

        SimpleCommand<object, object> _newBathymetry;

        void NewBathymetryHandler()
        {
            var vm = new BathymetryExtractionViewModel(Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename), new GeoRect(SelectedOverlayDescriptor.Data.Shapes[0].BoundingBox));
            var result = _visualizerService.ShowDialog("BathymetryExtractionView", vm);
            if ((!result.HasValue) || (!result.Value)) return;
            var extractionArea = new GeoRect(SelectedOverlayDescriptor.Data.Shapes[0].BoundingBox);
            var tempPath = Path.GetTempPath().Remove(Path.GetTempPath().Length - 1);
            if (!Directory.Exists(tempPath)) Directory.CreateDirectory(tempPath);
            var destinationPath = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, SelectedRangeComplexDescriptor.Data.Name, "Bathymetry");
            var destinationFile = vm.BathymetryName + ".txt";
            var bathymetryExtractor = new DBDBBackgroundExtractor
            {
                    WorkerSupportsCancellation = false,
                    ExtractionArea = extractionArea,
                    NAVOConfiguration = Globals.AppSettings.NAVOConfiguration,
                    DestinationPath = destinationPath,
                    UseExpandedExtractionArea = false,
                    SelectedResolution = vm.SelectedResolution,
                    SaveAsFilename = destinationFile,
                    TaskName = "Bathymetry data extraction",
            };
            bathymetryExtractor.RunWorkerCompleted += (s, e) => Task.Factory.StartNew(() =>
            {
                Bathymetry bathymetry;
                var bathymetryFilePath = Path.Combine(destinationPath, bathymetryExtractor.SaveAsFilename);
                var metadata = NAEMOBathymetryMetadata.FromBathymetryFile(bathymetryFilePath, out bathymetry);
                metadata.OverlayFilename = vm.OverlayName;
                metadata.Bounds = vm.BoundingBox;
                metadata.Save();
                var bathymetryDescriptor = new NAEMOBathymetryDescriptor { DataFilename = bathymetryFilePath, Metadata = metadata };
                NAEMOBathymetryDescriptors.Add(new KeyValuePair<string, NAEMOBathymetryDescriptor>(vm.BathymetryName, bathymetryDescriptor));
                NAEMOBathymetryDescriptors.Sort();
                SelectedBathymetryDescriptor = bathymetryDescriptor;
            });
            BackgroundTaskAggregator.TaskName = "Bathymetry data extraction";
            BackgroundTaskAggregator.BackgroundTasks.Add(bathymetryExtractor);
        }

        #endregion

        #region DeleteBathymetryCommand
        public SimpleCommand<object, object> DeleteBathymetryCommand
        {
            get { return _deleteBathymetry ?? (_deleteBathymetry = new SimpleCommand<object, object>(delegate { return IsBathymetryFileSelected; }, delegate { DeleteBathymetryHandler(); })); }
        }

        SimpleCommand<object, object> _deleteBathymetry;

        void DeleteBathymetryHandler()
        {
            var dependentEnvironments = NAEMOEnvironmentDescriptors.GetDependentEnvironments(SelectedBathymetryDescriptor).ToList();
            var dependentEnvironmentList = new StringBuilder();
            foreach (var environment in dependentEnvironments)
                dependentEnvironmentList.Append(Path.GetFileNameWithoutExtension(environment.DataFilename) + ", ");
            if (dependentEnvironmentList.Length > 0) dependentEnvironmentList.Remove(dependentEnvironmentList.Length - 2, 2);

            var message = new StringBuilder();
            message.Append("Warning: You are about to delete the bathymetry data \"" + Path.GetFileNameWithoutExtension(SelectedBathymetryDescriptor.Metadata.Filename) + "\"");
            if (dependentEnvironments.Count > 0) message.Append(". This will also delete the dependent environment data:\n\n" + dependentEnvironmentList);

            message.Append("\n\nThis operation CANNOT be undone.\n\nProceed with deletion?");

            var result = _messageBoxService.ShowYesNo(message.ToString(), CustomDialogIcons.Exclamation);

            if (result == CustomDialogResults.No) return;
            NAEMOBathymetryDescriptors.DeleteBathymetry(SelectedBathymetryDescriptor);
            NAEMOEnvironmentDescriptors.DeleteEnvironment(dependentEnvironments);
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
                if ((_selectedEnvironmentDescriptor != null) && (_selectedEnvironmentDescriptor.Metadata != null)) SelectedEnvironmentInfo = string.Format("Name: {0}\nTime Period: {1}\nSource Overlay: {2}\nSource Bathymetry: {3}\nPoint count: {4:n}", Path.GetFileNameWithoutExtension(_selectedEnvironmentDescriptor.DataFilename), _selectedEnvironmentDescriptor.Metadata.TimePeriod, _selectedEnvironmentDescriptor.Metadata.OverlayFilename ?? "[Unknown]", _selectedEnvironmentDescriptor.Metadata.BathymetryName, _selectedEnvironmentDescriptor.Data.Locations.Count);
                IsEnvironmentFileSelected = _selectedEnvironmentDescriptor != null;
                if (SelectedEnvironmentBathymetryName != null) Task.Factory.StartNew(() => SelectedBathymetryDescriptor = (NAEMOBathymetryDescriptor)NAEMOBathymetryDescriptors[SelectedEnvironmentBathymetryName]);
                Task.Factory.StartNew(() => _dispatcher.InvokeIfRequired(DisplayEnvironment, DispatcherPriority.Normal));
            }
        }

        static readonly PropertyChangedEventArgs SelectedEnvironmentDescriptorChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedEnvironmentDescriptor);
        NAEMOEnvironmentDescriptor _selectedEnvironmentDescriptor;

        public string SelectedEnvironmentBathymetryName
        {
            get
            {
                return (_selectedEnvironmentDescriptor != null) && (_selectedEnvironmentDescriptor.Metadata != null)
                               ? _selectedEnvironmentDescriptor.Metadata.BathymetryName : null;
            }
        }

        public string SelectedEnvironmentName
        {
            get
            {
                return (_selectedEnvironmentDescriptor != null) && (_selectedEnvironmentDescriptor.Metadata != null)
                    ? Path.GetFileNameWithoutExtension(_selectedEnvironmentDescriptor.Metadata.Filename) : null;
            }
        }

        void DisplayEnvironment()
        {
            if ((!_allViewModelsAreReady) || (!_viewIsActivated)) return;
            if (_selectedEnvironmentDescriptor == null)
            {
                var overlayLayer = EnvironmentTabMapLayers.Find<OverlayShapeMapLayer>(LayerType.SoundSpeed, "Environment");
                if (overlayLayer != null) overlayLayer.IsChecked = false;
                return;
            }
            var samplePoints = _selectedEnvironmentDescriptor.Data.Locations.Select(samplePoint => new OverlayPoint(samplePoint));
            EnvironmentTabMapLayers.DisplayOverlayShapes("Environment", LayerType.SoundSpeed, Colors.Transparent, samplePoints, 6, PointSymbolType.Circle, true, null, false);
#if false
            //_dispatcher.InvokeIfRequired(DisplayWorldMap, DispatcherPriority.Normal);
            overlayLayer = EnvironmentTabMapLayers.Find<OverlayShapeMapLayer>(LayerType.SoundSpeed, "Environment") ?? new OverlayShapeMapLayer
            {
                Name = "Environment",
                CanBeRemoved = true,
                CanBeReordered = true,
                CanChangeAreaColor = false,
                CanChangeLineColor = true,
                LineWidth = 6,
                PointSymbolType = PointSymbolType.Circle,
                LayerType = LayerType.SoundSpeed,
            };
            if (_selectedEnvironmentDescriptor.Metadata == null) return;
            overlayLayer.Clear();
            overlayLayer.Add(samplePoints);
            overlayLayer.Done();
            overlayLayer.IsChecked = true;
            if (EnvironmentTabMapLayers.IndexOf(overlayLayer) == -1) EnvironmentTabMapLayers.Add(overlayLayer);
#endif
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
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

        #region NewEnvironmentCommand
        public SimpleCommand<object, object> NewEnvironmentCommand
        {
            get
            {
                return _newEnvironment ??
                       (_newEnvironment =
                        new SimpleCommand<object, object>(delegate { return IsRangeComplexSelected && SelectedOverlayDescriptor != null && SelectedBathymetryDescriptor != null; },
                                                          delegate { NewEnvironmentHandler(); }));
            }
        }

        SimpleCommand<object, object> _newEnvironment;

        void NewEnvironmentHandler()
        {
            var overlayName = Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename);
            var bathymetryName = Path.GetFileNameWithoutExtension(SelectedBathymetryDescriptor.DataFilename);
            var vm = new EnvironmentExtractionViewModel(overlayName, bathymetryName);
            var result = _visualizerService.ShowDialog("EnvironmentExtractionView", vm);
            if ((!result.HasValue) || (!result.Value)) return;
            var bathymetry = SelectedBathymetryDescriptor.Data;
            var maxDepth = new EarthCoordinate<float>(bathymetry.Minimum, Math.Abs(bathymetry.Minimum.Data));
            var extractionArea = new GeoRect(SelectedOverlayDescriptor.Data.Shapes[0].BoundingBox);
            var tempPath = Path.GetTempPath().Remove(Path.GetTempPath().Length - 1);
            if (!Directory.Exists(tempPath)) Directory.CreateDirectory(tempPath);

            var assemblyLocation = Assembly.GetCallingAssembly().Location;
            var extractionPath = Path.GetDirectoryName(assemblyLocation);
            if (extractionPath == null) throw new ApplicationException("Extraction path can't be null!");

            var gdemExtractionProgramPath = Path.Combine(extractionPath, "ImportGDEM.exe");
            var gdemRequiredSupportFiles = new List<string>
            {
                Path.Combine(extractionPath, "netcdf.dll"),
                Path.Combine(extractionPath, "NetCDF_Wrapper.dll")
            };

            var extendedMonthlySoundSpeeds = new SoundSpeed();
            var extendedAndAveragedSoundSpeeds = new SoundSpeed();
            var monthlyTemperature = new SoundSpeed();
            var monthlySalinity = new SoundSpeed();

            var soundSpeedExtractors = new List<GDEMBackgroundExtractor>();

            var selectedTimePeriods = vm.EnvironmentDescriptors.Select(t => t.TimePeriod).Distinct().ToList();
            var requiredMonths = selectedTimePeriods.Select(Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod).ToList();
            var allMonths = new List<NAVOTimePeriod>();
            foreach (var curPeriod in requiredMonths) allMonths.AddRange(curPeriod);
            var uniqueMonths = allMonths.Distinct().ToList();
            uniqueMonths.Sort();
            var environmentPath = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, SelectedRangeComplexDescriptor.Data.Name, "Environment");
            var dataPath = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, SelectedRangeComplexDescriptor.Data.Name, "Data");
            Directory.CreateDirectory(dataPath);
            BackgroundTaskAggregator = new BackgroundTaskAggregator();

            Debug.Assert(bathymetryName != null, "bathymetryName != null");
            var bathyName = bathymetryName.EndsWith("_bathy") ? bathymetryName.Remove(bathymetryName.Length - 6, 6) : bathymetryName;
            var naemoEnvironmentExporters = selectedTimePeriods.Select(t => new CASSBackgroundExporter
            {
                WorkerSupportsCancellation = false,
                BathymetryFileName = Path.GetFileName(SelectedBathymetryDescriptor.DataFilename),
                OverlayFileName = Path.GetFileName(SelectedOverlayDescriptor.DataFilename),
                TimePeriod = t,
                ExtractionArea = extractionArea,
                NAVOConfiguration = Globals.AppSettings.NAVOConfiguration,
                DestinationPath = Path.Combine(environmentPath, string.Format("{0}_env_{1}", bathyName, t.ToString().ToLower())),
                UseExpandedExtractionArea = false,
                ExportHFEVA = vm.GenerateHFEVA,
                ExportHFBL = vm.GenerateHFBL,
                ExportLFBLHFB = vm.GenerateLFBLHFB,
                ExportLFBLPE = vm.GenerateLFBLPE,
                TaskName = "Export NAEMO environment for " + t,
            }).ToList();
            foreach (var exporter in naemoEnvironmentExporters)
            {
                exporter.RunWorkerCompleted += (s, e) =>
                {
                    var curExporter = (CASSBackgroundExporter)s;
                    
                    lock (NAEMOEnvironmentDescriptors)
                    {
                        string environmentFilename;
                        NAEMOEnvironmentDescriptor environmentDescriptor;
                        if (curExporter.ExportHFEVA)
                        {
                            environmentFilename = string.Format("{0}.dat", curExporter.DestinationPath);
                            environmentDescriptor = new NAEMOEnvironmentDescriptor
                            {
                                DataFilename = environmentFilename,
                                Metadata = new NAEMOEnvironmentMetadata
                                {
                                    BathymetryName = Path.GetFileNameWithoutExtension(curExporter.BathymetryFileName),
                                    OverlayFilename = Path.GetFileNameWithoutExtension(curExporter.OverlayFileName),
                                    TimePeriod = curExporter.TimePeriod,
                                    Bounds = extractionArea,
                                    Filename = NAEMOMetadataBase.MetadataFilename(environmentFilename),
                                }
                            };
                            environmentDescriptor.Metadata.Save();
                            NAEMOEnvironmentDescriptors.Add(new KeyValuePair<string, NAEMOEnvironmentDescriptor>(Path.GetFileNameWithoutExtension(environmentFilename), environmentDescriptor));
                        }
                        if (curExporter.ExportHFBL)
                        {
                            environmentFilename = string.Format("{0}-hfbl.dat", curExporter.DestinationPath);
                            environmentDescriptor = new NAEMOEnvironmentDescriptor
                            {
                                DataFilename = environmentFilename,
                                Metadata = new NAEMOEnvironmentMetadata
                                {
                                    BathymetryName = Path.GetFileNameWithoutExtension(curExporter.BathymetryFileName),
                                    OverlayFilename = Path.GetFileNameWithoutExtension(curExporter.OverlayFileName),
                                    TimePeriod = curExporter.TimePeriod,
                                    Bounds = extractionArea,
                                    Filename = NAEMOMetadataBase.MetadataFilename(environmentFilename),
                                }
                            };
                            environmentDescriptor.Metadata.Save();
                            NAEMOEnvironmentDescriptors.Add(new KeyValuePair<string, NAEMOEnvironmentDescriptor>(Path.GetFileNameWithoutExtension(environmentFilename), environmentDescriptor));
                        }
                        if (curExporter.ExportLFBLHFB)
                        {
                            environmentFilename = string.Format("{0}-lfbl-hfb.dat", curExporter.DestinationPath);
                            environmentDescriptor = new NAEMOEnvironmentDescriptor
                            {
                                DataFilename = environmentFilename,
                                Metadata = new NAEMOEnvironmentMetadata
                                {
                                    BathymetryName = Path.GetFileNameWithoutExtension(curExporter.BathymetryFileName),
                                    OverlayFilename = Path.GetFileNameWithoutExtension(curExporter.OverlayFileName),
                                    TimePeriod = curExporter.TimePeriod,
                                    Bounds = extractionArea,
                                    Filename = NAEMOMetadataBase.MetadataFilename(environmentFilename),
                                }
                            };
                            environmentDescriptor.Metadata.Save();
                            NAEMOEnvironmentDescriptors.Add(new KeyValuePair<string, NAEMOEnvironmentDescriptor>(Path.GetFileNameWithoutExtension(environmentFilename), environmentDescriptor));
                        }
                        if (curExporter.ExportLFBLPE)
                        {
                            environmentFilename = string.Format("{0}-lfbl-pe.dat", curExporter.DestinationPath);
                            environmentDescriptor = new NAEMOEnvironmentDescriptor
                            {
                                DataFilename = environmentFilename,
                                Metadata = new NAEMOEnvironmentMetadata
                                {
                                    BathymetryName = Path.GetFileNameWithoutExtension(curExporter.BathymetryFileName),
                                    OverlayFilename = Path.GetFileNameWithoutExtension(curExporter.OverlayFileName),
                                    TimePeriod = curExporter.TimePeriod,
                                    Bounds = extractionArea,
                                    Filename = NAEMOMetadataBase.MetadataFilename(environmentFilename),
                                }
                            };
                            environmentDescriptor.Metadata.Save();
                            NAEMOEnvironmentDescriptors.Add(new KeyValuePair<string, NAEMOEnvironmentDescriptor>(Path.GetFileNameWithoutExtension(environmentFilename), environmentDescriptor));
                        }
                        NAEMOBathymetryDescriptors.Sort();
                        SelectedEnvironmentDescriptor = null;
                    }
                };
            }

            var windExtractor = new SMGCBackgroundExtractor
            {
                WorkerSupportsCancellation = false,
                ExtractionArea = extractionArea,
                SelectedTimePeriods = selectedTimePeriods,
                NAVOConfiguration = Globals.AppSettings.NAVOConfiguration,
                UseExpandedExtractionArea = false,
                TaskName = "Wind data extraction",
            };
            BackgroundTaskAggregator.BackgroundTasks.Add(windExtractor);
            windExtractor.RunWorkerCompleted += (s, e) => { foreach (var naemo in naemoEnvironmentExporters) naemo.Wind = ((SMGCBackgroundExtractor)s).Wind; };

            // Create a sediment extractor
            var sedimentExtractor = new BSTBackgroundExtractor
            {
                WorkerSupportsCancellation = false,
                ExtractionArea = extractionArea,
                NAVOConfiguration = Globals.AppSettings.NAVOConfiguration,
                UseExpandedExtractionArea = false,
                TaskName = "Sediment data extraction",
            };
            sedimentExtractor.RunWorkerCompleted += (s, e) => { foreach (var naemo in naemoEnvironmentExporters) naemo.Sediment = ((BSTBackgroundExtractor)s).Sediment; };
            BackgroundTaskAggregator.BackgroundTasks.Add(sedimentExtractor);

            if (vm.GenerateHFBL || vm.GenerateLFBLHFB || vm.GenerateLFBLPE)
            {
                var bottomLossExtractor = new BottomLossBackgroundExtractor
                {
                    UseHFBL = vm.GenerateHFBL || vm.GenerateLFBLHFB,
                    UseLFBL = vm.GenerateLFBLHFB || vm.GenerateLFBLPE,
                    WorkerSupportsCancellation = false,
                    ExtractionArea = extractionArea,
                    NAVOConfiguration = Globals.AppSettings.NAVOConfiguration,
                    UseExpandedExtractionArea = false,
                    TaskName = "Bottom loss extraction",
                };
                //bottomLossExtractor.RunWorkerCompleted += (s, e) => { foreach (var naemo in naemoEnvironmentExporters) naemo.BottomLossSample = ((BottomLossBackgroundExtractor)s).BottomLossData; };
                BackgroundTaskAggregator.BackgroundTasks.Add(bottomLossExtractor);
            }

            //var temperatureAndSalinityFileWriter = new TemperatureAndSalinityFileWriter
            //{
            //        WorkerSupportsCancellation = false,
            //        DestinationPath = tempPath,
            //        TaskName = "Save soundspeed data"
            //};
            var averagers = selectedTimePeriods.Select(timePeriod => new SoundSpeedBackgroundAverager
            {
                WorkerSupportsCancellation = false,
                TimePeriod = timePeriod,
                ExtractionArea = extractionArea,
                NAVOConfiguration = Globals.AppSettings.NAVOConfiguration,
                UseExpandedExtractionArea = false,
                TaskName = "Calculate extended sound speeds for " + timePeriod,
            }).ToList();

            foreach (var month in uniqueMonths)
            {
                var soundSpeedExtractor = new GDEMBackgroundExtractor
                {
                    WorkerSupportsCancellation = false,
                    TimePeriod = month,
                    ExtractionArea = extractionArea,
                    NAVOConfiguration = Globals.AppSettings.NAVOConfiguration,
                    DestinationPath = tempPath,
                    UseExpandedExtractionArea = false,
                    ExtractionProgramPath = gdemExtractionProgramPath,
                    MaxDepth = maxDepth,
                    PointExtractionMode = false,
                };
                soundSpeedExtractor.RunWorkerCompleted += (sender, e) =>
                {
                    var extractor = (GDEMBackgroundExtractor)sender;
                    monthlyTemperature.SoundSpeedFields.Add(extractor.TemperatureField);
                    monthlySalinity.SoundSpeedFields.Add(extractor.SalinityField);
                    extendedMonthlySoundSpeeds.SoundSpeedFields.Add(extractor.ExtendedSoundSpeedField);
                    //Console.WriteLine("soundspeed extractor for {0} complete. {1} extractors are still busy", extractor.TimePeriod, soundSpeedExtractors.Where(s => s.IsBusy).Count());
                    if (soundSpeedExtractors.Any(ssfExtractor => ssfExtractor.IsBusy)) return;
                    //temperatureAndSalinityFileWriter.Temperature = monthlyTemperature;
                    //temperatureAndSalinityFileWriter.Salinity = monthlySalinity;
                    foreach (var averager in averagers) averager.ExtendedMonthlySoundSpeeds = extendedMonthlySoundSpeeds;
                };
                soundSpeedExtractors.Add(soundSpeedExtractor);
                BackgroundTaskAggregator.BackgroundTasks.Add(soundSpeedExtractor);
            }
            //BackgroundTaskAggregator.BackgroundTasks.Add(temperatureAndSalinityFileWriter);
            foreach (var averager in averagers)
            {
                BackgroundTaskAggregator.BackgroundTasks.Add(averager);
                averager.RunWorkerCompleted += (sender, e) =>
                {
                    var avg = (SoundSpeedBackgroundAverager)sender;
                    extendedAndAveragedSoundSpeeds.SoundSpeedFields.Add(avg.ExtendedAverageSoundSpeedField);

                    var averageTemperatures = new SoundSpeed();
                    averageTemperatures.SoundSpeedFields.Add(SoundSpeed.Average(monthlyTemperature, avg.TimePeriod));
                    averageTemperatures.Save(Path.Combine(dataPath, string.Format("{0}_env_{1}.temperature", bathyName, avg.TimePeriod.ToString().ToLower())));
                    
                    var averageSalinities = new SoundSpeed();
                    averageSalinities.SoundSpeedFields.Add(SoundSpeed.Average(monthlySalinity, avg.TimePeriod));
                    averageSalinities.Save(Path.Combine(dataPath, string.Format("{0}_env_{1}.salinity", bathyName, avg.TimePeriod.ToString().ToLower())));
                    
                    var averageSoundSpeed = new SoundSpeed();
                    averageSoundSpeed.SoundSpeedFields.Add(avg.ExtendedAverageSoundSpeedField);
                    averageSoundSpeed.Save(Path.Combine(dataPath, string.Format("{0}_env_{1}.soundspeed", bathyName, avg.TimePeriod.ToString().ToLower())));

                    if (averagers.Any(a => a.IsBusy)) return;
                    foreach (var naemo in naemoEnvironmentExporters) naemo.ExtendedAndAveragedSoundSpeeds = extendedAndAveragedSoundSpeeds;
                };
            }
            foreach (var naemo in naemoEnvironmentExporters)
                BackgroundTaskAggregator.BackgroundTasks.Add(naemo);
            BackgroundTaskAggregator.TaskName = "Environmental data extraction";
        }

        #endregion

        #region DeleteEnvironmentCommand
        public SimpleCommand<object, object> DeleteEnvironmentCommand
        {
            get { return _deleteEnvironment ?? (_deleteEnvironment = new SimpleCommand<object, object>(delegate { return IsEnvironmentFileSelected; }, delegate { DeleteEnvironmentHandler(); })); }
        }

        SimpleCommand<object, object> _deleteEnvironment;

        void DeleteEnvironmentHandler()
        {
            var message = new StringBuilder();
            message.Append("Warning: You are about to delete the environment data \"" + Path.GetFileNameWithoutExtension(SelectedEnvironmentDescriptor.Metadata.Filename) + "\"");

            message.Append("\n\nThis operation CANNOT be undone.\n\nProceed with deletion?");

            var result = _messageBoxService.ShowYesNo(message.ToString(), CustomDialogIcons.Exclamation);

            if (result == CustomDialogResults.No) return;
            NAEMOEnvironmentDescriptors.DeleteEnvironment(SelectedEnvironmentDescriptor);
        }
        #endregion
        #endregion
#endif

    }
}
