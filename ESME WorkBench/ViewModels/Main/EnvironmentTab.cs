using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media;
using System.Windows.Threading;
using Cinch;
using ESME;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Environment.NAVO;
using ESME.Mapping;
using ESME.Metadata;
using ESME.NEMO.Overlay;
using ESME.TransmissionLoss.CASS;
using ESME.Views.EnvironmentBuilder;
using ESME.Views.Locations;
using ESME.Views.InstallationWizard;
using HRC.Navigation;
using HRC.Utility;
using Microsoft.Windows.Controls.Ribbon;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        [MediatorMessageSink(MediatorMessage.AllViewModelsAreReady)]
        void AllViewModelsAreReady(bool allViewModelsAreReady)
        {
            Console.WriteLine("All view models are ready!");
            WizardViewModel.LaunchWizardIfNeeded(_visualizerService);
            _dispatcher.InvokeIfRequired(DisplayWorldMap, DispatcherPriority.Normal);
            AreAllViewModelsReady = true;
            UpdateMapLayerVisibility();
            if (ESME.Globals.AppSettings.ScenarioDataDirectory == null) return;
            
            //_dispatcher.InvokeIfRequired(DisplayRangeComplex, DispatcherPriority.Normal);
            //_dispatcher.InvokeIfRequired(DisplayBathymetry, DispatcherPriority.Normal);
            //_dispatcher.InvokeIfRequired(DisplayOverlay, DispatcherPriority.Normal);
            //_dispatcher.InvokeIfRequired(DisplayEnvironment, DispatcherPriority.Normal);
        }

        public MapLayerCollection EnvironmentTabMapLayers { get; set; }

#if false
        T FindEnvironmentTabMapLayer<T>(LayerType layerType, string layerName) where T : class
        {
            if (EnvironmentTabMapLayers == null) return null;
            return EnvironmentTabMapLayers.Where(layer => layer.LayerType == layerType).Where(layer => layer.Name == layerName).FirstOrDefault() as T;
        }
#endif

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

        void DisplayWorldMap()
        {
            if (MapLayerCollections["Environment"] == null)
            {
#if true
                MapLayerCollections.Add("Environment", Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"Sample GIS Data\Countries02.shp"));
                EnvironmentTabMapLayers = MapLayerCollections["Environment"];
                //EnvironmentTabMapLayers = new MapLayerCollection(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"Sample GIS Data\Countries02.shp"));
                ZoomToWorldMap();
#else
                appPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
                EnvironmentTabMapLayers = new MapLayerCollection
                {
                        new ShapefileMapLayer
                        {
                                LayerType = LayerType.BaseMap,
                                LineColor = Colors.Beige,
                                AreaStyle = AreaStyles.Country2,
                                CanBeRemoved = false,
                                CanBeReordered = true,
                                CanChangeAreaColor = true,
                                CanChangeLineColor = true,
                                ShapefileName = Path.Combine(appPath, @"Sample GIS Data\Countries02.shp"),
                                Name = "Base Map",
                        },
                };
                ZoomToWorldMap();
#endif
            }
            if (MapLayerCollections["Home"] != null) return;
#if true
            MapLayerCollections.Add("Home", Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"Sample GIS Data\Countries02.shp"));
            //HomeTabMapLayers = new MapLayerCollection(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"Sample GIS Data\Countries02.shp"));
#else
            appPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
            HomeTabMapLayers = new MapLayerCollection
            {
                    new ShapefileMapLayer
                    {
                            LayerType = LayerType.BaseMap,
                            LineColor = Colors.Beige,
                            AreaStyle = AreaStyles.Country2,
                            CanBeRemoved = false,
                            CanBeReordered = true,
                            CanChangeAreaColor = true,
                            CanChangeLineColor = true,
                            ShapefileName = Path.Combine(appPath, @"Sample GIS Data\Countries02.shp"),
                            Name = "Base Map",
                    },
            };
#endif
        }

        void DisplayRangeComplex()
        {
            if ((!_allViewModelsAreReady) || (!_viewIsActivated)) return;
            OverlayShapeMapLayer opAreaLayer;
            OverlayShapeMapLayer simAreaLayer;
            if (_selectedRangeComplexDescriptor == null)
            {
                opAreaLayer = EnvironmentTabMapLayers.Find<OverlayShapeMapLayer>(LayerType.OverlayFile, "Op Area");
                if (opAreaLayer != null) opAreaLayer.IsChecked = false;
                simAreaLayer = EnvironmentTabMapLayers.Find<OverlayShapeMapLayer>(LayerType.OverlayFile, "Sim Area");
                if (simAreaLayer != null) simAreaLayer.IsChecked = false;
                return;
            }

            var opAreaOverlayFilename = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, _selectedRangeComplexDescriptor.Data.Name, "Areas", _selectedRangeComplexDescriptor.Data.OpsLimitFile);
            var simAreaOverlayFilename = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, _selectedRangeComplexDescriptor.Data.Name, "Areas", _selectedRangeComplexDescriptor.Data.SimLimitFile);
            ZoomToRangeComplex();
#if true
            EnvironmentTabMapLayers.DisplayOverlayFile("Op Area", opAreaOverlayFilename);
#else
            opAreaLayer = EnvironmentTabMapLayers.Find<OverlayShapeMapLayer>(LayerType.OverlayFile, "Op Area") ?? new OverlayShapeMapLayer
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
            opAreaLayer.IsChecked = true;
            if (EnvironmentTabMapLayers.IndexOf(opAreaLayer) == -1) EnvironmentTabMapLayers.Add(opAreaLayer);
#endif

            if (simAreaOverlayFilename != opAreaOverlayFilename)
            {
#if true
                EnvironmentTabMapLayers.DisplayOverlayFile("Sim Area", simAreaOverlayFilename);
#else
                simAreaLayer = EnvironmentTabMapLayers.Find<OverlayShapeMapLayer>(LayerType.OverlayFile, "Sim Area") ?? new OverlayShapeMapLayer
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
                simAreaLayer.IsChecked = true;
                if (EnvironmentTabMapLayers.IndexOf(simAreaLayer) == -1) EnvironmentTabMapLayers.Add(simAreaLayer);
#endif
            }
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }

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

        static readonly PropertyChangedEventArgs RangeComplexIsSelectedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsRangeComplexSelected);
        bool _isRangeComplexSelected;

        #endregion

        #region NewRangeComplexCommand
        public SimpleCommand<object, object> NewRangeComplexCommand
        {
            get
            {
                return _newRangeComplex ??
                       (_newRangeComplex =
                        new SimpleCommand<object, object>(delegate { return AreRangeComplexesLoaded; },
                                                          delegate { NewRangeComplexHandler(); }));
            }
        }

        SimpleCommand<object, object> _newRangeComplex;

        void NewRangeComplexHandler()
        {
            var vm = new NewRangeComplexViewModel(Globals.AppSettings);
            var result = _visualizerService.ShowDialog("NewRangeComplexView", vm);
            if ((result.HasValue) && (result.Value))
            {
                Task.Factory.StartNew(() =>
                {
                    SelectedRangeComplexDescriptor = RangeComplexDescriptors.CreateRangeComplex(vm.LocationName, vm.Height, vm.ReferencePointLatitude, vm.ReferencePointLongitude, vm.GeoidSeparation, vm.ExistingOpAreaOverlayFilename,
                                                               vm.NewOpAreaOverlayEarthCoordinates, vm.ExistingSimAreaOverlayFilename, vm.NewSimAreaOverlayEarthCoordinates, _dispatcher);
                    RangeComplexDescriptors.Sort();
                });
            }

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
            var opAreaOverlayFilename = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, _selectedRangeComplexDescriptor.Data.Name, "Areas", _selectedRangeComplexDescriptor.Data.OpsLimitFile);
            var opsLimit = new OverlayFile(opAreaOverlayFilename);
            var north = (float)opsLimit.Shapes[0].BoundingBox.Bottom + 3;
            var west = (float)opsLimit.Shapes[0].BoundingBox.Left - 3;
            var south = (float)opsLimit.Shapes[0].BoundingBox.Top - 3;
            var east = (float)opsLimit.Shapes[0].BoundingBox.Right + 3;
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
            var result =
                    _messageBoxService.ShowYesNo("Warning: Deleting the range complex \"" + SelectedRangeComplexDescriptor.Data.Name +
                                                 "\" will also delete any overlays, bathymetry and environment data that have previously been created or extracted.\n\nThis operation CANNOT be undone.\n\nProceed with deletion?", 
                                                 CustomDialogIcons.Exclamation);
            if (result == CustomDialogResults.No) return;
            RangeComplexDescriptors.DeleteRangeComplex(SelectedRangeComplexDescriptor);
            SelectedRangeComplexDescriptor = null;
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

        void DisplayOverlay()
        {
            if ((!_allViewModelsAreReady) || (!_viewIsActivated)) return;
            OverlayShapeMapLayer overlayLayer;
            if (_selectedOverlayDescriptor == null)
            {
                overlayLayer = EnvironmentTabMapLayers.Find<OverlayShapeMapLayer>(LayerType.OverlayFile, "Overlay");
                if (overlayLayer != null) overlayLayer.IsChecked = false;
                return;
            }
            //_dispatcher.InvokeIfRequired(DisplayWorldMap, DispatcherPriority.Normal);
            var overlayFilename = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, _selectedRangeComplexDescriptor.Data.Name, "Areas", Path.GetFileNameWithoutExtension(_selectedOverlayDescriptor.DataFilename) + ".ovr");
            EnvironmentTabMapLayers.DisplayOverlayFile("Overlay", overlayFilename);
#if false
		    overlayLayer = EnvironmentTabMapLayers.Find<OverlayShapeMapLayer>(LayerType.OverlayFile, "Overlay") ?? new OverlayShapeMapLayer
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
            overlayLayer.IsChecked = true;
            if (EnvironmentTabMapLayers.IndexOf(overlayLayer) == -1) EnvironmentTabMapLayers.Add(overlayLayer);
  
#endif            
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
            var vm = new NewOverlayViewModel(Globals.AppSettings, SelectedRangeComplexDescriptor.Data.Name);
            var result = _visualizerService.ShowDialog("NewOverlayView", vm);
            if ((!result.HasValue) || (!result.Value)) return;
            NAEMOOverlayDescriptors.CreateNewOverlay(SelectedRangeComplexDescriptor.Data.Name, Path.GetFileNameWithoutExtension(vm.OverlayName), vm.OverlayEarthCoordinates, vm.BoundingBox, 0, null);
            SelectedOverlayDescriptor = (NAEMOOverlayDescriptor)NAEMOOverlayDescriptors[Path.GetFileNameWithoutExtension(vm.OverlayName)];
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
            var originalBufferSize = 0f;
            if (!string.IsNullOrEmpty(SelectedOverlayDescriptor.Metadata.OverlayFilename))
            {
                originalBufferSize = SelectedOverlayDescriptor.Metadata.BufferZoneSize;
                SelectedOverlayDescriptor = (NAEMOOverlayDescriptor)NAEMOOverlayDescriptors[SelectedOverlayDescriptor.Metadata.OverlayFilename];
            }
            if (!IsOverlayFileSelected) return;
            var vm = new OverlayExpandViewModel(SelectedOverlayDescriptor.Metadata) {BufferSize = originalBufferSize};
            var result = _visualizerService.ShowDialog("OverlayExpandView", vm);
            if ((!result.HasValue) || (!result.Value)) return;
            
            var curOverlay = SelectedOverlayDescriptor.Data;
            //var limits = (Limits)(new GeoRect(curOverlay.Shapes[0].EarthCoordinates));
            var limits = new Limits(ConvexHull.Create(curOverlay.Shapes[0].EarthCoordinates, true));
            var expandedLimits = limits.CreateExpandedLimit(vm.BufferSize);  //in km.
            var boundingBox = new GeoRect(expandedLimits.GeoPointList);
            var coordinateList = expandedLimits.GeoPointList.Select(geo => new EarthCoordinate(geo)).ToList();
            var testShape = new OverlayLineSegments(coordinateList.ToArray(), Colors.Black);

            if (!testShape.IsUsableAsPerimeter) coordinateList = ConvexHull.Create(coordinateList, true);

            NAEMOOverlayDescriptors.CreateNewOverlay(SelectedRangeComplexDescriptor.Data.Name, vm.OverlayName, coordinateList, boundingBox, vm.BufferSize, Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename));
            SelectedOverlayDescriptor = (NAEMOOverlayDescriptor)NAEMOOverlayDescriptors[vm.OverlayName];
        }

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
#if false
            //_dispatcher.InvokeIfRequired(DisplayWorldMap, DispatcherPriority.Normal);
            bathyBitmapLayer = EnvironmentTabMapLayers.Find<RasterMapLayer>(LayerType.BathymetryRaster, "Bathymetry") ?? new RasterMapLayer
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
            bathyBitmapLayer.RasterFilename = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, SelectedRangeComplexDescriptor.Data.Name, "Images",
                                                           Path.GetFileNameWithoutExtension(_selectedBathymetryDescriptor.DataFilename) + ".bmp");
            bathyBitmapLayer.IsChecked = true;
            if (EnvironmentTabMapLayers.IndexOf(bathyBitmapLayer) == -1) EnvironmentTabMapLayers.Add(bathyBitmapLayer);
            MediatorMessage.Send(MediatorMessage.MoveLayerToBottom, bathyBitmapLayer);
#endif
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
            OverlayShapeMapLayer overlayLayer;
            if (_selectedEnvironmentDescriptor == null)
            {
                overlayLayer = EnvironmentTabMapLayers.Find<OverlayShapeMapLayer>(LayerType.SoundSpeed, "Environment");
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
            BackgroundTaskAggregator = new BackgroundTaskAggregator();

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
                    HighFrequencyExtractor = vm.GenerateHFBL || vm.GenerateLFBLHFB ? Globals.AppSettings.NAVOConfiguration.HFBLEXEPath : null,
                    LowFrequencyExtractor = vm.GenerateLFBLHFB || vm.GenerateLFBLPE ? Globals.AppSettings.NAVOConfiguration.LFBLEXEPath : null,
                    WorkerSupportsCancellation = false,
                    ExtractionArea = extractionArea,
                    NAVOConfiguration = Globals.AppSettings.NAVOConfiguration,
                    UseExpandedExtractionArea = false,
                    TaskName = "Bottom loss extraction",
                };
                bottomLossExtractor.RunWorkerCompleted += (s, e) => { foreach (var naemo in naemoEnvironmentExporters) naemo.BottomLossData = ((BottomLossBackgroundExtractor)s).BottomLossData; };
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
                    RequiredSupportFiles = gdemRequiredSupportFiles,
                    MaxDepth = maxDepth,
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
    }
}
