using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.IO;
using System.Reflection;
using System.Windows;
using Cinch;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.RecentFiles;
using ESMEWorkBench.ViewModels.Ribbon;
using MEFedMVVM.Common;
using MEFedMVVM.ViewModelLocator;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Main
{
    [ExportViewModel("MainViewModel")]
    public class MainViewModel : ViewModelBase, IDesignTimeAware
    {
        #region Data

        private readonly IMessageBoxService _messageBoxService;
        private readonly IOpenFileService _openFileService;
        private readonly IViewAwareStatus _viewAwareStatusService;

        //private WpfMap _map;

        #endregion

        private static readonly PropertyChangedEventArgs RibbonViewModelChangedEventArgs =
            ObservableHelper.CreateArgs<MainViewModel>(x => x.RibbonViewModel);

        private RibbonViewModel _ribbonViewModel;

        #region Ctor

        [ImportingConstructor]
        public MainViewModel(IViewAwareStatus viewAwareStatusService, IMessageBoxService messageBoxService,
                             IOpenFileService openFileService)
        {
            _viewAwareStatusService = viewAwareStatusService;
            _viewAwareStatusService.ViewLoaded += ViewAwareStatusServiceViewLoaded;
            _messageBoxService = messageBoxService;
            _openFileService = openFileService;

            MapViewModel = new MapViewModel(_viewAwareStatusService, _messageBoxService);

            AddShapefileCommand = new SimpleCommand<object, object>(ExecuteAddShapefileCommand);
            AddOverlayFileCommand = new SimpleCommand<object, object>(ExecuteAddOverlayFileCommand);
            AddScenarioFileCommand = new SimpleCommand<object, object>(ExecuteAddScenarioFileCommand);
            DisabledCommand = new SimpleCommand<object, object>(CanNeverExecuteCommand, ExecuteDisabledCommand);

            CreateRibbonBindings();
            //RibbonViewModel.RecentExperiments.InsertFile(@"C:\Users\Dave Anderson\Documents\ESME WorkBench\test.esme");
        }

        #region public LayerDisplayViewModel LayerDisplayViewModel { get; set; }

        public LayerDisplayViewModel LayerDisplayViewModel
        {
            get { return _layerDisplayViewModel; }
            set
            {
                if (_layerDisplayViewModel == value) return;
                _layerDisplayViewModel = value;
                NotifyPropertyChanged(LayersChangedEventArgs);
            }
        }

        private LayerDisplayViewModel _layerDisplayViewModel;

        private static readonly PropertyChangedEventArgs LayersChangedEventArgs =
            ObservableHelper.CreateArgs<MainViewModel>(x => x.LayerDisplayViewModel);

        #endregion

        #region public MapViewModel MapViewModel { get; set; }

        public MapViewModel MapViewModel
        {
            get { return _mapViewModel; }
            set
            {
                if (_mapViewModel == value) return;
                _mapViewModel = value;
                NotifyPropertyChanged(MapViewModelChangedEventArgs);
            }
        }
        private MapViewModel _mapViewModel;
        private static readonly PropertyChangedEventArgs MapViewModelChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.MapViewModel);

        #endregion

        private void ViewAwareStatusServiceViewLoaded()
        {
#if false
            if (Designer.IsInDesignMode)
                return;

            //_messageBoxService.ShowInformation("ViewModel created successfully");
            if ((_viewAwareStatusService == null) || (_viewAwareStatusService.View == null)) return;

            _map = ((MainWindow) _viewAwareStatusService.View).Map1;
            _map.MapUnit = GeographyUnit.DecimalDegree;
            _map.MapTools.PanZoomBar.HorizontalAlignment = HorizontalAlignment.Left;
            _map.MapTools.PanZoomBar.VerticalAlignment = VerticalAlignment.Top;
            _map.MapTools.Logo.IsEnabled = false;

            string appPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);

            LayerDisplayViewModel = new LayerDisplayViewModel(_map);

            LayerDisplayViewModel.Layers.Add(new ShapefileLayerViewModel(_map,
                                                                         Path.Combine(appPath,
                                                                                      @"Sample GIS Data\Countries02.shp"))
                                             {IsBaseMapLayer = true});
            LayerDisplayViewModel.Layers.Add(new AdornmentLayerViewModel(_map, "Grid", new MyGraticuleAdornmentLayer())
                                             {IsAdornmentLayer = true});

            _layerOverlay = new LayerOverlay {TileType = TileType.SingleTile};

            var worldLayer = new ShapeFileFeatureLayer(Path.Combine(appPath, @"Sample GIS Data\Countries02.shp"));
            worldLayer.ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = AreaStyles.Country1;
            worldLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;

            _layerOverlay.Layers.Add("WorldLayer", worldLayer);
            _map.Overlays.Add("Layers", _layerOverlay);
            _map.CurrentExtent = GetFullExtent(_layerOverlay.Layers);

            var graticuleAdornmentLayer = new MyGraticuleAdornmentLayer();

            _map.AdornmentOverlay.Layers.Add(graticuleAdornmentLayer);
            RectangleShape(-180, 90, 180, -90);
            _map.ZoomToScale(_map.ZoomLevelScales[3]);
            //_map.CurrentExtent = _map.MaxExtent;
#endif
        }

        #endregion

        #region Commands

        public SimpleCommand<Object, Object> AddShapefileCommand { get; private set; }

        public SimpleCommand<Object, Object> AddOverlayFileCommand { get; private set; }

        public SimpleCommand<Object, Object> AddScenarioFileCommand { get; private set; }

        public SimpleCommand<Object, Object> DisabledCommand { get; private set; }

        private void ExecuteAddShapefileCommand(Object args)
        {
            _openFileService.Filter = "ESRI Shapefiles (*.shp)|*.shp";
            bool? result = _openFileService.ShowDialog(null);
            if (!result.HasValue || !result.Value) return;
            var overlayLayer = new ShapefileLayerViewModel(_openFileService.FileName, MapViewModel);
            _layerDisplayViewModel.Layers.Add(overlayLayer);
        }

        private void ExecuteAddOverlayFileCommand(Object args)
        {
            _openFileService.Filter = "NUWC Overlay Files (*.ovr)|*.ovr";
            bool? result = _openFileService.ShowDialog(null);
            if (!result.HasValue || !result.Value) return;
            try
            {
                var overlayLayer = new OverlayFileLayerViewModel(_openFileService.FileName, MapViewModel);
                _layerDisplayViewModel.Layers.Add(overlayLayer);
            }
            catch (Exception e)
            {
                _messageBoxService.ShowError(string.Format("Error opening Overlay File {0}:\n{1}",
                                                           _openFileService.FileName, e.Message));
            }
        }

        private void ExecuteAddScenarioFileCommand(Object args)
        {
            _openFileService.Filter = "NUWC Scenario Files (*.nemo)|*.nemo";
            bool? result = _openFileService.ShowDialog(null);
            if (!result.HasValue || !result.Value) return;
            //NemoFile nemoFile;
            try
            {
                var overlayLayer = new ScenarioFileLayerViewModel(_openFileService.FileName,
                                                                  @"C:\Users\Dave Anderson\Desktop\Scenario Builder 1.5.508\Sim Areas", MapViewModel);
                _layerDisplayViewModel.Layers.Add(overlayLayer);
                //nemoFile = new NemoFile(_openFileService.FileName, @"C:\Users\Dave Anderson\Desktop\Scenario Builder 1.5.508\Sim Areas");
            }
            catch (Exception ex)
            {
                _messageBoxService.ShowError("Error opening scenario file: " + ex.Message);
                return;
            }
        }

        private static bool CanNeverExecuteCommand(Object args)
        {
            return false;
        }

        private static void ExecuteDisabledCommand(Object args)
        {
        }

#if false
        void AddShape(string layerName, OverlayShape shape)
        {
            var newLayer = new InMemoryFeatureLayer();
            newLayer.InternalFeatures.Add(layerName, new Feature(BaseShape.CreateShapeFromWellKnownData(shape.WellKnownText)));
            newLayer.ZoomLevelSet.ZoomLevel01.DefaultLineStyle.OuterPen = new GeoPen(GeoColor.FromArgb(shape.Color.A, shape.Color.R, shape.Color.G, shape.Color.B), shape.Width);
            newLayer.ZoomLevelSet.ZoomLevel01.DefaultPointStyle.SymbolPen = new GeoPen(GeoColor.FromArgb(shape.Color.A, shape.Color.R, shape.Color.G, shape.Color.B), shape.Width);
            newLayer.ZoomLevelSet.ZoomLevel01.DefaultPointStyle.SymbolSize = shape.Width;
            newLayer.ZoomLevelSet.ZoomLevel01.DefaultPointStyle.SymbolType = PointSymbolType.Circle;
            newLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            _layerOverlay.Layers.Add(newLayer);
        }
#endif

        #endregion

        public RibbonViewModel RibbonViewModel
        {
            get { return _ribbonViewModel; }

            private set
            {
                if (_ribbonViewModel == value) return;
                _ribbonViewModel = value;
                NotifyPropertyChanged(RibbonViewModelChangedEventArgs);
            }
        }

        #region IDesignTimeAware Members

        void IDesignTimeAware.DesignTimeInitialization()
        {
            CreateRibbonBindings();
        }

        #endregion

        #region Map utility functions

        //Function for getting the extent based on a collection of layers.
        //It gets the overall extent of all the layers.
        private static RectangleShape GetFullExtent(IEnumerable<Layer> layers)
        {
            var rectangleShapes = new Collection<BaseShape>();

            foreach (Layer layer in layers)
            {
                layer.Open();
                if (layer.HasBoundingBox) rectangleShapes.Add(layer.GetBoundingBox());
            }
            return ExtentHelper.GetBoundingBoxOfItems(rectangleShapes);
        }

        #endregion

        #region Create ribbon tabs, groups, and controls

        private void CreateRibbonBindings()
        {
            RibbonViewModel = new RibbonViewModel
                              {
                                  ApplicationMenuItems =
                                      new ApplicationMenuItemList
                                      {
                                          new ApplicationMenuItemDataViewModel
                                          {
                                              RecentFiles =
                                                  new RecentFileList
                                                  {MaxNumberOfFiles = 9, Persister = new RegistryPersister(),},
                                              MenuItems =
                                                  new MenuItemList
                                                  {
                                                      new MenuItemDataViewModel
                                                      {
                                                          Label = "Save Experiment",
                                                          LargeImage =
                                                              new Uri("Images/LargeIcons/save-icon.png",
                                                                      UriKind.Relative),
                                                          SmallImage =
                                                              new Uri("Images/SmallIcons/save-icon.png",
                                                                      UriKind.Relative),
                                                          ToolTipTitle = "Save Experiment",
                                                          ToolTipDescription = "Save the current experiment",
                                                          Command = DisabledCommand,
                                                      },
                                                      new MenuItemDataViewModel
                                                      {
                                                          Label = "Open Experiment",
                                                          LargeImage =
                                                              new Uri("Images/LargeIcons/open-icon.png",
                                                                      UriKind.Relative),
                                                          SmallImage =
                                                              new Uri("Images/SmallIcons/open-icon.png",
                                                                      UriKind.Relative),
                                                          ToolTipTitle = "Open Experiment",
                                                          ToolTipDescription =
                                                              "Open a previously saved experiment from an experiment file",
                                                          Command = DisabledCommand,
                                                      },
                                                      new MenuItemDataViewModel
                                                      {
                                                          Label = "Close Experiment",
                                                          LargeImage =
                                                              new Uri("Images/LargeIcons/close-icon.png",
                                                                      UriKind.Relative),
                                                          SmallImage =
                                                              new Uri("Images/SmallIcons/close-icon.png",
                                                                      UriKind.Relative),
                                                          ToolTipTitle = "Close Experiment",
                                                          ToolTipDescription = "Close the current experiment",
                                                          Command = DisabledCommand,
                                                      },
                                                      new MenuItemDataViewModel
                                                      {
                                                          Label = "New Experiment",
                                                          LargeImage =
                                                              new Uri("Images/LargeIcons/new-icon.png", UriKind.Relative),
                                                          SmallImage =
                                                              new Uri("Images/SmallIcons/new-icon.png", UriKind.Relative),
                                                          ToolTipTitle = "New Experiment",
                                                          ToolTipDescription = "Create a new experiment",
                                                          Command = DisabledCommand,
                                                      },
                                                      new MenuItemDataViewModel
                                                      {
                                                          Label = "Info",
                                                          LargeImage =
                                                              new Uri("Images/LargeIcons/about-icon.png",
                                                                      UriKind.Relative),
                                                          SmallImage =
                                                              new Uri("Images/SmallIcons/about-icon.png",
                                                                      UriKind.Relative),
                                                          ToolTipTitle = "Info",
                                                          ToolTipDescription = "Experiment information",
                                                          Command = DisabledCommand,
                                                      },
                                                      new MenuItemDataViewModel
                                                      {
                                                          Label = "Options",
                                                          LargeImage =
                                                              new Uri("Images/LargeIcons/Options.png", UriKind.Relative),
                                                          SmallImage =
                                                              new Uri("Images/SmallIcons/Options.png", UriKind.Relative),
                                                          ToolTipTitle = "Options",
                                                          ToolTipDescription = "Edit application options and settings",
                                                          Command = DisabledCommand,
                                                      },
                                                  },
                                          },
                                      },
                                  Tabs = new TabList
                                         {
                                             #region Experiment Tab
                                             new TabDataViewModel
                                             {
                                                 Header = "Experiment",
                                                 Groups =
                                                     new GroupList
                                                     {
                                                         new GroupDataViewModel
                                                         {
                                                             Label = "Scenario",
                                                             Controls =
                                                                 new ControlList
                                                                 {
                                                                     new ButtonDataViewModel
                                                                     {
                                                                         Label = "Load",
                                                                         LargeImage =
                                                                             new Uri("Images/LargeIcons/AddFile.png",
                                                                                     UriKind.Relative),
                                                                         SmallImage =
                                                                             new Uri("Images/SmallIcons/AddFile.png",
                                                                                     UriKind.Relative),
                                                                         ToolTipTitle = "Load Scenario File (Ctrl+L)",
                                                                         ToolTipDescription =
                                                                             "Load a scenario file into the simulation.",
                                                                         Command = DisabledCommand,
                                                                         KeyTip = "L",
                                                                     },
                                                                     new ButtonDataViewModel
                                                                     {
                                                                         Label = "Edit",
                                                                         LargeImage =
                                                                             new Uri("Images/LargeIcons/new-icon.png",
                                                                                     UriKind.Relative),
                                                                         SmallImage =
                                                                             new Uri("Images/SmallIcons/new-icon.png",
                                                                                     UriKind.Relative),
                                                                         ToolTipTitle = "Edit Scenario File (Ctrl+E)",
                                                                         ToolTipDescription =
                                                                             "Edit the scenario file with the Scenario Builder.",
                                                                         Command = DisabledCommand,
                                                                         KeyTip = "E",
                                                                     },
                                                                 },
                                                         },
                                                         new GroupDataViewModel
                                                         {
                                                             Label = "Data",
                                                             Controls =
                                                                 new ControlList
                                                                 {
                                                                     new ButtonDataViewModel
                                                                     {
                                                                        Label = "Reset",
                                                                        LargeImage =
                                                                            new Uri(
                                                                            "Images/LargeIcons/new-icon.png",
                                                                            UriKind.Relative),
                                                                        SmallImage =
                                                                            new Uri(
                                                                            "Images/SmallIcons/new-icon.png",
                                                                            UriKind.Relative),
                                                                        ToolTipTitle =
                                                                            "Reset",
                                                                        ToolTipDescription =
                                                                            "Clear all layers that have been added to the map",
                                                                        Command = MapViewModel.ClearAllLayersCommand,
                                                                     },
                                                                     new ButtonDataViewModel
                                                                     {
                                                                        Label = "Shapefile",
                                                                        SmallImage =
                                                                            new Uri(
                                                                            "Images/SmallIcons/AddFile.png",
                                                                            UriKind.Relative),
                                                                        ToolTipTitle =
                                                                            "Shapefile",
                                                                        ToolTipDescription =
                                                                            "Add an ESRI Shapefile to the map",
                                                                        Command = AddShapefileCommand,
                                                                     },
                                                                     new ButtonDataViewModel
                                                                     {
                                                                        Label = "Overlay",
                                                                        SmallImage =
                                                                            new Uri(
                                                                            "Images/SmallIcons/AddFile.png",
                                                                            UriKind.Relative),
                                                                        ToolTipTitle =
                                                                            "Overlay",
                                                                        ToolTipDescription =
                                                                            "Add a NUWC Overlay file to the map",
                                                                        Command = AddOverlayFileCommand,
                                                                     },
                                                                     new ButtonDataViewModel
                                                                     {
                                                                        Label = "Scenario",
                                                                        SmallImage =
                                                                            new Uri(
                                                                            "Images/SmallIcons/AddFile.png",
                                                                            UriKind.Relative),
                                                                        ToolTipTitle =
                                                                            "Scenario",
                                                                        ToolTipDescription =
                                                                            "Add a NUWC Scenario file to the map",
                                                                        //Command = AddScenarioFileCommand,
                                                                        Command = DisabledCommand,
                                                                     },
                                                                 },
                                                         },
                                                         new GroupDataViewModel
                                                         {
                                                             Label = "Show",
                                                             Controls =
                                                                 new ControlList
                                                                 {
                                                                     new CheckBoxDataViewModel
                                                                     {
                                                                         Label = "Base Map",
                                                                         ToolTipTitle = "Base Map",
                                                                         ToolTipDescription =
                                                                             "Toggles the base map display",
                                                                         IsChecked = true,
                                                                         Command = MapViewModel.ToggleBaseMapDisplayCommand,
                                                                     },
                                                                     new CheckBoxDataViewModel
                                                                     {
                                                                         Label = "Grid Overlay",
                                                                         ToolTipTitle = "Grid Overlay",
                                                                         ToolTipDescription =
                                                                             "Toggles the grid overlay",
                                                                         IsChecked = true,
                                                                         Command = MapViewModel.ToggleGridOverlayDisplayCommand,
                                                                     },
                                                                     new CheckBoxDataViewModel
                                                                     {
                                                                         Label = "Pan/Zoom",
                                                                         ToolTipTitle = "Pan/Zoom",
                                                                         ToolTipDescription =
                                                                             "Toggles the pan/zoom control",
                                                                         IsChecked = true,
                                                                         Command = MapViewModel.TogglePanZoomDisplayCommand,
                                                                     },
                                                                 },
                                                         },
                                                         new GroupDataViewModel
                                                         {
                                                             Label = "Sounds",
                                                             Controls =
                                                                 new ControlList
                                                                 {
                                                                     new ButtonDataViewModel
                                                                     {
                                                                         Label = "Analysis Point",
                                                                         LargeImage =
                                                                             new Uri(
                                                                             "Images/LargeIcons/bullet-2-icon.png",
                                                                             UriKind.Relative),
                                                                         SmallImage =
                                                                             new Uri(
                                                                             "Images/SmallIcons/bullet-2-icon.png",
                                                                             UriKind.Relative),
                                                                         ToolTipTitle = "Analysis Point",
                                                                         ToolTipDescription =
                                                                             "Add a new analysis point to the experiment (not functional)",
                                                                         Command = DisabledCommand,
                                                                     },
                                                                     new ButtonDataViewModel
                                                                     {
                                                                         Label = "Fixed Source",
                                                                         LargeImage =
                                                                             new Uri("Images/LargeIcons/Sound.png",
                                                                                     UriKind.Relative),
                                                                         SmallImage =
                                                                             new Uri("Images/SmallIcons/Sound.png",
                                                                                     UriKind.Relative),
                                                                         ToolTipTitle = "Fixed Source",
                                                                         ToolTipDescription =
                                                                             "Add a new fixed sound source to the experiment (not functional)",
                                                                         Command = DisabledCommand,
                                                                     },
                                                                     new ButtonDataViewModel
                                                                     {
                                                                         Label = "Quick Look",
                                                                         LargeImage =
                                                                             new Uri(
                                                                             "Images/LargeIcons/Button-Play-icon.png",
                                                                             UriKind.Relative),
                                                                         SmallImage =
                                                                             new Uri(
                                                                             "Images/SmallIcons/Button-Play-icon.png",
                                                                             UriKind.Relative),
                                                                         ToolTipTitle = "Quick Look",
                                                                         ToolTipDescription =
                                                                             "Add a new quick look point to the experiment (not functional)",
                                                                         Command = DisabledCommand,
                                                                     },
                                                                 },
                                                         },
                                                     },
                                             },

                                             #endregion
                                             #region Scenario Tab
                                             new TabDataViewModel
                                             {
                                                 Header = "Scenario",
                                                 Groups =
                                                     new GroupList
                                                     {
                                                         new GroupDataViewModel
                                                         {
                                                             Label = "Scenario File",
                                                             Controls =
                                                                 new ControlList
                                                                 {
                                                                     new ButtonDataViewModel
                                                                     {
                                                                         Label = "Open",
                                                                         LargeImage =
                                                                             new Uri("Images/LargeIcons/open-icon.png",
                                                                                     UriKind.Relative),
                                                                         SmallImage =
                                                                             new Uri("Images/SmallIcons/open-icon.png",
                                                                                     UriKind.Relative),
                                                                         ToolTipTitle = "Open scenario file",
                                                                         ToolTipDescription =
                                                                             "Open a scenario file (not functional)",
                                                                         Command = DisabledCommand,
                                                                     },
                                                                     new ButtonDataViewModel
                                                                     {
                                                                         Label = "Close",
                                                                         LargeImage =
                                                                             new Uri("Images/LargeIcons/close-icon.png",
                                                                                     UriKind.Relative),
                                                                         SmallImage =
                                                                             new Uri("Images/SmallIcons/close-icon.png",
                                                                                     UriKind.Relative),
                                                                         ToolTipTitle = "Close scenario file",
                                                                         ToolTipDescription =
                                                                             "Close the current scenario file (not functional)",
                                                                         Command = DisabledCommand,
                                                                     },
                                                                     new ButtonDataViewModel
                                                                     {
                                                                         Label = "Edit",
                                                                         LargeImage =
                                                                             new Uri("Images/LargeIcons/AddFile.png",
                                                                                     UriKind.Relative),
                                                                         SmallImage =
                                                                             new Uri("Images/SmallIcons/AddFile.png",
                                                                                     UriKind.Relative),
                                                                         ToolTipTitle = "Edit scenario file",
                                                                         ToolTipDescription =
                                                                             "Launch the scenario editor (not functional)",
                                                                         Command = DisabledCommand,
                                                                     },
                                                                 },
                                                         },
                                                     },
                                             },

                                             #endregion
                                             #region Environment Tab
                                             new TabDataViewModel
                                             {
                                                 Header = "Environment",
                                                 Groups =
                                                     new GroupList
                                                     {
                                                         new GroupDataViewModel
                                                         {
                                                             Label = "Location",
                                                             Controls =
                                                                 new ControlList
                                                                 {
                                                                     new ButtonDataViewModel
                                                                     {
                                                                         Label = "Open",
                                                                         LargeImage =
                                                                             new Uri("Images/LargeIcons/open-icon.png",
                                                                                     UriKind.Relative),
                                                                         SmallImage =
                                                                             new Uri("Images/SmallIcons/open-icon.png",
                                                                                     UriKind.Relative),
                                                                         ToolTipTitle = "Open environment file",
                                                                         ToolTipDescription =
                                                                             "Open an environment file (not functional)",
                                                                         Command = DisabledCommand,
                                                                     },
                                                                     new ButtonDataViewModel
                                                                     {
                                                                         Label = "Close",
                                                                         LargeImage =
                                                                             new Uri("Images/LargeIcons/close-icon.png",
                                                                                     UriKind.Relative),
                                                                         SmallImage =
                                                                             new Uri("Images/SmallIcons/close-icon.png",
                                                                                     UriKind.Relative),
                                                                         ToolTipTitle = "Close environment file",
                                                                         ToolTipDescription =
                                                                             "Close the current environment file (not functional)",
                                                                         Command = DisabledCommand,
                                                                     },
                                                                     new ButtonDataViewModel
                                                                     {
                                                                         Label = "Edit",
                                                                         LargeImage =
                                                                             new Uri("Images/LargeIcons/AddFile.png",
                                                                                     UriKind.Relative),
                                                                         SmallImage =
                                                                             new Uri("Images/SmallIcons/AddFile.png",
                                                                                     UriKind.Relative),
                                                                         ToolTipTitle = "Edit environment file",
                                                                         ToolTipDescription =
                                                                             "Launch the environment builder (not functional)",
                                                                         Command = DisabledCommand,
                                                                     },
                                                                 },
                                                         },
                                                         new GroupDataViewModel
                                                         {
                                                             Label = "Layers",
                                                             Controls = new ControlList
                                                                        {
                                                                            new ComboBoxDataViewModel
                                                                            {
                                                                                Label = "Wind",
                                                                                //LargeImage = new Uri("Images/LargeIcons/open-icon.png", UriKind.Relative),
                                                                                SmallImage =
                                                                                    new Uri(
                                                                                    "Images/SmallIcons/open-icon.png",
                                                                                    UriKind.Relative),
                                                                                ToolTipTitle = "Wind data layer",
                                                                                ToolTipDescription =
                                                                                    "Select a source for wind data (not functional)",
                                                                                IsEnabled = false,
                                                                                IsEditable = false,
                                                                            },
                                                                            new ComboBoxDataViewModel
                                                                            {
                                                                                Label = "Sound Speed",
                                                                                //LargeImage = new Uri("Images/LargeIcons/open-icon.png", UriKind.Relative),
                                                                                SmallImage =
                                                                                    new Uri(
                                                                                    "Images/SmallIcons/open-icon.png",
                                                                                    UriKind.Relative),
                                                                                ToolTipTitle = "Sound speed layer",
                                                                                ToolTipDescription =
                                                                                    "Select a source for sound speed data (not functional)",
                                                                                IsEnabled = false,
                                                                                IsEditable = false,
                                                                            },
                                                                            new ComboBoxDataViewModel
                                                                            {
                                                                                Label = "Sediment",
                                                                                //LargeImage = new Uri("Images/LargeIcons/open-icon.png", UriKind.Relative),
                                                                                SmallImage =
                                                                                    new Uri(
                                                                                    "Images/SmallIcons/open-icon.png",
                                                                                    UriKind.Relative),
                                                                                ToolTipTitle = "Sediment data layer",
                                                                                ToolTipDescription =
                                                                                    "Select a source for sediment data (not functional)",
                                                                                IsEnabled = false,
                                                                                IsEditable = false,
                                                                            },
                                                                            new ComboBoxDataViewModel
                                                                            {
                                                                                Label = "Bathymetry",
                                                                                //LargeImage = new Uri("Images/LargeIcons/open-icon.png", UriKind.Relative),
                                                                                SmallImage =
                                                                                    new Uri(
                                                                                    "Images/SmallIcons/open-icon.png",
                                                                                    UriKind.Relative),
                                                                                ToolTipTitle = "Bathymetry data layer",
                                                                                ToolTipDescription =
                                                                                    "Select a source for bathymetry data (not functional)",
                                                                                IsEnabled = false,
                                                                                IsEditable = false,
                                                                            },
                                                                        },
                                                         },
                                                     },
                                             },

                                             #endregion
                                             #region Animals Tab
                                             new TabDataViewModel {Header = "Animals",},

                                             #endregion
                                             #region Acoustics Tab
                                             new TabDataViewModel {Header = "Acoustics",},

                                             #endregion
                                             #region Reports Tab
                                             new TabDataViewModel {Header = "Reports",},

                                             #endregion
                                         },
                              };
        }

        #endregion
    }
}