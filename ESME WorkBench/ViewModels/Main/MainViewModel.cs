using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel.Composition;
using System.IO;
using System.Windows;
using Cinch;
using ESME.NEMO;
using ESME.Overlay;
using ESME.Platform;
using ESMERibbonDemo.ViewModels.Layers;
using ESMERibbonDemo.ViewModels.Ribbon;
using MEFedMVVM.Common;
using MEFedMVVM.ViewModelLocator;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMERibbonDemo.ViewModels.Main
{
    [ExportViewModel("MainViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class MainViewModel : ViewModelBase, IDesignTimeAware
    {
        #region Data

        readonly IMessageBoxService _messageBoxService;
        readonly IOpenFileService _openFileService;
        readonly IViewAwareStatus _viewAwareStatusService;
        LayerOverlay _layerOverlay;
        WpfMap _map;
        //private bool _showContextMenu;

        #endregion

        public TabList Tabs { get; private set; }

        #region Ctor

        [ImportingConstructor]
        public MainViewModel(IViewAwareStatus viewAwareStatusService, IMessageBoxService messageBoxService, IOpenFileService openFileService)
        {
            _viewAwareStatusService = viewAwareStatusService;
            _viewAwareStatusService.ViewLoaded += ViewAwareStatusServiceViewLoaded;
            _messageBoxService = messageBoxService;
            _openFileService = openFileService;

            AddShapefileCommand = new SimpleCommand<object, object>(ExecuteAddShapefileCommand);
            AddOverlayFileCommand = new SimpleCommand<object, object>(ExecuteAddOverlayFileCommand);
            AddScenarioFileCommand = new SimpleCommand<object, object>(ExecuteAddScenarioFileCommand);

            CreateRibbonBindings();
        }

        void ViewAwareStatusServiceViewLoaded()
        {
            if (Designer.IsInDesignMode)
                return;

            //_messageBoxService.ShowInformation("ViewModel created successfully");
            if ((_viewAwareStatusService == null) || (_viewAwareStatusService.View == null)) return;

            _map = ((MainWindow) _viewAwareStatusService.View).Map1;
            _map.MapUnit = GeographyUnit.DecimalDegree;
            _map.MapTools.PanZoomBar.HorizontalAlignment = HorizontalAlignment.Left;
            _map.MapTools.PanZoomBar.VerticalAlignment = VerticalAlignment.Top;
            _layerOverlay = new LayerOverlay();
            _layerOverlay.TileType = TileType.SingleTile;
            var offsetProj = new OffsetProjection();

            var worldLayer = new ShapeFileFeatureLayer(@"Sample GIS Data\Countries02.shp");
            worldLayer.ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = AreaStyles.Country1;
            worldLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;

            //var offsetWorldLayer = new ShapeFileFeatureLayer(@"Sample GIS Data\Countries02.shp");
            //offsetWorldLayer.ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = AreaStyles.Country1;
            //offsetWorldLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            //offsetWorldLayer.FeatureSource.Projection = offsetProj;
            //var proj4Projection = new Proj4Projection();
            //proj4Projection.InternalProjectionParametersString = Proj4Projection.GetEpsgParametersString(4326);
            //proj4Projection.ExternalProjectionParametersString = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs";
            //worldLayer.FeatureSource.Projection = proj4Projection;
            //var proj = new ManagedProj4Projection();
            //proj.InternalProjectionParameters = ManagedProj4Projection.GetEpsgParameters(4326);
            //proj.ExternalProjectionParameters = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs";

            _layerOverlay.Layers.Add("WorldLayer", worldLayer);
            //_layerOverlay.Layers.Add("OffsetWorldLayer", offsetWorldLayer);
            _map.Overlays.Add("Layers", _layerOverlay);
            //_map.CurrentExtent = GetFullExtent(_layerOverlay.Layers);

            var graticuleAdornmentLayer = new MyGraticuleAdornmentLayer();
            //var northArrowAdornmentLayer = new RotatingAdornmentLayer(new GeoImage(@"Images\north_arrow.png"));

            _map.AdornmentOverlay.Layers.Add(graticuleAdornmentLayer);
            //_map.AdornmentOverlay.Layers.Add(northArrowAdornmentLayer);

            _map.Refresh();
            //String imagePath = ConfigurationManager.AppSettings["YourImagePath"].ToString();

            //var workspace1 = new WorkspaceData(@"/CinchV2DemoWPF;component/Images/imageIcon.png",
            //    "ImageLoaderView", imagePath, "Image View", true);

            //var workspace2 = new WorkspaceData(@"/CinchV2DemoWPF;component/Images/About.png",
            //        "AboutView", null, "About Cinch V2", true);

            //Views.Add(workspace1);
            //Views.Add(workspace2);
            //SetActiveWorkspace(workspace1);
        }

        #endregion

        #region Commands

        public SimpleCommand<Object, Object> AddShapefileCommand { get; private set; }

        public SimpleCommand<Object, Object> AddOverlayFileCommand { get; private set; }

        public SimpleCommand<Object, Object> AddScenarioFileCommand { get; private set; }

        void ExecuteAddShapefileCommand(Object args)
        {
            string projection = null;
            _openFileService.Filter = "ESRI Shapefiles (*.shp)|*.shp";
            bool? result = _openFileService.ShowDialog(null);
            if (!result.HasValue || !result.Value) return;
            string projectionFile = Path.Combine(Path.GetDirectoryName(_openFileService.FileName), "projection.txt");
            if (File.Exists(projectionFile))
            {
                using (var sr = new StreamReader(projectionFile))
                    projection = sr.ReadToEnd();
            }
            var newLayer = new ShapeFileFeatureLayer(_openFileService.FileName);
            newLayer.ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = AreaStyles.County1;
            newLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            newLayer.RequireIndex = false;
            if (projection != null)
                newLayer.FeatureSource.Projection = new ManagedProj4Projection {InternalProjectionParameters = projection, ExternalProjectionParameters = ManagedProj4Projection.GetEpsgParameters(4326),};

            _layerOverlay.Layers.Add(_openFileService.FileName, newLayer);
            _layerOverlay.Refresh();
            _map.Refresh();
        }

        void ExecuteAddOverlayFileCommand(Object args)
        {
            _openFileService.Filter = "NUWC Overlay Files (*.ovr)|*.ovr";
            bool? result = _openFileService.ShowDialog(null);
            if (!result.HasValue || !result.Value) return;
            var overlayFile = new OverlayFile(_openFileService.FileName);
            foreach (OverlayShape shape in overlayFile.Shapes)
                AddShape(_openFileService.FileName, shape);
            _map.Refresh();
        }

        void ExecuteAddScenarioFileCommand(Object args)
        {
            _openFileService.Filter = "NUWC Scenario Files (*.nemo)|*.nemo";
            bool? result = _openFileService.ShowDialog(null);
            if (!result.HasValue || !result.Value) return;
            NemoFile nemoFile;
            try
            {
                nemoFile = new NemoFile(_openFileService.FileName, @"C:\Users\Dave Anderson\Desktop\Scenario Builder 1.5.508\Sim Areas");
            }
            catch (Exception ex)
            {
                _messageBoxService.ShowError("Error opening scenario file: " + ex.Message);
                return;
            }
            int shapeCount = 0;
            foreach (OverlayShape shape in nemoFile.Scenario.OverlayFile.Shapes)
                AddShape("Overlay: " + nemoFile.Scenario.OverlayFile.FileName + "." + shapeCount++, shape);
            int platformCount = 0;
            foreach (NemoPlatform platform in nemoFile.Scenario.Platforms)
            {
                var behavior = new BehaviorModel(platform);
                AddShape("Platform " + platformCount + ": " + platform.Name + " course track", behavior.CourseOverlay);
                AddShape("Platform " + platformCount + ": " + platform.Name + " start", behavior.CourseStart);
                AddShape("Platform " + platformCount + ": " + platform.Name + " end", behavior.CourseEnd);
                foreach (NemoTrackdef trackdef in platform.Trackdefs)
                {
                    foreach (OverlayShape shape in trackdef.OverlayFile.Shapes)
                        AddShape("Platform " + platformCount + ": " + platform.Name + " operational area", shape);
                }
                platformCount++;
            }
            _map.Refresh();
        }

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

        #endregion

        #region IDesignTimeAware Members

        void IDesignTimeAware.DesignTimeInitialization()
        {
            CreateRibbonBindings();
        }

        #endregion

        #region Map utility functions

        //Function for getting the extent based on a collection of layers.
        //It gets the overall extent of all the layers.
        RectangleShape GetFullExtent(IEnumerable<Layer> layers)
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

        void CreateRibbonBindings()
        {
            Tabs = new TabList
            {
                #region Experiment Tab
                new TabDataViewModel
                {
                    Header = "Experiment",
                    Groups = new GroupList
                    {
                        new GroupDataViewModel
                        {
                            Label = "Scenario",
                            Controls = new ControlList
                            {
                                new ButtonDataViewModel
                                {
                                    Label = "Load",
                                    SmallImage = new Uri("Images/SmallIcons/AddFile.png", UriKind.Relative),
                                    LargeImage = new Uri("Images/LargeIcons/AddFile.png", UriKind.Relative),
                                    ToolTipTitle = "Load Scenario File (Ctrl+L)",
                                    ToolTipDescription = "Load a scenario file into the simulation.",
                                    //Command = ApplicationCommands.Cut,
                                    KeyTip = "L",
                                },
                                new ButtonDataViewModel
                                {
                                    Label = "Edit",
                                    SmallImage = new Uri("Images/SmallIcons/new-icon.png", UriKind.Relative),
                                    LargeImage = new Uri("Images/LargeIcons/new-icon.png", UriKind.Relative),
                                    ToolTipTitle = "Edit Scenario File (Ctrl+E)",
                                    ToolTipDescription = "Edit the scenario file with the Scenario Builder.",
                                    //Command = ApplicationCommands.Cut,
                                    KeyTip = "E",
                                },
                            },
                        },
                        new GroupDataViewModel
                        {
                            Label = "Map",
                            Controls = new ControlList
                            {
                                new MenuButtonDataViewModel
                                {
                                    Label = "Base Map",
                                    SmallImage = new Uri("Images/SmallIcons/System-Globe-icon.png", UriKind.Relative),
                                    LargeImage = new Uri("Images/LargeIcons/System-Globe-icon.png", UriKind.Relative),
                                    ToolTipTitle = "Base Map settings",
                                    ToolTipDescription = "Select the base map image",
                                    //Command = ApplicationCommands.Paste,
                                    MenuItems =
                                        new MenuItemList
                                        {
                                            new MenuItemDataViewModel
                                            {
                                                Label = "NASA 1 minute topographic map",
                                                SmallImage = new Uri("Images/SmallIcons/System-Map-icon.png", UriKind.Relative),
                                                LargeImage = new Uri("Images/LargeIcons/System-Map-icon.png", UriKind.Relative),
                                                ToolTipTitle = "Base Map Settings",
                                                ToolTipDescription = "Use this as the base map image",
                                                //Command = ApplicationCommands.Paste,
                                            },
                                            new MenuItemDataViewModel
                                            {
                                                Label = "Custom base map",
                                                SmallImage = new Uri("Images/SmallIcons/System-Map-icon.png", UriKind.Relative),
                                                LargeImage = new Uri("Images/LargeIcons/System-Map-icon.png", UriKind.Relative),
                                                ToolTipTitle = "Base Map Settings",
                                                ToolTipDescription = "Choose your own base map image\nNote that this map must must be full global coverage\nleft edge 180W, right edge 180E, top 90N, bottom 90S, Mercator projection",
                                                //Command = ApplicationCommands.Paste,
                                            },
                                        },
                                },
                                new MenuButtonDataViewModel
                                {
                                    Label = "Add Content",
                                    SmallImage = new Uri("Images/SmallIcons/Plus.png", UriKind.Relative),
                                    LargeImage = new Uri("Images/LargeIcons/Plus.png", UriKind.Relative),
                                    ToolTipTitle = "Add Content to the map",
                                    ToolTipDescription = "Select the type of content you wish to add to the map",
                                    MenuItems =
                                        new MenuItemList
                                        {
                                            new MenuItemDataViewModel
                                            {
                                                Label = "ESRI Shapefile (*.shp)", 
                                                SmallImage = new Uri("Images/SmallIcons/Layers-icon.png", UriKind.Relative), 
                                                LargeImage = new Uri("Images/LargeIcons/Layers-icon.png", UriKind.Relative), 
                                                ToolTipTitle = "Add Content to the map", 
                                                ToolTipDescription = "Add an ESRI Shapefile to the map", 
                                                Command = AddShapefileCommand,
                                            },
                                            new MenuItemDataViewModel
                                            {
                                                Label = "NUWC Overlay File (*.ovr)", 
                                                SmallImage = new Uri("Images/SmallIcons/Layers-icon.png", UriKind.Relative), 
                                                LargeImage = new Uri("Images/LargeIcons/Layers-icon.png", UriKind.Relative), 
                                                ToolTipTitle = "Add Content to the map", 
                                                ToolTipDescription = "Add a NUWC Overlay file to the map", 
                                                Command = AddOverlayFileCommand,
                                            },
                                            new MenuItemDataViewModel
                                            {
                                                Label = "NUWC Scenario File (*.nemo)", 
                                                SmallImage = new Uri("Images/SmallIcons/Layers-icon.png", UriKind.Relative), 
                                                LargeImage = new Uri("Images/LargeIcons/Layers-icon.png", UriKind.Relative), 
                                                ToolTipTitle = "Add Content to the map", 
                                                ToolTipDescription = "Add a NUWC Scenario file to the map", 
                                                Command = AddScenarioFileCommand,
                                            },
                                        },
                                },
                                new MenuButtonDataViewModel
                                {
                                    Label = "Pan/Zoom Control",
                                    SmallImage = new Uri("Images/SmallIcons/System-Map-icon.png", UriKind.Relative),
                                    LargeImage = new Uri("Images/LargeIcons/System-Map-icon.png", UriKind.Relative),
                                    ToolTipTitle = "Pan/Zoom Control",
                                    ToolTipDescription = "Change the visibility and position of the pan/zoom control",
                                    MenuItems =
                                        new MenuItemList
                                        {
                                            new MenuItemDataViewModel
                                            {
                                                Label = "Visible", 
                                                ToolTipTitle = "Pan/Zoom Control", 
                                                ToolTipDescription = "Change the visibility of the pan/zoom control", 
                                                //Command = AddShapefileCommand,
                                            },
                                            new MenuItemDataViewModel
                                            {
                                                Label = "Upper Left", 
                                                ToolTipTitle = "Pan/Zoom Control", 
                                                ToolTipDescription = "Move the pan/zoom control to the upper left corner of the map display", 
                                                //Command = AddOverlayFileCommand,
                                            },
                                            new MenuItemDataViewModel
                                            {
                                                Label = "Upper Right", 
                                                ToolTipTitle = "Pan/Zoom Control", 
                                                ToolTipDescription = "Move the pan/zoom control to the upper right corner of the map display", 
                                                //Command = AddOverlayFileCommand,
                                            },
                                            new MenuItemDataViewModel
                                            {
                                                Label = "Lower Left", 
                                                ToolTipTitle = "Pan/Zoom Control", 
                                                ToolTipDescription = "Move the pan/zoom control to the lower left corner of the map display", 
                                                //Command = AddOverlayFileCommand,
                                            },
                                            new MenuItemDataViewModel
                                            {
                                                Label = "Lower Right", 
                                                ToolTipTitle = "Pan/Zoom Control", 
                                                ToolTipDescription = "Move the pan/zoom control to the lower right corner of the map display", 
                                                //Command = AddOverlayFileCommand,
                                            },
                                        },
                                },
                            },
                        },
                    },
                },

                #endregion
                new TabDataViewModel {Header = "Scenario",},
                new TabDataViewModel {Header = "Environment",},
                new TabDataViewModel {Header = "Animals",},
                new TabDataViewModel {Header = "Acoustics",},
                new TabDataViewModel {Header = "Reports",},
            };
        }

        #endregion
    }
}