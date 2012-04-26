using System.IO;
using System.Reflection;
using System.Windows.Media;
using ESME.Mapping;
using ESMEWorkbench.Properties;
using ESMEWorkbench.ViewModels.Map;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class WorldMapNode : TreeNodeBase
    {
        public WorldMapNode(MapViewModel mapViewModel)
        {
            MapViewModel = mapViewModel;
            Features.Add(new WorldMapFeatureNode("Pan/Zoom", () => MapViewModel.IsPanZoomVisible, v => MapViewModel.IsPanZoomVisible = v));
            Features.Add(new WorldMapFeatureNode("Lat/Lon Grid", () => MapViewModel.IsGridVisible, v => MapViewModel.IsGridVisible = v));
            Features.Add(new WorldMapFeatureNode("Scale", () => MapViewModel.IsScaleVisible, v => MapViewModel.IsScaleVisible = v));
            MapViewModel.Add(_worldMapLayer);
            MapViewModel.SetIsVisible(_worldMapLayer, Settings.Default.ShowWorldMap);
        }

        [Affects("LineColor", "LineWeight", "IsChecked", "Features")]
        public MapViewModel MapViewModel { get; set; }

        public Color LineColor
        {
            get { return Settings.Default.WorldMapLineColor; }
            set
            {
                _worldMapLayer.LineColor = value;
                Settings.Default.WorldMapLineColor = value;
            }
        }

        public double LineWeight
        {
            get { return Settings.Default.WorldMapLineWeight; }
            set
            {
                _worldMapLayer.LineWidth = (float)value;
                Settings.Default.WorldMapLineWeight = value;
            }
        }

        public bool IsVisible
        {
            get { return Settings.Default.ShowWorldMap; }
            set
            {
                Settings.Default.ShowWorldMap = value;
                MapViewModel.SetIsVisible(_worldMapLayer, value);
            }
        }

        readonly MapLayerViewModel _worldMapLayer = new ShapefileMapLayer
        {
            LayerType = LayerType.BaseMap,
            AreaColor = Colors.Transparent,
            AreaStyle = MapLayerViewModel.CreateAreaStyle(Settings.Default.WorldMapLineColor, (float)Settings.Default.WorldMapLineWeight, Colors.Transparent),
            CanBeRemoved = false,
            CanBeReordered = true,
            CanChangeAreaColor = true,
            CanChangeLineColor = true,
            ShapefileName = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"Sample GIS Data\Countries02.shp"),
            Name = "World Map",
        };

        [Initialize] public ObservableList<WorldMapFeatureNode> Features { get; set; }
    }
}