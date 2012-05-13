using System.IO;
using System.Reflection;
using System.Windows.Media;
using ESME;
using ESME.Mapping;
using ESMEWorkbench.Properties;
using ESMEWorkbench.ViewModels.Map;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

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
            AreaColor = Colors.Transparent,
            AreaStyle = MapLayerViewModel.CreateAreaStyle(Settings.Default.WorldMapLineColor, (float)Settings.Default.WorldMapLineWeight, Colors.Transparent),
            ShapefileName = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"Sample GIS Data\Countries02.shp"),
            Name = "World Map",
        };

        [Initialize] public ObservableList<WorldMapFeatureNode> Features { get; set; }
        #region Layer Move commands
        #region MoveLayerToFrontCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToFrontCommand { get { return _moveLayerToFront ?? (_moveLayerToFront = new SimpleCommand<object, EventToCommandArgs>(MoveLayerToFront)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerToFront;
        void MoveLayerToFront(EventToCommandArgs args)
        {
            MediatorMessage.Send(MediatorMessage.MoveLayerToFront, _worldMapLayer);
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion

        #region MoveLayerForwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerForwardCommand { get { return _moveLayerForward ?? (_moveLayerForward = new SimpleCommand<object, EventToCommandArgs>(MoveLayerForward)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerForward;
        void MoveLayerForward(EventToCommandArgs args)
        {
            MediatorMessage.Send(MediatorMessage.MoveLayerForward, _worldMapLayer);
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion

        #region MoveLayerBackwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerBackwardCommand { get { return _moveLayerBackward ?? (_moveLayerBackward = new SimpleCommand<object, EventToCommandArgs>(MoveLayerBackward)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerBackward;
        void MoveLayerBackward(EventToCommandArgs args)
        {
            MediatorMessage.Send(MediatorMessage.MoveLayerBackward, _worldMapLayer);
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion

        #region MoveLayerToBackCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToBackCommand { get { return _moveLayerToBack ?? (_moveLayerToBack = new SimpleCommand<object, EventToCommandArgs>(MoveLayerToBack)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerToBack;
        void MoveLayerToBack(EventToCommandArgs args)
        {
            MediatorMessage.Send(MediatorMessage.MoveLayerToBack, _worldMapLayer);
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion
        #endregion
    }
}