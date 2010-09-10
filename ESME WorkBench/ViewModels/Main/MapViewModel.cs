using System;
using System.ComponentModel.Composition;
using System.IO;
using System.Reflection;
using System.Windows;
using Cinch;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Ribbon;
using MEFedMVVM.Common;
using MEFedMVVM.ViewModelLocator;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Main
{
    [ExportViewModel("MapViewModel")]
    public class MapViewModel : ViewModelBase, IDesignTimeAware
    {
        private readonly IMessageBoxService _messageBoxService;
        private readonly IViewAwareStatus _viewAwareStatusService;
        private WpfMap _map;

        [ImportingConstructor]
        public MapViewModel(IViewAwareStatus viewAwareStatusService, IMessageBoxService messageBoxService)
        {
            _viewAwareStatusService = viewAwareStatusService;
            _viewAwareStatusService.ViewLoaded += ViewAwareStatusServiceViewLoaded;
            _messageBoxService = messageBoxService;

            ToggleBaseMapDisplayCommand = new SimpleCommand<object, object>(ExecuteToggleBaseMapDisplayCommand);
            ToggleGridOverlayDisplayCommand = new SimpleCommand<object, object>(ExecuteToggleGridOverlayDisplayCommand);
            TogglePanZoomDisplayCommand = new SimpleCommand<object, object>(ExecuteTogglePanZoomDisplayCommand);
            ClearAllLayersCommand = new SimpleCommand<object, object>(ExecuteClearAllLayersCommand);
        }

        public LayerViewModel BaseMapViewModel { get; private set; }
        public LayerViewModel GridOverlayViewModel { get; private set; }

        public SimpleCommand<Object, Object> ToggleBaseMapDisplayCommand { get; private set; }
        public SimpleCommand<Object, Object> ToggleGridOverlayDisplayCommand { get; private set; }
        public SimpleCommand<Object, Object> TogglePanZoomDisplayCommand { get; private set; }
        public SimpleCommand<Object, Object> ClearAllLayersCommand { get; private set; }

        #region IDesignTimeAware Members

        public void DesignTimeInitialization()
        {
            throw new NotImplementedException();
        }

        #endregion

        private void ViewAwareStatusServiceViewLoaded()
        {
            if (Designer.IsInDesignMode)
                return;

            //_messageBoxService.ShowInformation("ViewModel created successfully");
            if ((_viewAwareStatusService == null) || (_viewAwareStatusService.View == null)) return;

            _map = ((MainWindow) _viewAwareStatusService.View).Map1;
            _map.MapUnit = GeographyUnit.DecimalDegree;
            _map.MapTools.PanZoomBar.HorizontalAlignment = HorizontalAlignment.Left;
            _map.MapTools.PanZoomBar.VerticalAlignment = VerticalAlignment.Top;
            _map.MapTools.Logo.IsEnabled = false;

            var appPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);

            BaseMapViewModel = new ShapefileLayerViewModel(Path.Combine(appPath, @"Sample GIS Data\Countries02.shp"), this)
                               {
                                   IsChecked = Properties.Settings.Default.ShowBasemap
                               };
            GridOverlayViewModel = new AdornmentLayerViewModel("Grid", new MyGraticuleAdornmentLayer(), this)
                                   {
                                       IsChecked = Properties.Settings.Default.ShowGrid
                                   };

            _map.MapTools.PanZoomBar.IsEnabled = Properties.Settings.Default.ShowPanZoom;

            _map.ZoomToScale(_map.ZoomLevelScales[3]);
        }

        public GeoCollection<Overlay> Overlays { get { return _map.Overlays; } }
        public AdornmentOverlay AdornmentOverlay { get { return _map.AdornmentOverlay; } }
        public void Refresh() {_map.Refresh();}

        private void ExecuteToggleBaseMapDisplayCommand(Object args)
        {
            var source = (CheckBoxDataViewModel) args;
            BaseMapViewModel.IsChecked = source.IsChecked;
            Properties.Settings.Default.ShowBasemap = source.IsChecked;
        }

        private void ExecuteToggleGridOverlayDisplayCommand(Object args)
        {
            var source = (CheckBoxDataViewModel) args;
            GridOverlayViewModel.IsChecked = source.IsChecked;
            Properties.Settings.Default.ShowGrid = source.IsChecked;
        }

        private void ExecuteTogglePanZoomDisplayCommand(Object args)
        {
            var source = (CheckBoxDataViewModel) args;
            _map.MapTools.PanZoomBar.IsEnabled = source.IsChecked;
            Properties.Settings.Default.ShowPanZoom = source.IsChecked;
        }

        private void ExecuteClearAllLayersCommand(Object args)
        {
            Overlays.Clear();
            AdornmentOverlay.Layers.Clear();
            BaseMapViewModel = null;
            GridOverlayViewModel = null;
            ViewAwareStatusServiceViewLoaded();
        }
#if false
        //Function for getting the extent based on a collection of layers.
        //It gets the overall extent of all the layers.
        private RectangleShape GetFullExtent(IEnumerable<Layer> layers)
        {
            var rectangleShapes = new Collection<BaseShape>();

            foreach (var layer in layers)
            {
                layer.Open();
                if (layer.HasBoundingBox) rectangleShapes.Add(layer.GetBoundingBox());
            }
            return ExtentHelper.GetBoundingBoxOfItems(rectangleShapes);
        }
#endif
    }
}