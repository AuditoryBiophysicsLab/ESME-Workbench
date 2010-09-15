using System;
using System.IO;
using System.Reflection;
using System.Windows;
using Cinch;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Ribbon;
using MEFedMVVM.Common;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Main
{
    public class MapViewModel : ViewModelBase
    {
        readonly IViewAwareStatus _viewAwareStatusService;
        readonly IOpenFileService _openFileService;
        readonly IMessageBoxService _messageBoxService;
        private WpfMap _map;
        
        public string MapDLLVersion { get; private set; }
        public LayerDisplayViewModel LayerDisplayViewModel { get; private set; }

        public MapViewModel(IViewAwareStatus viewAwareStatusService, IMessageBoxService messageBoxService, IOpenFileService openFileService)
        {
            _viewAwareStatusService = viewAwareStatusService;
            _viewAwareStatusService.ViewLoaded += ViewAwareStatusServiceViewLoaded;
            _messageBoxService = messageBoxService;
            _openFileService = openFileService;

            LayerDisplayViewModel = new LayerDisplayViewModel(this);

            ToggleBaseMapDisplayCommand = new SimpleCommand<object, object>(delegate (Object args)
            {
                var source = (CheckBoxDataViewModel)args;
                BaseMapViewModel.IsChecked = source.IsChecked;
                Properties.Settings.Default.ShowBasemap = source.IsChecked;
            });

            ToggleGridOverlayDisplayCommand = new SimpleCommand<object, object>(delegate(Object args)
            {
                var source = (CheckBoxDataViewModel)args;
                GridOverlayViewModel.IsChecked = source.IsChecked;
                Properties.Settings.Default.ShowGrid = source.IsChecked;
            });

            TogglePanZoomDisplayCommand = new SimpleCommand<object, object>(delegate(Object args)
            {
                var source = (CheckBoxDataViewModel)args;
                _map.MapTools.PanZoomBar.Visibility = source.IsChecked ? Visibility.Visible : Visibility.Hidden;
                Properties.Settings.Default.ShowPanZoom = source.IsChecked;
            });

            ClearAllLayersCommand = new SimpleCommand<object, object>(delegate
            {
                Overlays.Clear();
                AdornmentOverlay.Layers.Clear();
                BaseMapViewModel = null;
                GridOverlayViewModel = null;
                ViewAwareStatusServiceViewLoaded();
            });

            AddShapefileCommand = new SimpleCommand<object, object>(delegate
            {
                _openFileService.Filter = "ESRI Shapefiles (*.shp)|*.shp";
                var result = _openFileService.ShowDialog(null);
                if (!result.HasValue || !result.Value) return;
                var overlayLayer = new ShapefileLayerViewModel(_openFileService.FileName, this);
                LayerDisplayViewModel.Layers.Add(overlayLayer);
            });

            AddOverlayFileCommand = new SimpleCommand<object, object>(delegate
            {
                _openFileService.Filter = "NUWC Overlay Files (*.ovr)|*.ovr";
                var result = _openFileService.ShowDialog(null);
                if (!result.HasValue || !result.Value) return;
                try
                {
                    var overlayLayer = new OverlayFileLayerViewModel(_openFileService.FileName, this);
                    LayerDisplayViewModel.Layers.Add(overlayLayer);
                }
                catch (Exception e)
                {
                    _messageBoxService.ShowError(string.Format("Error opening Overlay File {0}:\n{1}",
                                                               _openFileService.FileName, e.Message));
                }
            });

            AddScenarioFileCommand = new SimpleCommand<object, object>(delegate
            {
                _openFileService.Filter = "NUWC Scenario Files (*.nemo)|*.nemo";
                var result = _openFileService.ShowDialog(null);
                if (!result.HasValue || !result.Value) return;
                //NemoFile nemoFile;
                try
                {
                    var overlayLayer = new ScenarioFileLayerViewModel(_openFileService.FileName, MainViewModel.AppSettings.ScenarioDataDirectory, this);
                    LayerDisplayViewModel.Layers.Add(overlayLayer);
                    //nemoFile = new NemoFile(_openFileService.FileName, @"C:\Users\Dave Anderson\Desktop\Scenario Builder 1.5.508\Sim Areas");
                }
                catch (Exception ex)
                {
                    _messageBoxService.ShowError("Error opening scenario file: " + ex.Message);
                    return;
                }
            });
        }

        public LayerViewModel BaseMapViewModel { get; private set; }
        public LayerViewModel GridOverlayViewModel { get; private set; }

        public SimpleCommand<Object, Object> ToggleBaseMapDisplayCommand { get; private set; }
        public SimpleCommand<Object, Object> ToggleGridOverlayDisplayCommand { get; private set; }
        public SimpleCommand<Object, Object> TogglePanZoomDisplayCommand { get; private set; }
        public SimpleCommand<Object, Object> ClearAllLayersCommand { get; private set; }

        public SimpleCommand<Object, Object> AddShapefileCommand { get; private set; }
        public SimpleCommand<Object, Object> AddOverlayFileCommand { get; private set; }
        public SimpleCommand<Object, Object> AddScenarioFileCommand { get; private set; }

        
        private void ViewAwareStatusServiceViewLoaded()
        {
            if (Designer.IsInDesignMode)
                return;

            //_messageBoxService.ShowInformation("ViewModel created successfully");
            if ((_viewAwareStatusService == null) || (_viewAwareStatusService.View == null)) return;

            _map = ((MainWindow) _viewAwareStatusService.View).Map1;
            MapDLLVersion = WpfMap.GetVersion();
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

            _map.MapTools.PanZoomBar.Visibility = Properties.Settings.Default.ShowPanZoom ? Visibility.Visible : Visibility.Hidden;

            _map.CurrentExtent = new RectangleShape(new PointShape(-180, 90), new PointShape(180, -90));
            _map.ZoomToScale(_map.ZoomLevelScales[3]);
        }

        public GeoCollection<Overlay> Overlays { get { return _map.Overlays; } }
        public AdornmentOverlay AdornmentOverlay { get { return _map.AdornmentOverlay; } }
        public void Refresh() {_map.Refresh();}

    }
}