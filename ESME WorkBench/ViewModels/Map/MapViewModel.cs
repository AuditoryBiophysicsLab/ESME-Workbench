using System;
using System.Linq;
using System.Reactive.Linq;
using System.Windows;
using System.Windows.Input;
using ESME;
using ESME.Environment;
using ESME.Mapping;
using ESME.Views.Locations;
using ESMEWorkbench.Properties;
using ESMEWorkbench.ViewModels.Layers;
using ESMEWorkbench.ViewModels.Main;
using ESMEWorkbench.Views;
using HRC;
using HRC.Navigation;
using HRC.Services;
using HRC.ViewModels;
using HRC.WPF;
using MEFedMVVM.Common;
using MEFedMVVM.ViewModelLocator;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;
using ESME.Views.Environment;

namespace ESMEWorkbench.ViewModels.Map
{
    [ExportViewModel("MapViewModel")]
    public class MapViewModel : ViewModelBase
    {
        #region Private fields

        readonly IViewAwareStatus _viewAwareStatus;
        readonly MainViewModel _mainViewModel;
        WpfMap _wpfMap;
        readonly IUIVisualizerService _visualizer;
        private readonly IHRCSaveFileService _saveFile;

        #endregion

        public string MapDLLVersion { get; private set; }
        public MapViewModel(IViewAwareStatus viewAwareStatus, IMessageBoxService messageBox, MainViewModel mainViewModel, IUIVisualizerService visualizer, IHRCSaveFileService saveFile)
        {
            _mainViewModel = mainViewModel;
            _visualizer = visualizer;
            _saveFile = saveFile;
            viewAwareStatus.ViewLoaded += ViewLoaded;
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Globals.DisplayException(messageBox, ex, "***********\nMapViewModel: Mediator registration failed\n***********");
                throw;
            }
            _viewAwareStatus = viewAwareStatus;
        }

        void ViewLoaded()
        {
            if (Designer.IsInDesignMode) return;

            _wpfMap = ((MainView)_viewAwareStatus.View).MapView.WpfMap;

            CreateMouseEventStreams();
            SubscribeToMouseEventStreams();
            
            EditableRectangleOverlayViewModel = new EditableRectangleOverlayViewModel(_wpfMap);
            EditablePolygonOverlayViewModel = new EditablePolygonOverlayViewModel(_wpfMap);
            //SoundSpeedProfileViewModel = new SoundSpeedProfileViewModel(((MainView)_viewAwareStatus.View).MapView.SoundSpeedProfileView);
            MapDLLVersion = WpfMap.GetVersion();
            _wpfMap.MapUnit = GeographyUnit.DecimalDegree;
            _wpfMap.MapTools.PanZoomBar.HorizontalAlignment = HorizontalAlignment.Left;
            _wpfMap.MapTools.PanZoomBar.VerticalAlignment = VerticalAlignment.Top;
            _wpfMap.MapTools.Logo.IsEnabled = false;

            _wpfMap.BackgroundOverlay.BackgroundBrush = new GeoSolidBrush(GeoColor.StandardColors.Black);
            _wpfMap.AdornmentOverlay.Layers.Add("Grid", new MyGraticuleAdornmentLayer());
            _wpfMap.AdornmentOverlay.Layers["Grid"].IsVisible = Settings.Default.ShowGrid;
            _wpfMap.CurrentExtent = new RectangleShape(-180, 90, 180, -90);
            
            //var localizedName = ((MainView)_viewAwareStatus.View).FontFamily.FamilyNames[XmlLanguage.GetLanguage(CultureInfo.CurrentUICulture.Name)];
            const string localizedName = "Segoe UI";

            var customUnitScaleBarAdornmentLayer = new CustomUnitScaleBarAdornmentLayer
                                                   {
                                                       // Text to be displayed on the scale bar
                                                       UnitText = "Km",
                                                       //Ratio of meters to specified units
                                                       MeterToUnit = 1000,
                                                       GeoFont = new GeoFont(localizedName, 10),
                                                       GeoSolidBrush = new GeoSolidBrush(GeoColor.StandardColors.White),
                                                   };
            _wpfMap.AdornmentOverlay.Layers.Add("Scale", customUnitScaleBarAdornmentLayer);
            _wpfMap.AdornmentOverlay.Layers["Scale"].IsVisible = Settings.Default.ShowScaleBar;
            _wpfMap.ExtentOverlay.DoubleLeftClickMode = MapDoubleLeftClickMode.Disabled;
            _wpfMap.ExtentOverlay.DoubleRightClickMode = MapDoubleRightClickMode.Disabled;

            _wpfMap.MapTools.PanZoomBar.Visibility = Settings.Default.ShowPanZoom ? Visibility.Visible : Visibility.Hidden;
            _mainViewModel.LayerTreeViewModel.MapViewModel = this;
        }

        public bool IsGridVisible
        {
            get { return Settings.Default.ShowGrid; }
            set
            {
                _wpfMap.AdornmentOverlay.Layers["Grid"].IsVisible = value;
                _wpfMap.AdornmentOverlay.Refresh();
                Settings.Default.ShowGrid = value;
            }
        }
        public bool IsScaleVisible
        {
            get { return Settings.Default.ShowScaleBar; }
            set
            {
                _wpfMap.AdornmentOverlay.Layers["Scale"].IsVisible = value;
                _wpfMap.AdornmentOverlay.Refresh();
                Settings.Default.ShowScaleBar = value;
            }
        }
        public bool IsPanZoomVisible
        {
            get { return Settings.Default.ShowPanZoom; }
            set
            {
                _wpfMap.MapTools.PanZoomBar.Visibility = value ? Visibility.Visible : Visibility.Hidden;
                Settings.Default.ShowPanZoom = value;
            }
        }

        [MediatorMessageSink(MediatorMessage.ApplicationClosing), UsedImplicitly] 
        void ApplicationClosing(bool mode) { if (_soundSpeedProfileWindowView != null) _soundSpeedProfileWindowView.Close(); }

        public SoundSpeedProfile MouseSoundSpeedProfile { get; set; }
        public bool IsSoundSpeedProfilePopupOpen { get; set; }
        public SoundSpeedProfileViewModel SoundSpeedProfileViewModel { get; set; }

        [MediatorMessageSink(MediatorMessage.SetMapExtent), UsedImplicitly]
        void SetMapExtent(GeoRect geoRect) { CurrentExtent = geoRect; }
        public GeoRect CurrentExtent
        {
            get { return new GeoRect(_wpfMap.CurrentExtent.UpperLeftPoint.Y, _wpfMap.CurrentExtent.LowerRightPoint.Y, _wpfMap.CurrentExtent.LowerRightPoint.X, _wpfMap.CurrentExtent.UpperLeftPoint.X); }
            set { _wpfMap.CurrentExtent = new RectangleShape(value.West, value.North, value.East, value.South); _wpfMap.Refresh();}
        }

        [MediatorMessageSink(MediatorMessage.RefreshMap), UsedImplicitly]
        void Refresh(bool dummy) { _wpfMap.Refresh(); }
        public void Refresh() { _wpfMap.Refresh(); }

        #region Add/Remove/Refresh map layer mediator message sinks
        [MediatorMessageSink(MediatorMessage.AddMapLayer)]
        public void Add(MapLayerViewModel layer) { if (!_wpfMap.Overlays.Contains(layer.Name)) _wpfMap.Overlays.Add(layer.Name, layer.LayerOverlay); }

        [MediatorMessageSink(MediatorMessage.RemoveMapLayer)]
        public void Remove(MapLayerViewModel layer) { _wpfMap.Overlays.Remove(layer.Name); }

        [MediatorMessageSink(MediatorMessage.ShowMapLayer), UsedImplicitly]
        void ShowMapLayer(MapLayerViewModel layer) { SetIsVisible(layer, true); }

        [MediatorMessageSink(MediatorMessage.HideMapLayer), UsedImplicitly]
        void HideMapLayer(MapLayerViewModel layer) { SetIsVisible(layer, false); }

        public void SetIsVisible(MapLayerViewModel layer, bool isVisible)
        {
            var layerOverlay = _wpfMap.Overlays[layer.Name];
            layerOverlay.IsVisible = isVisible;
            //layer.LayerOverlay.IsVisible = isVisible;
            try
            {
                _wpfMap.Refresh(layerOverlay);
            }
            catch (Exception)
            {
                _wpfMap.Overlays.Remove(layerOverlay);
                _wpfMap.Refresh();
            }
        }

        [MediatorMessageSink(MediatorMessage.RefreshMapLayer)]
        public void Refresh(MapLayerViewModel layer)
        {
            if (layer == null || layer.LayerOverlay == null || layer.LayerOverlay.Layers == null || layer.LayerOverlay.Layers.Count <= 0) return;
            var layerOverlay = (LayerOverlay)_wpfMap.Overlays[layer.Name];
            if (layerOverlay == null || layerOverlay.Layers == null || layerOverlay.Layers.Count <= 0) return;
            if (layer.GetType() == typeof(ShapefileMapLayer))
                ((ShapeFileFeatureLayer)layerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = layer.AreaStyle;
            else if (layer.GetType() == typeof(OverlayShapeMapLayer))
            {
                if (layer.CustomLineStyle != null)
                {
                    ((InMemoryFeatureLayer)layerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.CustomStyles.Clear();
                    ((InMemoryFeatureLayer)layerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.CustomStyles.Add(layer.CustomLineStyle);
                }
                else
                {
                    if (layer.LineStyle != null)
                    {
                        ((InMemoryFeatureLayer)layerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultLineStyle = layer.LineStyle;
                        ((InMemoryFeatureLayer)layerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = layer.AreaStyle;
                    }
                    if (layer.PointStyle != null) ((InMemoryFeatureLayer)layerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultPointStyle = layer.PointStyle;
                }
            }
            _wpfMap.Refresh(layerOverlay);
        }
        #endregion

        #region Move layer top/up/down/bottom mediator message sinks
        [MediatorMessageSink(MediatorMessage.MoveLayerToFront), UsedImplicitly]
        void MoveLayerToFront(MapLayerViewModel layer)
        {
            //Debug.WriteLine(string.Format("MediatorMessage.MoveLayerToFront: {0}", layer.LayerOverlay.Name));
            _wpfMap.Overlays.MoveToTop(layer.Name);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerForward), UsedImplicitly]
        void MoveLayerForward(MapLayerViewModel layer)
        {
            //Debug.WriteLine(string.Format("MediatorMessage.MoveLayerForward: {0}", layer.LayerOverlay.Name));
            _wpfMap.Overlays.MoveUp(layer.Name);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerBackward), UsedImplicitly]
        void MoveLayerBackward(MapLayerViewModel layer)
        {
            //Debug.WriteLine(string.Format("MediatorMessage.MoveLayerBackward: {0}", layer.LayerOverlay.Name));
            _wpfMap.Overlays.MoveDown(layer.Name);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerToBack), UsedImplicitly]
        void MoveLayerToBack(MapLayerViewModel layer)
        {
            //Debug.WriteLine(string.Format("MediatorMessage.MoveLayerToBack: {0}", layer.LayerOverlay.Name));
            _wpfMap.Overlays.MoveToBottom(layer.Name);
        }
        #endregion

        #region Mouse event streams

        public IObservable<Geo> LeftButtonDown { get; private set; }
        public IObservable<Geo> LeftButtonUp { get; private set; }
        public IObservable<Geo> RightButtonDown { get; private set; }
        public IObservable<Geo> RightButtonUp { get; private set; }
        public IObservable<Geo> MouseGeo { get; private set; }
        public IObservable<Geo> Click { get; private set; }
        public IObservable<Geo> DoubleClick { get; private set; }
        public IObservable<Geo> MouseHoverGeo { get; private set; }

        void SubscribeToMouseEventStreams()
        {
            DoubleClick.Subscribe(g =>
            {
                if (MouseSoundSpeedProfile != null)
                {
                    if (SoundSpeedProfileViewModel == null) SoundSpeedProfileViewModel = new SoundSpeedProfileViewModel(_saveFile);
                    if (_soundSpeedProfileWindowView == null)
                    {
                        _soundSpeedProfileWindowView = (SoundSpeedProfileWindowView)_visualizer.ShowWindow("SoundSpeedProfileWindowView", SoundSpeedProfileViewModel, false, (sender, args) => { _soundSpeedProfileWindowView = null; });
                        _soundSpeedProfileWindowView.Closed += (s, e1) => { SoundSpeedProfileViewModel = null; };
                        SoundSpeedProfileViewModel.View = _soundSpeedProfileWindowView.FindChildren<SoundSpeedProfileView>().First();
                        SoundSpeedProfileViewModel.WindowView = _soundSpeedProfileWindowView;
                    }
                    SoundSpeedProfileViewModel.SoundSpeedProfile = MouseSoundSpeedProfile;
                }
                IsSoundSpeedProfilePopupOpen = true;
            });
        }

        //IDisposable _mouseHover, _timeObserver;
        void CreateMouseEventStreams()
        {
            LeftButtonDown = Observable.FromEventPattern<MouseButtonEventArgs>(_wpfMap, "MouseDown")
                .Where(e => e.EventArgs.ChangedButton == MouseButton.Left)
                .Select(e => GetMouseEventArgsGeo(e.EventArgs));
            RightButtonDown = Observable.FromEventPattern<MouseButtonEventArgs>(_wpfMap, "MouseDown")
                .Where(e => e.EventArgs.ChangedButton == MouseButton.Right)
                .Select(e => GetMouseEventArgsGeo(e.EventArgs));
            LeftButtonUp = Observable.FromEventPattern<MouseButtonEventArgs>(_wpfMap, "MouseUp")
                .Where(e => e.EventArgs.ChangedButton == MouseButton.Left)
                .Select(e => GetMouseEventArgsGeo(e.EventArgs));
            RightButtonUp = Observable.FromEventPattern<MouseButtonEventArgs>(_wpfMap, "MouseUp")
                .Where(e => e.EventArgs.ChangedButton == MouseButton.Right)
                .Select(e => GetMouseEventArgsGeo(e.EventArgs));
            MouseGeo = Observable.FromEventPattern<MouseEventArgs>(_wpfMap, "MouseMove")
                .Select(e => GetMouseEventArgsGeo(e.EventArgs))
                .Merge(Observable.FromEventPattern<MouseEventArgs>(_wpfMap, "MouseLeave")
                           .Select(e => (Geo)null));
            Click = Observable.FromEventPattern<MapClickWpfMapEventArgs>(_wpfMap, "MapClick")
                .Select(e => new Geo(e.EventArgs.WorldY, e.EventArgs.WorldX));
            DoubleClick = Observable.FromEventPattern<MapClickWpfMapEventArgs>(_wpfMap, "MapDoubleClick")
                .Select(e => new Geo(e.EventArgs.WorldY, e.EventArgs.WorldX));
            MouseHover();
        }

        void MouseHover()
        {
            // Here create an event stream called mouseGeosWithMouseWheelNulls, which consists of the MouseGeo event stream, 
            // merged with another stream that produces a null every time the mouse wheel is moved
            // Here create an event stream called MouseHoverGeo, which consists of mouseGeosWithMouseWheelNulls events 
            // filtered to only give us an event after 300 ms of inactivity with duplicate events and null events removed
            MouseHoverGeo = MouseGeo.Throttle(TimeSpan.FromMilliseconds(300))
                .Merge(Observable.FromEventPattern<MouseWheelEventArgs>(_wpfMap, "MouseWheel")
                                                                .Select(e => (Geo)null))
                .Merge(MouseGeo.Select(e => (Geo)null))
                .DistinctUntilChanged();
            //MouseHoverGeo.ObserveOn(TaskPoolScheduler.Default).Subscribe(g => Debug.WriteLine(string.Format("{0:HH:mm:ss:fff} MouseHoverGeo is now {1}", DateTime.Now, g)));

#if false
    // Observe the MouseHoveGeo event stream on the TaskPool
            _mouseHover = MouseHoverGeo.ObserveOn(TaskPoolScheduler.Default)
                .Subscribe(g =>
                {
                    var pointShape = new PointShape(g.Longitude, g.Latitude);
                    foreach (var activeLayerOverlay in _wpfMap.Overlays.OfType<ActiveLayerOverlay>().ToList())
                    {
                        foreach (var featureLayer in activeLayerOverlay.Layers.OfType<FeatureLayer>().ToList())
                        {
                            try
                            {
                                featureLayer.Open();
                                var featureCollection = featureLayer.QueryTools.GetFeaturesContaining(pointShape, ReturningColumnsType.NoColumns);
                                if (featureCollection.Count == 0) continue;
                                activeLayerOverlay.MouseIsHovering.OnNext(true);
                                _hoveringOverlays.Add(activeLayerOverlay);
                            }
                            finally
                            {
                                if (featureLayer.IsOpen) featureLayer.Close();
                            }
                        }
                    }
                });
            mouseGeoWithMouseWheelNull.ObserveOn(TaskPoolScheduler.Default)
                .Subscribe(g =>
                {
                    while (!_hoveringOverlays.IsEmpty)
                    {
                        ActiveLayerOverlay overlay;
                        if (_hoveringOverlays.TryTake(out overlay)) overlay.MouseIsHovering.OnNext(false);
                    }
                });
#endif
        }

        SoundSpeedProfileWindowView _soundSpeedProfileWindowView;
        //readonly ConcurrentBag<ActiveLayerOverlay> _hoveringOverlays = new ConcurrentBag<ActiveLayerOverlay>();

        Geo GetMouseEventArgsGeo(MouseEventArgs e)
        {
            var point = e.GetPosition(_wpfMap);
            var pointShape = ExtentHelper.ToWorldCoordinate(_wpfMap.CurrentExtent, (float)point.X, (float)point.Y, (float)_wpfMap.ActualWidth, (float)_wpfMap.ActualHeight);
            return new Geo(pointShape.Y, pointShape.X);
        }

        #endregion

        public EditableRectangleOverlayViewModel EditableRectangleOverlayViewModel { get; set; }
        public EditablePolygonOverlayViewModel EditablePolygonOverlayViewModel { get; set; }
    }
}