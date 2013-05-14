using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Threading;
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
using HRC.Aspects;
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

        [Initialize, UsedImplicitly] public Subject<Geo> LeftButtonDown { get; private set; }
        [Initialize, UsedImplicitly] public Subject<Geo> LeftButtonUp { get; private set; }
        [Initialize, UsedImplicitly] public Subject<Geo> RightButtonDown { get; private set; }
        [Initialize, UsedImplicitly] public Subject<Geo> RightButtonUp { get; private set; }
        [Initialize, UsedImplicitly] public Subject<Geo> MouseGeo { get; private set; }
        [Initialize, UsedImplicitly] public Subject<Geo> Click { get; private set; }
        [Initialize, UsedImplicitly] public Subject<Geo> DoubleClick { get; private set; }
        [Initialize, UsedImplicitly] public Subject<Geo> MouseWheel { get; private set; }

        void SubscribeToMouseEventStreams()
        {
            MouseGeo.Subscribe(g =>
            {
                _hoveringOverlays.ForEach(l => l.MouseIsHovering.OnNext(false));
                _hoveringOverlays.Clear();
            });
            
            
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

        IDisposable _mouseHover, _timeObserver;
        void CreateMouseEventStreams()
        {
            Observable.FromEventPattern<MouseButtonEventArgs>(_wpfMap, "MouseDown").ObserveOnDispatcher()
                .Select(e => new { Button = e.EventArgs.ChangedButton, Geo = GetMouseEventArgsGeo(e.EventArgs) })
                .Subscribe(e =>
                {
                    if (e.Button == MouseButton.Left) LeftButtonDown.OnNext(e.Geo);
                    if (e.Button == MouseButton.Right) RightButtonDown.OnNext(e.Geo);
                });
            Observable.FromEventPattern<MouseButtonEventArgs>(_wpfMap, "MouseUp").ObserveOnDispatcher()
                .Select(e => new { Button = e.EventArgs.ChangedButton, Geo = GetMouseEventArgsGeo(e.EventArgs) })
                .Subscribe(e =>
                {
                    if (e.Button == MouseButton.Left) LeftButtonUp.OnNext(e.Geo);
                    if (e.Button == MouseButton.Right) RightButtonUp.OnNext(e.Geo);
                });
            Observable.FromEventPattern<MouseWheelEventArgs>(_wpfMap, "MouseWheel").ObserveOnDispatcher().Subscribe(e =>
            {
                if (_mouseHover != null || _timeObserver != null)
                {
                    if (_mouseHover != null) _mouseHover.Dispose();
                    if (_timeObserver != null) _timeObserver.Dispose();
                    _mouseHover = _timeObserver = null;
                }
                
                _timeObserver = Observable.Interval(TimeSpan.FromMilliseconds(200)).Subscribe(f =>
                {
                    if (_mouseHover != null) return;
                    _mouseHover = MouseGeo.Throttle(TimeSpan.FromMilliseconds(300)).DistinctUntilChanged()
                        .Where(g => g != null)
                        .Select(g => new PointShape(g.Longitude, g.Latitude))
                        .Subscribe(p =>
                        {
                            foreach (var activeLayerOverlay in _wpfMap.Overlays.OfType<ActiveLayerOverlay>().ToList())
                            {
                                foreach (var featureLayer in activeLayerOverlay.Layers.OfType<FeatureLayer>().ToList())
                                {
                                    featureLayer.Open();
                                    try
                                    {
                                        var featureCollection = featureLayer.QueryTools.GetFeaturesContaining(p, ReturningColumnsType.NoColumns);
                                        if (featureCollection.Count != 0)
                                        {
                                            activeLayerOverlay.MouseIsHovering.OnNext(true);
                                            _hoveringOverlays.Add(activeLayerOverlay);
                                        }
                                    }
                                    catch {}
                                    if (featureLayer.IsOpen) featureLayer.Close();
                                }
                            }
                        });
                    _timeObserver.Dispose();
                });

            });
            
           
            Observable.FromEventPattern<MouseEventArgs>(_wpfMap, "MouseLeave").ObserveOnDispatcher() 
                .Subscribe(e => MouseGeo.OnNext(null));
            Observable.FromEventPattern<MouseEventArgs>(_wpfMap, "MouseMove").ObserveOnDispatcher()
                .Subscribe(e => MouseGeo.OnNext(GetMouseEventArgsGeo(e.EventArgs)));
            Observable.FromEventPattern<MapClickWpfMapEventArgs>(_wpfMap, "MapClick").ObserveOnDispatcher()
                .Subscribe(e => Click.OnNext(new Geo(e.EventArgs.WorldY, e.EventArgs.WorldX)));
            Observable.FromEventPattern<MapClickWpfMapEventArgs>(_wpfMap, "MapDoubleClick").ObserveOnDispatcher()
                .Subscribe(e => DoubleClick.OnNext(new Geo(e.EventArgs.WorldY, e.EventArgs.WorldX)));
            
        }
        SoundSpeedProfileWindowView _soundSpeedProfileWindowView;
        readonly List<ActiveLayerOverlay> _hoveringOverlays = new List<ActiveLayerOverlay>();

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