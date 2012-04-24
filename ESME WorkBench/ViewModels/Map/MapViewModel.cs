using System;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Windows;
using System.Windows.Input;
using System.Windows.Markup;
using ESME;
using ESME.Environment;
using ESME.Mapping;
using ESME.NEMO;
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

        #endregion

        public string MapDLLVersion { get; private set; }
        public MapViewModel(IViewAwareStatus viewAwareStatus, IMessageBoxService messageBox, MainViewModel mainViewModel, IUIVisualizerService visualizer)
        {
            _mainViewModel = mainViewModel;
            _visualizer = visualizer;
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
            //_synchronizationContext = synchronizationContext;

            Cursor = Cursors.Arrow;
        }

        enum MouseModeEnum
        {
            Normal,
            AnalysisPoint,
            SelectionRectangle,
        }

        MouseModeEnum MouseMode
        {
            get { return _mouseMode; }
            set
            {
                _mouseMode = value;
                switch (_mouseMode)
                {
                    case MouseModeEnum.Normal:
                        Cursor = Cursors.Arrow;
                        break;
                    case MouseModeEnum.AnalysisPoint:
                        Cursor = Cursors.Cross;
                        break;
                    case MouseModeEnum.SelectionRectangle:
                        Cursor = Cursors.Cross;
                        break;
                    default:
                        throw new ParameterOutOfRangeException(string.Format("Unknown enum value {0} for MouseMode", _mouseMode));
                }
            }
        }
        MouseModeEnum _mouseMode = MouseModeEnum.Normal;

        public bool IsInAnalysisPointMode
        {
            get { return _isInAnalysisPointMode; }
            set
            {
                _isInAnalysisPointMode = value;
                Cursor = _isInAnalysisPointMode ? Cursors.Cross : Cursors.Arrow;
            }
        }
        bool _isInAnalysisPointMode;

        #region public Cursor Cursor { get; set; }

        Cursor _cursor;

        public Cursor Cursor
        {
            get { return _cursor; }
            set
            {
                if (_cursor == value) return;
                _cursor = value;
            }
        }

        #endregion

        void ViewLoaded()
        {
            if (Designer.IsInDesignMode) return;

            _wpfMap = ((MainView)_viewAwareStatus.View).MapView.WpfMap;
            EditOverlayViewModel = new EditOverlayViewModel(_wpfMap);
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
            var localizedName = ((MainView)_viewAwareStatus.View).FontFamily.FamilyNames[XmlLanguage.GetLanguage(CultureInfo.CurrentUICulture.Name)];

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

        [MediatorMessageSink(MediatorMessage.ApplicationClosing), UsedImplicitly] void ApplicationClosing(bool mode) { if (_soundSpeedProfileWindowView != null) _soundSpeedProfileWindowView.Close(); }

        [MediatorMessageSink(MediatorMessage.SetAnalysisPointMode), UsedImplicitly]
        void SetAnalysisPointMode(bool mode)
        {
            MouseMode = MouseModeEnum.AnalysisPoint;
        }

        [MediatorMessageSink(MediatorMessage.SetEditMode), UsedImplicitly]
        void SetEditMode(GeoRect geoRect)
        {
            _wpfMap.EditOverlay = new CustomEditInteractiveOverlay();

            var rectangle = new Feature(new RectangleShape(geoRect.West, geoRect.North, geoRect.East, geoRect.South));
            // Set the value of column "Edit" to "rectangle", so this shape will be editing by custom way.
            rectangle.ColumnValues.Add("Edit", null);
            _wpfMap.EditOverlay.EditShapesLayer.InternalFeatures.Add(rectangle);

            _wpfMap.EditOverlay.EditShapesLayer.Open();
            _wpfMap.EditOverlay.EditShapesLayer.Columns.Add(new FeatureSourceColumn("Edit"));
            _wpfMap.EditOverlay.EditShapesLayer.Close();
            _wpfMap.EditOverlay.EditShapesLayer.ZoomLevelSet.ZoomLevel01.DefaultTextStyle = new TextStyle("Edit", new GeoFont("Arial", 18), new GeoSolidBrush(GeoColor.StandardColors.Black));
            _wpfMap.EditOverlay.EditShapesLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            _wpfMap.EditOverlay.CalculateAllControlPoints();
            _wpfMap.EditOverlay.FeatureResized += (sender, args) =>
            {
                _wpfMap.EditOverlay.EditShapesLayer.Open();
                var bounds = _wpfMap.EditOverlay.EditShapesLayer.GetBoundingBox();
                _wpfMap.EditOverlay.EditShapesLayer.Close();
                Debug.WriteLine("Resized: North {0} South {1} East {2} West {3}", bounds.UpperLeftPoint.Y, bounds.LowerRightPoint.Y, bounds.LowerRightPoint.X, bounds.UpperLeftPoint.X);
            };
            _wpfMap.EditOverlay.FeatureDragged += (sender, args) =>
            {
                _wpfMap.EditOverlay.EditShapesLayer.Open();
                var bounds = _wpfMap.EditOverlay.EditShapesLayer.GetBoundingBox();
                _wpfMap.EditOverlay.EditShapesLayer.Close();
                Debug.WriteLine("Dragged: North {0} South {1} East {2} West {3}", bounds.UpperLeftPoint.Y, bounds.LowerRightPoint.Y, bounds.LowerRightPoint.X, bounds.UpperLeftPoint.X);
            };

            // Draw the map image on the screen
            _wpfMap.Refresh();
        }

        void CreateEditOverlay()
        {
            _wpfMap.EditOverlay = new CustomEditInteractiveOverlay
            {
                CanAddVertex = false,
                CanRemoveVertex = false,
                CanRotate = false
            };
            _wpfMap.Refresh();
        }

        double _north, _south, _east, _west;
        void UpdateEditOverlay()
        {
            _wpfMap.EditOverlay.EditShapesLayer.InternalFeatures.Clear();
            var rectangle = new Feature(new RectangleShape(_west, _north, _east, _south));
            rectangle.ColumnValues.Add("Edit", null);
            _wpfMap.EditOverlay.EditShapesLayer.InternalFeatures.Add(rectangle);
            _wpfMap.EditOverlay.EditShapesLayer.Open();
            _wpfMap.EditOverlay.EditShapesLayer.Columns.Add(new FeatureSourceColumn("Edit"));
            _wpfMap.EditOverlay.EditShapesLayer.Close();
            _wpfMap.EditOverlay.EditShapesLayer.ZoomLevelSet.ZoomLevel01.DefaultTextStyle = new TextStyle("Edit", new GeoFont("Arial", 18), new GeoSolidBrush(GeoColor.StandardColors.Black));
            _wpfMap.EditOverlay.EditShapesLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            _wpfMap.EditOverlay.CalculateAllControlPoints();
            _wpfMap.Refresh();
        }

        void DeleteEditOverlay()
        {
            _wpfMap.EditOverlay.EditShapesLayer.Open();
            _wpfMap.EditOverlay.EditShapesLayer.InternalFeatures.Clear();
            _wpfMap.EditOverlay.EditShapesLayer.Close();
            _wpfMap.EditOverlay = null;
            _wpfMap.Refresh();
        }

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

        [MediatorMessageSink(MediatorMessage.SetMapCursor), UsedImplicitly]
        void SetMapCursor(Cursor cursor) { Cursor = cursor; }

        [MediatorMessageSink(MediatorMessage.RefreshMap), UsedImplicitly]
        void Refresh(bool dummy) { _wpfMap.Refresh(); }
        public void Refresh() { _wpfMap.Refresh(); }

        #region Add/Remove/Refresh map layer mediator message sinks
        [MediatorMessageSink(MediatorMessage.AddMapLayer)]
        public void Add(MapLayerViewModel layer) { _wpfMap.Overlays.Add(layer.LayerOverlay); }

        [MediatorMessageSink(MediatorMessage.RemoveMapLayer)]
        public void Remove(MapLayerViewModel layer) { _wpfMap.Overlays.Remove(layer.LayerOverlay); }

        [MediatorMessageSink(MediatorMessage.ShowMapLayer), UsedImplicitly]
        void ShowMapLayer(MapLayerViewModel layer) { SetIsVisible(layer, true); }

        [MediatorMessageSink(MediatorMessage.HideMapLayer), UsedImplicitly]
        void HideMapLayer(MapLayerViewModel layer) { SetIsVisible(layer, false); }

        public void SetIsVisible(MapLayerViewModel layer, bool isVisible)
        {
            layer.LayerOverlay.IsVisible = isVisible;
            _wpfMap.Refresh(layer.LayerOverlay);
        }

        [MediatorMessageSink(MediatorMessage.RefreshMapLayer)]
        public void Refresh(MapLayerViewModel layer)
        {
            if (layer.LayerOverlay.Layers.Count <= 0) return;
            if (layer.GetType() == typeof(ShapefileMapLayer))
                ((ShapeFileFeatureLayer)layer.LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = layer.AreaStyle;
            else if (layer.GetType() == typeof(OverlayShapeMapLayer))
            {
                if (layer.CustomLineStyle != null)
                {
                    ((InMemoryFeatureLayer)layer.LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.CustomStyles.Clear();
                    ((InMemoryFeatureLayer)layer.LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.CustomStyles.Add(layer.CustomLineStyle);
                }
                else
                {
                    if (layer.LineStyle != null) ((InMemoryFeatureLayer)layer.LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultLineStyle = layer.LineStyle;
                    if (layer.PointStyle != null) ((InMemoryFeatureLayer)layer.LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultPointStyle = layer.PointStyle;
                }
            }
            _wpfMap.Refresh(layer.LayerOverlay);
        }
        #endregion

        #region Move layer top/up/down/bottom mediator message sinks
        [MediatorMessageSink(MediatorMessage.MoveLayerToTop), UsedImplicitly]
        void MoveLayerToTop(Overlay overlay)
        {
            Debug.WriteLine(string.Format("MediatorMessage.MoveLayerToTop: {0}", overlay.Name));
            _wpfMap.Overlays.MoveToTop(overlay);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerUp), UsedImplicitly]
        void MoveLayerUp(Overlay overlay)
        {
            Debug.WriteLine(string.Format("MediatorMessage.MoveLayerUp: {0}", overlay.Name));
            _wpfMap.Overlays.MoveUp(overlay);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerDown), UsedImplicitly]
        void MoveLayerDown(Overlay overlay)
        {
            Debug.WriteLine(string.Format("MediatorMessage.MoveLayerDown: {0}", overlay.Name));
            _wpfMap.Overlays.MoveDown(overlay);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerToBottom), UsedImplicitly]
        void MoveLayerToBottom(Overlay overlay)
        {
            Debug.WriteLine(string.Format("MediatorMessage.MoveLayerToBottom: {0}", overlay.Name));
            _wpfMap.Overlays.MoveToBottom(overlay);
        }
        #endregion

        #region Mouse events

        #region MouseLeftButtonDownCommand
        public SimpleCommand<object, EventToCommandArgs> MouseLeftButtonDownCommand
        {
            get { return _mouseLeftButtonDown ?? (_mouseLeftButtonDown = new SimpleCommand<object, EventToCommandArgs>(MouseLeftButtonDownHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _mouseLeftButtonDown;

        void MouseLeftButtonDownHandler(EventToCommandArgs arg)
        {
            var geo = GetMouseEventArgsGeo((MouseButtonEventArgs)arg.EventArgs);
            Debug.WriteLine("Mouse left button down at {0}", geo);
            switch (MouseMode)
            {
                case MouseModeEnum.Normal:
                case MouseModeEnum.AnalysisPoint:
                    break;
                case MouseModeEnum.SelectionRectangle:
                    _isMouseCaptured = Mouse.Capture(_wpfMap, CaptureMode.Element);
                    if (_isMouseCaptured)
                    {
                        CreateEditOverlay();
                        _north = _south = geo.Latitude;
                        _east = _west = geo.Longitude;
                    }
                    break;
                default:
                    throw new ParameterOutOfRangeException(string.Format("Unknown enum value {0} for MouseMode", _mouseMode));
            }
        }

        bool _isMouseCaptured;
        #endregion

        #region MouseLeftButtonUpCommand
        public SimpleCommand<object, EventToCommandArgs> MouseLeftButtonUpCommand
        {
            get { return _mouseLeftButtonUp ?? (_mouseLeftButtonUp = new SimpleCommand<object, EventToCommandArgs>(MouseLeftButtonUpHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _mouseLeftButtonUp;

        void MouseLeftButtonUpHandler(EventToCommandArgs arg)
        {
            var geo = GetMouseEventArgsGeo((MouseButtonEventArgs)arg.EventArgs);
            Debug.WriteLine("Mouse left button up at {0}", geo);
            switch (MouseMode)
            {
                case MouseModeEnum.Normal:
                    break;
                case MouseModeEnum.AnalysisPoint:
                    MouseMode = MouseModeEnum.Normal;
                    if (Cursor == Cursors.No) return;
                    MediatorMessage.Send(MediatorMessage.SetAnalysisPointMode, false);
                    MediatorMessage.Send(MediatorMessage.PlaceAnalysisPoint);
                    break;
                case MouseModeEnum.SelectionRectangle:
                    if (_isMouseCaptured)
                    {
                        _north = Math.Max(_north, geo.Latitude);
                        _south = Math.Max(_south, geo.Latitude);
                        _east = Math.Max(_east, geo.Longitude);
                        _west = Math.Min(_west, geo.Longitude);
                        UpdateEditOverlay();
                        Mouse.Capture(null);
                        _isMouseCaptured = false;
                    }
                    MouseMode = MouseModeEnum.Normal;
                    break;
                default:
                        throw new ParameterOutOfRangeException(string.Format("Unknown enum value {0} for MouseMode", _mouseMode));
            }
        }
        #endregion

        #region MouseRightButtonDownCommand
        public SimpleCommand<object, EventToCommandArgs> MouseRightButtonDownCommand
        {
            get { return _mouseRightButtonDown ?? (_mouseRightButtonDown = new SimpleCommand<object, EventToCommandArgs>(MouseRightButtonDownHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _mouseRightButtonDown;

        void MouseRightButtonDownHandler(EventToCommandArgs arg)
        {
            var geo = GetMouseEventArgsGeo((MouseButtonEventArgs)arg.EventArgs);
            Debug.WriteLine("Mouse right button down at {0}", geo);
        }
        #endregion

        #region MouseRightButtonUpCommand
        public SimpleCommand<object, EventToCommandArgs> MouseRightButtonUpCommand
        {
            get { return _mouseRightButtonUp ?? (_mouseRightButtonUp = new SimpleCommand<object, EventToCommandArgs>(MouseRightButtonUpHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _mouseRightButtonUp;

        void MouseRightButtonUpHandler(EventToCommandArgs arg)
        {
            var geo = GetMouseEventArgsGeo((MouseButtonEventArgs)arg.EventArgs);
            Debug.WriteLine("Mouse right button up at {0}", geo);
        }
        #endregion

        Geo GetMouseEventArgsGeo(MouseEventArgs e)
        {
            var point = e.GetPosition(_wpfMap);
            var pointShape = ExtentHelper.ToWorldCoordinate(_wpfMap.CurrentExtent, (float)point.X, (float)point.Y, (float)_wpfMap.ActualWidth, (float)_wpfMap.ActualHeight);
            return new Geo(pointShape.Y, pointShape.X);
        }

        #region MouseMoveCommand
        public SimpleCommand<object, EventToCommandArgs> MouseMoveCommand
        {
            get { return _mouseMove ?? (_mouseMove = new SimpleCommand<object, EventToCommandArgs>(MouseMoveHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _mouseMove;

        void MouseMoveHandler(EventToCommandArgs arg)
        {
            var e = (MouseEventArgs)arg.EventArgs;
            var point = e.MouseDevice.GetPosition(_wpfMap);
            var pointShape = ExtentHelper.ToWorldCoordinate(_wpfMap.CurrentExtent, (float)point.X, (float)point.Y, (float)_wpfMap.ActualWidth, (float)_wpfMap.ActualHeight);
            MediatorMessage.Send(MediatorMessage.SetMouseEarthCoordinate, new Geo(pointShape.Y, pointShape.X));
            switch (MouseMode)
            {
                case MouseModeEnum.Normal:
                case MouseModeEnum.AnalysisPoint:
                    break;
                case MouseModeEnum.SelectionRectangle:
                    if (_isMouseCaptured)
                    {
                        _north = Math.Max(_north, pointShape.Y);
                        _south = Math.Max(_south, pointShape.Y);
                        _east = Math.Max(_east, pointShape.X);
                        _west = Math.Min(_west, pointShape.X);
                        UpdateEditOverlay();
                    }

                    break;
                default:
                    throw new ParameterOutOfRangeException(string.Format("Unknown enum value {0} for MouseMode", _mouseMode));
            }
        }
        #endregion

        #region MapClickCommand
        public SimpleCommand<object, EventToCommandArgs> MapClickCommand
        {
            get { return _mapClick ?? (_mapClick = new SimpleCommand<object, EventToCommandArgs>(MapClickHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _mapClick;

        static void MapClickHandler(EventToCommandArgs arg)
        {
            var e = (MapClickWpfMapEventArgs)arg.EventArgs;
            MediatorMessage.Send(MediatorMessage.MapClick, new Geo(e.WorldY, e.WorldX));
        }
        #endregion

        #region MapDoubleClickCommand
        public SimpleCommand<object, EventToCommandArgs> MapDoubleClickCommand
        {
            get { return _mapDoubleClick ?? (_mapDoubleClick = new SimpleCommand<object, EventToCommandArgs>(MapDoubleClickHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _mapDoubleClick;

        void MapDoubleClickHandler(EventToCommandArgs arg)
        {
            var e = (MapClickWpfMapEventArgs)arg.EventArgs;
            MediatorMessage.Send(MediatorMessage.MapDoubleClick, new Geo(e.WorldY, e.WorldX));
            if (MouseSoundSpeedProfile != null)
            {
                if (SoundSpeedProfileViewModel == null) SoundSpeedProfileViewModel = new SoundSpeedProfileViewModel();
                if (_soundSpeedProfileWindowView == null)
                {
                    _soundSpeedProfileWindowView = (SoundSpeedProfileWindowView)_visualizer.ShowWindow("SoundSpeedProfileWindowView", SoundSpeedProfileViewModel, false, (sender, args) => { _soundSpeedProfileWindowView = null; });
                    SoundSpeedProfileViewModel.View = _soundSpeedProfileWindowView.FindChildren<SoundSpeedProfileView>().First();
                }
                SoundSpeedProfileViewModel.SoundSpeedProfile = MouseSoundSpeedProfile;
            }
            IsSoundSpeedProfilePopupOpen = true;
        }

        SoundSpeedProfileWindowView _soundSpeedProfileWindowView;
        #endregion

        #endregion

        public EditOverlayViewModel EditOverlayViewModel { get; set; }
    }
}