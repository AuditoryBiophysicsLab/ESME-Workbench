using System;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Globalization;
using System.Windows;
using System.Windows.Input;
using System.Windows.Markup;
using System.Xml.Serialization;
using Cinch;
using ESME;
using ESME.Mapping;
using ESMEWorkbench.Properties;
using ESMEWorkbench.ViewModels.Layers;
using ESMEWorkbench.ViewModels.Main;
using ESMEWorkbench.Views;
using HRC;
using HRC.Aspects;
using HRC.Navigation;
using MEFedMVVM.Common;
using MEFedMVVM.ViewModelLocator;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkbench.ViewModels.Map
{
    [ExportViewModel("MapViewModel")]
    [NotifyPropertyChanged]
    public class MapViewModel
    {
        #region Private fields

        readonly IViewAwareStatus _viewAwareStatus;
        readonly MainViewModel _mainViewModel;
        WpfMap _wpfMap;

        #endregion

        public string MapDLLVersion { get; private set; }

        [ImportingConstructor]
        public MapViewModel(IViewAwareStatus viewAwareStatus, IMessageBoxService messageBox, MainViewModel mainViewModel)
        {
            _mainViewModel = mainViewModel;
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

        [XmlIgnore]
        public bool IsQuickLookPointMode { get; set; }

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

        [MediatorMessageSink(MediatorMessage.SetAnalysisPointMode), UsedImplicitly]
        void SetAnalysisPointMode(bool mode)
        {
            IsInAnalysisPointMode = mode;
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

        [MediatorMessageSink(MediatorMessage.SetMapExtent), UsedImplicitly]
        void SetMapExtent(GeoRect geoRect)
        {
            _wpfMap.CurrentExtent = new RectangleShape(geoRect.West, geoRect.North, geoRect.East, geoRect.South);
            _wpfMap.Refresh();
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
        public SimpleCommand<object, object> MouseLeftButtonDownCommand
        {
            get { return _mouseLeftButtonDown ?? (_mouseLeftButtonDown = new SimpleCommand<object, object>(delegate { MouseLeftButtonDownHandler(); })); }
        }

        SimpleCommand<object, object> _mouseLeftButtonDown;

        static void MouseLeftButtonDownHandler() { }
        #endregion

        #region MouseLeftButtonUpCommand
        public SimpleCommand<object, object> MouseLeftButtonUpCommand
        {
            get { return _mouseLeftButtonUp ?? (_mouseLeftButtonUp = new SimpleCommand<object, object>(MouseLeftButtonUpHandler)); }
        }

        SimpleCommand<object, object> _mouseLeftButtonUp;

        void MouseLeftButtonUpHandler(object o)
        {
            if (IsInAnalysisPointMode)
            {
                IsInAnalysisPointMode = false;
                MediatorMessage.Send(MediatorMessage.SetAnalysisPointMode, false);
                MediatorMessage.Send(MediatorMessage.PlaceAnalysisPoint);
            }
            if (!IsQuickLookPointMode) return;
            IsQuickLookPointMode = false;
            MediatorMessage.Send(MediatorMessage.SetupAndRunQuickLookPoint);
        }
        #endregion

        #region MouseRightButtonDownCommand
        public SimpleCommand<object, object> MouseRightButtonDownCommand
        {
            get { return _mouseRightButtonDown ?? (_mouseRightButtonDown = new SimpleCommand<object, object>(MouseRightButtonDownHandler)); }
        }

        SimpleCommand<object, object> _mouseRightButtonDown;

        static void MouseRightButtonDownHandler(object o) { }
        #endregion

        #region MouseRightButtonUpCommand
        public SimpleCommand<object, object> MouseRightButtonUpCommand
        {
            get { return _mouseRightButtonUp ?? (_mouseRightButtonUp = new SimpleCommand<object, object>(MouseRightButtonUpHandler)); }
        }

        SimpleCommand<object, object> _mouseRightButtonUp;

        static void MouseRightButtonUpHandler(object o) { }
        #endregion

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
        }
        #endregion
        #endregion
    }
}