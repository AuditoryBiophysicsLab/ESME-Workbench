﻿using System;
using System.Diagnostics;
using System.Linq;
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
            var localizedName = "Segoe UI";

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

        [MediatorMessageSink(MediatorMessage.ApplicationClosing), UsedImplicitly] void ApplicationClosing(bool mode) { if (_soundSpeedProfileWindowView != null) _soundSpeedProfileWindowView.Close(); }

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
            MediatorMessage.Send(MediatorMessage.MapLeftButtonDown, geo);
        }

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
            MediatorMessage.Send(MediatorMessage.MapLeftButtonUp, geo);
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
            MediatorMessage.Send(MediatorMessage.MapRightButtonDown, geo);
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
            MediatorMessage.Send(MediatorMessage.MapRightButtonUp, geo);
        }
        #endregion

        Geo GetMouseEventArgsGeo(MouseEventArgs e)
        {
            var point = e.GetPosition(_wpfMap);
            var pointShape = ExtentHelper.ToWorldCoordinate(_wpfMap.CurrentExtent, (float)point.X, (float)point.Y, (float)_wpfMap.ActualWidth, (float)_wpfMap.ActualHeight);
            return new Geo(pointShape.Y, pointShape.X);
        }

        #region MouseLeaveCommand
        public SimpleCommand<object, EventToCommandArgs> MouseLeaveCommand { get { return _mouseLeave ?? (_mouseLeave = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.SetMouseGeo, (Geo)null))); } }
        SimpleCommand<object, EventToCommandArgs> _mouseLeave;
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
            MediatorMessage.Send(MediatorMessage.SetMouseGeo, new Geo(pointShape.Y, pointShape.X));
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
        }

        SoundSpeedProfileWindowView _soundSpeedProfileWindowView;
        #endregion

        #endregion

        public EditableRectangleOverlayViewModel EditableRectangleOverlayViewModel { get; set; }
        public EditablePolygonOverlayViewModel EditablePolygonOverlayViewModel { get; set; }
    }
}