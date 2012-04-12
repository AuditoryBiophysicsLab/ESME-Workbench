using System;
using System.Collections.Specialized;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Markup;
using System.Windows.Threading;
using System.Xml.Serialization;
using Cinch;
using ESME;
using ESME.Mapping;
using ESMEWorkbench.Properties;
using ESMEWorkbench.ViewModels.Layers;
using ESMEWorkbench.Views;
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
        Dispatcher _dispatcher;
        WpfMap _wpfMap;

        #endregion

        public string MapDLLVersion { get; private set; }

        [ImportingConstructor]
        public MapViewModel(IViewAwareStatus viewAwareStatus, IMessageBoxService messageBox)
        {
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
            _dispatcher = ((MainView)_viewAwareStatus.View).Dispatcher;

            MapLayerViewModel.MapOverlay = _wpfMap.Overlays;

            MapDLLVersion = WpfMap.GetVersion();
            _wpfMap.MapUnit = GeographyUnit.DecimalDegree;
            _wpfMap.MapTools.PanZoomBar.HorizontalAlignment = HorizontalAlignment.Left;
            _wpfMap.MapTools.PanZoomBar.VerticalAlignment = VerticalAlignment.Top;
            _wpfMap.MapTools.Logo.IsEnabled = false;

            _wpfMap.BackgroundOverlay.BackgroundBrush = new GeoSolidBrush(GeoColor.StandardColors.Black);
            _wpfMap.AdornmentOverlay.Layers.Add("Grid", new MyGraticuleAdornmentLayer());
            _wpfMap.AdornmentOverlay.Layers["Grid"].IsVisible = Settings.Default.ShowGrid;
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
            WorldMapLayer = new ShapefileMapLayer
            {
                LayerType = LayerType.BaseMap,
                AreaStyle = AreaStyles.Country2,
                CanBeRemoved = false,
                CanBeReordered = true,
                CanChangeAreaColor = true,
                CanChangeLineColor = true,
                ShapefileName = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"Sample GIS Data\Countries02.shp"),
                Name = "World Map",
            };
            _wpfMap.Overlays.Add(WorldMapLayer.Overlay);

            MediatorMessage.Send(MediatorMessage.MapViewModelInitialized);
        }

        public MapLayerViewModel WorldMapLayer { get; set; }
        public bool IsGridVisible
        {
            get { return Settings.Default.ShowGrid; }
            set
            {
                _wpfMap.AdornmentOverlay.Layers["Grid"].IsVisible = value;
                Settings.Default.ShowGrid = value;
            }
        }
        public bool IsScaleVisible
        {
            get { return Settings.Default.ShowScaleBar; }
            set
            {
                _wpfMap.AdornmentOverlay.Layers["Scale"].IsVisible = value;
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

        #region public MapLayerCollection MapLayers { get; set; }

        [MediatorMessageSink(MediatorMessage.SetMapLayers)]
        void SetMapLayers(MapLayerCollection mapLayers)
        {
            MapLayers = mapLayers;
        }

        public MapLayerCollection MapLayers
        {
            get { return _mapLayers; }
            set
            {
                if (_mapLayers == value) return;
                if (_mapLayers != null)
                {
                    _mapLayers.CurrentExtent = _wpfMap.CurrentExtent;
                    _mapLayers.CollectionChanged -= MapLayersCollectionChanged;
                }
                _wpfMap.Overlays.Clear();
                _mapLayers = value;
                if (_mapLayers != null)
                {
                    _wpfMap.CurrentExtent = _mapLayers.CurrentExtent;
                    foreach (var layer in _mapLayers.Where(layer => layer.Overlay != null))
                    {
                        //if (layer.LayerType == LayerType.BathymetryRaster) continue;
                        if (!_wpfMap.Overlays.Contains(layer.Name)) _wpfMap.Overlays.Add(layer.Name, layer.Overlay);
                        try
                        {
                            _wpfMap.Refresh(layer.Overlay);
                        }
                        catch (Exception) {}
                    }
                }
                if (_mapLayers != null) _mapLayers.CollectionChanged += MapLayersCollectionChanged;
            }
        }

        MapLayerCollection _mapLayers;

        void MapLayersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    //Debug.WriteLine("MapView: LayerCollection.Add");
                    for (var itemIndex = 0; itemIndex < e.NewItems.Count; itemIndex++)
                    {
                        var index = itemIndex;
                        _dispatcher.InvokeInBackgroundIfRequired(() => _wpfMap.Overlays.Add(((MapLayerViewModel)e.NewItems[index]).Overlay));
                        _dispatcher.InvokeInBackgroundIfRequired(() => _wpfMap.Refresh(((MapLayerViewModel)e.NewItems[index]).Overlay));
                    }
                    break;
                case NotifyCollectionChangedAction.Move:
                    //Debug.WriteLine("MapView: LayerCollection.Move");
                    for (var itemIndex = 0; itemIndex < e.NewItems.Count; itemIndex++)
                    {
                        _dispatcher.InvokeInBackgroundIfRequired(() => _wpfMap.Overlays.RemoveAt(e.OldStartingIndex));
                        var index = itemIndex;
                        _dispatcher.InvokeInBackgroundIfRequired(() =>  _wpfMap.Overlays.Insert(e.NewStartingIndex + index, ((MapLayerViewModel)e.NewItems[index]).Overlay));
                    }
                    _wpfMap.Refresh();
                    break;
                case NotifyCollectionChangedAction.Remove:
                    //Debug.WriteLine("MapView: LayerCollection.Remove");
                    for (var itemIndex = 0; itemIndex < e.OldItems.Count; itemIndex++)
                    {
                        var index = itemIndex;
                        _dispatcher.InvokeInBackgroundIfRequired(() => _wpfMap.Overlays.Remove(((MapLayerViewModel)e.OldItems[index]).Overlay));
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    //Debug.WriteLine("MapView: LayerCollection.Replace (OldItemCount = {0}, OldStartIndex = {1}, NewItemCount = {2}, NewStartIndex = {3}", e.OldItems.Count, e.OldStartingIndex, e.NewItems.Count, e.NewStartingIndex);
                    for (var itemIndex = 0; itemIndex < e.OldItems.Count; itemIndex++)
                    {
                        var index = itemIndex;
                        _dispatcher.InvokeInBackgroundIfRequired(() => _wpfMap.Overlays.Remove(((MapLayerViewModel)e.OldItems[index]).Overlay));
                    }
                    for (var itemIndex = 0; itemIndex < e.NewItems.Count; itemIndex++)
                    {
                        var index = itemIndex;
                        _dispatcher.InvokeInBackgroundIfRequired(() => _wpfMap.Overlays.Insert(e.NewStartingIndex + index - 1, ((MapLayerViewModel)e.NewItems[index]).Overlay));
                    }
                    //_wpfMap.Refresh();
                    break;
                case NotifyCollectionChangedAction.Reset:
                    //Debug.WriteLine("MapView: LayerCollection.Reset");
                    _wpfMap.Overlays.Clear();
                    foreach (var layer in MapLayers) _wpfMap.Overlays.Add(layer.Overlay);
                    break;
            }
        }

        #endregion

        [MediatorMessageSink(MediatorMessage.SetAnalysisPointMode)]
        void SetAnalysisPointMode(bool mode)
        {
            IsInAnalysisPointMode = mode;
        }

        [MediatorMessageSink(MediatorMessage.QuickLookPointCommand)]
        void QuickLookPointCommand(bool dummy)
        {
            IsQuickLookPointMode = true;
            Cursor = Cursors.Cross;
        }

        #region public RectangleShape CurrentExtent { get; set; }

        [MediatorMessageSink(MediatorMessage.SetCurrentExtent)]
        void SetCurrentExtent(RectangleShape currentExtent)
        {
            CurrentExtent = currentExtent;
        }

        public RectangleShape CurrentExtent
        {
            get { return _currentExtent; }
            set
            {
                if (_currentExtent == value) return;
                _currentExtent = value;
                _wpfMap.CurrentExtent = _currentExtent;
                _wpfMap.Refresh();
                if (MapLayers != null) MapLayers.CurrentExtent = _currentExtent;
            }
        }

        RectangleShape _currentExtent;

        #endregion

        #region public double CurrentScale { get; set; }

        [MediatorMessageSink(MediatorMessage.SetCurrentScale)]
        void SetCurrentScale(double currentScale)
        {
            CurrentScale = currentScale;
        }

        public double CurrentScale
        {
            get { return _currentScale; }
            set
            {
                if (_currentScale == value) return;
                _currentScale = value;
                _wpfMap.CurrentScale = _currentScale;
                SetCurrentScale(_currentScale);
            }
        }

        double _currentScale;

        #endregion

        [MediatorMessageSink(MediatorMessage.RemoveLayer)]
        void RemoveLayer(MapLayerViewModel mapLayer) { MapLayers.Remove(mapLayer); }

        [MediatorMessageSink(MediatorMessage.RefreshMap)]
        void RefreshMap(bool dummy) { _wpfMap.Refresh(); }

        [MediatorMessageSink(MediatorMessage.RefreshLayer)]
        void RefreshLayer(MapLayerViewModel layer) { _wpfMap.Refresh(layer.LayerOverlay); }

        [MediatorMessageSink(MediatorMessage.SetMapCursor)]
        void SetMapCursor(Cursor cursor) { Cursor = cursor; }

        [MediatorMessageSink(MediatorMessage.MoveLayerToTop)]
        void MoveLayerToTop(MapLayerViewModel mapLayer)
        {
            //_wpfMap.Overlays.MoveToTop(mapLayer.Overlay);
            //RefreshMap(true);
            //MediatorMessage.Send(MediatorMessage.LayersReordered, mapLayer);
            Debug.WriteLine(string.Format("MediatorMessage.MoveLayerToTop: {0}", mapLayer.Name));
            var curIndex = MapLayers.IndexOf(mapLayer);
            MapLayers.Move(curIndex, MapLayers.Count);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerUp)]
        void MoveLayerUp(MapLayerViewModel mapLayer)
        {
            Debug.WriteLine(string.Format("MediatorMessage.MoveLayerUp: {0}", mapLayer.Name));
            var curIndex = MapLayers.IndexOf(mapLayer);
            MapLayers.Move(curIndex, curIndex + 1);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerDown)]
        void MoveLayerDown(MapLayerViewModel mapLayer)
        {
            Debug.WriteLine(string.Format("MediatorMessage.MoveLayerDown: {0}", mapLayer.Name));
            var curIndex = MapLayers.IndexOf(mapLayer);
            MapLayers.Move(curIndex, curIndex - 1);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerToBottom)]
        void MoveLayerToBottom(MapLayerViewModel mapLayer)
        {
            Debug.WriteLine(string.Format("MediatorMessage.MoveLayerToBottom: {0}", mapLayer.Name));
            var curIndex = MapLayers.IndexOf(mapLayer);
            //if (curIndex == -1) return;
            MapLayers.Move(curIndex, 0);
        }

        [MediatorMessageSink(MediatorMessage.EnableGUI)]
        void EnableGUI(bool enable)
        {
            ((UserControl)_viewAwareStatus.View).IsEnabled = enable;
        }
        #region Mouse events

        #region MouseLeftButtonDownCommand
        public SimpleCommand<object, object> MouseLeftButtonDownCommand
        {
            get { return _mouseLeftButtonDown ?? (_mouseLeftButtonDown = new SimpleCommand<object, object>(delegate { MouseLeftButtonDownHandler(); })); }
        }

        SimpleCommand<object, object> _mouseLeftButtonDown;

        void MouseLeftButtonDownHandler() { }
        #endregion

        #region MouseLeftButtonUpCommand
        public SimpleCommand<object, object> MouseLeftButtonUpCommand
        {
            get { return _mouseLeftButtonUp ?? (_mouseLeftButtonUp = new SimpleCommand<object, object>(delegate { MouseLeftButtonUpHandler(); })); }
        }

        SimpleCommand<object, object> _mouseLeftButtonUp;

        void MouseLeftButtonUpHandler()
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
            get { return _mouseRightButtonDown ?? (_mouseRightButtonDown = new SimpleCommand<object, object>(delegate { MouseRightButtonDownHandler(); })); }
        }

        SimpleCommand<object, object> _mouseRightButtonDown;

        void MouseRightButtonDownHandler() { }
        #endregion

        #region MouseRightButtonUpCommand
        public SimpleCommand<object, object> MouseRightButtonUpCommand
        {
            get { return _mouseRightButtonUp ?? (_mouseRightButtonUp = new SimpleCommand<object, object>(delegate { MouseRightButtonUpHandler(); })); }
        }

        SimpleCommand<object, object> _mouseRightButtonUp;

        void MouseRightButtonUpHandler() { }
        #endregion

        #region MouseMoveCommand
        public SimpleCommand<object, EventToCommandArgs> MouseMoveCommand
        {
            get { return _mouseMove ?? (_mouseMove = new SimpleCommand<object, EventToCommandArgs>(delegate(EventToCommandArgs arg) { MouseMoveHandler(arg); })); }
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