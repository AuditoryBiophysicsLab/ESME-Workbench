using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Markup;
using System.Xml.Serialization;
using Cinch;
using ESME;
using ESMEWorkBench.Controls;
using ESMEWorkBench.Data;
using ESMEWorkBench.Properties;
using ESMEWorkBench.ViewModels.Layers;
using HRC.Navigation;
using HRC.Services;
using MEFedMVVM.Common;
using MEFedMVVM.ViewModelLocator;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Map
{
    [ExportViewModel("MapViewModel")]
    public class MapViewModel : ViewModelBase
    {
        #region Private fields

        readonly IViewAwareStatus _viewAwareStatus;
        readonly IMessageBoxService _messageBoxService;
        WpfMap _wpfMap;

        #endregion

        public string MapDLLVersion { get; private set; }

        [ImportingConstructor]
        public MapViewModel(IViewAwareStatus viewAwareStatus, IMessageBoxService messageBoxService, IHRCColorPickerService colorPickerService)
        {
            MapLayerViewModel.ColorPickerService = colorPickerService;
            viewAwareStatus.ViewLoaded += ViewLoaded;
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Globals.DisplayException(messageBoxService, ex, "***********\nMapViewModel: Mediator registration failed\n***********");
                throw;
            }
            _viewAwareStatus = viewAwareStatus;
            _messageBoxService = messageBoxService;
            //_synchronizationContext = synchronizationContext;

            Cursor = Cursors.Arrow;

            MouseMoveCommand = new SimpleCommand<object, EventToCommandArgs>(delegate(EventToCommandArgs arg)
                                                                             {
                                                                                 var e = (MouseEventArgs) arg.EventArgs;
                                                                                 var point = e.MouseDevice.GetPosition(_wpfMap);
                                                                                 var pointShape = ExtentHelper.ToWorldCoordinate(_wpfMap.CurrentExtent, (float) point.X, (float) point.Y, (float) _wpfMap.ActualWidth, (float) _wpfMap.ActualHeight);
                                                                                 MediatorMessage.Send(MediatorMessage.SetMouseEarthCoordinate, new EarthCoordinate(pointShape.Y, pointShape.X));
                                                                             });

            MouseLeftButtonUpCommand = new SimpleCommand<object, object>(delegate
                                                                         {
                                                                             if (IsInAnalysisPointMode)
                                                                             {
                                                                                 IsInAnalysisPointMode = false;
                                                                                 MediatorMessage.Send(MediatorMessage.SetAnalysisPointMode, false);
                                                                                 MediatorMessage.Send(MediatorMessage.SetupAndRunAnalysisPoint);
                                                                             }
                                                                             if (IsQuickLookPointMode)
                                                                             {
                                                                                 IsQuickLookPointMode = false;
                                                                                 MediatorMessage.Send(MediatorMessage.SetupAndRunQuickLookPoint);
                                                                             }
                                                                         });
        }

        [XmlIgnore]

        #region public bool IsInAnalysisPointMode { get; set; }

        public bool IsInAnalysisPointMode
        {
            get { return _isInAnalysisPointMode; }
            set
            {
                if (_isInAnalysisPointMode == value) return;
                _isInAnalysisPointMode = value;
                NotifyPropertyChanged(IsInAnalysisPointModeChangedEventArgs);
                Cursor = _isInAnalysisPointMode ? Cursors.Cross : Cursors.Arrow;
            }
        }

        static readonly PropertyChangedEventArgs IsInAnalysisPointModeChangedEventArgs = ObservableHelper.CreateArgs<MapViewModel>(x => x.IsInAnalysisPointMode);
        bool _isInAnalysisPointMode;

        #endregion

        [XmlIgnore]
        public bool IsQuickLookPointMode { get; set; }

        #region public Cursor Cursor { get; set; }

        static readonly PropertyChangedEventArgs CursorChangedEventArgs = ObservableHelper.CreateArgs<MapViewModel>(x => x.Cursor);
        Cursor _cursor;

        public Cursor Cursor
        {
            get { return _cursor; }
            set
            {
                if (_cursor == value) return;
                _cursor = value;
                NotifyPropertyChanged(CursorChangedEventArgs);
            }
        }

        #endregion

        public SimpleCommand<Object, EventToCommandArgs> MouseMoveCommand { get; private set; }
        public SimpleCommand<Object, Object> MouseLeftButtonUpCommand { get; private set; }

        void ViewLoaded()
        {
            if (Designer.IsInDesignMode) return;

            _wpfMap = ((MapView) _viewAwareStatus.View).WpfMap;

            MapLayerViewModel.MapOverlay = _wpfMap.Overlays;

            MapDLLVersion = WpfMap.GetVersion();
            _wpfMap.MapUnit = GeographyUnit.DecimalDegree;
            _wpfMap.MapTools.PanZoomBar.HorizontalAlignment = HorizontalAlignment.Left;
            _wpfMap.MapTools.PanZoomBar.VerticalAlignment = VerticalAlignment.Top;
            _wpfMap.MapTools.Logo.IsEnabled = false;

            _wpfMap.CurrentScaleChanged += (s, e) => { if (_experiment != null) _experiment.CurrentScale = e.CurrentScale; };
            _wpfMap.CurrentExtentChanged += (s, e) => { if (_experiment != null) _experiment.CurrentExtent = e.CurrentExtent.GetWellKnownText(); };
            _wpfMap.BackgroundOverlay.BackgroundBrush = new GeoSolidBrush(GeoColor.StandardColors.Black);
            AdornmentOverlay.Layers.Add("Grid", new MyGraticuleAdornmentLayer());
            AdornmentOverlay.Layers["Grid"].IsVisible = Settings.Default.ShowGrid;
            var localizedName = ((MapView) _viewAwareStatus.View).FontFamily.FamilyNames[XmlLanguage.GetLanguage(CultureInfo.CurrentUICulture.Name)];

            var customUnitScaleBarAdornmentLayer = new CustomUnitScaleBarAdornmentLayer
                                                   {
                                                       // Text to be displayed on the scale bar
                                                       UnitText = "Km",
                                                       //Ratio of meters to specified units
                                                       MeterToUnit = 1000,
                                                       GeoFont = new GeoFont(localizedName, 10),
                                                       GeoSolidBrush = new GeoSolidBrush(GeoColor.StandardColors.White),
                                                   };
            AdornmentOverlay.Layers.Add("Scale", customUnitScaleBarAdornmentLayer);
            AdornmentOverlay.Layers["Scale"].IsVisible = Settings.Default.ShowScaleBar;

            _wpfMap.MapTools.PanZoomBar.Visibility = Settings.Default.ShowPanZoom ? Visibility.Visible : Visibility.Hidden;

            MediatorMessage.Send(MediatorMessage.MapViewModelInitialized);
        }

        public GeoCollection<Overlay> Overlays
        {
            get { return _wpfMap.Overlays; }
        }

        public AdornmentOverlay AdornmentOverlay
        {
            get { return _wpfMap.AdornmentOverlay; }
        }

        #region public ObservableCollection<MapLayerViewModel> MapLayers { get; set; }

        [MediatorMessageSink(MediatorMessage.SetMapLayers)]
        void SetMapLayers(ObservableCollection<MapLayerViewModel> mapLayers) { MapLayers = mapLayers; }

        public ObservableCollection<MapLayerViewModel> MapLayers
        {
            get { return _mapLayers ?? (_mapLayers = new ObservableCollection<MapLayerViewModel>()); }
            set
            {
                if (_mapLayers == value) return;
                if (_mapLayers != null) _mapLayers.CollectionChanged -= MapLayersCollectionChanged;
                _wpfMap.Overlays.Clear();
                _mapLayers = value;
                if (_mapLayers != null)
                {
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
                NotifyPropertyChanged(MapLayersChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MapLayersChangedEventArgs = ObservableHelper.CreateArgs<MapViewModel>(x => x.MapLayers);
        ObservableCollection<MapLayerViewModel> _mapLayers;

        #endregion

        #region public Experiment Experiment { get; set; }

        [MediatorMessageSink(MediatorMessage.SetExperiment)]
        void SetExperiment(Experiment experiment) { Experiment = experiment; }

        public Experiment Experiment
        {
            get { return _experiment; }
            set
            {
                if (_experiment == value) return;
                if (_experiment != null) _experiment.MapLayers.CollectionChanged -= MapLayersCollectionChanged;
                _wpfMap.Overlays.Clear();
                _experiment = value;
                if (_experiment != null)
                {
                    foreach (var layer in _experiment.MapLayers.Where(layer => layer.Overlay != null))
                    {
                        //if (layer.LayerType == LayerType.BathymetryRaster) continue;
                        if (!_wpfMap.Overlays.Contains(layer.Name)) _wpfMap.Overlays.Add(layer.Name, layer.Overlay);
                        try
                        {
                            _wpfMap.Refresh(layer.Overlay);
                        }
                        catch(Exception) {}
                    }
                    _experiment.MapLayers.CollectionChanged += MapLayersCollectionChanged;
                    _wpfMap.CurrentExtent = new RectangleShape(_experiment.CurrentExtent);
                    _wpfMap.CurrentScale = _experiment.CurrentScale;
                    _wpfMap.Refresh();
                }
                NotifyPropertyChanged(ExperimentChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExperimentChangedEventArgs = ObservableHelper.CreateArgs<MapViewModel>(x => x.Experiment);
        Experiment _experiment;

        void MapLayersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null)
                        foreach (var layer in e.NewItems.Cast<MapLayerViewModel>().Where(layer => !_wpfMap.Overlays.Contains(layer.Name)).Where(layer => layer.Overlay != null))
                        {
                            _wpfMap.Overlays.Add(layer.Name, layer.Overlay);
                            _wpfMap.Refresh(layer.Overlay);
                        }
                    break;
                case NotifyCollectionChangedAction.Move:
                    Debug.WriteLine("MapView: LayerCollection.Move");
                    break;
                case NotifyCollectionChangedAction.Remove:
                    if (e.OldItems != null) foreach (var layer in e.OldItems) _wpfMap.Overlays.Remove(((MapLayerViewModel) layer).LayerOverlay);
                    break;
                case NotifyCollectionChangedAction.Replace:
                    Debug.WriteLine("MapView: LayerCollection.Replace");
                    break;
                case NotifyCollectionChangedAction.Reset:
                    _wpfMap.Overlays.Clear();
                    var mapLayers = (ObservableCollection<MapLayerViewModel>)sender;
                    foreach (var layer in mapLayers) _wpfMap.Overlays.Add(layer.Name, layer.LayerOverlay);
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
                NotifyPropertyChanged(CurrentExtentChangedEventArgs);
                _wpfMap.CurrentExtent = _currentExtent;
                _wpfMap.Refresh();
                SetCurrentExtent(_currentExtent);
            }
        }

        static readonly PropertyChangedEventArgs CurrentExtentChangedEventArgs = ObservableHelper.CreateArgs<MapViewModel>(x => x.CurrentExtent);
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
                NotifyPropertyChanged(CurrentScaleChangedEventArgs);
                _wpfMap.CurrentScale = _currentScale;
                SetCurrentScale(_currentScale);
            }
        }

        static readonly PropertyChangedEventArgs CurrentScaleChangedEventArgs = ObservableHelper.CreateArgs<MapViewModel>(x => x.CurrentScale);
        double _currentScale;

        #endregion

        [MediatorMessageSink(MediatorMessage.RemoveLayer)]
        void RemoveLayer(MapLayerViewModel mapLayer) { _wpfMap.Overlays.Remove(mapLayer.LayerOverlay); }

        [MediatorMessageSink(MediatorMessage.RefreshMap)]
        void RefreshMap(bool dummy) { _wpfMap.Refresh(); }

        [MediatorMessageSink(MediatorMessage.RefreshLayer)]
        void RefreshLayer(MapLayerViewModel layer) { _wpfMap.Refresh(layer.LayerOverlay); }

        [MediatorMessageSink(MediatorMessage.SetGridOverlayDisplay)]
        void SetGridOverlayDisplay(Boolean isVisible)
        {
            AdornmentOverlay.Layers["Grid"].IsVisible = isVisible;
            RefreshMap(true);
        }

        [MediatorMessageSink(MediatorMessage.SetPanZoomDisplay)]
        void SetPanZoomDisplay(Boolean isVisible)
        {
            _wpfMap.MapTools.PanZoomBar.Visibility = isVisible ? Visibility.Visible : Visibility.Hidden;
            //RefreshMap(true);
        }

        [MediatorMessageSink(MediatorMessage.SetScaleBarDisplay)]
        void SetScaleBarDisplay(Boolean isVisible)
        {
            AdornmentOverlay.Layers["Scale"].IsVisible = isVisible;
            RefreshMap(true);
        }

        [MediatorMessageSink(MediatorMessage.SetMapCursor)]
        void SetMapCursor(Cursor cursor) { Cursor = cursor; }

        [MediatorMessageSink(MediatorMessage.MoveLayerToTop)]
        void MoveLayerToTop(MapLayerViewModel mapLayer)
        {
            _wpfMap.Overlays.MoveToTop(mapLayer.Overlay);
            RefreshMap(true);
            MediatorMessage.Send(MediatorMessage.LayersReordered, mapLayer);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerUp)]
        void MoveLayerUp(MapLayerViewModel mapLayer)
        {
            _wpfMap.Overlays.MoveUp(mapLayer.Overlay);
            RefreshMap(true);
            MediatorMessage.Send(MediatorMessage.LayersReordered, mapLayer);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerDown)]
        void MoveLayerDown(MapLayerViewModel mapLayer)
        {
            _wpfMap.Overlays.MoveDown(mapLayer.Overlay);
            RefreshMap(true);
            MediatorMessage.Send(MediatorMessage.LayersReordered, mapLayer);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerToBottom)]
        void MoveLayerToBottom(MapLayerViewModel mapLayer)
        {
            if (_wpfMap.Overlays.Count > 0)
            {
                //if (_wpfMap.Overlays.IndexOf(mapLayer.Overlay) == -1) _wpfMap.Overlays.Add(mapLayer.Overlay);
                _wpfMap.Overlays.MoveToBottom(mapLayer.Overlay);
                RefreshMap(true);
                MediatorMessage.Send(MediatorMessage.LayersReordered, mapLayer);
            }
        }

        [MediatorMessageSink(MediatorMessage.EnableGUI)]
        void EnableGUI(bool enable)
        {
            ((UserControl)_viewAwareStatus.View).IsEnabled = enable;
        }

    }
}