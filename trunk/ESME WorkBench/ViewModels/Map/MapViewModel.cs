using System;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Windows;
using System.Windows.Input;
using System.Windows.Markup;
using Cinch;
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

        readonly IViewAwareStatus _viewAwareStatusService;
        readonly IMessageBoxService _messageBoxService;
        WpfMap _wpfMap;

        #endregion

        public string MapDLLVersion { get; private set; }

        [ImportingConstructor]
        public MapViewModel(IViewAwareStatus viewAwareStatusService, IMessageBoxService messageBoxService, IHRCColorPickerService colorPickerService)
        {
            MapLayerViewModel.ColorPickerService = colorPickerService;
            viewAwareStatusService.ViewLoaded += ViewLoaded;
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Globals.DisplayException(messageBoxService, ex, "***********\nMapViewModel: Mediator registration failed\n***********");
                throw;
            }
            _viewAwareStatusService = viewAwareStatusService;
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
                                                                             if (!IsQuickLookMode) return;
                                                                             IsQuickLookMode = false;
                                                                             MediatorMessage.Send(MediatorMessage.RunQuickLook);
                                                                         });
        }

        public bool IsQuickLookMode { get; set; }

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

            _wpfMap = ((MapView) _viewAwareStatusService.View).WpfMap;

            MapLayerViewModel.MapOverlay = _wpfMap.Overlays;

            MapDLLVersion = WpfMap.GetVersion();
            _wpfMap.MapUnit = GeographyUnit.DecimalDegree;
            _wpfMap.MapTools.PanZoomBar.HorizontalAlignment = HorizontalAlignment.Left;
            _wpfMap.MapTools.PanZoomBar.VerticalAlignment = VerticalAlignment.Top;
            _wpfMap.MapTools.Logo.IsEnabled = false;

            _wpfMap.CurrentScaleChanged += (s, e) => { if (_experiment != null) _experiment.CurrentScale = e.CurrentScale; };
            _wpfMap.CurrentExtentChanged += (s, e) => { if (_experiment != null) _experiment.CurrentExtent = e.CurrentExtent.GetWellKnownText(); };

            AdornmentOverlay.Layers.Add("Grid", new MyGraticuleAdornmentLayer());
            AdornmentOverlay.Layers["Grid"].IsVisible = Settings.Default.ShowGrid;
            var localizedName = ((MapView) _viewAwareStatusService.View).FontFamily.FamilyNames[XmlLanguage.GetLanguage(CultureInfo.CurrentUICulture.Name)];

            var customUnitScaleBarAdornmentLayer = new CustomUnitScaleBarAdornmentLayer
                                                   {
                                                       // Text to be displayed on the scale bar
                                                       UnitText = "Km",
                                                       //Ratio of meters to specified units
                                                       MeterToUnit = 1000,
                                                       GeoFont = new GeoFont(localizedName, 10),
                                                       GeoSolidBrush = new GeoSolidBrush(GeoColor.StandardColors.Black),
                                                   };
            AdornmentOverlay.Layers.Add("Scale", customUnitScaleBarAdornmentLayer);

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

        #region public Experiment Experiment { get; set; }

        public Experiment Experiment
        {
            get { return _experiment; }
            set
            {
                if (_experiment == value) return;
                if (_experiment != null) _experiment.MapLayers.CollectionChanged -= MapLayers_CollectionChanged;
                _wpfMap.Overlays.Clear();
                _experiment = value;
                if (_experiment != null)
                {
                    foreach (var layer in _experiment.MapLayers.Where(layer => layer.Overlay != null))
                    {
                        _wpfMap.Overlays.Add(layer.Name, layer.Overlay);
                        _wpfMap.Refresh(layer.Overlay);
                    }
                    _experiment.MapLayers.CollectionChanged += MapLayers_CollectionChanged;
                    _wpfMap.CurrentExtent = new RectangleShape(_experiment.CurrentExtent);
                    _wpfMap.CurrentScale = _experiment.CurrentScale;
                    _wpfMap.Refresh();
                }
                NotifyPropertyChanged(ExperimentChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExperimentChangedEventArgs = ObservableHelper.CreateArgs<MapViewModel>(x => x.Experiment);
        Experiment _experiment;

        void MapLayers_CollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
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
                    foreach (var layer in _experiment.MapLayers) _wpfMap.Overlays.Add(layer.Name, layer.LayerOverlay);
                    break;
            }
        }

        #endregion

        [MediatorMessageSink(MediatorMessage.QuickLookCommand)]
        void QuickLookCommand(bool dummy)
        {
            IsQuickLookMode = true;
            Cursor = Cursors.Cross;
        }

        [MediatorMessageSink(MediatorMessage.SetExperiment)]
        void SetExperiment(Experiment experiment) { Experiment = experiment; }

        [MediatorMessageSink(MediatorMessage.AddMapLayer)]
        void AddMapLayer(MapLayerViewModel mapLayer)
        {
            if (mapLayer.Index >= 0) _wpfMap.Overlays.Insert(mapLayer.Index, mapLayer.LayerOverlay);
            else _wpfMap.Overlays.Add(mapLayer.LayerOverlay);

            MediatorMessage.Send(MediatorMessage.LayerAdded, mapLayer);
        }

        [MediatorMessageSink(MediatorMessage.RemoveLayer)]
        void RemoveLayer(MapLayerViewModel mapLayer) { _wpfMap.Overlays.Remove(mapLayer.LayerOverlay); }

        [MediatorMessageSink(MediatorMessage.RefreshMap)]
        void RefreshMap(bool dummy) { _wpfMap.Refresh(); }

        [MediatorMessageSink(MediatorMessage.RefreshLayer)]
        void RefreshLayer(MapLayerViewModel layer) { _wpfMap.Refresh(layer.LayerOverlay); }

        [MediatorMessageSink(MediatorMessage.ToggleGridOverlayDisplayCommand)]
        void ToggleGridOverlayDisplay(Boolean isVisible)
        {
            AdornmentOverlay.Layers["Grid"].IsVisible = Settings.Default.ShowGrid = isVisible;
            RefreshMap(true);
        }

        [MediatorMessageSink(MediatorMessage.TogglePanZoomDisplayCommand)]
        void TogglePanZoomDisplay(Boolean isVisible)
        {
            _wpfMap.MapTools.PanZoomBar.Visibility = isVisible ? Visibility.Visible : Visibility.Hidden;
            Settings.Default.ShowPanZoom = isVisible;
        }

        [MediatorMessageSink(MediatorMessage.ToggleScaleBarDisplayCommand)]
        void ToggleScaleBarDisplayCommand(Boolean isVisible)
        {
            AdornmentOverlay.Layers["Scale"].IsVisible = Settings.Default.ShowScaleBar = isVisible;
            RefreshMap(true);
        }

        [MediatorMessageSink(MediatorMessage.SetMapCursor)]
        void SetMapCursor(Cursor cursor) { Cursor = cursor; }

        [MediatorMessageSink(MediatorMessage.MoveLayerToTop)]
        void MoveLayerToTop(MapLayerViewModel mapLayer)
        {
            _wpfMap.Overlays.MoveToTop(mapLayer.LayerOverlay);
            RefreshMap(true);
            MediatorMessage.Send(MediatorMessage.LayersReordered, mapLayer);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerUp)]
        void MoveLayerUp(MapLayerViewModel mapLayer)
        {
            _wpfMap.Overlays.MoveUp(mapLayer.LayerOverlay);
            RefreshMap(true);
            MediatorMessage.Send(MediatorMessage.LayersReordered, mapLayer);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerDown)]
        void MoveLayerDown(MapLayerViewModel mapLayer)
        {
            _wpfMap.Overlays.MoveDown(mapLayer.LayerOverlay);
            RefreshMap(true);
            MediatorMessage.Send(MediatorMessage.LayersReordered, mapLayer);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerToBottom)]
        void MoveLayerToBottom(MapLayerViewModel mapLayer)
        {
            _wpfMap.Overlays.MoveToBottom(mapLayer.LayerOverlay);
            RefreshMap(true);
            MediatorMessage.Send(MediatorMessage.LayersReordered, mapLayer);
        }

    }
}