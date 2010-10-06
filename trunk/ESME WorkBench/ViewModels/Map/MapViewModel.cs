using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;
using Cinch;
using ESME.NEMO;
using ESME.Platform;
using ESMEWorkBench.Controls;
using ESMEWorkBench.Data;
using ESMEWorkBench.Properties;
using ESMEWorkBench.ViewModels.Layers;
using HRC.Navigation;
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
        public MapViewModel(IViewAwareStatus viewAwareStatusService, IMessageBoxService messageBoxService)
        {
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

            _wpfMap.CurrentScaleChanged += (s, e) => MediatorMessage.Send(MediatorMessage.CurrentScaleChanged, e);
            _wpfMap.CurrentExtentChanged += (s, e) => MediatorMessage.Send(MediatorMessage.CurrentExtentChanged, e);

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

        [MediatorMessageSink(MediatorMessage.InitializeMapView)]
        void InitializeMapView(bool dummy)
        {
            //var appPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
            //MediatorMessage.Send(MediatorMessage.AddMapLayer, new ShapefileMapLayer
            //                                                  {
            //                                                      AreaStyle = MapLayerViewModel.RandomAreaStyle,
            //                                                      ShapefileName = Path.Combine(appPath, @"Sample GIS Data\Countries02.shp"),
            //                                                      Name = "Base Map",
            //                                                      LayerType = LayerType.BaseMap,
            //                                                  });
            //Overlays.Add("Basemap", new ShapefileMapLayer
            //                        {
            //                            LineColor = Colors.Purple,
            //                            AreaColor = Colors.Yellow,
            //                            ShapefileName = Path.Combine(appPath, @"Sample GIS Data\Countries02.shp"),
            //                        });
            //Overlays["Basemap"].IsVisible = Settings.Default.ShowBasemap;
            AdornmentOverlay.Layers.Add("Grid", new MyGraticuleAdornmentLayer());
            AdornmentOverlay.Layers["Grid"].IsVisible = Settings.Default.ShowGrid;

            _wpfMap.MapTools.PanZoomBar.Visibility = Settings.Default.ShowPanZoom ? Visibility.Visible : Visibility.Hidden;

            //_wpfMap.CurrentExtent = new RectangleShape(new PointShape(-180, 90), new PointShape(180, -90));
            //_wpfMap.ZoomToScale(_wpfMap.ZoomLevelScales[3]);
        }

        [MediatorMessageSink(MediatorMessage.QuickLookCommand)]
        void QuickLookCommand(bool dummy)
        {
            IsQuickLookMode = true;
            Cursor = Cursors.Cross;
        }

        [MediatorMessageSink(MediatorMessage.AddMapLayer)]
        void AddMapLayer(MapLayerViewModel mapLayer)
        {
            if (mapLayer.Index >= 0)
                _wpfMap.Overlays.Insert(mapLayer.Index, mapLayer.LayerOverlay);
            else
                _wpfMap.Overlays.Add(mapLayer.LayerOverlay);

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

        [MediatorMessageSink(MediatorMessage.SetCurrentScale)]
        void SetCurrentScale(double newScale)
        {
            _wpfMap.CurrentScale = newScale;
        }

        [MediatorMessageSink(MediatorMessage.SetCurrentExtent)]
        void SetCurrentExtent(RectangleShape newExtent)
        {
            _wpfMap.CurrentExtent = newExtent;
            _wpfMap.Refresh();
        }
    }
}