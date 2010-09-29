using System;
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
                Debug.WriteLine("***********\nMapViewModel: Mediator registration failed: " + ex.Message + "\n***********");
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
            MapDLLVersion = WpfMap.GetVersion();
            _wpfMap.MapUnit = GeographyUnit.DecimalDegree;
            _wpfMap.MapTools.PanZoomBar.HorizontalAlignment = HorizontalAlignment.Left;
            _wpfMap.MapTools.PanZoomBar.VerticalAlignment = VerticalAlignment.Top;
            _wpfMap.MapTools.Logo.IsEnabled = false;

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
            var appPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
            MediatorMessage.Send(MediatorMessage.AddMapLayer, new ShapefileMapLayer
                                                                  {
                                                                      AreaStyle = MapLayer.RandomAreaStyle,
                                                                      ShapefileName = Path.Combine(appPath, @"Sample GIS Data\Countries02.shp"),
                                                                      Name = "Base Map",
                                                                  });
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

            _wpfMap.CurrentExtent = new RectangleShape(new PointShape(-180, 90), new PointShape(180, -90));
            _wpfMap.ZoomToScale(_wpfMap.ZoomLevelScales[3]);
        }

        [MediatorMessageSink(MediatorMessage.QuickLookCommand)]
        void QuickLook(bool dummy)
        {
            IsQuickLookMode = true;
            Cursor = Cursors.Cross;
        }

        [MediatorMessageSink(MediatorMessage.AddMapLayer)]
        void AddLayer(MapLayer layer)
        {
            _wpfMap.Overlays.Add(layer);
            layer.LayerViewModel = new LayerViewModel(layer);

            //_synchronizationContext.InvokeWithoutBlocking(x => MediatorMessage.Send(MediatorMessage.AddListLayer, layer.LayerViewModel), null);
            MediatorMessage.Send(MediatorMessage.AddListLayer, layer.LayerViewModel);
        }

        [MediatorMessageSink(MediatorMessage.RemoveLayer)]
        void RemoveLayer(MapLayer layer) { _wpfMap.Overlays.Remove(layer); }

        [MediatorMessageSink(MediatorMessage.RefreshMap)]
        void RefreshMapView(bool dummy) { _wpfMap.Refresh(); }

        [MediatorMessageSink(MediatorMessage.ToggleGridOverlayDisplayCommand)]
        void ToggleGridOverlayDisplay(Boolean isVisible)
        {
            AdornmentOverlay.Layers["Grid"].IsVisible = Settings.Default.ShowGrid = isVisible;
            RefreshMapView(true);
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
        void MoveLayerToTop(MapLayer mapLayer)
        {
            _wpfMap.Overlays.MoveToTop(mapLayer);
            MapLayerIndexQuery(mapLayer);
            MediatorMessage.Send(MediatorMessage.ListLayerMoveToIndex, mapLayer);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerUp)]
        void MoveLayerUp(MapLayer mapLayer)
        {
            _wpfMap.Overlays.MoveUp(mapLayer);
            MapLayerIndexQuery(mapLayer);
            MediatorMessage.Send(MediatorMessage.ListLayerMoveToIndex, mapLayer);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerDown)]
        void MoveLayerDown(MapLayer mapLayer)
        {
            _wpfMap.Overlays.MoveDown(mapLayer);
            MapLayerIndexQuery(mapLayer);
            MediatorMessage.Send(MediatorMessage.ListLayerMoveToIndex, mapLayer);
        }

        [MediatorMessageSink(MediatorMessage.MoveLayerToBottom)]
        void MoveLayerToBottom(MapLayer mapLayer)
        {
            _wpfMap.Overlays.MoveToBottom(mapLayer);
            MapLayerIndexQuery(mapLayer);
            MediatorMessage.Send(MediatorMessage.ListLayerMoveToIndex, mapLayer);
        }

        [MediatorMessageSink(MediatorMessage.MapLayerIndexQuery)]
        void MapLayerIndexQuery(MapLayer mapLayer)
        {
            mapLayer.MapLayerIndex = _wpfMap.Overlays.IndexOf(mapLayer);
        }

        [MediatorMessageSink(MediatorMessage.AddFileLayer)]
        void AddFileLayer(string fileName)
        {
            try
            {
                switch (Path.GetExtension(fileName).ToLower())
                {
                    case ".shp":
                        MediatorMessage.Send(MediatorMessage.AddMapLayer, new ShapefileMapLayer
                                                                              {
                                                                                  AreaStyle = MapLayer.RandomAreaStyle,
                                                                                  ShapefileName = fileName,
                                                                              });
                        break;
                    case ".ovr":
                        MediatorMessage.Send(MediatorMessage.AddMapLayer, new OverlayFileMapLayer
                                                                              {
                                                                                  OverlayFileName = fileName,
                                                                              });
                        break;
                    case ".eeb":
                        MediatorMessage.Send(MediatorMessage.AddMapLayer, new BathymetryBoundsMapLayer
                                                                              {
                                                                                  BathymetryFileName = fileName,
                                                                              });
                        break;
                }
                _wpfMap.Refresh();
            }
            catch (Exception ex)
            {
                Globals.DisplayException(_messageBoxService, ex, "Error opening file {0}", fileName);
            }
        }

        [MediatorMessageSink(MediatorMessage.AddScenarioLayer)]
        void AddScenarioLayer(NemoFile nemoFile)
        {
            var simArea = new OverlayShapeMapLayer
                          {
                              Name = nemoFile.Scenario.SimAreaName + " sim area",
                              LayerType = LayerType.Scenario,
                          };
            simArea.Add(nemoFile.Scenario.OverlayFile.Shapes);
            simArea.Done();
            MediatorMessage.Send(MediatorMessage.AddMapLayer, simArea);
            var platformCount = 0;
            foreach (var platform in nemoFile.Scenario.Platforms)
            {
                platformCount++;
                var platformLayerName = "Platform " + platformCount + ": " + platform.Name;
                var track = new OverlayShapeMapLayer
                            {
                                Name = platformLayerName + " track",
                                CustomLineStyle = new CustomStartEndLineStyle(PointSymbolType.Circle, Colors.Green, 5, PointSymbolType.Square, Colors.Red, 5, Colors.DarkGray, 1),
                                LayerType = LayerType.Track,
                            };
                var behavior = new BehaviorModel(platform);
                track.Add(behavior.CourseOverlay);
                track.Done();
                MediatorMessage.Send(MediatorMessage.AddMapLayer, track);
                var opAreaCount = 0;
                foreach (var trackdef in platform.Trackdefs)
                {
                    opAreaCount++;
                    var opArea = new OverlayShapeMapLayer
                                 {
                                     Name = platformLayerName + " op area" + (platform.Trackdefs.Count == 1 ? "" : " " + opAreaCount),
                                     LayerType = LayerType.Scenario,
                                 };
                    opArea.Add(trackdef.OverlayFile.Shapes);
                    opArea.Done();
                    MediatorMessage.Send(MediatorMessage.AddMapLayer, opArea);
                }
            }

            _wpfMap.Refresh();
        }
    }
}