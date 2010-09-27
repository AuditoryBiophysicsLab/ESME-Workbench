using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Windows;
using System.Windows.Input;
using Cinch;
using ESME.NEMO;
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

        WpfMap _map;

        #endregion

        public string MapDLLVersion { get; private set; }

        [ImportingConstructor]
        public MapViewModel(IViewAwareStatus viewAwareStatusService, IMessageBoxService messageBoxService)
        {
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

            _viewAwareStatusService.ViewLoaded += ViewLoaded;

            Cursor = Cursors.Arrow;

            MouseMoveCommand = new SimpleCommand<object, EventToCommandArgs>(delegate(EventToCommandArgs arg)
                                                                             {
                                                                                 var e = (MouseEventArgs) arg.EventArgs;
                                                                                 var point = e.MouseDevice.GetPosition(_map);
                                                                                 var pointShape = ExtentHelper.ToWorldCoordinate(_map.CurrentExtent, (float) point.X, (float) point.Y, (float) _map.ActualWidth, (float) _map.ActualHeight);
                                                                                 Mediator.Instance.NotifyColleagues("SetMouseEarthCoordinateMessage", new EarthCoordinate(pointShape.Y, pointShape.X));
                                                                             });

            MouseLeftButtonUpCommand = new SimpleCommand<object, object>(delegate
                                                                         {
                                                                             if (!IsQuickLookMode) return;
                                                                             IsQuickLookMode = false;
                                                                             Mediator.Instance.NotifyColleagues("RunQuickLookMessage");
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

            _map = ((MapView) _viewAwareStatusService.View).Map1;
            MapDLLVersion = WpfMap.GetVersion();
            _map.MapUnit = GeographyUnit.DecimalDegree;
            _map.MapTools.PanZoomBar.HorizontalAlignment = HorizontalAlignment.Left;
            _map.MapTools.PanZoomBar.VerticalAlignment = VerticalAlignment.Top;
            _map.MapTools.Logo.IsEnabled = false;

            var appPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);

            Overlays.Add("Basemap", new ShapefileMapLayer(Path.Combine(appPath, @"Sample GIS Data\Countries02.shp")));
            //Overlays["Basemap"].IsVisible = Settings.Default.ShowBasemap;

            AdornmentOverlay.Layers.Add("Grid", new MyGraticuleAdornmentLayer());
            AdornmentOverlay.Layers["Grid"].IsVisible = Settings.Default.ShowGrid;

            _map.MapTools.PanZoomBar.Visibility = Settings.Default.ShowPanZoom ? Visibility.Visible : Visibility.Hidden;

            _map.CurrentExtent = new RectangleShape(new PointShape(-180, 90), new PointShape(180, -90));
            _map.ZoomToScale(_map.ZoomLevelScales[3]);

            Mediator.Instance.NotifyColleagues("MapViewModelInitializedMessage", true);
        }

        public GeoCollection<Overlay> Overlays
        {
            get { return _map.Overlays; }
        }

        public AdornmentOverlay AdornmentOverlay
        {
            get { return _map.AdornmentOverlay; }
        }

        [MediatorMessageSink("ReorderLayersInMapViewMessage")]
        void ReorderLayersInMapView(MapLayerReorderDescriptor mapLayerReorderDescriptor)
        {
            var source = mapLayerReorderDescriptor.SourceLayer;
            var target = mapLayerReorderDescriptor.TargetLayer;
            var minIndex = int.MaxValue;
            var maxIndex = int.MinValue;
            switch (mapLayerReorderDescriptor.MapLayerReorderCommand)
            {
                case MapLayerReorderCommand.MoveToBack:
                    Overlays.MoveTo(source.Overlay, 1);
                    break;
                case MapLayerReorderCommand.MoveBackward:
                    if ((target.Children == null) || (target.Children.Count == 0))
                        Overlays.MoveDown(source.Overlay);
                    else
                    {
                        FindMinMaxIndices(target, ref minIndex, ref maxIndex);
                        Overlays.MoveTo(source.Overlay, minIndex);
                    }
                    break;
                case MapLayerReorderCommand.MoveForward:
                    if ((target.Children == null) || (target.Children.Count == 0))
                        Overlays.MoveUp(source.Overlay);
                    else
                    {
                        FindMinMaxIndices(target, ref minIndex, ref maxIndex);
                        Overlays.MoveTo(source.Overlay, maxIndex);
                    }
                    break;
                case MapLayerReorderCommand.MoveToFront:
                    Overlays.MoveToTop(source.Overlay);
                    break;
            }
        }

        void FindMinMaxIndices(LayerViewModel layer, ref int minIndex, ref int maxIndex)
        {
            if (layer.Overlay != null)
            {
                var curIndex = Overlays.IndexOf(layer.Overlay);
                minIndex = Math.Min(minIndex, curIndex);
                maxIndex = Math.Max(maxIndex, curIndex);
            }
            foreach (var child in layer.Children)
                FindMinMaxIndices(child, ref minIndex, ref maxIndex);
        }

        [MediatorMessageSink("RemoveOverlayFromMapViewMessage")]
        void RemoveOverlayFromMapView(Overlay overlay) { Overlays.Remove(overlay); }

        [MediatorMessageSink("RefreshMapViewMessage")]
        void RefreshMapView(bool dummy) { _map.Refresh(); }

        [MediatorMessageSink("AddOverlayToMapViewMessage")]
        void AddOverlayToMapView(Overlay overlay) { Overlays.Add(overlay); }

        [MediatorMessageSink("SetBaseMapDisplayMessage")]
        void SetBaseMapDisplay(Boolean isVisible) { Overlays["Basemap"].IsVisible = Settings.Default.ShowBasemap = isVisible; }

        [MediatorMessageSink("SetGridOverlayDisplayMessage")]
        void SetGridOverlayDisplay(Boolean isVisible)
        {
            AdornmentOverlay.Layers["Grid"].IsVisible = Settings.Default.ShowGrid = isVisible;
            RefreshMapView(true);
        }

        [MediatorMessageSink("SetPanZoomDisplayMessage")]
        void SetPanZoomDisplay(Boolean isVisible)
        {
            _map.MapTools.PanZoomBar.Visibility = isVisible ? Visibility.Visible : Visibility.Hidden;
            Settings.Default.ShowPanZoom = isVisible;
        }

        [MediatorMessageSink("SetMapCursorMessage")]
        void SetMapCursor(Cursor cursor) { Cursor = cursor; }

        [MediatorMessageSink("AddFileLayerToMapViewMessage")]
        void AddFileLayerToMapView(string fileName)
        {
            try
            {
                switch (Path.GetExtension(fileName).ToLower())
                {
                    case ".shp":
                        var shapefileLayer = new ShapefileMapLayer(fileName);
                        Mediator.Instance.NotifyColleagues("AddLayerToTreeViewMessage", new LayerViewModel(shapefileLayer.Name, shapefileLayer));
                        Overlays.Add(shapefileLayer.Name, shapefileLayer);
                        break;
                    case ".ovr":
                        var overlayLayer = new OverlayFileMapLayer(fileName);
                        Mediator.Instance.NotifyColleagues("AddLayerToTreeViewMessage", new LayerViewModel(overlayLayer.Name, overlayLayer));
                        Overlays.Add(overlayLayer.Name, overlayLayer);
                        break;
                    case ".eeb":
                        var bathymetryLayer = new BathymetryBoundsMapLayer(fileName);
                        Mediator.Instance.NotifyColleagues("AddLayerToTreeViewMessage", new LayerViewModel(bathymetryLayer.Name, bathymetryLayer));
                        Overlays.Add(bathymetryLayer.Name, bathymetryLayer);
                        Mediator.Instance.NotifyColleagues("EnvironmentFileLoaded");
                        break;
                    case ".nemo":
                        var layerViewModel = new LayerViewModel(null, null);
                        var scenarioLayer = new ScenarioFileMapLayer(fileName, Overlays, layerViewModel);
                        Mediator.Instance.NotifyColleagues("AddLayerToTreeViewMessage", layerViewModel);
                        Overlays.Add(scenarioLayer.Name, scenarioLayer);
                        break;
                }
                _map.Refresh();
            }
            catch (Exception ex)
            {
                Globals.DisplayException(_messageBoxService, ex, "Error opening file {0}", fileName);
            }
        }

        [MediatorMessageSink("AddScenarioToExperimentMessage")]
        void AddScenarioToExperiment(NemoFile nemoFile)
        {
            var scenarioLayer = new ScenarioFileLayerViewModel(nemoFile, Overlays);
            Mediator.Instance.NotifyColleagues("AddLayerToTreeViewMessage", scenarioLayer);
            _map.Refresh();
        }
    }
}