using System;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System.Windows.Media;
using Cinch;
using ESME.NEMO;
using ESME.Platform;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Map;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.Data
{
    public partial class Experiment
    {
        static bool _mainViewModelInitialized;
        static bool _mapViewModelInitialized;
        static bool _layerListViewModelInitialized;

        [MediatorMessageSink(MediatorMessage.MainViewModelInitialized)]
        void MainViewModelInitialized(bool dummy)
        {
            _mainViewModelInitialized = true;
            InitializeIfViewModelsReady();
        }

        [MediatorMessageSink(MediatorMessage.MapViewModelInitialized)]
        void MapViewModelInitialized(bool dummy)
        {
            _mapViewModelInitialized = true;
            InitializeIfViewModelsReady();
        }

        [MediatorMessageSink(MediatorMessage.LayerListViewModelInitialized)]
        void LayerListViewModelInitialized(bool dummy)
        {
            _layerListViewModelInitialized = true;
            InitializeIfViewModelsReady();
        }

        [MediatorMessageSink(MediatorMessage.ExperimentLoaded)]
        void ExperimentLoaded(bool dummy) { InitializeIfViewModelsReady(); }

        [MediatorMessageSink(MediatorMessage.AddFileCommand)]
        void AddFileCommand(string fileName)
        {
            try
            {
                switch (Path.GetExtension(fileName).ToLower())
                {
                    case ".shp":
                        MediatorMessage.Send(MediatorMessage.AddMapLayer, new ShapefileMapLayer
                                                                          {
                                                                              AreaStyle = MapLayerViewModel.RandomAreaStyle,
                                                                              ShapefileName = fileName,
                                                                              CanBeRemoved = true,
                                                                              CanBeReordered = true,
                                                                              CanChangeAreaColor = true,
                                                                              CanChangeLineColor = true,
                                                                              LayerType = LayerType.Shapefile,
                                                                          });
                        break;
                    case ".ovr":
                        MediatorMessage.Send(MediatorMessage.AddMapLayer, new OverlayFileMapLayer
                                                                          {
                                                                              OverlayFileName = fileName,
                                                                              CanBeRemoved = true,
                                                                              CanBeReordered = true,
                                                                              CanChangeAreaColor = false,
                                                                              CanChangeLineColor = true,
                                                                              LayerType = LayerType.OverlayFile,
                                                                          });
                        break;
                }
            }
            catch (Exception ex)
            {
                Globals.DisplayException(MessageBoxService, ex, "Error opening file {0}", fileName);
            }
        }

        [MediatorMessageSink(MediatorMessage.CloseExperiment)]
        void CloseExperiment(bool dummy)
        {
            MapLayers.Clear();
            MediatorMessage.Send(MediatorMessage.ExperimentClosed, true);
        }

        OverlayShapeMapLayer FindOverlayShapeMapLayer(LayerType layerType, string layerName) { return (OverlayShapeMapLayer) MapLayers.Where(layer => layer.LayerType == layerType).Where(layer => layer.Name == layerName).FirstOrDefault(); }

        [MediatorMessageSink(MediatorMessage.AddScenarioFileCommand)]
        void AddScenarioFileCommand(string fileName)
        {
            try
            {
                ScenarioFileName = fileName;
                NemoFile = new NemoFile(fileName, Globals.AppSettings.ScenarioDataDirectory);
                var simAreaName = NemoFile.Scenario.SimAreaName + " sim area";
                var simArea = FindOverlayShapeMapLayer(LayerType.SimArea, simAreaName) ?? new OverlayShapeMapLayer
                                                                                          {
                                                                                              Name = NemoFile.Scenario.SimAreaName + " sim area",
                                                                                              CanBeRemoved = false,
                                                                                              CanBeReordered = true,
                                                                                              CanChangeAreaColor = false,
                                                                                              CanChangeLineColor = true,
                                                                                              LayerType = LayerType.SimArea,
                                                                                          };
                simArea.Add(NemoFile.Scenario.OverlayFile.Shapes);
                simArea.Done();
                MediatorMessage.Send(MediatorMessage.AddMapLayer, simArea);
                var platformCount = 0;
                foreach (var platform in NemoFile.Scenario.Platforms)
                {
                    platformCount++;
                    var platformLayerName = "Platform " + platformCount + ": " + platform.Name;
                    var platformTrackName = platformLayerName + " track";
                    var track = FindOverlayShapeMapLayer(LayerType.Track, platformTrackName) ?? new OverlayShapeMapLayer
                                                                                                {
                                                                                                    Name = platformTrackName,
                                                                                                    CanBeRemoved = false,
                                                                                                    CanBeReordered = true,
                                                                                                    CanChangeAreaColor = false,
                                                                                                    CanChangeLineColor = false,
                                                                                                    LayerType = LayerType.Track,
                                                                                                };
                    track.CustomLineStyle = new CustomStartEndLineStyle(PointSymbolType.Circle, Colors.Green, 5, PointSymbolType.Square, Colors.Red, 5, Colors.DarkGray, 1);
                    var behavior = new BehaviorModel(platform);
                    track.Add(behavior.CourseOverlay);
                    track.Done();
                    MediatorMessage.Send(MediatorMessage.AddMapLayer, track);
                    var opAreaCount = 0;
                    foreach (var trackdef in platform.Trackdefs)
                    {
                        opAreaCount++;
                        var opAreaName = platformLayerName + " op area" + (platform.Trackdefs.Count == 1 ? "" : " " + opAreaCount);
                        var opArea = FindOverlayShapeMapLayer(LayerType.OpArea, opAreaName) ?? new OverlayShapeMapLayer
                                                                                               {
                                                                                                   Name = opAreaName,
                                                                                                   CanBeRemoved = false,
                                                                                                   CanBeReordered = true,
                                                                                                   CanChangeAreaColor = false,
                                                                                                   CanChangeLineColor = true,
                                                                                                   LayerType = LayerType.OpArea,
                                                                                               };
                        opArea.Add(trackdef.OverlayFile.Shapes);
                        opArea.Done();
                        MediatorMessage.Send(MediatorMessage.AddMapLayer, opArea);
                    }
                }
                MediatorMessage.Send(MediatorMessage.RefreshMap, true);
            }
            catch (Exception e)
            {
                MessageBoxService.ShowError("Error opening scenario file: \n" + e.Message);
            }
        }

        [MediatorMessageSink(MediatorMessage.LayersReordered)]
        void LayersReordered(MapLayerViewModel layer)
        {
            MapLayers.Move(MapLayers.IndexOf(layer), layer.Index);
            IsChanged = true;
        }

        [MediatorMessageSink(MediatorMessage.LayerAdded)]
        void LayerAdded(MapLayerViewModel mapLayer)
        {
            //if (!_isInitialized) return;
            if (MapLayers == null) MapLayers = new ObservableCollection<MapLayerViewModel>();
            if (MapLayers.IndexOf(mapLayer) != -1) return;
            if ((mapLayer.LayerType == LayerType.BaseMap) && (MapLayers.Any(layer => layer.LayerType == LayerType.BaseMap))) return;
            if (MapLayers.Any(layer => layer.Name == mapLayer.Name)) return;
            MapLayers.Add(mapLayer);
        }

        [MediatorMessageSink(MediatorMessage.CurrentScaleChanged)]
        void CurrentScaleChanged(CurrentScaleChangedWpfMapEventArgs e)
        {
            if (!_isInitialized) return;
            if (e == null) return;
            CurrentScale = e.CurrentScale;
        }

        [MediatorMessageSink(MediatorMessage.CurrentExtentChanged)]
        void CurrentExtentChanged(CurrentExtentChangedWpfMapEventArgs e)
        {
            if (!_isInitialized) return;
            if (e == null) return;
            if (e.CurrentExtent == null) return;
            CurrentExtent = e.CurrentExtent.GetWellKnownText();
        }
    }
}