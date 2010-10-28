using System;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System.Windows.Media;
using Cinch;
using ESME.Platform;
using ESME.TransmissionLoss;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Map;
using ThinkGeo.MapSuite.Core;

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

        [MediatorMessageSink(MediatorMessage.AddFileCommand)]
        void AddFileCommand(string fileName)
        {
            try
            {
                switch (Path.GetExtension(fileName).ToLower())
                {
                    case ".shp":
                        MapLayers.Add(new ShapefileMapLayer
                                      {
                                          ShapefileName = fileName,
                                          CanBeRemoved = true,
                                          CanBeReordered = true,
                                          CanChangeAreaColor = true,
                                          CanChangeLineColor = true,
                                          LayerType = LayerType.Shapefile,
                                      });
                        break;
                    case ".ovr":
                        MapLayers.Add(new OverlayFileMapLayer
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

        [MediatorMessageSink(MediatorMessage.AddAnalysisPoint)]
        void AddAnalysisPoint(AnalysisPoint analysisPoint)
        {
            if (AnalysisPoints == null) AnalysisPoints = new ObservableCollection<AnalysisPoint>();
            AnalysisPoints.Add(analysisPoint);
        }

        [MediatorMessageSink(MediatorMessage.EditAnalysisPoint)]
        void EditAnalysisPoint(AnalysisPoint analysisPoint)
        {
        }

        [MediatorMessageSink(MediatorMessage.DeleteAnalysisPoint)]
        void RemoveAnalysisPoint(AnalysisPoint analysisPoint)
        {
            AnalysisPoints.Remove(analysisPoint);
        }

        OverlayShapeMapLayer FindOverlayShapeMapLayer(LayerType layerType, string layerName) { return (OverlayShapeMapLayer)MapLayers.Where(layer => layer.LayerType == layerType).Where(layer => layer.Name == layerName).FirstOrDefault(); }

        [MediatorMessageSink(MediatorMessage.AddScenarioFileCommand)]
        void AddScenarioFileCommand(string fileName)
        {
            if ((fileName == null) || (!File.Exists(fileName))) return;
            try
            {
                ScenarioFileName = fileName;
                if (NemoFile.Scenario.OverlayFile != null)
                {
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
                    if (MapLayers.IndexOf(simArea) == -1) MapLayers.Add(simArea);
                }
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
                    //behavior.CourseChangePoints
                    track.Add(behavior.CourseOverlay);
                    track.Done();
                    if (MapLayers.IndexOf(track) == -1) MapLayers.Add(track);
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
                        if (MapLayers.IndexOf(opArea) == -1) MapLayers.Add(opArea);
                    }
                }
            }
            catch (Exception e)
            {
                Globals.DisplayException(MessageBoxService, e, "Error opening scenario file");
            }
        }
    }
}