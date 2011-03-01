using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using Cinch;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.CASS;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Map;
using ThinkGeo.MapSuite.Core;
using ESME.Views.AcousticBuilder;
using BehaviorModel = ESME.Platform.BehaviorModel;

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
            Globals.AppSettings.AddExperiment(FileName);
        }

        [MediatorMessageSink(MediatorMessage.EditAnalysisPoint)]
        void EditAnalysisPoint(AnalysisPoint analysisPoint)
        {
            var analysisPointSettingsViewModel = new AnalysisPointSettingsViewModel(analysisPoint);
            var settingsResult = VisualizerService.ShowDialog("AnalysisPointSettingsView", analysisPointSettingsViewModel);
            if (settingsResult.HasValue && settingsResult.Value)
            {
                DisplayAnalysisPoint(analysisPoint);
                if (analysisPointSettingsViewModel.AnalysisPointIsChanged) IsChanged = true;
            }
        }

        [MediatorMessageSink(MediatorMessage.RemoveAnalysisPoint)]
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
                var layersToAdd = new List<OverlayShapeMapLayer>();

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
                    if(MapLayers.IndexOf(simArea) == -1) layersToAdd.Add(simArea);
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
                    if (MapLayers.IndexOf(track) == -1) layersToAdd.Add(track);
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
                        if (MapLayers.IndexOf(opArea) == -1) layersToAdd.Add(opArea);
                    }
                }

                //wait for all the tests to finish before adding everything at once.
                foreach (var layer in layersToAdd) MapLayers.Add(layer);
                SetScenarioMapExtent(true);
            }
            catch (Exception e)
            {
                Globals.DisplayException(MessageBoxService, e, "Error opening scenario file");
                ScenarioFileName = null;
            }
        }


        [MediatorMessageSink(MediatorMessage.SetScenarioMapExtent)]
        void SetScenarioMapExtent(bool dummy)
        {
            var boundingBox = new Rect();
            if (NemoFile.Scenario.OverlayFile != null) boundingBox = NemoFile.Scenario.OverlayFile.Shapes[0].BoundingBox;
            else
            {
                foreach (var platform in NemoFile.Scenario.Platforms)
                    foreach (var trackdef in platform.Trackdefs)
                    {
                        if ((boundingBox.Width == 0) && (boundingBox.Height == 0)) boundingBox = trackdef.OverlayFile.Shapes[0].BoundingBox;
                        else boundingBox.Union(trackdef.OverlayFile.Shapes[0].BoundingBox);
                    }
            }
            var north = (float) boundingBox.Bottom + 3;
            var west = (float) boundingBox.Left - 3;
            var south = (float) boundingBox.Top - 3;
            var east = (float) boundingBox.Right + 3;

            var mapExtent = new RectangleShape(west, north, east, south);
            MediatorMessage.Send(MediatorMessage.SetCurrentExtent, mapExtent);
        }

        [MediatorMessageSink(MediatorMessage.RequestTransmissionLossBathymetry)]
        void RequestTransmissionLossBathymetry(bool dummy)
        {
            MediatorMessage.Send(MediatorMessage.SetTransmissionLossBathymetry,Bathymetry);
        }

        [MediatorMessageSink(MediatorMessage.ExportAnalysisPointsToCASS)]
        void ExportAnalysisPointsToCASS(bool dummy)
        {
            if ((AnalysisPoints == null) || (AnalysisPoints.Count == 0)) return;
            var timePeriods = new List<string>
                              {
                                  "Spring",
                                  "Summer",
                                  "Fall",
                                  "Winter",
                              };
            CASSFiles.WriteCASSInputFiles(Globals.AppSettings.ScenarioDataDirectory, timePeriods, AnalysisPoints, NemoFile.Scenario, "bathy.txt");
        }
    }
}