using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using System.Windows.Threading;
using Cinch;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Model;
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
        static Dispatcher _mainViewModelDispatcher;

        [MediatorMessageSink(MediatorMessage.MainViewModelInitialized)]
        void MainViewModelInitialized(Dispatcher dispatcher)
        {
            _mainViewModelInitialized = true;
            _mainViewModelDispatcher = dispatcher;
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
            }
            DisplayAnalysisPoint(analysisPoint);
            if (analysisPointSettingsViewModel.AnalysisPointIsChanged) IsChanged = true;
            analysisPoint.Validate();
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
                    simArea.Clear();
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
                    track.Clear();
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
                        opArea.Clear();
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

        public void ExportAnalysisPoints(bool dummy)
        {
            if ((AnalysisPoints == null) || (AnalysisPoints.Count == 0)) return;
            var soundspeedFiles = Directory.GetFiles(EnvironmentRoot, "*-soundspeed.xml");
            var timePeriods = soundspeedFiles.Select(curFile => Path.GetFileName(curFile).Split('-')[0]).ToList();
            var bottomTypeData = EnvironmentData<SedimentSample>.Load(SedimentFileName);
            foreach (var timePeriod in timePeriods)
            {
                var soundSpeedFile = Path.Combine(EnvironmentRoot, string.Format("{0}-soundspeed.xml", timePeriod));
                var windSpeedFile = Path.Combine(EnvironmentRoot, string.Format("{0}-wind.txt", timePeriod));
                var soundSpeedField = new SoundSpeedField(SerializedOutput.Load(soundSpeedFile, GeneralizedDigitalEnvironmentModelDatabase.ReferencedTypes), NemoFile.Scenario.TimeFrame);
                var windSpeedField = SurfaceMarineGriddedClimatologyDatabase.Parse(windSpeedFile);
                var environmentInfo = new EnvironmentInformation
                                      {
                                          LocationName = NemoFile.Scenario.SimAreaName,
                                          SoundSpeedFieldName = timePeriod,
                                          Bathymetry = Bathymetry,
                                          Sediment = bottomTypeData,
                                          WindSpeed = windSpeedField,
                                          SoundSpeedField = soundSpeedField,
                                      };
                foreach (var analysisPoint in AnalysisPoints)
                {
                    foreach (var soundSource in analysisPoint.SoundSources)
                    {
                        if (!soundSource.ShouldBeCalculated) continue;
                        var algorithm = NemoModeToAcousticModelNameMap[soundSource.Name];
                        switch (algorithm)
                        {
                            case TransmissionLossAlgorithm.Bellhop:
                            case TransmissionLossAlgorithm.RAMGEO:
                                var metadata = string.Format("Transmission loss calculation.\r\nScenario file: {0}\r\nAnalysis point coordinates: {1}\r\nMode name: {2}\r\nTime period: {3}\r\nAlgorithm: {4}", NemoFile.FileName, analysisPoint, soundSource.Name, timePeriod, TransmissionLossAlgorithm.Bellhop);
                                var job = new TransmissionLossJob
                                          {
                                              SoundSource = soundSource,
                                              Name = soundSource.SoundSourceID,
                                              Metadata = metadata,
                                              Filename = soundSource.SoundSourceID,
                                              AnalysisPointID = analysisPoint.AnalysisPointID,
                                              MaxDepth = (int) Globals.AppSettings.CASSSettings.MaximumDepth,
                                          };
                                // Create the job
                                var runfile = TransmissionLossRunFile.Create(algorithm, job, environmentInfo, Globals.AppSettings);

                                // delete any files that have the same base filename
                                var filesToDelete = Directory.GetFiles(TransmissionLossJobRoot, runfile.Name + ".*");
                                foreach (var file in filesToDelete)
                                    File.Delete(file);

                                // Save the job
                                runfile.Save(TransmissionLossJobRoot);
                                break;
                            default:
                                break;
                        }
                    }
                }
            }
#if false
            var bellhopFileManager = new TransmissionLossFileManager
                                     {
                                         TransmissionLossJobRoot = TransmissionLossJobRoot,
                                         TransmissionLossFileRoot = TransmissionLossFileRoot,
                                         TimePeriods = timePeriods,
                                         AppSettings = Globals.AppSettings,
                                         NemoFile = NemoFile,
                                         Bathymetry = Bathymetry,
                                     };
            bellhopFileManager.FindNewJobs(AnalysisPoints);
#endif
        }
    }
}