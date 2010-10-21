using System.ComponentModel;
using System.IO;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Threading;
using Cinch;
using ESME.Model;
using ESME.Overlay;
using ESME.TransmissionLoss;
using ESMEWorkBench.Data;
using ESMEWorkBench.Properties;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Map;
using ESMEWorkBench.ViewModels.TransmissionLoss;
using HRC.Navigation;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        [MediatorMessageSink(MediatorMessage.SetMouseEarthCoordinate)]
        void SetMouseEarthCoordinate(EarthCoordinate mouseEarthCoordinate) { MouseEarthCoordinate = mouseEarthCoordinate; }

        [MediatorMessageSink(MediatorMessage.TestTransmissionLossViewCommand)]
        void TestTransmissionLossView(bool dummy)
        {
            _openFileService.Filter = "Transmission Loss files (*.tlf)|*.tlf|All files (*.*)|*.*";
            _openFileService.FileName = null;
            var result = _openFileService.ShowDialog((Window)_viewAwareStatus.View);
            if ((!result.HasValue) || (!result.Value)) return;
            var transmissionLossFieldViewModel = new TransmissionLossFieldViewModel(_openFileService.FileName, _saveFileService);
            _visualizerService.Show("TransmissionLossView", transmissionLossFieldViewModel, true, null);
        }

        [MediatorMessageSink(MediatorMessage.RunQuickLook)]
        void RunQuickLook(bool dummy)
        {
            #region create transmission loss job. the new base class for all acoustic simulations!

#if false
            var transmissionLossJob = new TransmissionLossJob
                                      {
                                          AcousticProperties = new AcousticProperties
                                                               {
                                                                   SourceDepth = 10,
                                                                   VerticalBeamWidth = 120,
                                                                   DepressionElevationAngle = 0f,
                                                                   LowFrequency = 3500,
                                                                   HighFrequency = 3500,
                                                               },
                                          AnalysisPoint = new AnalysisPoint
                                                          {
                                                              IDField = 1,
                                                              Location = MouseEarthCoordinate,
                                                              RadialBearing = 0,
                                                              RadialCount = 16,
                                                          },
                                          Radius = 30000,
                                          MaxDepth = 3000,
                                      };

#endif
            #endregion

            #region create bellhop run file from tlj (and stuff)

            var environmentInformation = new EnvironmentInformation
                                         {
                                             Environment2DData = _experiment.Bathymetry,
                                             SoundSpeedField = _experiment.SoundSpeedField,
                                             Sediment = SedimentTypes.SedimentArray[0],
                                         };

            var transmissionLossSettings = new TransmissionLossSettings
                                           {
                                               DepthCellSize = 50,
                                               RangeCellSize = 50,
                                           };

#if true
            var analysisPointViewModel = new AnalysisPointViewModel
            {
                AnalysisPoint = new AnalysisPoint
                {
                    Location = MouseEarthCoordinate,
                    RadialBearing = 0,
                    RadialCount = 16,
                }
            };
            foreach (var platform in _experiment.NemoFile.Scenario.Platforms)
                foreach (var source in platform.Sources)
                    foreach (var mode in source.Modes)
                    {
                        var transmissionLossJobViewModel = new TransmissionLossJobViewModel(MouseEarthCoordinate, -platform.Trackdefs[0].InitialHeight, mode, 16, 3000)
                                                           {
                                                               Name = string.Format("{0}.{1}.{2}", platform.Name, source.Name, mode.Name),
                                                           };
                        analysisPointViewModel.TransmissionLossJobViewModels.Add(transmissionLossJobViewModel);
                    }
            var result = _visualizerService.ShowDialog("AnalysisPointView", analysisPointViewModel);
            if ((!result.HasValue) || (!result.Value))
            {
                MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);
                return;
            }
            MediatorMessage.Send(MediatorMessage.AddAnalysisPoint, analysisPointViewModel.AnalysisPoint);
            try
            {
                if (_bellhopQueueCalculatorViewModel == null)
                {
                    _bellhopQueueCalculatorViewModel = new BellhopQueueCalculatorViewModel(_experiment.LocalStorageRoot);
                    _visualizerService.Show("BellhopQueueCalculatorView", _bellhopQueueCalculatorViewModel, false, null);
                }
                var bw1 = new BackgroundWorker();
                bw1.DoWork += delegate
                              {
                                  foreach (var transmissionLossJobViewModel in analysisPointViewModel.TransmissionLossJobViewModels)
                                  {
                                      var bw2 = new BackgroundWorker();
                                      var model = transmissionLossJobViewModel;
                                      bw2.DoWork += delegate { _dispatcher.BeginInvoke(new MediatorSendDelegate(MediatorMessage.Send), DispatcherPriority.Background, MediatorMessage.QueueBellhopJob, BellhopRunFile.Create(model.TransmissionLossJob, environmentInformation, transmissionLossSettings)); };
                                      bw2.RunWorkerAsync();
                                  }
                              };
                bw1.RunWorkerAsync();
            }
            catch (BathymetryOutOfBoundsException)
            {
                _messageBoxService.ShowError("Unable to add analysis point.\nDid you click outside the bounds of the simulation area?");
                MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);
                return;
            }
            catch (BathymetryTooShallowException)
            {
                _messageBoxService.ShowError("This area is too shallow to place an analysis point.  Pick a different area.");
                MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);
                return;
            }
#else
            var transmissionLossJobViewModel = new TransmissionLossJobViewModel(MouseEarthCoordinate, 0, mode, 16, 3000);
            var result = _visualizerService.ShowDialog("TransmissionLossJobView", transmissionLossJobViewModel);
            if ((!result.HasValue) || (!result.Value))
            {
                MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);
                return;
            }
            transmissionLossJobViewModel.TransmissionLossJob.AnalysisPoint.TransmissionLossJobs.Add(transmissionLossJobViewModel.TransmissionLossJob);
            MediatorMessage.Send(MediatorMessage.AddAnalysisPoint, transmissionLossJobViewModel.TransmissionLossJob.AnalysisPoint);

            MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Wait);
            TransmissionLossField transmissionLossField;
            try
            {
                var bellhopRunFile = BellhopRunFile.Create(transmissionLossJobViewModel.TransmissionLossJob, environmentInformation, transmissionLossSettings);
                //transmissionLossField = FieldCalculator.ComputeField(bellhopRunFile, null);
                var bellhopCalculatorViewModel = new BellhopCalculatorViewModel
                                                 {
                                                     BellhopRunFile = bellhopRunFile
                                                 };
                _visualizerService.ShowDialog("BellhopCalculatorView", bellhopCalculatorViewModel);
                transmissionLossField = bellhopCalculatorViewModel.TransmissionLossField;
            }
            catch (BathymetryOutOfBoundsException)
            {
                _messageBoxService.ShowError("Unable to run quick look.\nDid you click outside the bounds of the simulation area?");
                MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);
                return;
            }
            catch (AggregateException ex)
            {
                var sb = new StringBuilder();
                foreach (var e in ex.InnerExceptions) sb.Append(e.Message + "\n");
                _messageBoxService.ShowError("One or more errors occurred calculating transmission loss:\n" + sb);
                MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);
                return;
            }
            catch (BathymetryTooShallowException)
            {
                _messageBoxService.ShowError("This area is too shallow to run a quick look.  Pick a different area.");
                MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);
                return;
            }
#endif

#if false
            _saveFileService.OverwritePrompt = true;
            _saveFileService.Filter = "Transmission Loss files (*.tlf)|*.tlf|All files (*.*)|*.*";
            _saveFileService.FileName = null;
            var saveResult = _saveFileService.ShowDialog((Window) _viewAwareStatusService.View);
            if (saveResult.HasValue && saveResult.Value)
            {
                transmissionLossField.Filename = _saveFileService.FileName;
                transmissionLossField.Save();
            }
            var transmissionLossFieldViewModel = new TransmissionLossFieldViewModel(transmissionLossField, _saveFileService);
            _visualizerService.Show("TransmissionLossView", transmissionLossFieldViewModel, true, null);
#endif

            MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);

            #endregion
        }

        delegate void MediatorSendDelegate(string message, object param);

        [MediatorMessageSink(MediatorMessage.ExperimentClosed)]
        void ExperimentClosed(bool dummy)
        {
            _experiment = new Experiment
                          {
                              MessageBoxService = _messageBoxService
                          };
            DecoratedExperimentName = "<New experiment>";
            _experiment.InitializeIfViewModelsReady();
            HookPropertyChanged(_experiment);
        }

        [MediatorMessageSink(MediatorMessage.LaunchMMMBSCommand)]
        void LaunchMMMBS(bool dummy)
        {

        }

        [MediatorMessageSink(MediatorMessage.CreateMMMBSBathymetryFileCommand)]
        void CreateMMMBSBathymetryFile(bool dummy)
        {
            if ((_experiment == null) || (_experiment.Bathymetry == null)) return;
            _saveFileService.Filter = "MMMBS bathymetry files (*.bth)|*.bth|All files (*.*)|*.*";
            _saveFileService.OverwritePrompt = true;
            _saveFileService.InitialDirectory = Settings.Default.LastBathymetryFileDirectory;
            _saveFileService.FileName = null;
            var result = _saveFileService.ShowDialog((Window)_viewAwareStatus.View);
            if ((!result.HasValue) || (!result.Value)) return;
            Settings.Default.LastBathymetryFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);
            _experiment.Bathymetry.SaveToYXZ(_saveFileService.FileName, 1); 
        }

        [MediatorMessageSink(MediatorMessage.AddAnimatPopulationFileCommand)]
        void AddAnimatPopulationFile(bool dummy)
        {
            _openFileService.Filter = "Animat Scenario Files (*.sce)|*.sce";
            _openFileService.FileName = null;
            var result = _openFileService.ShowDialog((Window)_viewAwareStatus.View);
            if ((!result.HasValue) || (!result.Value)) return;
            var animatInterface =  AnimatInterface.Create(_openFileService.FileName);
           

            //begin hackery; create an overlay object from the animat interface.
            
            animatInterface.Test(); // right now, dumps a log file of all positions to a test file.

#if true
            var layer = new OverlayShapeMapLayer
                            {
                                Name = "animats",
                                CanBeRemoved = false,
                                CanBeReordered = true,
                                LayerType = LayerType.Animal,
                            };

            foreach (var animat in animatInterface.AnimatList) layer.Add(new OverlayPoint(animat.Location, Colors.Black, 2));

            layer.Done();
            _experiment.MapLayers.Add(layer);
            MediatorMessage.Send(MediatorMessage.RefreshMap, true); 
#else
            //create one layer for each species.
            var layers = new OverlayShapeMapLayer[animatInterface.AnimatList.SpeciesList.Count];
            //for each species...
            for (int i = 0; i < animatInterface.AnimatList.SpeciesList.Count; i++)
            {
                var speciesID = animatInterface.AnimatList.SpeciesList[i].SpeciesName;
                //name and set properties on the layer
                layers[i].Name = speciesID;
                layers[i].CanBeRemoved = false;
                layers[i].CanBeReordered = true;
                layers[i].LayerType = LayerType.Animal;
    
                //find all the animats who have the same speciesName
                var animatsInSpecies = animatInterface.AnimatList.Find(a => a.SpeciesName == speciesID);
                //and add each one to the layer.
                foreach (var animat in animatsInSpecies)
                {
                    layers[i].Add(new OverlayPoint(animat.Location,Colors.Black,2));    
                }

                //finish the layer, add it to the map, force a refresh.
                layers[i].Done();
                _experiment.MapLayers.Add(layers[i]);
                MediatorMessage.Send(MediatorMessage.RefreshMap,true);

            }
#endif
        }
    }
}