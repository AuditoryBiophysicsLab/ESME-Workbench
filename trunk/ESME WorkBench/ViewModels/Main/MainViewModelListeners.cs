using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Input;
using Cinch;
using ESME.TransmissionLoss;
using ESMEWorkBench.Data;
using ESMEWorkBench.Properties;
using ESMEWorkBench.ViewModels.TransmissionLoss;
using HRC.Navigation;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        [MediatorMessageSink(MediatorMessage.SetMouseEarthCoordinate)]
        void SetMouseEarthCoordinate(EarthCoordinate mouseEarthCoordinate) { MouseEarthCoordinate = mouseEarthCoordinate; }

        [MediatorMessageSink(MediatorMessage.SetupAndRunQuickLookPoint)]
        void SetupAndRunQuickLookPoint(bool dummy)
        {
            
        }

        [MediatorMessageSink(MediatorMessage.SetupAndRunAnalysisPoint)]
        void SetupAndRunAnalysisPoint(bool dummy)
        {
#if false
            var environmentInformation = new EnvironmentInformation
                                         {
                                             Bathymetry = _experiment.Bathymetry,
                                             SoundSpeedField = _experiment.SoundSpeedField,
                                             Sediment = SedimentTypes.SedimentArray[0],
                                         };

            var transmissionLossSettings = new TransmissionLossSettings
                                           {
                                               DepthCellSize = (float)_experiment.BellhopDepthCellSize,
                                               RangeCellSize = (float)_experiment.BellhopRangeCellSize,
                                           };

#endif
            var analysisPoint = new AnalysisPoint(MouseEarthCoordinate);
#if false
            var analysisPointViewModel = new AnalysisPointCalculationPreviewViewModel
            {
                AnalysisPoint = analysisPoint,
            };
#endif

            var distinctModes = (from platform in _experiment.NemoFile.Scenario.Platforms
                                 from source in platform.Sources
                                 from mode in source.Modes
                                 select mode).Distinct();
            foreach (var mode in distinctModes)
            {
                analysisPoint.SoundSources.Add(new SoundSource(analysisPoint, mode, 16));
#if false
                var transmissionLossJobViewModel = new TransmissionLossJobViewModel(MouseEarthCoordinate, mode, 16, 3000)
                                                   {
                                                       Name = string.Format("{0}", mode.PSMName),
                                                       IDField = _experiment.NextObjectID,
                                                   };
                try
                {
                    analysisPointViewModel.AnalysisPoint.SoundSources.Add(transmissionLossJobViewModel.TransmissionLossJob.SoundSource);
                    //var cassRunFile = CassRunFile.Create(transmissionLossJobViewModel.TransmissionLossJob, environmentInformation, transmissionLossSettings, _experiment.NemoFile.Scenario.TimeFrame);
                    //cassRunFile.Save(Path.GetDirectoryName(_experiment.FileName));
                    var ramRunFile = TransmissionLossRunFile.Create(TransmissionLossAlgorithm.RAM, transmissionLossJobViewModel.TransmissionLossJob, environmentInformation, transmissionLossSettings);
                    ramRunFile.Save(_experiment.LocalStorageRoot);
                    var bellhopRunFile = TransmissionLossRunFile.Create(TransmissionLossAlgorithm.Bellhop, transmissionLossJobViewModel.TransmissionLossJob, environmentInformation, transmissionLossSettings);
                    bellhopRunFile.Save(_experiment.LocalStorageRoot);
                    analysisPointViewModel.TransmissionLossJobViewModels.Add(transmissionLossJobViewModel);
                    analysisPointViewModel.TransmissionLossRunFiles.Add(bellhopRunFile);
                }
                catch (BathymetryOutOfBoundsException ex)
                {
                    _dispatcher.InvokeIfRequired(() => _messageBoxService.ShowError("Unable to add analysis point.\nDid you click outside the bounds of the simulation area?\n\n" + ex.Message));
                    _dispatcher.InvokeIfRequired(() => MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow));
                    return;
                }
                catch (BathymetryTooShallowException ex)
                {
                    _dispatcher.InvokeIfRequired(() => _messageBoxService.ShowError("This area is too shallow to place an analysis point.  Pick a different area.\n\n" + ex.Message));
                    _dispatcher.InvokeIfRequired(() => MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow));
                    return;
                }
#endif
            }
            var analysisPointSettingsViewModel = new AnalysisPointSettingsViewModel(analysisPoint, _messageBoxService);
            var settingsResult = _visualizerService.ShowDialog("AnalysisPointSettingsView", analysisPointSettingsViewModel);
            if ((!settingsResult.HasValue) || (!settingsResult.Value))
            {
                MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);
                return;
            }
            MediatorMessage.Send(MediatorMessage.AddAnalysisPoint, analysisPoint);
#if false
            var result = _visualizerService.ShowDialog("AnalysisPointCalculationPreviewView", analysisPointViewModel);
            if ((!result.HasValue) || (!result.Value))
            {
                MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);
                return;
            }
            MediatorMessage.Send(MediatorMessage.AddAnalysisPoint, analysisPointViewModel.AnalysisPoint);
            if (_bellhopQueueCalculatorViewModel == null)
            {
                _bellhopQueueCalculatorViewModel = new BellhopQueueCalculatorViewModel(_experiment.LocalStorageRoot, _messageBoxService);
                _visualizerService.Show("BellhopQueueCalculatorView", _bellhopQueueCalculatorViewModel, false, null);
            }

            var backgroundWorker = new BackgroundWorker();
            backgroundWorker.DoWork += delegate
                                        {
                                            foreach (var bellhopRunFile in analysisPointViewModel.TransmissionLossRunFiles) 
                                                _dispatcher.BeginInvoke(new MediatorSendDelegate(MediatorMessage.Send), DispatcherPriority.Background, MediatorMessage.QueueTransmissionLossJob, bellhopRunFile);
                                        };
            backgroundWorker.RunWorkerAsync();
#endif

            MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);
        }

        delegate void MediatorSendDelegate(string message, object param);

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
            _openFileService.InitialDirectory = Settings.Default.LastAnimatPopulationDirectory;
            _openFileService.Filter = "Animat Scenario Files (*.sce)|*.sce";
            _openFileService.FileName = null;
            var result = _openFileService.ShowDialog((Window)_viewAwareStatus.View);
            if ((!result.HasValue) || (!result.Value)) return;
            Settings.Default.LastAnimatPopulationDirectory = Path.GetDirectoryName(_openFileService.FileName);
            _experiment.AnimalPopulationFiles.Add(_openFileService.FileName);
        }

        [MediatorMessageSink(MediatorMessage.RunExperimentCommand)]
        void RunExperiment(Experiment experiment)
        {
            var simulationViewModel = new SimulationViewModel(experiment);
            var result = _visualizerService.ShowDialog("SimulationView", simulationViewModel);
        }

        [MediatorMessageSink(MediatorMessage.AddAnalysisPoint)]
        void AddAnalysisPoint(AnalysisPoint analysisPoint)
        {
        }

        [MediatorMessageSink(MediatorMessage.ViewAnalysisPoint)]
        void ViewAnalysisPoint(AnalysisPoint analysisPoint)
        {
            var analysisPointVisualizerViewModel = new AnalysisPointVisualizerViewModel(analysisPoint, _saveFileService);
            _visualizerService.ShowDialog("AnalysisPointVisualizerView", analysisPointVisualizerViewModel);
        }

        [MediatorMessageSink(MediatorMessage.EnableGUI)]
        void EnableGUI(bool enable)
        {
            ((Window) _viewAwareStatus.View).IsEnabled = enable;
        }

        [MediatorMessageSink(MediatorMessage.CalculateAnalysisPoint)]
        void CalculateAnalysisPoint(AnalysisPoint analysisPoint)
        {
        }

        [MediatorMessageSink(MediatorMessage.AcousticOptions)]
        void AcousticOptions(bool dummy)
        {
            var acousticOptions = new AcousticEngineParameterViewModel(_experiment);
            _visualizerService.ShowDialog("AcousticEngineParameterConfigurationView", acousticOptions);
        }

        [MediatorMessageSink(MediatorMessage.DavesTestCommand)]
        void DavesTestCommandHandler(bool dummy)
        {
            var testViewModel = new ESME.Views.TestViewModel
                                {
                                    TestString = "Set from ESME"
                                };
            _visualizerService.ShowDialog("TestView", testViewModel);
        }
    }
}