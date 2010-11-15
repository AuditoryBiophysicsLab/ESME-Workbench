using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Input;
using System.Windows.Threading;
using Cinch;
using ESME.Model;
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
            var environmentInformation = new EnvironmentInformation
                                         {
                                             Bathymetry = _experiment.Bathymetry,
                                             SoundSpeedField = _experiment.SoundSpeedField,
                                             Sediment = SedimentTypes.SedimentArray[0],
                                         };

            var transmissionLossSettings = new TransmissionLossSettings
                                           {
                                               DepthCellSize = 50,
                                               RangeCellSize = 50,
                                           };

            var analysisPointViewModel = new AnalysisPointCalculationPreviewViewModel
            {
                AnalysisPoint = new AnalysisPoint
                {
                    EarthCoordinate = MouseEarthCoordinate,
                    RadialBearing = 0,
                    RadialCount = 16,
                }
            };

            var distinctModes = (from platform in _experiment.NemoFile.Scenario.Platforms
                                 from source in platform.Sources
                                 from mode in source.Modes
                                 select mode).Distinct();
            foreach (var mode in distinctModes)
            {
                var transmissionLossJobViewModel = new TransmissionLossJobViewModel(MouseEarthCoordinate, mode, 16, 3000)
                                                   {
                                                       Name = string.Format("{0}", mode.PSMName),
                                                       IDField = _experiment.NextObjectID,
                                                   };
                try
                {
                    var bellhopRunFile = BellhopRunFile.Create(transmissionLossJobViewModel.TransmissionLossJob, environmentInformation, transmissionLossSettings);
                    analysisPointViewModel.TransmissionLossJobViewModels.Add(transmissionLossJobViewModel);
                    analysisPointViewModel.BellhopRunFiles.Add(bellhopRunFile);
                }
                catch (BathymetryOutOfBoundsException)
                {
                    _dispatcher.InvokeIfRequired(() => _messageBoxService.ShowError("Unable to add analysis point.\nDid you click outside the bounds of the simulation area?"));
                    _dispatcher.InvokeIfRequired(() => MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow));
                    return;
                }
                catch (BathymetryTooShallowException)
                {
                    _dispatcher.InvokeIfRequired(() => _messageBoxService.ShowError("This area is too shallow to place an analysis point.  Pick a different area."));
                    _dispatcher.InvokeIfRequired(() => MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow));
                    return;
                }
            }
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
                                            foreach (var bellhopRunFile in analysisPointViewModel.BellhopRunFiles) 
                                                _dispatcher.BeginInvoke(new MediatorSendDelegate(MediatorMessage.Send), DispatcherPriority.Background, MediatorMessage.QueueBellhopJob, bellhopRunFile);
                                        };
            backgroundWorker.RunWorkerAsync();

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

        [MediatorMessageSink(MediatorMessage.CalculateAnalysisPoint)]
        void CalculateAnalysisPoint(AnalysisPoint analysisPoint)
        {
        }
    }
}