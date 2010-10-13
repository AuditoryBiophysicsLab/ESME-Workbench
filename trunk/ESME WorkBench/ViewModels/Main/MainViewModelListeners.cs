using System;
using System.Text;
using System.Windows;
using System.Windows.Input;
using Cinch;
using ESME.Model;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using ESMEWorkBench.Data;
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
            var result = _openFileService.ShowDialog((Window)_viewAwareStatusService.View);
            if ((!result.HasValue) || (!result.Value)) return;
            var transmissionLossFieldViewModel = new TransmissionLossFieldViewModel(_openFileService.FileName, _saveFileService);
            _visualizerService.Show("TransmissionLossView", transmissionLossFieldViewModel, true, null);
        }

        [MediatorMessageSink(MediatorMessage.RunQuickLook)]
        void RunQuickLook(bool dummy)
        {
            #region create transmission loss job. the new base class for all acoustic simulations!

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

            #endregion

            #region create bellhop run file from tlj (and stuff)

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

            var transmissionLossJobViewModel = new TransmissionLossJobViewModel
                                               {
                                                   TransmissionLossJob = transmissionLossJob
                                               };
            var result = _visualizerService.ShowDialog("TransmissionLossJobView", transmissionLossJobViewModel);
            if ((!result.HasValue) || (!result.Value))
            {
                MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);
                return;
            }

            transmissionLossJob.AnalysisPoint.TransmissionLossJobs.Add(transmissionLossJob);
            MediatorMessage.Send(MediatorMessage.AddAnalysisPoint, transmissionLossJob.AnalysisPoint);

            MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Wait);

            TransmissionLossField transmissionLossField;
            try
            {
                var bellhopRunFile = BellhopRunFile.Create(transmissionLossJob, environmentInformation, transmissionLossSettings);
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
#endif
            var transmissionLossFieldViewModel = new TransmissionLossFieldViewModel(transmissionLossField, _saveFileService);
            _visualizerService.Show("TransmissionLossView", transmissionLossFieldViewModel, true, null);

            MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);

            #endregion
        }

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
    }
}