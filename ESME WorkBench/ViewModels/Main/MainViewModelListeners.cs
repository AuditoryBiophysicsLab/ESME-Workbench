using System.IO;
using System.Windows;
using Cinch;
using ESME;
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
        void SetMouseEarthCoordinate(Geo mouseEarthCoordinate) { MouseGeo = mouseEarthCoordinate; }

        [MediatorMessageSink(MediatorMessage.SetupAndRunQuickLookPoint)]
        void SetupAndRunQuickLookPoint(bool dummy)
        {

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
            ((Window)_viewAwareStatus.View).IsEnabled = enable;
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
#if false

        bool IsExperimentNUWCSimulateable
        {
            get
            {
                //is the nemo file there?

                //are the species files in the nemo file there?
                foreach (var speciesgroup in _experiment.NemoFile.Scenario.Animals)
                {
                    foreach (var nemoAnimal in speciesgroup.Species)
                    {
                        if (!(File.Exists(Path.Combine(Path.Combine(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, _experiment.NemoFile.Scenario.SimAreaName), "Species"), nemoAnimal.SpeciesFile))))
                        {
                            _messageBoxService.ShowError(string.Format("Could not find species file {0} : is the Scenario Data Directory properly set in NUWC scenario builder and simulator?",nemoAnimal.SpeciesFile));
                        }

                    }
                }
            }
        }
#endif
    }
}