using System.IO;
using System.Windows;
using Cinch;
using ESME;
using ESME.TransmissionLoss;
using ESMEWorkbench.Data;
using ESMEWorkbench.Properties;
using ESMEWorkbench.ViewModels.TransmissionLoss;
using HRC.Navigation;

namespace ESMEWorkbench.ViewModels.Main
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
            _openFile.InitialDirectory = Settings.Default.LastAnimatPopulationDirectory;
            _openFile.Filter = "Animat Scenario Files (*.sce)|*.sce";
            _openFile.FileName = null;
            var result = _openFile.ShowDialog((Window)_viewAwareStatus.View);
            if ((!result.HasValue) || (!result.Value)) return;
            Settings.Default.LastAnimatPopulationDirectory = Path.GetDirectoryName(_openFile.FileName);
            _experiment.AnimalPopulationFiles.Add(_openFile.FileName);
        }

        [MediatorMessageSink(MediatorMessage.RunExperimentCommand)]
        void RunExperiment(Experiment experiment)
        {
            var simulationViewModel = new SimulationViewModel(experiment);
            var result = _visualizer.ShowDialog("SimulationView", simulationViewModel);
        }

        [MediatorMessageSink(MediatorMessage.AddAnalysisPoint)]
        void AddAnalysisPoint(AnalysisPoint analysisPoint)
        {
        }

        [MediatorMessageSink(MediatorMessage.ViewAnalysisPoint)]
        void ViewAnalysisPoint(AnalysisPoint analysisPoint)
        {
            var analysisPointVisualizerViewModel = new AnalysisPointVisualizerViewModel(analysisPoint, _saveFile);
            _visualizer.ShowDialog("AnalysisPointVisualizerView", analysisPointVisualizerViewModel);
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
            _visualizer.ShowDialog("AcousticEngineParameterConfigurationView", acousticOptions);
        }

        [MediatorMessageSink(MediatorMessage.DavesTestCommand)]
        void DavesTestCommandHandler(bool dummy)
        {
            var testViewModel = new ESME.Views.TestViewModel
                                {
                                    TestString = "Set from ESME"
                                };
            _visualizer.ShowDialog("TestView", testViewModel);
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