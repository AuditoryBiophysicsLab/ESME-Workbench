using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Threading.Tasks;
using Cinch;
using ESME;
using ESME.Data;
using ESME.Environment.Descriptors;
using ESME.Mapping;
using ESME.Views.LogFileViewer;
using ESMEWorkBench.Properties;
using ESMEWorkBench.ViewModels.NAVO;
using ESMEWorkBench.ViewModels.TransmissionLoss;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        #region Commands

        #region EditOptionsCommand
        public SimpleCommand<object, object> EditOptionsCommand
        {
            get
            {
                return _editOptions ?? (_editOptions = new SimpleCommand<object, object>(obj =>
                {
                    var extraTypes = new List<Type>
                    {
                            typeof (MapLayerViewModel),
                            typeof (ShapefileMapLayer),
                            typeof (OverlayShapeMapLayer),
                            typeof (OverlayFileMapLayer)
                    };
                    var programOptionsViewModel = new ApplicationOptionsViewModel(_messageBoxService);
                    var result = _visualizerService.ShowDialog("ApplicationOptionsView", programOptionsViewModel);
                    if ((result.HasValue) && (result.Value)) Globals.AppSettings.Save(extraTypes);
                    Globals.AppSettings = AppSettings.Load(extraTypes);
                    ESME.Globals.AppSettings = Globals.AppSettings;
                    if (Globals.AppSettings != null && Globals.AppSettings.ScenarioDataDirectory != null &&
                        File.Exists(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"))) 
                        RangeComplexes.ReadRangeComplexFileAsync(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"));
                }));
            }
        }

        SimpleCommand<object, object> _editOptions;
        #endregion

        #region LaunchScenarioEditorCommand
#if EXPERIMENTS_SUPPORTED
        public SimpleCommand<object, object> LaunchScenarioEditorCommand
        {
            get
            {
                return _launchScenarioEditor ??
                       (_launchScenarioEditor =
                        new SimpleCommand<object, object>(
                                arg => (Globals.AppSettings.NAEMOTools.ScenarioEditorExecutablePath != null) && (File.Exists(Globals.AppSettings.NAEMOTools.ScenarioEditorExecutablePath)), obj =>
                                {
                                    string arguments;

                                    if ((_experiment == null) || (_experiment.ScenarioFileName == null) || (!File.Exists(_experiment.ScenarioFileName))) arguments = null;
                                    else arguments = "\"" + _experiment.ScenarioFileName + "\"";
                                    new Process
                                    {
                                            StartInfo =
                                                    {
                                                            FileName = Globals.AppSettings.NAEMOTools.ScenarioEditorExecutablePath,
                                                            WorkingDirectory = Path.GetDirectoryName(Globals.AppSettings.NAEMOTools.ScenarioEditorExecutablePath),
                                                            Arguments = arguments,
                                                    }
                                    }.Start();
                                }));
            }
        }

        SimpleCommand<object, object> _launchScenarioEditor;
#endif
        #endregion

        #region LaunchEnvironmentBuilderCommand
        public SimpleCommand<object, object> LaunchEnvironmentBuilderCommand
        {
            get
            {
                return _launchEnvironmentBuilder ?? (_launchEnvironmentBuilder = new SimpleCommand<object, object>(arg =>
                {
                    var environmentBuilder = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "EnvironmentBuilder.exe");
                    return (File.Exists(environmentBuilder) && ((Globals.AppSettings.EnvironmentDatabaseDirectory != null) && (Directory.Exists(Globals.AppSettings.EnvironmentDatabaseDirectory))));
                }, obj =>
                {
                    var environmentBuilder = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "EnvironmentBuilder.exe");
                    new Process
                    {
                            StartInfo =
                                    {
                                            FileName = environmentBuilder,
                                            Arguments = string.Format("\"{0}\"", Globals.AppSettings.EnvironmentDatabaseDirectory),
                                    }
                    }.Start();
                }));
            }
        }

        SimpleCommand<object, object> _launchEnvironmentBuilder;
        #endregion

        #region LaunchScenarioSimulatorCommand
#if EXPERIMENTS_SUPPORTED
        public SimpleCommand<object, object> LaunchScenarioSimulatorCommand
        {
            get
            {
                return _launchScenarioSimulator ?? (_launchScenarioSimulator = new SimpleCommand<object, object>(
                                                                                       delegate
                                                                                       {
                                                                                           return ((Globals.AppSettings.NAEMOTools.ScenarioExecutablePath != null)
                                                                                                   && File.Exists(Globals.AppSettings.NAEMOTools.ScenarioExecutablePath)
                                                                                                   && _experiment != null && _experiment.NemoFile != null && _experiment.Bathymetry != null);
                                                                                       }, // todo && _experiment.AnalysisPoints != null && species are defined. 
                                                                                       delegate
                                                                                       {
                                                                                           var vm = new ScenarioSimulatorOptionsViewModel
                                                                                           {
                                                                                                   ScenarioSimulatorSettings =
                                                                                                           _experiment.ScenarioSimulatorSettings ?? Globals.AppSettings.ScenarioSimulatorSettings,
                                                                                                   NemoFile = _experiment.NemoFile,
                                                                                           };

                                                                                           var result = _visualizerService.ShowDialog("ScenarioSimulatorOptionsView", vm);
                                                                                           if (result.HasValue && result.Value) _experiment.ScenarioSimulatorSettings = vm.ScenarioSimulatorSettings;
                                                                                       }));
            }
        }

        SimpleCommand<object, object> _launchScenarioSimulator;
#endif
        #endregion

        #region ViewSimulatorLogsCommand
        public SimpleCommand<object, object> ViewSimulatorLogsCommand
        {
            get
            {
                return _viewSimulatorLogs ??
                       (_viewSimulatorLogs =
                        new SimpleCommand<object, object>(delegate { ViewSimulatorLogsHandler(); }));
            }
        }

        SimpleCommand<object, object> _viewSimulatorLogs;

        void ViewSimulatorLogsHandler()
        {
            var scenarioSimulatorLogViewModel = new LogFileViewModel(Settings.Default.LogLastDir, "*.log.*", _dispatcher);
            _visualizerService.Show("LogFileView", scenarioSimulatorLogViewModel, true, null);
        }
        #endregion

        #region HelpCommand
        public SimpleCommand<object, object> HelpCommand
        {
            get { return _help ?? (_help = new SimpleCommand<object, object>(delegate { HelpHandler(); })); }
        }

        SimpleCommand<object, object> _help;

        void HelpHandler()
        {
            var userManual = Directory.GetFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "ESME Workbench*Manual*.pdf");
            if (userManual.Length == 0)
            {
                _messageBoxService.ShowError("The user manual was not found!");
                return;
            }
            var info = new ProcessStartInfo(userManual[0]) {UseShellExecute = true, Verb = "open"};

            Process.Start(info);
        }
        #endregion

        #region ViewScenarioSimulatorLogDirCommand
#if EXPERIMENTS_SUPPORTED
        public SimpleCommand<object, object> ViewScenarioSimulatorLogDirCommand
        {
            get
            {
                return _viewScenarioSimulatorLogDir ?? (_viewScenarioSimulatorLogDir = new SimpleCommand<object, object>(
                                                                                               delegate
                                                                                               {
                                                                                                   return ((_experiment != null && _experiment.NemoFile != null) &&
                                                                                                           Directory.Exists(Path.Combine(Path.GetDirectoryName(_experiment.NemoFile.FileName), "Reports")));
                                                                                               },
                                                                                               delegate
                                                                                               {
                                                                                                   Process.Start("explorer.exe",
                                                                                                                 Path.Combine(Path.GetDirectoryName(_experiment.NemoFile.FileName), "Reports"));
                                                                                               }));
            }
        }

        SimpleCommand<object, object> _viewScenarioSimulatorLogDir;
#endif
        #endregion

        #region LaunchScenarioPostProcessorCommand
        public SimpleCommand<object, object> LaunchScenarioPostProcessorCommand
        {
            get
            {
                return _launchNUWCReportGenerator ?? (_launchNUWCReportGenerator = new SimpleCommand<object, object>(
                                                                                           delegate
                                                                                           {
                                                                                               return (Globals.AppSettings.NAEMOTools.ReportGeneratorExecutablePath != null &&
                                                                                                       File.Exists(Globals.AppSettings.NAEMOTools.ReportGeneratorExecutablePath));
                                                                                           },
                                                                                           delegate
                                                                                           {
                                                                                               new Process
                                                                                               {
                                                                                                       StartInfo =
                                                                                                               {
                                                                                                                       FileName = Globals.AppSettings.NAEMOTools.ReportGeneratorExecutablePath,
                                                                                                               },
                                                                                               }.Start();
                                                                                           }));
            }
        }

        SimpleCommand<object, object> _launchNUWCReportGenerator;
        #endregion

        #region LaunchExposureReportGeneratorCommand
        public SimpleCommand<object, object> LaunchExposureReportGeneratorCommand
        {
            get
            {
                return _launchExposureReportGenerator ?? (_launchExposureReportGenerator = new SimpleCommand<object, object>(
                                                                                                   delegate
                                                                                                   {
                                                                                                       return (Globals.AppSettings.NAEMOTools.ExposureReportGeneratorExecutablePath != null &&
                                                                                                               File.Exists((Globals.AppSettings.NAEMOTools.ExposureReportGeneratorExecutablePath)));
                                                                                                   },
                                                                                                   delegate
                                                                                                   {
                                                                                                       new Process
                                                                                                       {
                                                                                                               StartInfo =
                                                                                                                       {
                                                                                                                               FileName =
                                                                                                                                       Globals.AppSettings.NAEMOTools.
                                                                                                                                               ExposureReportGeneratorExecutablePath,
                                                                                                                       },
                                                                                                       }.Start();
                                                                                                   }));
            }
        }

        SimpleCommand<object, object> _launchExposureReportGenerator;
        #endregion

        #region DisabledCommand
        public SimpleCommand<object, object> DisabledCommand
        {
            get { return _disabled ?? (_disabled = new SimpleCommand<object, object>(arg => false, obj => { })); }
        }

        SimpleCommand<object, object> _disabled;
        #endregion

        #region CancelCurrentCommand
        public SimpleCommand<Object, Object> CancelCurrentCommand
        {
            get { return _cancelCurrentCommand ?? (_cancelCurrentCommand = new SimpleCommand<object, object>(obj => MediatorMessage.Send(MediatorMessage.CancelCurrentCommand))); }
        }

        SimpleCommand<Object, Object> _cancelCurrentCommand;
        #endregion

        #region ViewClosingCommand
#if EXPERIMENTS_SUPPORTED

        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand
        {
            get
            {
                return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(vcArgs =>
                {
                    var ea = (CancelEventArgs)vcArgs.EventArgs;
                    ScenarioMetadata = null;
                    Globals.AppSettings.Save();
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;
#endif
        #endregion

        #region OpenExperimentCommand
#if EXPERIMENTS_SUPPORTED

        public SimpleCommand<object, object> OpenExperimentCommand
        {
            get { return _openExperiment ?? (_openExperiment = new SimpleCommand<object, object>(obj => OpenExperiment(null))); }
        }

        SimpleCommand<object, object> _openExperiment;
#endif
        #endregion

        #region RefreshMapCommand
        public SimpleCommand<object, object> RefreshMapCommand
        {
            get { return _refreshMap ?? (_refreshMap = new SimpleCommand<object, object>(obj => MediatorMessage.Send(MediatorMessage.RefreshMap, true))); }
        }

        SimpleCommand<object, object> _refreshMap;
        #endregion

        #region ResetScenarioZoomLevelCommand
#if EXPERIMENTS_SUPPORTED

        public SimpleCommand<object, object> ResetScenarioZoomLevelCommand
        {
            get
            {
                return _resetScenarioZoomLevel ?? (_resetScenarioZoomLevel = new SimpleCommand<object, object>(
                                                                                     obj => ((_experiment != null) && (_experiment.NemoFile != null)),
                                                                                     obj =>
                                                                                     MediatorMessage.Send(MediatorMessage.SetScenarioMapExtent, true)));
            }
        }

        SimpleCommand<object, object> _resetScenarioZoomLevel;
#endif
        #endregion

        #region ShowEnvironmentSettingsCommand
        public SimpleCommand<object, object> ShowEnvironmentSettingsCommand
        {
            get
            {
                return _showEnvironmentSettings ?? (_showEnvironmentSettings = new SimpleCommand<object, object>(
                                                                                       arg => CanShowEnvironmentSettings,
                                                                                       obj => ShowEnvironmentSettingsView()));
            }
        }

        SimpleCommand<object, object> _showEnvironmentSettings;
        #endregion

        #region SaveExperimentCommand
#if EXPERIMENTS_SUPPORTED

        public SimpleCommand<object, object> SaveExperimentCommand
        {
            get { return _saveExperiment ?? (_saveExperiment = new SimpleCommand<object, object>(arg => _experiment.CanSave, obj => SaveExperiment())); }
        }

        SimpleCommand<object, object> _saveExperiment;
#endif
        #endregion

        #region SaveExperimentAsCommand
#if EXPERIMENTS_SUPPORTED

        public SimpleCommand<object, object> SaveExperimentAsCommand
        {
            get { return _saveExperimentAs ?? (_saveExperimentAs = new SimpleCommand<object, object>(arg => _experiment.CanSaveAs, obj => SaveExperimentAs())); }
        }

        SimpleCommand<object, object> _saveExperimentAs;
#endif
        #endregion

        #region NewExperimentCommand
#if EXPERIMENTS_SUPPORTED

        public SimpleCommand<object, object> NewExperimentCommand
        {
            get { return _newExperiment ?? (_newExperiment = new SimpleCommand<object, object>(obj => NewExperiment())); }
        }

        SimpleCommand<object, object> _newExperiment;
#endif
        #endregion

        #region AddShapefileCommand
        public SimpleCommand<object, object> AddShapefileCommand
        {
            get
            {
                return _addShapefile ?? (_addShapefile = new SimpleCommand<object, object>(obj =>
                {
                    _openFileService.Filter = "ESRI Shapefiles (*.shp)|*.shp";
                    _openFileService.InitialDirectory = Settings.Default.LastShapefileDirectory;
                    _openFileService.FileName = null;
                    var result = _openFileService.ShowDialog(null);
                    if (!result.HasValue || !result.Value) return;
                    Settings.Default.LastShapefileDirectory = Path.GetDirectoryName(_openFileService.FileName);
                    MediatorMessage.Send(MediatorMessage.AddFileCommand, _openFileService.FileName);
                }));
            }
        }

        SimpleCommand<object, object> _addShapefile;
        #endregion

        #region AddScenarioFileCommand
        public SimpleCommand<object, object> AddScenarioFileCommand
        {
            get { return _addScenarioFile ?? (_addScenarioFile = new SimpleCommand<object, object>(arg => IsAddScenarioFilePossible(), obj => OpenScenarioFile(null))); }
        }

        SimpleCommand<object, object> _addScenarioFile;
        #endregion

        #region AddOverlayFileCommand
        public SimpleCommand<Object, Object> AddOverlayFileCommand
        {
            get
            {
                return _addOverlayFileCommand ?? (_addOverlayFileCommand = new SimpleCommand<object, object>(obj =>
                {
                    _openFileService.Filter = "NUWC Overlay Files (*.ovr)|*.ovr";
                    _openFileService.InitialDirectory = Settings.Default.LastOverlayFileDirectory;
                    _openFileService.FileName = "";
                    var result = _openFileService.ShowDialog(null);
                    if (!result.HasValue || !result.Value) return;
                    Settings.Default.LastOverlayFileDirectory = Path.GetDirectoryName(_openFileService.FileName);
                    MediatorMessage.Send(MediatorMessage.AddFileCommand, _openFileService.FileName);
                }));
            }
        }

        SimpleCommand<Object, Object> _addOverlayFileCommand;
        #endregion

        #region QuickLookCommand
#if EXPERIMENTS_SUPPORTED

        public SimpleCommand<object, object> QuickLookCommand
        {
            get { return _quickLookPoint ?? (_quickLookPoint = new SimpleCommand<object, object>(o => CanRunQuickLook(), obj => MediatorMessage.Send(MediatorMessage.QuickLookPointCommand))); }
        }

        bool CanRunQuickLook() { return (_experiment != null) && (_experiment.Bathymetry != null) && (_experiment.SoundSpeedField != null) && (_experiment.FileName != null); }
        SimpleCommand<object, object> _quickLookPoint;
#endif
        #endregion

        #region AddAnimalPopulationFileCommand
        public SimpleCommand<object, object> AddAnimalPopulationFileCommand
        {
            get { return _addAnimalPopulationFile ?? (_addAnimalPopulationFile = new SimpleCommand<object, object>(obj => MediatorMessage.Send(MediatorMessage.AddAnimatPopulationFileCommand))); }
        }

        SimpleCommand<object, object> _addAnimalPopulationFile;
        #endregion

        #region LaunchMMMBSCommand
        public SimpleCommand<object, object> LaunchMMMBSCommand
        {
            get
            {
                return _launchMMMBS ??
                       (_launchMMMBS =
                        new SimpleCommand<object, object>(delegate { return IsLaunchMMMBSCommandEnabled; },
                                                          delegate { LaunchMMMBSHandler(); }));
            }
        }

        SimpleCommand<object, object> _launchMMMBS;

        static bool IsLaunchMMMBSCommandEnabled
        {
            get { return File.Exists(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "3MB.exe")); }
        }

        static void LaunchMMMBSHandler()
        {
            var mbsPath = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "3MB.exe");

            new Process
            {
                    StartInfo =
                            {
                                    FileName = mbsPath,
                                    WorkingDirectory = Path.GetDirectoryName(mbsPath),
                            }
            }.Start();
        }
        #endregion

        #region LaunchMMMBSpeciesBuilderCommand
        public SimpleCommand<object, object> LaunchMMMBSpeciesBuilderCommand
        {
            get
            {
                return _launchMMMBSpeciesBuilder ?? (_launchMMMBSpeciesBuilder = new SimpleCommand<object, object>(obj =>
                {
                    var mbsPath = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "3mbSpeciesBuilder.exe");

                    new Process
                    {
                            StartInfo =
                                    {
                                            FileName = mbsPath,
                                            WorkingDirectory = Path.GetDirectoryName(mbsPath),
                                    }
                    }.Start();
                }));
            }
        }

        SimpleCommand<object, object> _launchMMMBSpeciesBuilder;
        #endregion

        #region CreateMMMBSBathymetryFileCommand

        public SimpleCommand<object, object> CreateMMMBSBathymetryFileCommand
        {
            get
            {
                return _createMMMBSBathymetryFileCommand ??
                       (_createMMMBSBathymetryFileCommand =
                        new SimpleCommand<object, object>(obj => false&&(IsScenarioLoaded),
                                                          obj => MediatorMessage.Send(MediatorMessage.CreateMMMBSBathymetryFileCommand)));
            }
        }

        SimpleCommand<object, object> _createMMMBSBathymetryFileCommand;

        #endregion

        #region RunExperimentCommand
#if EXPERIMENTS_SUPPORTED

        public SimpleCommand<object, object> RunExperimentCommand
        {
            get
            {
                return _runExperiment ??
                       (_runExperiment = new SimpleCommand<object, object>(arg => CanRunExperiment(), delegate { MediatorMessage.Send(MediatorMessage.RunExperimentCommand, _experiment); }));
            }
        }

        bool CanRunExperiment()
        {
            if ((_experiment == null) || (_experiment.AnalysisPoints == null) || (_experiment.AnimatInterface == null)) return false;
            return CanPlaceAnalysisPoint && (_experiment.AnalysisPoints.Count > 0);
        }

        SimpleCommand<object, object> _runExperiment;
#endif
        #endregion

        #region AnalysisPointCommand
        public SimpleCommand<object, object> AnalysisPointCommand
        {
            get { return _analysisPoint ?? (_analysisPoint = new SimpleCommand<object, object>(delegate { return CanPlaceAnalysisPoint; }, delegate { })); }
        }

        SimpleCommand<object, object> _analysisPoint;
        #endregion

        #region AboutCommand
        public SimpleCommand<object, object> AboutCommand
        {
            get { return _about ?? (_about = new SimpleCommand<object, object>(arg => ShowAboutView())); }
        }

        SimpleCommand<object, object> _about;
        #endregion

        #region NAVOEnvironmentBuilderCommand
#if EXPERIMENTS_SUPPORTED

        public SimpleCommand<object, object> NAVOEnvironmentBuilderCommand
        {
            get
            {
                return _navoEnvironmentBuilder ?? (_navoEnvironmentBuilder = new SimpleCommand<object, object>(delegate { return true; }, delegate
                {
                    var environmentBuilderViewModel = new EnvironmentBuilderViewModel(_messageBoxService, Globals.AppSettings, _experiment);
                    var result = _visualizerService.ShowDialog("EnvironmentBuilderView", environmentBuilderViewModel);
                    if (result.HasValue && result.Value) {}
                }));
            }
        }

        SimpleCommand<object, object> _navoEnvironmentBuilder;
#endif
        #endregion

        #region ConfigureAcousticModelsCommand
#if false

        public SimpleCommand<object, object> ConfigureAcousticModelsCommand
        {
            get
            {
                return _configureAcousticModelsCommand ?? (_configureAcousticModelsCommand = new SimpleCommand<object, object>(delegate { return ((_experiment != null) && (_experiment.NemoFile != null)); }, delegate
                                                                                                                                                                                  {
                                                                                                                                                                                      var modeAcousticModelSelectionViewModel = new ModeAcousticModelSelectionViewModel(_experiment.NemoModeToAcousticModelNameMap, Globals.ValidTransmissionLossAlgorithms);
                                                                                                                                                                                      var result = _visualizerService.ShowDialog("ModeAcousticModelSelectionView", modeAcousticModelSelectionViewModel);
                                                                                                                                                                                      if (result.HasValue && result.Value)
                                                                                                                                                                                      {
                                                                                                                                                                                          _experiment.IsChanged = true;
                                                                                                                                                                                      }
                                                                                                                                                                                  }));
            }
        }

        SimpleCommand<object, object> _configureAcousticModelsCommand;
#endif
        #endregion

        #region AcousticSimulatorOptionsCommand
        public SimpleCommand<object, object> AcousticSimulatorOptionsCommand
        {
            get
            {
                return _acousticSimulatorOptions ?? (_acousticSimulatorOptions = new SimpleCommand<object, object>(delegate
                {
                    var extraTypes = new List<Type>
                    {
                            typeof (MapLayerViewModel),
                            typeof (ShapefileMapLayer),
                            typeof (OverlayShapeMapLayer),
                            typeof (OverlayFileMapLayer)
                    };
                    var viewModel = new AcousticSimulatorOptionsViewModel();
                    var result = _visualizerService.ShowDialog("AcousticSimulatorOptionsView", viewModel);
                    if ((result.HasValue) && (result.Value)) Globals.AppSettings.Save(extraTypes);
                    else Globals.AppSettings = AppSettings.Load(extraTypes);
                }));
            }
        }

        SimpleCommand<object, object> _acousticSimulatorOptions;
        #endregion

        #region RunTransmissionLossCalculatorCommand
        public SimpleCommand<object, object> RunTransmissionLossCalculatorCommand
        {
            get
            {
                return _runTransmissionLossCalculator ??
                       (_runTransmissionLossCalculator =
                        new SimpleCommand<object, object>(delegate { RunTransmissionLossCalculatorHandler(); }));
            }
        }

        SimpleCommand<object, object> _runTransmissionLossCalculator;

        static void RunTransmissionLossCalculatorHandler()
        {
            Process.Start(Path.Combine(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location),
                                       "TransmissionLossCalculator.exe"));
        }
        #endregion

        #endregion
    }
}