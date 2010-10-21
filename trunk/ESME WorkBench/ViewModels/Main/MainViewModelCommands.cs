using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using Cinch;
using ESMEWorkBench.Data;
using ESMEWorkBench.Properties;
using ESMEWorkBench.ViewModels.Map;

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
                                                                                             var extraTypes = new[]
                                                                                                              {
                                                                                                                  typeof (MapLayerViewModel), typeof (ShapefileMapLayer), typeof (OverlayShapeMapLayer), typeof (OverlayFileMapLayer)
                                                                                                              };
                                                                                             var programOptionsViewModel = new ProgramOptionsViewModel();
                                                                                             var result = _visualizerService.ShowDialog("ApplicationOptionsView", programOptionsViewModel);
                                                                                             if ((result.HasValue) && (result.Value)) Globals.AppSettings.Save(extraTypes);
                                                                                             else Globals.AppSettings.Reload(extraTypes);
                                                                                         }));
            }
        }

        SimpleCommand<object, object> _editOptions;

        #endregion

        #region LaunchScenarioEditorCommand

        public SimpleCommand<object, object> LaunchScenarioEditorCommand
        {
            get
            {
                return _launchScenarioEditor ?? (_launchScenarioEditor = new SimpleCommand<object, object>(arg => (Globals.AppSettings.ScenarioEditorExecutablePath != null) && (File.Exists(Globals.AppSettings.ScenarioEditorExecutablePath)), obj =>
                                                                                                                                                                                                                                                 {
                                                                                                                                                                                                                                                     string arguments;
                                                                                                                                                                                                                                                     if ((_experiment == null) || (_experiment.ScenarioFileName == null) || (!File.Exists(_experiment.ScenarioFileName))) arguments = null;
                                                                                                                                                                                                                                                     else arguments = "\"" + _experiment.ScenarioFileName + "\"";
                                                                                                                                                                                                                                                     new Process
                                                                                                                                                                                                                                                     {
                                                                                                                                                                                                                                                         StartInfo =
                                                                                                                                                                                                                                                             {
                                                                                                                                                                                                                                                                 FileName = Globals.AppSettings.ScenarioEditorExecutablePath,
                                                                                                                                                                                                                                                                 WorkingDirectory = Path.GetDirectoryName(Globals.AppSettings.ScenarioEditorExecutablePath),
                                                                                                                                                                                                                                                                 Arguments = arguments,
                                                                                                                                                                                                                                                             }
                                                                                                                                                                                                                                                     }.Start();
                                                                                                                                                                                                                                                 }));
            }
        }

        SimpleCommand<object, object> _launchScenarioEditor;

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

        #region TestTransmissionLossViewCommand

        public SimpleCommand<object, object> TestTransmissionLossViewCommand
        {
            get { return _testTransmissionLossView ?? (_testTransmissionLossView = new SimpleCommand<object, object>(obj => MediatorMessage.Send(MediatorMessage.TestTransmissionLossViewCommand, true))); }
        }

        SimpleCommand<object, object> _testTransmissionLossView;

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

        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand
        {
            get
            {
                return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(vcArgs =>
                                                                                                     {
                                                                                                         var ea = (CancelEventArgs) vcArgs.EventArgs;
                                                                                                         if ((_experiment == null) || (!_experiment.IsChanged)) return;
                                                                                                         if (UserCanceledBecauseExperimentUnsaved())
                                                                                                         {
                                                                                                             ea.Cancel = true;
                                                                                                             return;
                                                                                                         }
                                                                                                     }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;

        #endregion

        #region OpenExperimentCommand

        public SimpleCommand<object, object> OpenExperimentCommand
        {
            get { return _openExperiment ?? (_openExperiment = new SimpleCommand<object, object>(obj => OpenExperiment(null))); }
        }

        SimpleCommand<object, object> _openExperiment;

        #endregion

        #region RefreshMapCommand

        public SimpleCommand<object, object> RefreshMapCommand
        {
            get { return _refreshMap ?? (_refreshMap = new SimpleCommand<object, object>(obj => MediatorMessage.Send(MediatorMessage.RefreshMap, true))); }
        }

        SimpleCommand<object, object> _refreshMap;

        #endregion

        #region ShowEnvironmentSettingsCommand

        public SimpleCommand<object, object> ShowEnvironmentSettingsCommand
        {
            get
            {
                return _showEnvironmentSettings ?? (_showEnvironmentSettings = new SimpleCommand<object, object>(arg => ((Globals.AppSettings.EnvironmentDatabaseDirectory != null) && (Directory.Exists(Globals.AppSettings.EnvironmentDatabaseDirectory)) && (_experiment != null) && (_experiment.NemoFile != null)), obj =>
                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                             var environmentSettingsViewModel = new EnvironmentSettingsViewModel(Globals.AppSettings.EnvironmentDatabaseDirectory, _experiment.NemoFile.Scenario.TimeFrame.ToLower());
                                                                                                                                                                                                                                                                                                                             var result = _visualizerService.ShowDialog("EnvironmentSettingsView", environmentSettingsViewModel);
                                                                                                                                                                                                                                                                                                                             if (!result.HasValue || !result.Value) return;
                                                                                                                                                                                                                                                                                                                             _experiment.BathymetryFileName = environmentSettingsViewModel.BathymetryData.SelectedItem.Name;
                                                                                                                                                                                                                                                                                                                             _experiment.BottomTypeFileName = environmentSettingsViewModel.BottomTypeData.SelectedItem.Name;
                                                                                                                                                                                                                                                                                                                             _experiment.SoundSpeedFileName = environmentSettingsViewModel.SoundSpeedData.SelectedItem.Name;
                                                                                                                                                                                                                                                                                                                             _experiment.WindSpeedFileName = environmentSettingsViewModel.WindSpeedData.SelectedItem.Name;
                                                                                                                                                                                                                                                                                                                         }));
            }
        }

        SimpleCommand<object, object> _showEnvironmentSettings;

        #endregion

        #region SaveExperimentCommand

        public SimpleCommand<object, object> SaveExperimentCommand
        {
            get { return _saveExperiment ?? (_saveExperiment = new SimpleCommand<object, object>(obj => SaveExperiment())); }
        }

        SimpleCommand<object, object> _saveExperiment;

        #endregion

        #region SaveExperimentAsCommand

        public SimpleCommand<object, object> SaveExperimentAsCommand
        {
            get { return _saveExperimentAs ?? (_saveExperimentAs = new SimpleCommand<object, object>(obj => SaveExperimentAs())); }
        }

        SimpleCommand<object, object> _saveExperimentAs;

        #endregion

        #region NewExperimentCommand

        public SimpleCommand<object, object> NewExperimentCommand
        {
            get
            {
                return _newExperiment ?? (_newExperiment = new SimpleCommand<object, object>(obj =>
                                                                                             {
                                                                                                 if (UserCanceledBecauseExperimentUnsaved()) return;
                                                                                                 if (_experiment != null) _experiment.Close();
                                                                                                 _experiment = new Experiment();
                                                                                                 _experiment.InitializeIfViewModelsReady();
                                                                                                 DecoratedExperimentName = "<New experiment>";
                                                                                                 HookPropertyChanged(_experiment);
                                                                                             }));
            }
        }

        SimpleCommand<object, object> _newExperiment;

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

        #region ToggleGridOverlayDisplayCommand

        public SimpleCommand<object, Boolean> ToggleGridOverlayDisplayCommand
        {
            get { return _toggleGridOverlayDisplay ?? (_toggleGridOverlayDisplay = new SimpleCommand<object, Boolean>(isChecked => MediatorMessage.Send(MediatorMessage.ToggleGridOverlayDisplayCommand, isChecked))); }
        }

        SimpleCommand<object, Boolean> _toggleGridOverlayDisplay;

        #endregion

        #region TogglePanZoomDisplayCommand

        public SimpleCommand<object, Boolean> TogglePanZoomDisplayCommand
        {
            get { return _togglePanZoomDisplay ?? (_togglePanZoomDisplay = new SimpleCommand<object, Boolean>(isChecked => MediatorMessage.Send(MediatorMessage.TogglePanZoomDisplayCommand, isChecked))); }
        }

        SimpleCommand<object, Boolean> _togglePanZoomDisplay;

        #endregion

        #region ToggleScaleBarDisplayCommand

        public SimpleCommand<object, Boolean> ToggleScaleBarDisplayCommand
        {
            get { return _toggleScaleBarDisplay ?? (_toggleScaleBarDisplay = new SimpleCommand<object, Boolean>(isChecked => MediatorMessage.Send(MediatorMessage.ToggleScaleBarDisplayCommand, isChecked))); }
        }

        SimpleCommand<object, Boolean> _toggleScaleBarDisplay;

        #endregion

        #region QuickLookCommand

        public SimpleCommand<object, object> QuickLookCommand
        {
            get { return _quickLook ?? (_quickLook = new SimpleCommand<object, object>(o => (((_experiment != null) && (_experiment.NemoFile != null)) && (_experiment.Bathymetry != null)) && (_experiment.SoundSpeedField != null) && (_experiment.FileName != null), obj => MediatorMessage.Send(MediatorMessage.QuickLookCommand))); }
        }

        SimpleCommand<object, object> _quickLook;

        #endregion

        #region SelectRecentFileCommand

        public SimpleCommand<object, string> SelectRecentFileCommand
        {
            get { return _selectRecentFileCommand ?? (_selectRecentFileCommand = new SimpleCommand<object, string>(file => _messageBoxService.ShowInformation(file))); }
        }

        SimpleCommand<object, string> _selectRecentFileCommand;

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
                return _launchMMMBS ?? (_launchMMMBS = new SimpleCommand<object, object>(arg => (Globals.AppSettings.MMMBSExecutablePath != null) && (File.Exists(Globals.AppSettings.MMMBSExecutablePath)), obj => new Process
                                                                                                                                                                                                                    {
                                                                                                                                                                                                                        StartInfo =
                                                                                                                                                                                                                            {
                                                                                                                                                                                                                                FileName = Globals.AppSettings.MMMBSExecutablePath,
                                                                                                                                                                                                                                WorkingDirectory = Path.GetDirectoryName(Globals.AppSettings.MMMBSExecutablePath),
                                                                                                                                                                                                                            }
                                                                                                                                                                                                                    }.Start()));
            }
        }

        SimpleCommand<object, object> _launchMMMBS;

        #endregion

        #region CreateMMMBSBathymetryFileCommand

        public SimpleCommand<object, object> CreateMMMBSBathymetryFileCommand
        {
            get { return _createMMMBSBathymetryFileCommand ?? (_createMMMBSBathymetryFileCommand = new SimpleCommand<object, object>(obj => ((_experiment != null) && (_experiment.Bathymetry != null)), obj => MediatorMessage.Send(MediatorMessage.CreateMMMBSBathymetryFileCommand))); }
        }

        SimpleCommand<object, object> _createMMMBSBathymetryFileCommand;

        #endregion

        #endregion
    }
}