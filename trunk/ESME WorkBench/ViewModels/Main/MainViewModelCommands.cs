﻿using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using Cinch;
using ESME.Data;
using ESMEWorkBench.Properties;
using ESMEWorkBench.ViewModels.Map;
using ESMEWorkBench.ViewModels.NAVO;

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

        #region LaunchScenarioSimulatorCommand

        public SimpleCommand<object, object> LaunchScenarioSimulatorCommand
        {
            get
            {
                return _launchScenarioSimulator ?? (_launchScenarioSimulator = new SimpleCommand<object, object>(
                    delegate { return ((Globals.AppSettings.ScenarioSimulatorSettings.ExecutablePath != null) && File.Exists(Globals.AppSettings.ScenarioSimulatorSettings.ExecutablePath) && _experiment.NemoFile != null && _experiment.Bathymetry != null); },
                    delegate
                    {
                        var vm = new ScenarioSimulatorOptionsViewModel
                                 {
                                     ScenarioSimulatorSettings = _experiment.ScenarioSimulatorSettings ?? Globals.AppSettings.ScenarioSimulatorSettings,
                                     NemoFile = _experiment.NemoFile,
                                 };

                        var result = _visualizerService.ShowDialog("ScenarioSimulatorOptionsView", vm);
                        if (result.HasValue && result.Value)
                        {
                            _experiment.ScenarioSimulatorSettings = vm.ScenarioSimulatorSettings;
                        }
                    }));
            }
        }

        SimpleCommand<object, object> _launchScenarioSimulator;

        #endregion

        #region ViewScenarioSimulatorLogDirCommand

        public SimpleCommand<object, object> ViewScenarioSimulatorLogDirCommand
        {
            get
            {
                return _viewScenarioSimulatorLogDir ?? (_viewScenarioSimulatorLogDir = new SimpleCommand<object, object>(
                    delegate { return ((_experiment.NemoFile != null) && Directory.Exists(Path.Combine(Path.GetDirectoryName(_experiment.NemoFile.FileName), "Reports"))); },
                    delegate
                    {
                        Process.Start("explorer.exe", Path.Combine(Path.GetDirectoryName(_experiment.NemoFile.FileName), "Reports"));

                    }));
            }
        }

        SimpleCommand<object, object> _viewScenarioSimulatorLogDir;

        #endregion

        #region LaunchNUWCReportGeneratorCommand

        public SimpleCommand<object, object> LaunchNUWCReportGeneratorCommand
        {
            get
            {
                return _launchNUWCReportGenerator ?? (_launchNUWCReportGenerator = new SimpleCommand<object, object>(
                    delegate { return (Globals.AppSettings.ReportGeneratorExecutablePath != null && File.Exists(Globals.AppSettings.ReportGeneratorExecutablePath)); },
                    delegate
                    {
                        new Process
                        {
                            StartInfo =
                            {
                                FileName = Globals.AppSettings.ReportGeneratorExecutablePath,
                            },
                        }.Start();
                    }));
            }
        }

        SimpleCommand<object, object> _launchNUWCReportGenerator;

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
                                                                                                         var ea = (CancelEventArgs)vcArgs.EventArgs;
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

        #region ResetScenarioZoomLevelCommand

        public SimpleCommand<object, object> ResetScenarioZoomLevelCommand
        {
            get
            {
                return _resetScenarioZoomLevel ?? (_resetScenarioZoomLevel = new SimpleCommand<object, object>(
                    obj => _experiment.NemoFile != null,
                    obj =>
                        MediatorMessage.Send(MediatorMessage.SetScenarioMapExtent, true)));
            }
        }

        SimpleCommand<object, object> _resetScenarioZoomLevel;

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

        public SimpleCommand<object, object> SaveExperimentCommand
        {
            get { return _saveExperiment ?? (_saveExperiment = new SimpleCommand<object, object>(arg => _experiment.CanSave, obj => SaveExperiment())); }
        }

        SimpleCommand<object, object> _saveExperiment;

        #endregion

        #region SaveExperimentAsCommand

        public SimpleCommand<object, object> SaveExperimentAsCommand
        {
            get { return _saveExperimentAs ?? (_saveExperimentAs = new SimpleCommand<object, object>(arg => _experiment.CanSaveAs, obj => SaveExperimentAs())); }
        }

        SimpleCommand<object, object> _saveExperimentAs;

        #endregion

        #region NewExperimentCommand

        public SimpleCommand<object, object> NewExperimentCommand
        {
            get { return _newExperiment ?? (_newExperiment = new SimpleCommand<object, object>(obj => NewExperiment())); }
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

        #region AnalysisPointCommand

        public SimpleCommand<object, object> AnalysisPointCommand
        {
            get { return _analysisPoint ?? (_analysisPoint = new SimpleCommand<object, object>(o => CanPlaceAnalysisPoint(), obj => MediatorMessage.Send(MediatorMessage.AnalysisPointCommand))); }
        }

        bool CanPlaceAnalysisPoint()
        {
            return (_experiment != null) && (_experiment.NemoFile != null) && (_experiment.Bathymetry != null) && (_experiment.SoundSpeedField != null) && (_experiment.FileName != null);
        }
        SimpleCommand<object, object> _analysisPoint;

        #endregion

        #region QuickLookCommand

        public SimpleCommand<object, object> QuickLookCommand
        {
            get { return _quickLookPoint ?? (_quickLookPoint = new SimpleCommand<object, object>(o => CanRunQuickLook(), obj => MediatorMessage.Send(MediatorMessage.QuickLookPointCommand))); }
        }

        bool CanRunQuickLook()
        {
            return (_experiment != null) && (_experiment.Bathymetry != null) && (_experiment.SoundSpeedField != null) && (_experiment.FileName != null);
        }
        SimpleCommand<object, object> _quickLookPoint;

        #endregion

        #region AcousticOptionsCommand

        public SimpleCommand<object, object> AcousticOptionsCommand
        {
            get { return _acousticOptions ?? (_acousticOptions = new SimpleCommand<object, object>((obj) => MediatorMessage.Send(MediatorMessage.AcousticOptions, true))); }
        }

        SimpleCommand<object, object> _acousticOptions;

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
                return _launchMMMBS ?? (_launchMMMBS = new SimpleCommand<object, object>(obj =>
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
                                                                                         }));
            }
        }

        SimpleCommand<object, object> _launchMMMBS;

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
            get { return _createMMMBSBathymetryFileCommand ?? (_createMMMBSBathymetryFileCommand = new SimpleCommand<object, object>(obj => ((_experiment != null) && (_experiment.Bathymetry != null)), obj => MediatorMessage.Send(MediatorMessage.CreateMMMBSBathymetryFileCommand))); }
        }

        SimpleCommand<object, object> _createMMMBSBathymetryFileCommand;

        #endregion

        #region RunExperimentCommand

        public SimpleCommand<object, object> RunExperimentCommand
        {
            get { return _runExperiment ?? (_runExperiment = new SimpleCommand<object, object>(arg => CanRunExperiment(), delegate { MediatorMessage.Send(MediatorMessage.RunExperimentCommand, _experiment); })); }
        }

        bool CanRunExperiment()
        {
            if ((_experiment == null) || (_experiment.AnalysisPoints == null) || (_experiment.AnimatInterface == null)) return false;
            return CanPlaceAnalysisPoint() && (_experiment.AnalysisPoints.Count > 0);
        }

        SimpleCommand<object, object> _runExperiment;

        #endregion

        #region AboutCommand

        public SimpleCommand<object, object> AboutCommand
        {
            get { return _about ?? (_about = new SimpleCommand<object, object>(arg => ShowAboutView())); }
        }

        SimpleCommand<object, object> _about;

        #endregion

        #region DavesTestCommand

        public SimpleCommand<object, object> DavesTestCommand
        {
            get { return _davesTest ?? (_davesTest = new SimpleCommand<object, object>((obj) => MediatorMessage.Send(MediatorMessage.DavesTestCommand, true))); }
        }

        SimpleCommand<object, object> _davesTest;

        #endregion

        #region NAVOEnvironmentBuilderCommand

        public SimpleCommand<object, object> NAVOEnvironmentBuilderCommand
        {
            get
            {
                return _nAVOEnvironmentBuilder ?? (_nAVOEnvironmentBuilder = new SimpleCommand<object, object>(delegate
                                                                                                               {
                                                                                                                   var environmentBuilderViewModel = new EnvironmentBuilderViewModel(_visualizerService, _messageBoxService, Globals.AppSettings, _experiment);
                                                                                                                   var result = _visualizerService.ShowDialog("EnvironmentBuilderView", environmentBuilderViewModel);
                                                                                                                   if (result.HasValue && result.Value)
                                                                                                                   {
                                                                                                                   }



                                                                                                               }));
            }
        }

        SimpleCommand<object, object> _nAVOEnvironmentBuilder;

        #endregion

        #region ExportAnalysisPointsToCASSCommand

        public SimpleCommand<object, object> ExportAnalysisPointsToCASSCommand
        {
            get { return _exportAnalysisPointsToCASS ?? (_exportAnalysisPointsToCASS = new SimpleCommand<object, object>(delegate { MediatorMessage.Send(MediatorMessage.ExportAnalysisPointsToCASS, true); })); }
        }

        SimpleCommand<object, object> _exportAnalysisPointsToCASS;

        #endregion

        #endregion
    }
}