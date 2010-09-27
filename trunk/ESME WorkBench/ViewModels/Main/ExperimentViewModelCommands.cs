﻿using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using Cinch;
using ESMEWorkBench.Data;
using ESMEWorkBench.Properties;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class ExperimentViewModel
    {
        #region Commands

        #region EditOptionsCommand

        public SimpleCommand<object, object> EditOptionsCommand
        {
            get
            {
                return _editOptions ?? (_editOptions = new SimpleCommand<object, object>(delegate
                                                                                         {
                                                                                             var programOptionsViewModel = new ProgramOptionsViewModel();
                                                                                             var result = _visualizerService.ShowDialog("ApplicationOptionsView", programOptionsViewModel);
                                                                                             if ((result.HasValue) && (result.Value)) Globals.AppSettings.Save();
                                                                                             else Globals.AppSettings.Reload();
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
                return _launchScenarioEditor ?? (_launchScenarioEditor = new SimpleCommand<object, object>(delegate { return (Globals.AppSettings.ScenarioEditorExecutablePath != null) && (File.Exists(Globals.AppSettings.ScenarioEditorExecutablePath)); }, delegate
                                                                                                                                                                                                                                                               {
                                                                                                                                                                                                                                                                   new Process
                                                                                                                                                                                                                                                                   {
                                                                                                                                                                                                                                                                       StartInfo =
                                                                                                                                                                                                                                                                           {
                                                                                                                                                                                                                                                                               FileName = Globals.AppSettings.ScenarioEditorExecutablePath,
                                                                                                                                                                                                                                                                               WorkingDirectory = Path.GetDirectoryName(Globals.AppSettings.ScenarioEditorExecutablePath),
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
                return _launchEnvironmentBuilder ?? (_launchEnvironmentBuilder = new SimpleCommand<object, object>(delegate
                                                                                                                   {
                                                                                                                       var environmentBuilder = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "EnvironmentBuilder.exe");
                                                                                                                       return (File.Exists(environmentBuilder) && ((Globals.AppSettings.EnvironmentDatabaseDirectory != null) && (Directory.Exists(Globals.AppSettings.EnvironmentDatabaseDirectory))));
                                                                                                                   }, delegate
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
            get { return _testTransmissionLossView ?? (_testTransmissionLossView = new SimpleCommand<object, object>(delegate { Mediator.Instance.NotifyColleagues("TestTransmissionLossViewCommandMessage"); })); }
        }

        SimpleCommand<object, object> _testTransmissionLossView;

        #endregion

        #region DisabledCommand

        public SimpleCommand<object, object> DisabledCommand
        {
            get { return _disabled ?? (_disabled = new SimpleCommand<object, object>(delegate { return false; }, delegate { })); }
        }

        SimpleCommand<object, object> _disabled;

        #endregion

        #region CancelCurrentCommand

        public SimpleCommand<Object, Object> CancelCurrentCommand
        {
            get { return _cancelCurrentCommand ?? (_cancelCurrentCommand = new SimpleCommand<object, object>(delegate { Mediator.Instance.NotifyColleagues("CancelCurrentCommandMessage"); })); }
        }

        SimpleCommand<Object, Object> _cancelCurrentCommand;

        #endregion

        #region ViewClosingCommand

        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand
        {
            get
            {
                return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(delegate(EventToCommandArgs vcArgs)
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
            get { return _openExperiment ?? (_openExperiment = new SimpleCommand<object, object>(delegate { SaveExperiment(); })); }
        }

        SimpleCommand<object, object> _openExperiment;

        #endregion

        #region CloseExperimentCommand

        public SimpleCommand<object, object> CloseExperimentCommand
        {
            get { return _closeExperiment ?? (_closeExperiment = new SimpleCommand<object, object>(delegate { if (UserCanceledBecauseExperimentUnsaved()) return; })); }
        }

        SimpleCommand<object, object> _closeExperiment;

        #endregion

        #region SaveExperimentCommand

        public SimpleCommand<object, object> SaveExperimentCommand
        {
            get { return _saveExperiment ?? (_saveExperiment = new SimpleCommand<object, object>(delegate { SaveExperiment(); })); }
        }

        SimpleCommand<object, object> _saveExperiment;

        #endregion

        #region SaveExperimentAsCommand

        public SimpleCommand<object, object> SaveExperimentAsCommand
        {
            get { return _saveExperimentAs ?? (_saveExperimentAs = new SimpleCommand<object, object>(delegate { Mediator.Instance.NotifyColleagues("SaveExperimentAsCommandMessage"); })); }
        }

        SimpleCommand<object, object> _saveExperimentAs;

        #endregion

        #region NewExperimentCommand

        public SimpleCommand<object, object> NewExperimentCommand
        {
            get
            {
                return _newExperiment ?? (_newExperiment = new SimpleCommand<object, object>(delegate
                                                                                             {
                                                                                                 if (UserCanceledBecauseExperimentUnsaved()) return;
                                                                                                 _experiment = new Experiment();
                                                                                                 DecoratedExperimentName = "<New experiment>";
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
                return _addShapefile ?? (_addShapefile = new SimpleCommand<object, object>(delegate
                                                                                           {
                                                                                               _openFileService.Filter = "ESRI Shapefiles (*.shp)|*.shp";
                                                                                               _openFileService.InitialDirectory = Settings.Default.LastShapefileDirectory;
                                                                                               var result = _openFileService.ShowDialog(null);
                                                                                               if (!result.HasValue || !result.Value) return;
                                                                                               Settings.Default.LastShapefileDirectory = Path.GetDirectoryName(_openFileService.FileName);
                                                                                               Mediator.Instance.NotifyColleagues("AddFileLayerToMapViewMessage", _openFileService.FileName);
                                                                                               Mediator.Instance.NotifyColleagues("RefreshMapViewMessage", true);
                                                                                           }));
            }
        }

        SimpleCommand<object, object> _addShapefile;

        #endregion

        #region AddScenarioFileCommand

        public SimpleCommand<object, object> AddScenarioFileCommand
        {
            get
            {
                return _addScenarioFile ?? (_addScenarioFile = new SimpleCommand<object, object>(delegate { return CanAddScenarioFile(); }, delegate
                                                                                                                                            {
                                                                                                                                                _openFileService.Filter = "NUWC Scenario Files (*.nemo)|*.nemo";
                                                                                                                                                _openFileService.InitialDirectory = Settings.Default.LastScenarioFileDirectory;
                                                                                                                                                var result = _openFileService.ShowDialog(null);
                                                                                                                                                if (!result.HasValue || !result.Value) return;
                                                                                                                                                if (!UserWantsToReplaceScenarioFileIfPresent(_openFileService.FileName)) return;
                                                                                                                                                _experiment.ScenarioFileName = _openFileService.FileName;
                                                                                                                                                Settings.Default.LastScenarioFileDirectory = Path.GetDirectoryName(_openFileService.FileName);
                                                                                                                                                Mediator.Instance.NotifyColleagues("AddFileLayerToMapViewMessage", _openFileService.FileName);
                                                                                                                                                Mediator.Instance.NotifyColleagues("RefreshMapViewMessage", true);
                                                                                                                                            }));
            }
        }

        SimpleCommand<object, object> _addScenarioFile;

        #endregion

        #region AddOverlayFileCommand

        public SimpleCommand<Object, Object> AddOverlayFileCommand
        {
            get
            {
                return _addOverlayFileCommand ?? (_addOverlayFileCommand = new SimpleCommand<object, object>(delegate
                                                                                                             {
                                                                                                                 _openFileService.Filter = "NUWC Overlay Files (*.ovr)|*.ovr";
                                                                                                                 _openFileService.InitialDirectory = Settings.Default.LastOverlayFileDirectory;
                                                                                                                 var result = _openFileService.ShowDialog(null);
                                                                                                                 if (!result.HasValue || !result.Value) return;
                                                                                                                 Settings.Default.LastOverlayFileDirectory = Path.GetDirectoryName(_openFileService.FileName);
                                                                                                                 Mediator.Instance.NotifyColleagues("AddFileLayerToMapViewMessage", _openFileService.FileName);
                                                                                                                 Mediator.Instance.NotifyColleagues("RefreshMapViewMessage", true);
                                                                                                             }));
            }
        }

        SimpleCommand<Object, Object> _addOverlayFileCommand;

        #endregion

        #region AddEnvironmentFileCommand

        public SimpleCommand<object, object> AddEnvironmentFileCommand
        {
            get
            {
                return _addEnvironmentFile ?? (_addEnvironmentFile = new SimpleCommand<object, object>(delegate
                                                                                                       {
                                                                                                           _openFileService.Filter = "ESME Environment File (*.eeb)|*.eeb";
                                                                                                           _openFileService.InitialDirectory = Settings.Default.LastEnvironmentFileDirectory;
                                                                                                           var result = _openFileService.ShowDialog(null);
                                                                                                           if (!result.HasValue || !result.Value) return;
                                                                                                           Settings.Default.LastEnvironmentFileDirectory = Path.GetDirectoryName(_openFileService.FileName);
                                                                                                           _environmentFileName = _openFileService.FileName;
                                                                                                           Mediator.Instance.NotifyColleagues("AddFileLayerToMapViewMessage", _openFileService.FileName);
                                                                                                           Mediator.Instance.NotifyColleagues("RefreshMapViewMessage", true);
                                                                                                       }));
            }
        }

        SimpleCommand<object, object> _addEnvironmentFile;

        #endregion

        #region ToggleBaseMapDisplayCommand

        public SimpleCommand<object, Boolean> ToggleBaseMapDisplayCommand
        {
            get { return _toggleBaseMapDisplay ?? (_toggleBaseMapDisplay = new SimpleCommand<object, Boolean>(isChecked => Mediator.Instance.NotifyColleagues("SetBaseMapDisplayMessage", isChecked))); }
        }

        SimpleCommand<object, Boolean> _toggleBaseMapDisplay;

        #endregion

        #region ToggleGridOverlayDisplayCommand

        public SimpleCommand<object, Boolean> ToggleGridOverlayDisplayCommand
        {
            get { return _toggleGridOverlayDisplay ?? (_toggleGridOverlayDisplay = new SimpleCommand<object, Boolean>(isChecked => Mediator.Instance.NotifyColleagues("SetGridOverlayDisplayMessage", isChecked))); }
        }

        SimpleCommand<object, Boolean> _toggleGridOverlayDisplay;

        #endregion

        #region TogglePanZoomDisplayCommand

        public SimpleCommand<object, Boolean> TogglePanZoomDisplayCommand
        {
            get { return _togglePanZoomDisplay ?? (_togglePanZoomDisplay = new SimpleCommand<object, Boolean>(isChecked => Mediator.Instance.NotifyColleagues("SetPanZoomDisplayMessage", isChecked))); }
        }

        SimpleCommand<object, Boolean> _togglePanZoomDisplay;

        #endregion

        #region QuickLookCommand

        public SimpleCommand<object, object> QuickLookCommand
        {
            get { return _quickLook ?? (_quickLook = new SimpleCommand<object, object>(delegate { Mediator.Instance.NotifyColleagues("QuickLookCommandMessage"); })); }
        }

        SimpleCommand<object, object> _quickLook;

        #endregion

        #endregion
    }
}