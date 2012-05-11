﻿using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Windows;
using System.Windows.Input;
using ESME;
using ESME.Data;
using ESMEWorkbench.ViewModels.TransmissionLoss;
using HRC.ViewModels;
using HRC.WPF;

namespace ESMEWorkbench.ViewModels.Main
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
                    var programOptionsViewModel = new ApplicationOptionsViewModel(_messageBox);
                    var result = _visualizer.ShowDialog("ApplicationOptionsView", programOptionsViewModel);
                    if ((result.HasValue) && (result.Value)) ESME.Globals.AppSettings.Save();
                    ESME.Globals.AppSettings = AppSettings.Load();
                    ESME.Globals.AppSettings = ESME.Globals.AppSettings;
                    //if (ESME.Globals.AppSettings != null && ESME.Globals.AppSettings.ScenarioDataDirectory != null &&
                    //    File.Exists(Path.Combine(ESME.Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"))) 
                    //    InitializeEnvironmentManager();
                }));
            }
        }

        SimpleCommand<object, object> _editOptions;
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
                _messageBox.ShowError("The user manual was not found!");
                return;
            }
            var info = new ProcessStartInfo(userManual[0]) {UseShellExecute = true, Verb = "open"};

            Process.Start(info);
        }
        #endregion

        #region DisabledCommand
        public SimpleCommand<object, object> DisabledCommand
        {
            get { return _disabled ?? (_disabled = new SimpleCommand<object, object>(arg => false, obj => { })); }
        }

        SimpleCommand<object, object> _disabled;
        #endregion

        #region SaveCommand
        public SimpleCommand<object, object> SaveCommand { get { return _save ?? (_save = new SimpleCommand<object, object>(o => IsSaveCommandEnabled, SaveHandler)); } }
        SimpleCommand<object, object> _save;
        bool IsSaveCommandEnabled { get { return Scenario != null; } }
        void SaveHandler(object o) { Database.SaveChanges(); }
        #endregion

        #region ViewClosingCommand
        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand
        {
            get
            {
                return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(o =>
                {
                    if (Database.Context.IsModified)
                    {
                        var result = _messageBox.ShowYesNoCancel("The experiment has been modified.  Would you like to save your changes before exiting?", MessageBoxImage.Question);
                        switch (result)
                        {
                            case MessageBoxResult.Yes:
                                Database.SaveChanges();
                                break;
                            case MessageBoxResult.Cancel:
                                ((CancelEventArgs)o.EventArgs).Cancel = true;
                                return;
                        }
                    }
                    MediatorMessage.Send(MediatorMessage.ApplicationClosing, true);
                    ESME.Globals.AppSettings.Save();
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;
        #endregion

        #region RefreshMapCommand
        public SimpleCommand<object, object> RefreshMapCommand
        {
            get { return _refreshMap ?? (_refreshMap = new SimpleCommand<object, object>(obj => MediatorMessage.Send(MediatorMessage.RefreshMap, true))); }
        }

        SimpleCommand<object, object> _refreshMap;
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
                    var viewModel = new AcousticSimulatorOptionsViewModel();
                    var result = _visualizer.ShowDialog("AcousticSimulatorOptionsView", viewModel);
                    if ((result.HasValue) && (result.Value)) ESME.Globals.AppSettings.Save();
                    else ESME.Globals.AppSettings = AppSettings.Load();
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