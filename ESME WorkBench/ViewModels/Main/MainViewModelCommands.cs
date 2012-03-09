using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using Cinch;
using ESME;
using ESME.Data;
using ESME.Mapping;
using ESME.Views.LogFileViewer;
using ESMEWorkbench.Properties;
using ESMEWorkbench.ViewModels.NAVO;
using ESMEWorkbench.ViewModels.TransmissionLoss;

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
                    var extraTypes = new List<Type>
                    {
                            typeof (MapLayerViewModel),
                            typeof (ShapefileMapLayer),
                            typeof (OverlayShapeMapLayer),
                            typeof (OverlayFileMapLayer)
                    };
                    var programOptionsViewModel = new ApplicationOptionsViewModel(_messageBoxService);
                    var result = _visualizerService.ShowDialog("ApplicationOptionsView", programOptionsViewModel);
                    if ((result.HasValue) && (result.Value)) ESME.Globals.AppSettings.Save(extraTypes);
                    ESME.Globals.AppSettings = AppSettings.Load(extraTypes);
                    ESME.Globals.AppSettings = ESME.Globals.AppSettings;
                    if (ESME.Globals.AppSettings != null && ESME.Globals.AppSettings.ScenarioDataDirectory != null &&
                        File.Exists(Path.Combine(ESME.Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"))) 
                        InitializeEnvironmentManager();
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
                _messageBoxService.ShowError("The user manual was not found!");
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
                    //ScenarioMetadata = null;
                    ESME.Globals.AppSettings.Save();
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;
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
                    var extraTypes = new List<Type>
                    {
                            typeof (MapLayerViewModel),
                            typeof (ShapefileMapLayer),
                            typeof (OverlayShapeMapLayer),
                            typeof (OverlayFileMapLayer)
                    };
                    var viewModel = new AcousticSimulatorOptionsViewModel();
                    var result = _visualizerService.ShowDialog("AcousticSimulatorOptionsView", viewModel);
                    if ((result.HasValue) && (result.Value)) ESME.Globals.AppSettings.Save(extraTypes);
                    else ESME.Globals.AppSettings = AppSettings.Load(extraTypes);
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