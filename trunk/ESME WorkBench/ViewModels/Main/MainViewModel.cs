using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Windows;
using System.Windows.Input;
using Cinch;
using ESMEWorkBench.Data;
using ESMEWorkBench.ViewModels.TransmissionLoss;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.Main
{
    [ExportViewModel("MainViewModel")]
    public class MainViewModel : ViewModelBase
    {
        #region Constructors

        [ImportingConstructor]
        public MainViewModel(IViewAwareStatus viewAwareStatusService, IMessageBoxService messageBoxService, IOpenFileService openFileService, ISaveFileService saveFileService, IUIVisualizerService visualizerService)
        {
            Globals.ViewAwareStatus = viewAwareStatusService;
            Globals.MessageBoxService = messageBoxService;
            Globals.OpenFileService = openFileService;
            Globals.SaveFileService = saveFileService;
            Globals.UIVisualizerService = visualizerService;
            Globals.MainViewModel = this;

            var args = Environment.GetCommandLineArgs();
            if (args.Length == 2)
            {
                if (File.Exists(args[1]))
                {
                    if (args[1].EndsWith(".esme"))
                    {
                        try
                        {
                            Globals.Experiment = Experiment.Load(args[1]);
                            Globals.IsInitializeExperimentNeeded = true;
                            DecoratedExperimentName = Path.GetFileName(args[1]);
                        }
                        catch (Exception ex)
                        {
                            Globals.MessageBoxService.ShowError(string.Format("Error opening experiment file \"{0}\":\n{1}", args[1], ex.Message));
                        }
                    }
                }
            }
            else
            {
                Globals.Experiment = new Experiment();
                DecoratedExperimentName = "<New experiment>";
            }
            Globals.Experiment.PropertyChanged += delegate
                                                  {
                                                      if (Globals.Experiment.IsChanged)
                                                      {
                                                          if (DecoratedExperimentName.EndsWith(" *")) return;
                                                          DecoratedExperimentName += " *";
                                                      }
                                                      else
                                                      {
                                                          if (!DecoratedExperimentName.EndsWith(" *")) return;
                                                          DecoratedExperimentName.Remove(DecoratedExperimentName.Length - 2);
                                                      }
                                                  };

            EditOptionsCommand = new SimpleCommand<object, object>(delegate
                                                                   {
                                                                       var programOptionsViewModel = new ProgramOptionsViewModel();
                                                                       var result = Globals.UIVisualizerService.ShowDialog("OptionsPopup", programOptionsViewModel);
                                                                       if ((result.HasValue) && (result.Value)) Globals.AppSettings.Save();
                                                                       else Globals.AppSettings.Reload();
                                                                   });

            TestTransmissionLossViewCommand = new SimpleCommand<object, object>(delegate
                                                                                {
                                                                                    Globals.OpenFileService.Filter = "Transmission Loss File (*.tlf)|*.tlf";
                                                                                    var result = Globals.OpenFileService.ShowDialog(null);
                                                                                    if (!result.HasValue || !result.Value) return;
                                                                                    var transmissionLossViewModel = new TransmissionLossFieldViewModel(Globals.OpenFileService.FileName);
                                                                                    Globals.UIVisualizerService.Show("TransmissionLossView", transmissionLossViewModel, true, null);
                                                                                });

            LaunchExternalProgramCommand = new SimpleCommand<object, string>(delegate(string executablePath)
                                                                             {
                                                                                 var executable = new Process
                                                                                                  {
                                                                                                      StartInfo =
                                                                                                          {
                                                                                                              FileName = executablePath
                                                                                                          }
                                                                                                  };
                                                                                 executable.Start();
                                                                             });

            LaunchScenarioEditorCommand = new SimpleCommand<object, object>(delegate { return (Globals.AppSettings.ScenarioEditorExecutablePath != null) && (File.Exists(Globals.AppSettings.ScenarioEditorExecutablePath)); }, delegate
                                                                                                                                                                                                                                {
                                                                                                                                                                                                                                    new Process
                                                                                                                                                                                                                                    {
                                                                                                                                                                                                                                        StartInfo =
                                                                                                                                                                                                                                            {
                                                                                                                                                                                                                                                FileName = Globals.AppSettings.ScenarioEditorExecutablePath,
                                                                                                                                                                                                                                                WorkingDirectory = Path.GetDirectoryName(Globals.AppSettings.ScenarioEditorExecutablePath),
                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                    }.Start();
                                                                                                                                                                                                                                });

            LaunchEnvironmentBuilderCommand = new SimpleCommand<object, object>(delegate
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
                                                                                   });

            Globals.MapViewModel = new MapViewModel();

            DisabledCommand = new SimpleCommand<object, object>(delegate { return false; }, delegate { });

            CancelCurrentCommand = new SimpleCommand<object, object>(delegate
                                                                     {
                                                                         Globals.MapViewModel.IsQuickLookMode = false;
                                                                         Globals.MapViewModel.Cursor = Cursors.Arrow;
                                                                     });
            ViewClosingCommand = new SimpleCommand<object, EventToCommandArgs>(delegate(EventToCommandArgs vcArgs)
            {
                var ea = (CancelEventArgs)vcArgs.EventArgs;
                ea.Cancel = UserCanceledBecauseExperimentUnsaved();
            });

            //RibbonViewModel.RecentExperiments.InsertFile(@"C:\Users\Dave Anderson\Documents\ESME WorkBench\test.esme");
        }

        #region ViewModel properties

        #region public string DecoratedExperimentName { get; set; }

        public string DecoratedExperimentName
        {
            get { return _decoratedExperimentName; }
            set
            {
                if (_decoratedExperimentName == value) return;
                _decoratedExperimentName = value;
                NotifyPropertyChanged(DecoratedExperimentNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DecoratedExperimentNameChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.DecoratedExperimentName);
        string _decoratedExperimentName;

        #endregion

        #endregion

        public void FilesDropped(Object sender, DragEventArgs e)
        {
            if (!e.Data.GetDataPresent(DataFormats.FileDrop)) return;
            var droppedFilePaths = (string[]) e.Data.GetData(DataFormats.FileDrop, true);
            foreach (var file in droppedFilePaths)
            {
                switch (Path.GetExtension(file).ToLower())
                {
                    case ".shp":
                        Globals.MapViewModel.AddShapeFile(file);
                        break;
                    case ".ovr":
                        Globals.MapViewModel.AddOverlayFile(file);
                        break;
                    case ".eeb":
                        Globals.MapViewModel.AddEnvironmentFile(file);
                        break;
                    case ".nemo":
                        if (Globals.MapViewModel.CanAddScenarioFile()) Globals.MapViewModel.AddScenarioFile(file);
                        break;
                }
            }
            Globals.MapViewModel.Refresh();
        }

        /// <summary>
        /// If the current experiment is unsaved, ask the user to save the experiment.
        /// </summary>
        /// <returns>true if the user wants to cancel the current operation, false otherwise.</returns>
        static bool UserCanceledBecauseExperimentUnsaved()
        {
            if (!Globals.Experiment.IsChanged) return false;
            var results = Globals.MessageBoxService.ShowYesNoCancel(@"There are unsaved changes in the current experiment.  Save them first?", CustomDialogIcons.Exclamation);
            if (results == CustomDialogResults.Cancel) return true;
            if (results == CustomDialogResults.No) return false;
            
            return !SaveExperiment();
        }

        /// <summary>
        /// If the experiment has not been given a file name, prompt the user for it, then save
        /// </summary>
        /// <returns>true if the file was saved, false otherwise.</returns>
        static bool SaveExperiment()
        {
            if (Globals.Experiment.FileName == null)
            {
                Globals.SaveFileService.Filter = "ESME files (*.esme)|*.esme|All files (*.*)|*.*";
                Globals.SaveFileService.OverwritePrompt = true;
                //Globals.SaveFileService.InitialDirectory
                var result = Globals.SaveFileService.ShowDialog((Window)Globals.ViewAwareStatus.View);
                if ((!result.HasValue) || (!result.Value)) return false;
                Globals.Experiment.FileName = Globals.SaveFileService.FileName;
            }
            Globals.Experiment.Save();
            return true;
        }
        #endregion

        #region Commands

        public SimpleCommand<Object, Object> EditOptionsCommand { get; private set; }
        public SimpleCommand<Object, string> LaunchExternalProgramCommand { get; private set; }
        public SimpleCommand<Object, Object> LaunchScenarioEditorCommand { get; private set; }
        public SimpleCommand<Object, Object> LaunchEnvironmentBuilderCommand { get; private set; }
        public SimpleCommand<Object, Object> TestTransmissionLossViewCommand { get; private set; }

        public SimpleCommand<Object, Object> DisabledCommand { get; private set; }
        public SimpleCommand<Object, Object> CancelCurrentCommand { get; private set; }
        public SimpleCommand<Object,EventToCommandArgs> ViewClosingCommand { get; private set; }
        #endregion
    }
}