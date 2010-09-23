using System;
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
        public MainViewModel(IViewAwareStatus viewAwareStatusService, IMessageBoxService messageBoxService, IOpenFileService openFileService, IUIVisualizerService visualizerService)
        {
            Globals.ViewAwareStatus = viewAwareStatusService;
            Globals.MessageBoxService = messageBoxService;
            Globals.OpenFileService = openFileService;
            Globals.UIVisualizerService = visualizerService;
            Globals.MainViewModel = this;

            EditOptionsCommand = new SimpleCommand<object, object>(delegate
                                                                   {
                                                                       var programOptionsViewModel = new ProgramOptionsViewModel();
                                                                       var result = Globals.UIVisualizerService.ShowDialog("OptionsPopup", programOptionsViewModel);
                                                                       if ((result.HasValue) && (result.Value)) Globals.AppSettings.Save();
                                                                       else Globals.AppSettings.Reload();
                                                                   });

            TestTransmissionLossViewCommand = new SimpleCommand<object, object>(delegate
                                                                                {
                                                                                    var transmissionLossViewModel = new TransmissionLossFieldViewModel(@"C:\Users\Dave Anderson\Desktop\Bahamas.tlf");
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

            //RibbonViewModel.RecentExperiments.InsertFile(@"C:\Users\Dave Anderson\Documents\ESME WorkBench\test.esme");

        }

        public void FilesDropped(Object sender, DragEventArgs e)
        {
            if (!e.Data.GetDataPresent(DataFormats.FileDrop)) return;
            var droppedFilePaths = (string[])e.Data.GetData(DataFormats.FileDrop, true);
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
                        if (Globals.MapViewModel.CanAddScenarioFile())
                            Globals.MapViewModel.AddScenarioFile(file);
                        break;
                }
            }
            Globals.MapViewModel.Refresh();
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

        #endregion
    }
}