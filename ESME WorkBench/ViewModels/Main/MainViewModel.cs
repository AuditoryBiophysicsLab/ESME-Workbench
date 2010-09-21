﻿using System;
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
        #region Data

        readonly IMessageBoxService _messageBoxService;
        readonly IOpenFileService _openFileService;
        readonly IViewAwareStatus _viewAwareStatusService;
        readonly IUIVisualizerService _visualizerService;

        public static AppSettings AppSettings { get; set; }

        #endregion

        #region Constructors

        static MainViewModel() { AppSettings = AppSettings.Load(); }

        [ImportingConstructor]
        public MainViewModel(IViewAwareStatus viewAwareStatusService, IMessageBoxService messageBoxService, IOpenFileService openFileService, IUIVisualizerService visualizerService)
        {
            _viewAwareStatusService = viewAwareStatusService;
            _messageBoxService = messageBoxService;
            _openFileService = openFileService;
            _visualizerService = visualizerService;

            EditOptionsCommand = new SimpleCommand<object, object>(delegate
                                                                   {
                                                                       var programOptionsViewModel = new ProgramOptionsViewModel();
                                                                       var result = _visualizerService.ShowDialog("OptionsPopup", programOptionsViewModel);
                                                                       if ((result.HasValue) && (result.Value)) AppSettings.Save();
                                                                       else AppSettings.Reload();
                                                                   });

            TestTransmissionLossViewCommand = new SimpleCommand<object, object>(delegate
                                                                                {
                                                                                    var transmissionLossViewModel = new TransmissionLossFieldViewModel(@"C:\Users\Dave Anderson\Desktop\Bahamas.tlf");
                                                                                    _visualizerService.Show("TransmissionLossView", transmissionLossViewModel, true, null);
                                                                                });

            LaunchExternalProgramCommand = new SimpleCommand<object, object>(delegate(Object arg)
                                                                             {
                                                                                 var executable = new Process();
                                                                                 var executablePath = (string) arg;
                                                                                 executable.StartInfo.FileName = executablePath;
                                                                                 executable.Start();
                                                                             });

            LaunchScenarioEditorCommand = new SimpleCommand<object, object>(delegate { return (AppSettings.ScenarioEditorExecutablePath != null) && (File.Exists(AppSettings.ScenarioEditorExecutablePath)); }, delegate
                                                                                                                                                                                                                {
                                                                                                                                                                                                                    new Process
                                                                                                                                                                                                                    {
                                                                                                                                                                                                                        StartInfo =
                                                                                                                                                                                                                            {
                                                                                                                                                                                                                                FileName = AppSettings.ScenarioEditorExecutablePath,
                                                                                                                                                                                                                                WorkingDirectory = Path.GetDirectoryName(AppSettings.ScenarioEditorExecutablePath),
                                                                                                                                                                                                                            }
                                                                                                                                                                                                                    }.Start();
                                                                                                                                                                                                                });

            LaunchEnvironmentBuilderCommand = new SimpleCommand<object, object>(delegate
                                                                                {
                                                                                    var environmentBuilder = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "EnvironmentBuilder.exe");
                                                                                    return (File.Exists(environmentBuilder) && ((AppSettings.EnvironmentDatabaseDirectory != null) && (Directory.Exists(AppSettings.EnvironmentDatabaseDirectory))));
                                                                                }, delegate
                                                                                   {
                                                                                       var environmentBuilder = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "EnvironmentBuilder.exe");
                                                                                       new Process
                                                                                       {
                                                                                           StartInfo =
                                                                                               {
                                                                                                   FileName = environmentBuilder,
                                                                                                   Arguments = string.Format("\"{0}\"", AppSettings.EnvironmentDatabaseDirectory),
                                                                                               }
                                                                                       }.Start();
                                                                                   });

            MapViewModel = new MapViewModel(_viewAwareStatusService, _messageBoxService, _openFileService, _visualizerService);

            DisabledCommand = new SimpleCommand<object, object>(delegate { return false; }, delegate { });

            CancelCurrentCommand = new SimpleCommand<object, object>(delegate
                                                                     {
                                                                         MapViewModel.IsQuickLookMode = false;
                                                                         MapViewModel.Cursor = Cursors.Arrow;
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
                        MapViewModel.AddShapeFile(file);
                        break;
                    case ".ovr":
                        MapViewModel.AddOverlayFile(file);
                        break;
                    case ".eeb":
                        MapViewModel.AddEnvironmentFile(file);
                        break;
                    case ".nemo":
                        if (MapViewModel.CanAddScenarioFile())
                            MapViewModel.AddScenarioFile(file);
                        break;
                }
            }
            MapViewModel.Refresh();
        }


        public MapViewModel MapViewModel { get; set; }

        #endregion

        #region Commands

        public SimpleCommand<Object, Object> EditOptionsCommand { get; private set; }
        public SimpleCommand<Object, Object> LaunchExternalProgramCommand { get; private set; }
        public SimpleCommand<Object, Object> LaunchScenarioEditorCommand { get; private set; }
        public SimpleCommand<Object, Object> LaunchEnvironmentBuilderCommand { get; private set; }
        public SimpleCommand<Object, Object> TestTransmissionLossViewCommand { get; private set; }

        public SimpleCommand<Object, Object> DisabledCommand { get; private set; }
        public SimpleCommand<Object, Object> CancelCurrentCommand { get; private set; }

        #endregion
    }
}