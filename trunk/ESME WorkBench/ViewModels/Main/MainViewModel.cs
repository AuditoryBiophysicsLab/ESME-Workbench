using System;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Windows;
using Cinch;
using ESMERibbonDemo.ViewModels;
using ESMEWorkBench.Data;
using ESMEWorkBench.Properties;
using HRC.Navigation;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.Main
{
    [ExportViewModel("MainViewModel")]
    public partial class MainViewModel : ViewModelBase
    {
        #region Private fields

        readonly IMessageBoxService _messageBoxService;
        readonly IOpenFileService _openFileService;
        readonly ISaveFileService _saveFileService;
        readonly IViewAwareStatus _viewAwareStatusService;
        readonly IUIVisualizerService _visualizerService;
        Experiment _experiment;

        #endregion

        #region Constructors

        [ImportingConstructor]
        public MainViewModel(IViewAwareStatus viewAwareStatusService, IMessageBoxService messageBoxService, IOpenFileService openFileService, ISaveFileService saveFileService, IUIVisualizerService visualizerService)
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nMainViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
            _viewAwareStatusService = viewAwareStatusService;
            _messageBoxService = messageBoxService;
            _openFileService = openFileService;
            _saveFileService = saveFileService;
            _visualizerService = visualizerService;
            _viewAwareStatusService.ViewLoaded += ViewLoaded;

            var args = Environment.GetCommandLineArgs();
            if (args.Length == 2)
            {
                if (File.Exists(args[1]))
                {
                    if (args[1].EndsWith(".esme"))
                    {
                        try
                        {
                            LoadExperimentFile(args[1]);
                        }
                        catch (Exception ex)
                        {
                            _messageBoxService.ShowError(string.Format("Error opening experiment file \"{0}\":\n{1}", args[1], ex.Message));
                        }
                    }
                }
            }
            else
            {
                _experiment = new Experiment
                              {
                                  MessageBoxService = _messageBoxService
                              };
                HookPropertyChanged(_experiment);
                _experiment.InitializeIfViewModelsReady();
                DecoratedExperimentName = "<New experiment>";
            }
            HookPropertyChanged(_experiment);
            TestRecentFiles();
        }

        void HookPropertyChanged(Experiment experiment)
        {
            experiment.PropertyChanged += delegate(object s, PropertyChangedEventArgs e)
                                          {
                                              switch (e.PropertyName)
                                              {
                                                  case "IsChanged":
                                                      if (_experiment.IsChanged)
                                                      {
                                                          if (DecoratedExperimentName.EndsWith(" *")) return;
                                                          DecoratedExperimentName += " *";
                                                      }
                                                      else
                                                      {
                                                          if (!DecoratedExperimentName.EndsWith(" *")) return;
                                                          DecoratedExperimentName.Remove(DecoratedExperimentName.Length - 2);
                                                      }
                                                      break;
                                              }
                                          };
        }

        static void ViewLoaded() { MediatorMessage.Send(MediatorMessage.MainViewModelInitialized); }

        protected override void OnDispose()
        {
            base.OnDispose();
            Mediator.Instance.Unregister(this);
        }

        #endregion

        #region ViewModel properties

        #region public string DecoratedExperimentName { get; set; }

        static readonly PropertyChangedEventArgs DecoratedExperimentNameChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.DecoratedExperimentName);
        string _decoratedExperimentName;

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

        #endregion

        #region public EarthCoordinate MouseEarthCoordinate { get; set; }

        public EarthCoordinate MouseEarthCoordinate
        {
            get { return _mouseEarthCoordinate; }
            set
            {
                if (_mouseEarthCoordinate == value) return;
                _mouseEarthCoordinate = value;
                NotifyPropertyChanged(MouseEarthCoordinateChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MouseEarthCoordinateChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.MouseEarthCoordinate);
        EarthCoordinate _mouseEarthCoordinate;

        #endregion

        #endregion

        #region Drag/Drop

        public bool IsAddScenarioFilePossible() { return ((Globals.AppSettings.ScenarioDataDirectory != null) && (Directory.Exists(Globals.AppSettings.ScenarioDataDirectory))); }

        public void FilesDropped(Object sender, DragEventArgs e)
        {
            if (!e.Data.GetDataPresent(DataFormats.FileDrop)) return;
            var droppedFilePaths = (string[]) e.Data.GetData(DataFormats.FileDrop, true);
            var refreshNeeded = false;
            foreach (var file in droppedFilePaths)
            {
                try
                {
                    switch (Path.GetExtension(file).ToLower())
                    {
                        case ".shp":
                        case ".ovr":
                            MediatorMessage.Send(MediatorMessage.AddFileCommand, file);
                            refreshNeeded = true;
                            break;
                        case ".nemo":
                            if (!UserWantsToReplaceScenarioFileIfPresent(file)) continue;
                            MediatorMessage.Send(MediatorMessage.AddScenarioFileCommand, file);
                            refreshNeeded = true;
                            break;
                    }
                }
                catch (Exception ex)
                {
                    Globals.DisplayException(_messageBoxService, ex, "Error opening dropped file {0}", file);
                }
            }
            if (refreshNeeded) MediatorMessage.Send(MediatorMessage.RefreshMap);
        }

        public bool UserWantsToAddScenarioFile(string fileName)
        {
            if (!IsAddScenarioFilePossible()) return false;
            if ((_experiment.ScenarioFileName != null) && (_messageBoxService.ShowYesNo("A scenario is already part of this experiment.\nWould you like to replace the current scenario file with this one?", CustomDialogIcons.Exclamation) != CustomDialogResults.Yes)) return false;
            return true;
        }

        #endregion

        #region Experiment Load/Save and associated utility functions

        /// <summary>
        ///   If the current experiment is unsaved, ask the user to save the experiment.
        /// </summary>
        /// <returns>true if the user wants to cancel the current operation, false otherwise.</returns>
        bool UserCanceledBecauseExperimentUnsaved()
        {
            if ((_experiment == null) || (!_experiment.IsChanged)) return false;
            var results = _messageBoxService.ShowYesNoCancel("The current experiment has changed.\nWould you like to save it first?", CustomDialogIcons.Exclamation);
            if (results == CustomDialogResults.Cancel) return true;
            if (results == CustomDialogResults.No) return false;

            return !SaveExperiment();
        }

        void OpenExperiment(string fileName)
        {
            if (UserCanceledBecauseExperimentUnsaved()) return;
            if (fileName == null)
            {
                _openFileService.Filter = "ESME files (*.esme)|*.esme|All files (*.*)|*.*";
                _openFileService.InitialDirectory = Settings.Default.LastExperimentFileDirectory;
                _openFileService.FileName = null;
                var result = _openFileService.ShowDialog((Window) _viewAwareStatusService.View);
                if ((!result.HasValue) || (!result.Value)) return;
                _experiment.FileName = _openFileService.FileName;
                Settings.Default.LastExperimentFileDirectory = Path.GetDirectoryName(_openFileService.FileName);
            }
            LoadExperimentFile(_openFileService.FileName);
        }

        void LoadExperimentFile(string fileName)
        {
            MediatorMessage.Send(MediatorMessage.SetExperiment, (Experiment) null);
            _experiment = Experiment.Load(fileName, Experiment.ReferencedTypes);
            _experiment.FileName = fileName;
            _experiment.MessageBoxService = _messageBoxService;
            DecoratedExperimentName = Path.GetFileName(_experiment.FileName);
            HookPropertyChanged(_experiment);
            _experiment.InitializeIfViewModelsReady();
        }

        void OpenScenarioFile(string fileName)
        {
            _openFileService.FileName = null;
            if (fileName == null)
            {
                _openFileService.Filter = "NUWC Scenario Files (*.nemo)|*.nemo";
                _openFileService.InitialDirectory = Settings.Default.LastScenarioFileDirectory;
                _openFileService.FileName = null;
                var result = _openFileService.ShowDialog((Window) _viewAwareStatusService.View);
                if (!result.HasValue || !result.Value) return;
                fileName = _openFileService.FileName;
            }
            if (!UserWantsToReplaceScenarioFileIfPresent(fileName)) return;
            if (_openFileService.FileName != null) Settings.Default.LastScenarioFileDirectory = Path.GetDirectoryName(_openFileService.FileName);
            MediatorMessage.Send(MediatorMessage.AddScenarioFileCommand, fileName);
        }

        bool SaveExperimentDialog()
        {
            _saveFileService.Filter = "ESME files (*.esme)|*.esme|All files (*.*)|*.*";
            _saveFileService.OverwritePrompt = true;
            _saveFileService.InitialDirectory = Settings.Default.LastExperimentFileDirectory;
            _saveFileService.FileName = null;
            var result = _saveFileService.ShowDialog((Window) _viewAwareStatusService.View);
            if ((!result.HasValue) || (!result.Value)) return false;
            _experiment.FileName = _saveFileService.FileName;
            Settings.Default.LastExperimentFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);
            return true;
        }

        bool SaveExperimentAs()
        {
            if (!SaveExperimentDialog()) return false;
            _experiment.Save();
            DecoratedExperimentName = Path.GetFileName(_experiment.FileName);
            return true;
        }

        /// <summary>
        ///   If the experiment has not been given a file name, prompt the user for it, then save
        /// </summary>
        /// <returns>true if the file was saved, false otherwise.</returns>
        bool SaveExperiment()
        {
            if (_experiment.FileName == null)
            {
                if (!SaveExperimentDialog()) return false;
            }
            _experiment.Save();
            DecoratedExperimentName = Path.GetFileName(_experiment.FileName);
            return true;
        }

        #endregion

        bool UserWantsToReplaceScenarioFileIfPresent(string filename)
        {
            if ((_experiment != null) && (_experiment.ScenarioFileName != null) && (filename != _experiment.ScenarioFileName))
            {
                var result = _messageBoxService.ShowYesNo(string.Format("This experiment already has a scenario file.  Replace it with \"{0}\"", filename), CustomDialogIcons.Exclamation);
                if (result == CustomDialogResults.No) return false;
            }
            return true;
        }

        #region public SelectableCollection<RecentFileDescriptor> RecentFiles { get; set; }

        public SelectableCollection<RecentFileDescriptor> RecentFiles
        {
            get { return _recentFiles; }
            set
            {
                if (_recentFiles == value) return;
                if (_recentFiles != null) _recentFiles.CollectionChanged -= RecentFilesCollectionChanged;
                _recentFiles = value;
                if (_recentFiles != null) _recentFiles.CollectionChanged += RecentFilesCollectionChanged;
                NotifyPropertyChanged(RecentFilesChangedEventArgs);
            }
        }

        void RecentFilesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(RecentFilesChangedEventArgs); }
        static readonly PropertyChangedEventArgs RecentFilesChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.RecentFiles);

        SelectableCollection<RecentFileDescriptor> _recentFiles;

        void TestRecentFiles()
        {
            RecentFiles = new SelectableCollection<RecentFileDescriptor>
                          {
                              new RecentFileDescriptor
                              {
                                  ShortName = "File1",
                                  LongName = "User picked File1",
                                  Command = SelectRecentFileCommand,
                              },
                              new RecentFileDescriptor
                              {
                                  ShortName = "File2",
                                  LongName = "User picked File2",
                                  Command = SelectRecentFileCommand,
                              }
                          };
        }

        #endregion
    }

    public class RecentFileDescriptor : IHasName
    {
        public string ShortName { get; set; }
        public string LongName { get; set; }
        public SimpleCommand<object, string> Command { get; set; }

        public string CommandParameter
        {
            get { return LongName; }
        }

        public string Name
        {
            get { return LongName; }
            set { LongName = value; }
        }
    }
}