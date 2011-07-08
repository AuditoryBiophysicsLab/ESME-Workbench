using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Windows;
using System.Windows.Input;
using System.Windows.Threading;
using Cinch;
using ESME;
using ESMEWorkBench.Data;
using ESMEWorkBench.Properties;
using ESMEWorkBench.ViewModels.NAVO;
using ESMEWorkBench.ViewModels.RecentFiles;
using HRC.Navigation;
using HRC.Services;
using MEFedMVVM.Common;
using MEFedMVVM.ViewModelLocator;
using ESME.Views.AcousticBuilder;


namespace ESMEWorkBench.ViewModels.Main
{
    [ExportViewModel("MainViewModel")]
    public partial class MainViewModel : ViewModelBase
    {
        #region Private fields

        readonly IMessageBoxService _messageBoxService;
        readonly IHRCOpenFileService _openFileService;
        readonly IHRCSaveFileService _saveFileService;
        readonly IViewAwareStatus _viewAwareStatus;
        readonly IUIVisualizerService _visualizerService;
        Experiment _experiment;
        //TransmissionLossQueueCalculatorViewModel _bellhopQueueCalculatorViewModel;
        Dispatcher _dispatcher;
        #endregion

        #region Constructors
        [ImportingConstructor]
        public MainViewModel(IViewAwareStatus viewAwareStatus, IMessageBoxService messageBoxService, IHRCOpenFileService openFileService, IHRCSaveFileService saveFileService, IUIVisualizerService visualizerService)
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

            AnalysisPointSettingsViewModel.MessageBoxService = messageBoxService;
            Experiment.MessageBoxService = messageBoxService;
            Experiment.VisualizerService = visualizerService;

            _viewAwareStatus = viewAwareStatus;
            _messageBoxService = messageBoxService;
            _openFileService = openFileService;
            _saveFileService = saveFileService;
            _visualizerService = visualizerService;
            if (Designer.IsInDesignMode) return;
            _viewAwareStatus.ViewUnloaded += () =>
            {
                if (Designer.IsInDesignMode) return;
                MediatorMessage.Send(MediatorMessage.ApplicationClosing);
            };
            
            _viewAwareStatus.ViewLoaded += () =>
            {
                if (Designer.IsInDesignMode) return;
                _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
                MediatorMessage.Send(MediatorMessage.MainViewModelInitialized, _dispatcher);
                InitializeEnvironmentTab();
            };

            IsLatLonGridVisible = Settings.Default.ShowGrid;
            IsScaleBarVisible = Settings.Default.ShowScaleBar;
            IsPanZoomVisible = Settings.Default.ShowPanZoom;

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
                _experiment = new Experiment();
                HookPropertyChanged(_experiment);
                _experiment.InitializeIfViewModelsReady();
                DecoratedExperimentName = "<New experiment>";
            }
            HookPropertyChanged(_experiment);
            //TestRecentFiles();
        }

        void HookPropertyChanged(INotifyPropertyChanged experiment)
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
                if (_experiment.Bathymetry != null)
                {
                    EarthCoordinate<float> mouseDepth;
                    if (_experiment.Bathymetry.Contains(_mouseEarthCoordinate))
                    {
                        mouseDepth = _experiment.Bathymetry[_mouseEarthCoordinate];
                        if (mouseDepth != null) MouseDepth = mouseDepth.Data;
                        else MouseDepth = null;
                    }
                    else MouseDepth = null;
                }
                NotifyPropertyChanged(MouseEarthCoordinateChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MouseEarthCoordinateChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.MouseEarthCoordinate);
        EarthCoordinate _mouseEarthCoordinate;

        #endregion

        #region public float? MouseDepth { get; set; }

        public float? MouseDepth
        {
            get { return _mouseDepth; }
            set
            {
                if (_mouseDepth == value) return;
                _mouseDepth = value;
                NotifyPropertyChanged(MouseDepthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MouseDepthChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.MouseDepth);
        float? _mouseDepth;

        #endregion


        #endregion

        #region Drag/Drop

        public bool IsAddScenarioFilePossible()
        {
            return ((Globals.AppSettings.ScenarioDataDirectory != null) && (Directory.Exists(Globals.AppSettings.ScenarioDataDirectory)) && (_experiment != null) && (_experiment.NemoFile == null));
        }

        public void FilesDropped(Object sender, DragEventArgs e)
        {
            if (!e.Data.GetDataPresent(DataFormats.FileDrop)) return;
            var droppedFilePaths = (string[])e.Data.GetData(DataFormats.FileDrop, true);
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
                var result = _openFileService.ShowDialog((Window)_viewAwareStatus.View);
                if ((!result.HasValue) || (!result.Value)) return;
                _experiment.FileName = _openFileService.FileName;
                Settings.Default.LastExperimentFileDirectory = Path.GetDirectoryName(_openFileService.FileName);
            }
            using (new OverrideCursor(Cursors.Wait)) 
            {
                LoadExperimentFile(_openFileService.FileName);
                RecentFiles.InsertFile(_openFileService.FileName);
            }
        }

        void LoadExperimentFile(string fileName)
        {
            try
            {
                using (new OverrideCursor(Cursors.Wait)) 
                {
                    MediatorMessage.Send(MediatorMessage.EnableGUI, false);
                    Experiment newExperiment;
                    try
                    {
                        newExperiment = Experiment.Load(fileName);
                        newExperiment.FileName = fileName;
                    }
                    catch (UserCanceledOperationException)
                    {
                        return;
                    }
                    catch (Exception e)
                    {
                        _messageBoxService.ShowError("Error opening experiment: " + e.Message);
                        return;
                    }
                    MediatorMessage.Send(MediatorMessage.SetExperiment, (Experiment)null);
                    _experiment = newExperiment;
                    DecoratedExperimentName = Path.GetFileName(_experiment.FileName);
                    HookPropertyChanged(_experiment);
                    _experiment.InitializeIfViewModelsReady();
                }
            }
            finally
            {
                MediatorMessage.Send(MediatorMessage.EnableGUI, true);
                IsLatLonGridVisible = Settings.Default.ShowGrid;
                IsScaleBarVisible = Settings.Default.ShowScaleBar;
                IsPanZoomVisible = Settings.Default.ShowPanZoom;
            }
        }

        void NewExperiment()
        {
            if (UserCanceledBecauseExperimentUnsaved()) return;
            if (_experiment != null) _experiment.Close();
            _experiment = new Experiment();
            _experiment.InitializeIfViewModelsReady();
            DecoratedExperimentName = "<New experiment>";
            HookPropertyChanged(_experiment);
        }

        void OpenScenarioFile(string fileName)
        {
            _openFileService.FileName = null;
            if (fileName == null)
            {
                _openFileService.Filter = "NUWC Scenario Files (*.nemo)|*.nemo";
                _openFileService.InitialDirectory = Settings.Default.LastScenarioFileDirectory;
                _openFileService.FileName = null;
                var result = _openFileService.ShowDialog((Window)_viewAwareStatus.View);
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
            var result = _saveFileService.ShowDialog((Window)_viewAwareStatus.View);
            if ((!result.HasValue) || (!result.Value)) return false;
            _experiment.FileName = _saveFileService.FileName;
            Settings.Default.LastExperimentFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);
            return true;
        }

        bool SaveExperimentAs()
        {
            var oldRoot = _experiment.LocalStorageRoot;
            if (!SaveExperimentDialog()) return false;
            var newRoot = _experiment.LocalStorageRoot;
            _experiment.Save();
            if (oldRoot != newRoot)
            {
                var oldRootInfo = new DirectoryInfo(oldRoot);
                var newRootInfo = new DirectoryInfo(newRoot);
                Experiment.CopyAllPrivateFiles(oldRootInfo, newRootInfo);
            }
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
            RecentFiles.InsertFile(_experiment.FileName);
            return true;
        }

        #endregion

        void ShowAboutView()
        {
            var aboutViewModel = new AboutViewModel();
            _visualizerService.ShowDialog("AboutView", aboutViewModel);
        }

        bool UserWantsToReplaceScenarioFileIfPresent(string filename)
        {
            if ((_experiment != null) && (_experiment.ScenarioFileName != null) && (filename != _experiment.ScenarioFileName))
            {
                var result = _messageBoxService.ShowYesNo(string.Format("This experiment already has a scenario file.  Replace it with \"{0}\"", filename), CustomDialogIcons.Exclamation);
                if (result == CustomDialogResults.No) return false;
            }
            return true;
        }

        bool CanShowEnvironmentSettings
        {
            get
            {
                return ((_experiment != null) && (_experiment.NemoFile != null) && (_experiment.FileName != null) && Globals.AppSettings.NAVOConfiguration.IsValid);
            }
        }

        public bool CanPlaceAnalysisPoint { get { return (_experiment != null) && (_experiment.NemoFile != null) && (_experiment.Bathymetry != null) && (_experiment.SoundSpeedField != null) && (_experiment.FileName != null); } }
        
        void ShowEnvironmentSettingsView()
        {
            var environmentBuilderViewModel = new EnvironmentBuilderViewModel(_messageBoxService, Globals.AppSettings, _experiment);
            try
            {
                var result = _visualizerService.ShowDialog("EnvironmentBuilderView", environmentBuilderViewModel);
                if (result.HasValue && result.Value)
                {
                    _experiment.InitializeEnvironment(false);
                }
            }
            catch (ApplicationException ex)
            {
                _messageBoxService.ShowError(string.Format("{0}: {1}", ex.Message, ex.InnerException.Message));
            }
        }

        #region public bool IsLatLonGridVisible { get; set; }

        public bool IsLatLonGridVisible
        {
            get { return _isLatLonGridVisible; }
            set
            {
                _isLatLonGridVisible = value;
                Settings.Default.ShowGrid = value;
                MediatorMessage.Send(MediatorMessage.SetGridOverlayDisplay, value);
                NotifyPropertyChanged(IsLatLonGridVisibleChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsLatLonGridVisibleChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsLatLonGridVisible);
        bool _isLatLonGridVisible;

        #endregion

        #region public bool IsPanZoomVisible { get; set; }

        public bool IsPanZoomVisible
        {
            get { return _isPanZoomVisible; }
            set
            {
                _isPanZoomVisible = value;
                Settings.Default.ShowPanZoom = value;
                MediatorMessage.Send(MediatorMessage.SetPanZoomDisplay, value);
                NotifyPropertyChanged(IsPanZoomVisibleChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsPanZoomVisibleChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsPanZoomVisible);
        bool _isPanZoomVisible;

        #endregion

        #region public bool IsScaleBarVisible { get; set; }

        public bool IsScaleBarVisible
        {
            get { return _isScaleBarVisible; }
            set
            {
                _isScaleBarVisible = value;
                Settings.Default.ShowScaleBar = value;
                MediatorMessage.Send(MediatorMessage.SetScaleBarDisplay, value);
                NotifyPropertyChanged(IsScaleBarVisibleChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsScaleBarVisibleChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsScaleBarVisible);
        bool _isScaleBarVisible;

        #endregion

        #region public RecentFileDescriptor RecentFilesSelectedItem { get; set; }

        public RecentFileDescriptor RecentFilesSelectedItem
        {
            get { return null; }
            set
            {
                _recentFilesSelectedItem = value;
                if (_recentFilesSelectedItem == null) return;
                try
                {
                    LoadExperimentFile(_recentFilesSelectedItem.LongName);
                }
                catch (Exception e)
                {
                    _messageBoxService.ShowError("Error opening experiment: " + e.Message);
                    RecentFiles.RemoveFile(_recentFilesSelectedItem.LongName);
                }
                NotifyPropertyChanged(RecentFilesSelectedItemChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RecentFilesSelectedItemChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.RecentFilesSelectedItem);
        RecentFileDescriptor _recentFilesSelectedItem;

        #endregion

        #region public RecentFileList RecentFiles { get; set; }

        public RecentFileList RecentFiles
        {
            get { return _recentFiles; }
            set
            {
                if (_recentFiles == value) return;
                _recentFiles = value;
                NotifyPropertyChanged(RecentFilesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RecentFilesChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.RecentFiles);
        RecentFileList _recentFiles = new RecentFileList();

        #endregion

        #region public bool IsInAnalysisPointMode { get; set; }

        public bool IsInAnalysisPointMode
        {
            get { return _isInAnalysisPointMode; }
            set
            {
                if (_isInAnalysisPointMode == value) return;
                _isInAnalysisPointMode = value;
                NotifyPropertyChanged(IsInAnalysisPointModeChangedEventArgs);
                MediatorMessage.Send(MediatorMessage.SetAnalysisPointMode, _isInAnalysisPointMode);
            }
        }

        static readonly PropertyChangedEventArgs IsInAnalysisPointModeChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsInAnalysisPointMode);
        bool _isInAnalysisPointMode;

        #endregion

        #region PreviewKeyDownCommand

        public SimpleCommand<object, EventToCommandArgs> PreviewKeyDownCommand
        {
            get
            {
                return _previewKeyDown ?? (_previewKeyDown = new SimpleCommand<object, EventToCommandArgs>(delegate(EventToCommandArgs args)
                {
                    // var commandRan = args.CommandRan;

                    // get command parameter
                    // var o = args.CommandParameter; 

                    // get KeyEventArgs
                    var keyEventArgs = (KeyEventArgs)args.EventArgs;
                    
                    // get the orginal event sender
                    // var sender = args.Sender; 
                    switch (keyEventArgs.Key)
                    {
                        case Key.Escape:
                            // Anything else that is to be canceled by the user hitting the ESC key must be put here
                            IsInAnalysisPointMode = false;
                            break;
                        default:
                            break;
                    }
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _previewKeyDown;

        #endregion

        #region public int SelectedRibbonTabIndex { get; set; }

        public int SelectedRibbonTabIndex
        {
            get
            {
                IsLayerListViewVisible = Settings.Default.SelectedTabIndex != 2;
                return Settings.Default.SelectedTabIndex;
            }
            set
            {
                Settings.Default.SelectedTabIndex = value;
                IsLayerListViewVisible = Settings.Default.SelectedTabIndex != 2;
                NotifyPropertyChanged(RibbonTabIndexChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RibbonTabIndexChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedRibbonTabIndex);

        #endregion

        #region RibbonTabSelectionChangedCommand
        public SimpleCommand<object, object> RibbonTabSelectionChangedCommand
        {
            get { return _ribbonTabSelectionChangedCommand ?? (_ribbonTabSelectionChangedCommand = new SimpleCommand<object, object>(delegate { RibbonTabSelectionChanged(); })); }
        }

        SimpleCommand<object, object> _ribbonTabSelectionChangedCommand;

        void RibbonTabSelectionChanged()
        {
            switch (((Views.MainView)_viewAwareStatus.View).Ribbon.SelectedIndex)
            {
                case 0:
                    Console.WriteLine("Experiment tab selected");
                    IsLayerListViewVisible = true;
                    break;
                case 1:
                    Console.WriteLine("Scenario tab selected");
                    IsLayerListViewVisible = true;
                    break;
                case 2:
                    Console.WriteLine("Environment tab selected");
                    IsLayerListViewVisible = false;
                    break;
                case 3:
                    Console.WriteLine("Animals tab selected");
                    IsLayerListViewVisible = true;
                    break;
                case 4:
                    Console.WriteLine("Acoustics tab selected");
                    IsLayerListViewVisible = true;
                    break;
                default:
                    Console.WriteLine("Other tab selected");
                    IsLayerListViewVisible = true;
                    break;
            }
        }

        #endregion

        #region public bool IsLayerListViewVisible { get; set; }

        public bool IsLayerListViewVisible
        {
            get { return _isLayerListViewVisible; }
            set
            {
                if (_isLayerListViewVisible == value) return;
                _isLayerListViewVisible = value;
                NotifyPropertyChanged(IsLayerListViewVisibleChangedEventArgs);
                NotifyPropertyChanged(LayersListWidthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsLayerListViewVisibleChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsLayerListViewVisible);
        bool _isLayerListViewVisible;

        #endregion

        #region public double LayersListWidth { get; set; }

        public double LayersListWidth
        {
            get { return IsLayerListViewVisible ? Math.Max(100, Settings.Default.LayersWidth) : 0; }
            set
            {
                if (!IsLayerListViewVisible) return;
                Settings.Default.LayersWidth = value;
                NotifyPropertyChanged(LayersListWidthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LayersListWidthChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.LayersListWidth);

        #endregion


    }
}