using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Text;
using System.Timers;
using System.Windows;
using System.Windows.Input;
using Cinch;
using ESME;
using ESME.Mapping;
using ESME.Metadata;
using ESME.Model;
using ESME.Scenarios;
using ESME.TransmissionLoss.CASS;
using ESME.Views.AcousticBuilder;
using ESME.Views.Locations;
using ESME.Views.TransmissionLoss;
using ESME.Views.TransmissionLossViewer;
using ESMEWorkbench.Properties;
using ESMEWorkbench.ViewModels.Layers;
using HRC.Aspects;
using AnalysisPoint = ESME.TransmissionLoss.AnalysisPoint;

namespace ESMEWorkbench.ViewModels.Main
{
    public partial class MainViewModel
    {
        public MapLayerCollection ScenarioMapLayers { get; set; }

        [Initialize] public LayerTreeViewModel LayerTreeViewModel { get; set; }

        Scenario _scenario;
        public Scenario Scenario
        {
            get { return _scenario; }
            set
            {
                _scenario = value;
                LayerTreeViewModel.Scenario = _scenario;
            }
        }

        #region public ScenarioMetadata ScenarioMetadata { get; set; }

        public ScenarioMetadata ScenarioMetadata
        {
            get { return _scenarioMetadata; }
            set
            {
                if (_scenarioMetadata == value) return;
                if (_scenarioMetadata != null) _scenarioMetadata.Save();
                _scenarioMetadata = value;
                _dispatcher.InvokeIfRequired(() =>
                {
                    NotifyPropertyChanged(ScenarioMetadataChangedEventArgs);
                    NotifyPropertyChanged(CanPlaceAnalysisPointChangedEventArgs);
                });
                if (_scenarioMetadata != null)
                {
                    _scenarioMetadata.PropertyChanged += (s, e) =>
                    {
                        if (e.PropertyName == "CanPlaceAnalysisPoint") _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(CanPlaceAnalysisPointChangedEventArgs));
                    };
                    _scenarioMetadata.Dispatcher = _dispatcher;
                    _scenarioMetadata.VisualizerService = _visualizer;
                }
                IsScenarioLoaded = _scenarioMetadata != null;
            }
        }

        static readonly PropertyChangedEventArgs ScenarioMetadataChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.ScenarioMetadata);
        ScenarioMetadata _scenarioMetadata;

        #endregion

        #region public bool IsScenarioLoaded { get; set; }

        public bool IsScenarioLoaded
        {
            get { return _isScenarioLoaded; }
            set
            {
                if (_isScenarioLoaded == value) return;
                _isScenarioLoaded = value;
                NotifyPropertyChanged(IsScenarioLoadedChangedEventArgs);
                CommandManager.InvalidateRequerySuggested();
                if (!_isScenarioLoaded) MainWindowTitle = "ESME Workbench: <No scenario loaded>";
            }
        }

        static readonly PropertyChangedEventArgs IsScenarioLoadedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsScenarioLoaded);
        bool _isScenarioLoaded;

        #endregion

        #region OpenScenarioCommand
        public SimpleCommand<object, object> OpenScenarioCommand
        {
            get { return _openScenario ?? (_openScenario = new SimpleCommand<object, object>(delegate { OpenScenarioHandler(null); })); }
        }

        SimpleCommand<object, object> _openScenario;

        void OpenScenarioHandler(string fileName)
        {
            _openFile.FileName = null;
            if (fileName == null)
            {
                _openFile.Filter = "NUWC Scenario Files (*.nemo)|*.nemo";
                _openFile.InitialDirectory = Settings.Default.LastScenarioFileDirectory;
                _openFile.FileName = null;
                var result = _openFile.ShowDialog((Window)_viewAwareStatus.View);
                if (!result.HasValue || !result.Value) return;
                fileName = _openFile.FileName;
                Settings.Default.LastScenarioFileDirectory = Path.GetDirectoryName(_openFile.FileName);
            }
            ScenarioMetadata = null;
            RecentFiles.InsertFile(fileName);
            //NemoScenario.Test(fileName);
            _pleaseWait.Message = "Loading scenario, please wait...";
            try
            {
                ScenarioMetadata = ScenarioMetadata.Load(ScenarioMetadata.MetadataFilename(fileName), RangeComplexes);
                ScenarioMetadata.MessageBoxService = _messageBox;
                ScenarioMetadata.CurrentMapLayers = CurrentMapLayers;
                _dispatcher.InvokeIfRequired(() =>
                {
                    ScenarioMetadata.ScenarioFilename = fileName;
                    MainWindowTitle = string.Format("ESME Workbench: {0} [{1}]", ScenarioMetadata.NemoFile.Scenario.EventName, ScenarioMetadata.NemoFile.Scenario.TimeFrame);
                });
                if (_scenarioFileWatcher != null)
                {
                    _scenarioFileWatcher.EnableRaisingEvents = false;
                    _scenarioFileWatcher.Dispose();
                }
                _scenarioFileWatcher = new FileSystemWatcher(Path.GetDirectoryName(fileName), "*" + Path.GetExtension(fileName))
                {
                    NotifyFilter = NotifyFilters.LastWrite,
                };
                _scenarioFileWatcher.Changed += (s, e) =>
                {
                    if (_scenarioFileTimer != null) return;
                    _scenarioFileTimer = new Timer(1000) { AutoReset = false };
                    CloseScenarioHandler();
                    _scenarioFileTimer.Start();
                    _scenarioFileTimer.Elapsed += (s1, e1) =>
                    {
                        _dispatcher.InvokeInBackgroundIfRequired(() => _pleaseWait.Message = "Reloading scenario, please wait...");
                        ScenarioMetadata =
                                ScenarioMetadata.Load(ScenarioMetadata.MetadataFilename(fileName), RangeComplexes) ??
                                new ScenarioMetadata
                                {
                                    Filename = ScenarioMetadata.MetadataFilename(fileName),
                                    RangeComplexes = RangeComplexes
                                };
                        ScenarioMetadata.CurrentMapLayers = CurrentMapLayers;
                        ScenarioMetadata.ScenarioFilename = fileName;
                        _dispatcher.InvokeIfRequired(() =>
                        {
                            MainWindowTitle = string.Format("ESME Workbench: {0} [{1}]",ScenarioMetadata.NemoFile.Scenario.EventName,ScenarioMetadata.NemoFile.Scenario.TimeFrame);
                        });
                        _scenarioFileTimer = null;
                        _dispatcher.InvokeInBackgroundIfRequired(() => _pleaseWait.Hide());
                    };
                };
                _scenarioFileWatcher.Deleted += (s, e) =>
                {
                    CloseScenarioHandler();
                    _messageBox.ShowError(string.Format("Scenario file {0} was deleted", fileName));
                };
                _scenarioFileWatcher.EnableRaisingEvents = true;
            }
            catch (Exception ex)
            {
                var sb = new StringBuilder();
                sb.AppendLine(ex.Message);
                var inner = ex.InnerException;
                while (inner != null)
                {
                    sb.AppendLine(inner.Message);
                    inner = inner.InnerException;
                }
                _messageBox.ShowError("Error opening scenario \"" + Path.GetFileName(fileName) + "\":\n" + sb);
                if (ScenarioMetadata != null) ScenarioMetadata.RemoveScenarioDisplay();
                ScenarioMetadata = null;
            }
            finally
            {
                _pleaseWait.Hide();
            }
        }
        FileSystemWatcher _scenarioFileWatcher;
        Timer _scenarioFileTimer;
        #endregion

        #region CloseScenarioCommand
        public SimpleCommand<object, object> CloseScenarioCommand
        {
            get { return _closeScenario ?? (_closeScenario = new SimpleCommand<object, object>(delegate { return IsScenarioLoaded; }, delegate { CloseScenarioHandler(); })); }
        }

        SimpleCommand<object, object> _closeScenario;

        void CloseScenarioHandler()
        {
            ScenarioMetadata.RemoveScenarioDisplay();
            ScenarioMetadata = null;
        }
        #endregion

        #region ConfigureAcousticModelsCommand
        public SimpleCommand<object, object> ConfigureAcousticModelsCommand
        {
            get
            {
                return _configureAcousticModels ??
                       (_configureAcousticModels =
                        new SimpleCommand<object, object>(delegate
                        {
                            return (ScenarioMetadata != null && ScenarioMetadata.AnalysisPoints != null && ScenarioMetadata.AnalysisPoints.Count > 0);
                        },
                        delegate { ConfigureAcousticModelsHandler(); }));
            }
        }

        SimpleCommand<object, object> _configureAcousticModels;

        void ConfigureAcousticModelsHandler()
        {
            var modeAcousticModelSelectionViewModel = new ModeAcousticModelSelectionViewModel(ScenarioMetadata.NemoModeToAcousticModelNameMap, ESME.Globals.ValidTransmissionLossAlgorithms);
            _visualizer.ShowDialog("ModeAcousticModelSelectionView", modeAcousticModelSelectionViewModel);
        }

        #endregion

        #region ReverifyAcousticModelsCommand
        public SimpleCommand<object, object> ReverifyAcousticModelsCommand
        {
            get { return _reverifyAcousticModels ?? (_reverifyAcousticModels = new SimpleCommand<object, object>(delegate { return IsScenarioLoaded; }, delegate { ReverifyAcousticModelsHandler(); })); }
        }

        SimpleCommand<object, object> _reverifyAcousticModels;

        void ReverifyAcousticModelsHandler()
        {
            if (ScenarioMetadata != null) ScenarioMetadata.ReverifyAcousticModels();
        }
        #endregion

        #region ExportAnalysisPointsCommand
        public SimpleCommand<object, object> ExportAnalysisPointsCommand
        {
            get
            {
                return _exportAnalysisPoints ??
                       (_exportAnalysisPoints =
                        new SimpleCommand<object, object>(delegate
                        {
                            return (ScenarioMetadata != null && ScenarioMetadata.AnalysisPoints != null && ScenarioMetadata.AnalysisPoints.Count > 0);
                        },
                        delegate { ExportAnalysisPointsHandler(); }));
            }
        }

        SimpleCommand<object, object> _exportAnalysisPoints;

        void ExportAnalysisPointsHandler() { ScenarioMetadata.ExportAnalysisPoints(); }
        #endregion

        #region public bool CanPlaceAnalysisPoint { get; set; }

        public bool CanPlaceAnalysisPoint { get { return ScenarioMetadata != null && ScenarioMetadata.CanPlaceAnalysisPoint; } }

        static readonly PropertyChangedEventArgs CanPlaceAnalysisPointChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.CanPlaceAnalysisPoint);

        #endregion

        #region public string MainWindowTitle { get; set; }

        public string MainWindowTitle
        {
            get { return _mainWindowTitle; }
            set
            {
                if (_mainWindowTitle == value) return;
                _mainWindowTitle = value;
                NotifyPropertyChanged(MainWindowTitleChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MainWindowTitleChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.MainWindowTitle);
        string _mainWindowTitle = "ESME Workbench: <No scenario loaded>";

        #endregion

        #region ImportScenarioFileCommand
        public SimpleCommand<object, object> ImportScenarioFileCommand
        {
            get { return _importScenarioFile ?? (_importScenarioFile = new SimpleCommand<object, object>(delegate { return IsImportScenarioFileCommandEnabled; }, delegate { ImportScenarioFileHandler(); })); }
        }

        SimpleCommand<object, object> _importScenarioFile;

        bool IsImportScenarioFileCommandEnabled
        {
            get { return true; }
        }

        void ImportScenarioFileHandler()
        {
            var vm = new ImportScenarioFileViewModel(Database, _cache, _plugins);
            var result = _visualizer.ShowDialog("ImportScenarioFileView", vm);
            if (result.HasValue && result.Value) Scenario = vm.Scenario;
        }
        #endregion
        

        readonly List<Tuple<IHaveProperties, Window>> _openPropertyWindows = new List<Tuple<IHaveProperties, Window>>();
        [MediatorMessageSink(MediatorMessage.ShowProperties)]
        public void ShowProperties(IHaveProperties propertyViewModel)
        {
            var target = _openPropertyWindows.Find(property => property.Item1 == propertyViewModel);
            if (target == null)
            {
                var window = _visualizer.ShowWindow(propertyViewModel.PropertyViewName, propertyViewModel, true, (s, e) => _openPropertyWindows.Remove(_openPropertyWindows.Find(property => property.Item1 == (IHaveProperties)e.State)));
                _openPropertyWindows.Add(new Tuple<IHaveProperties, Window>(propertyViewModel, window));
            }
            else
            {
                target.Item2.Activate();
            }
        }

        [MediatorMessageSink(MediatorMessage.PlaceAnalysisPoint)]
        public void PlaceAnalysisPoint(bool dummy)
        {
            if (MouseDepth > 0) throw new AnalysisPointLocationException("Analysis Points cannot be placed on land.");
            if (ScenarioMetadata == null) return;
            ScenarioMetadata.PlaceAnalysisPoint(MouseGeo);
        }

        [MediatorMessageSink(MediatorMessage.EditAnalysisPoint)]
        public void EditAnalysisPoint(AnalysisPoint analysisPoint)
        {
            var analysisPointPropertiesViewModel = new AnalysisPointPropertiesViewModel(analysisPoint);
            var settingsResult = _visualizer.ShowDialog("AnalysisPointPropertiesView", analysisPointPropertiesViewModel);
            if (settingsResult.HasValue && settingsResult.Value)
            {
                ScenarioMetadata.CurrentMapLayers.DisplayAnalysisPoint(analysisPoint);
                MediatorMessage.Send(MediatorMessage.RefreshMap, true);
            }
        }

        [MediatorMessageSink(MediatorMessage.RemoveAnalysisPoint)]
        public void RemoveAnalysisPoint(AnalysisPoint analysisPoint)
        {
            ScenarioMetadata.AnalysisPoints.Remove(analysisPoint);
            ScenarioMetadata.Save();
        }

        [MediatorMessageSink(MediatorMessage.ViewPropagation)]
        public void ViewPropagation(CASSOutput cassOutput)
        {
            var propagationViewModel = new PropagationViewModel(cassOutput,_saveFile,_openFile,_messageBox,_visualizer);
            _visualizer.ShowDialog("PropagationView", propagationViewModel);
        }
    }
}