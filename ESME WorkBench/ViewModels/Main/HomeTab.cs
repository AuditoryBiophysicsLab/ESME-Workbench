using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Text;
using System.Windows;
using System.Windows.Input;
using Cinch;
using ESME;
using ESME.Environment.Descriptors;
using ESME.Mapping;
using ESME.Metadata;
using ESME.Model;
using ESME.NEMO;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.CASS;
using ESME.Views.AcousticBuilder;
using ESME.Views.TransmissionLoss;
using ESMEWorkBench.Properties;
using ESMEWorkBench.ViewModels.NAVO;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        public MapLayerCollection ScenarioMapLayers { get; set; }

        #region LoadEnvironmentCommand
        public SimpleCommand<object, object> LoadEnvironmentCommand
        {
            get { return _loadEnvironment ?? (_loadEnvironment = new SimpleCommand<object, object>(delegate { return IsLoadEnvironmentCommandEnabled; }, delegate { LoadEnvironmentHandler(); })); }
        }

        SimpleCommand<object, object> _loadEnvironment;

        bool IsLoadEnvironmentCommandEnabled
        {
            get { return true; }
        }

        void LoadEnvironmentHandler()
        {
            try
            {
                if (RangeComplexes.SelectedBathymetry != BathymetryFile.None) RangeComplexes.SelectedBathymetry.GetMyDataAsync();
                if (RangeComplexes.SelectedWind != WindFile.None) RangeComplexes.SelectedWind.GetMyDataAsync();
                if (RangeComplexes.SelectedBottomLoss != BottomLossFile.None) RangeComplexes.SelectedBottomLoss.GetMyDataAsync();
                if (RangeComplexes.SelectedSediment != SedimentFile.None) RangeComplexes.SelectedSediment.GetMyDataAsync();
                if (RangeComplexes.SelectedSoundSpeed != SoundSpeedFile.None) RangeComplexes.SelectedSoundSpeed.GetMyDataAsync();
            }
            catch (Exception e)
            {
                _messageBoxService.ShowError(e.Message);
            }
        }
        #endregion

        #region ClearEnvironmentCommand
        public SimpleCommand<object, object> ClearEnvironmentCommand
        {
            get { return _clearEnvironment ?? (_clearEnvironment = new SimpleCommand<object, object>(delegate { return IsClearEnvironmentCommandEnabled; }, delegate { ClearEnvironmentHandler(); })); }
        }

        SimpleCommand<object, object> _clearEnvironment;

        bool IsClearEnvironmentCommandEnabled
        {
            get { return true; }
        }

        void ClearEnvironmentHandler()
        {
            if (RangeComplexes.SelectedBathymetry != BathymetryFile.None) RangeComplexes.SelectedBathymetry.Reset();
            if (RangeComplexes.SelectedWind != WindFile.None) RangeComplexes.SelectedWind.Reset();
            if (RangeComplexes.SelectedBottomLoss != BottomLossFile.None) RangeComplexes.SelectedBottomLoss.Reset();
            if (RangeComplexes.SelectedSediment != SedimentFile.None) RangeComplexes.SelectedSediment.Reset();
            if (RangeComplexes.SelectedSoundSpeed != SoundSpeedFile.None) RangeComplexes.SelectedSoundSpeed.Reset();
        }
        #endregion

        #region public NemoFile NemoFile { get; set; }

        public NemoFile NemoFile
        {
            get { return _nemoFile; }
            set
            {
                if (_nemoFile == value) return;
                _nemoFile = value;
                NotifyPropertyChanged(NemoFileChangedEventArgs);
                NotifyPropertyChanged(IsScenarioLoadedChangedEventArgs);

                if (_nemoFile != null)
                {
                    MainWindowTitle = string.Format("ESME WorkBench 2011{0}: {1} [{2}]", Configuration.IsUnclassifiedModel ? " (public)" : "", NemoFile.Scenario.EventName, NemoFile.Scenario.TimeFrame);
                }
                else
                {
                    MainWindowTitle = string.Format("ESME WorkBench 2011{0}: <No scenario loaded>", Configuration.IsUnclassifiedModel ? " (public)" : "");
                }
            }
        }

        public bool IsScenarioLoaded { get { return NemoFile != null; } }
        static readonly PropertyChangedEventArgs IsScenarioLoadedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsScenarioLoaded);

        static readonly PropertyChangedEventArgs NemoFileChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.NemoFile);
        NemoFile _nemoFile;

        #endregion

        #region public NAEMOScenarioMetadata ScenarioMetadata { get; set; }
#if false
        public NAEMOScenarioMetadata ScenarioMetadata
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
                        if (e.PropertyName == "CanPlaceAnalysisPoint") _dispatcher.InvokeIfRequired(() =>  NotifyPropertyChanged(CanPlaceAnalysisPointChangedEventArgs));
                    };
                    _scenarioMetadata.Dispatcher = _dispatcher;
                    _scenarioMetadata.VisualizerService = _visualizerService;
                    _dispatcher.InvokeIfRequired(() =>
                    {
                        MapLayerCollections.Add("Scenario", Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"Sample GIS Data\Countries02.shp"));
                        ScenarioMapLayers = MapLayerCollections["Scenario"];
                        _scenarioMetadata.MapLayers = ScenarioMapLayers;
                        MapLayerCollections.ActiveLayer = ScenarioMapLayers;
                    });
                }
                else
                {
                    _dispatcher.InvokeIfRequired(() =>
                    {
                        MapLayerCollections.ActiveLayer = MapLayerCollections["Home"];
                        if (ScenarioMapLayers != null) MapLayerCollections.Remove(ScenarioMapLayers);
                    });
                }
                IsScenarioLoaded = _scenarioMetadata != null;
            }
        }
#endif

        public NAEMOScenarioMetadata ScenarioMetadata
        {
            get { return _scenarioMetadata; }
            set
            {
                if (_scenarioMetadata == value) return;
                _scenarioMetadata = value;
                NotifyPropertyChanged(ScenarioMetadataChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ScenarioMetadataChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.ScenarioMetadata);
        NAEMOScenarioMetadata _scenarioMetadata;

        #endregion

        #region OpenScenarioCommand
        public SimpleCommand<object, object> OpenScenarioCommand
        {
            get { return _openScenario ?? (_openScenario = new SimpleCommand<object, object>(delegate { OpenScenarioHandler(null); })); }
        }

        SimpleCommand<object, object> _openScenario;

        void OpenScenarioHandler(string fileName)
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
                Settings.Default.LastScenarioFileDirectory = Path.GetDirectoryName(_openFileService.FileName);
            }
            RecentFiles.InsertFile(fileName);
            try
            {
                NemoFile = new NemoFile(fileName, Globals.AppSettings.ScenarioDataDirectory);
                var metadataFileName = Path.Combine(Path.GetDirectoryName(fileName), Path.GetFileNameWithoutExtension(fileName) + ".emf");
                if (File.Exists(metadataFileName)) ScenarioMetadata = NAEMOScenarioMetadata.Load(metadataFileName);
                else ScenarioMetadata = new NAEMOScenarioMetadata
                {
                    Filename = metadataFileName,
                    NemoFileName = fileName,
                };
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
                _messageBoxService.ShowError("Error opening scenario \"" + Path.GetFileName(fileName) + "\":\n" + sb);
                //ScenarioMetadata = null;
            }
        }
        #endregion

        #region CloseScenarioCommand
        public SimpleCommand<object, object> CloseScenarioCommand
        {
            get { return _closeScenario ?? (_closeScenario = new SimpleCommand<object, object>(
                delegate { return IsScenarioLoaded; }, 
                delegate
                {
                    NemoFile = null;
                    ScenarioMetadata = null;
                })); }
        }

        SimpleCommand<object, object> _closeScenario;
        #endregion

        #region ConfigureAcousticModelsCommand
        public SimpleCommand<object, object> ConfigureAcousticModelsCommand
        {
            get { return _configureAcousticModels ?? (_configureAcousticModels = new SimpleCommand<object, object>(
                delegate { ConfigureAcousticModelsHandler(); })); }
        }

        SimpleCommand<object, object> _configureAcousticModels;

        void ConfigureAcousticModelsHandler()
        {
            var modeAcousticModelSelectionViewModel = new ModeAcousticModelSelectionViewModel(ScenarioMetadata.NemoModeToAcousticModelNameMap, ESME.Globals.ValidTransmissionLossAlgorithms);
            _visualizerService.ShowDialog("ModeAcousticModelSelectionView", modeAcousticModelSelectionViewModel);
        }

        #endregion

        #region ExportAnalysisPointsCommand
        public SimpleCommand<object, object> ExportAnalysisPointsCommand
        {
            get
            {
                return _exportAnalysisPoints ?? (_exportAnalysisPoints = new SimpleCommand<object, object>(
                    delegate { return ScenarioMetadata != null && RangeComplexes.IsEnvironmentFullySpecified; },
                    delegate { ExportAnalysisPointsHandler(); }));
            }
        }

        SimpleCommand<object, object> _exportAnalysisPoints;

        void ExportAnalysisPointsHandler()
        {
            throw new NotImplementedException();
            //ScenarioMetadata.ExportAnalysisPoints();
        }
        #endregion

        #region RunScenarioCommand
        public SimpleCommand<object, object> RunScenarioCommand
        {
            get
            {
                return _runScenario ?? (_runScenario = new SimpleCommand<object, object>(
                    delegate { return IsScenarioLoaded; },
                    delegate
                    {
                        var vm = new ScenarioSimulatorOptionsViewModel
                        {
                            ScenarioSimulatorSettings = Globals.AppSettings.ScenarioSimulatorSettings,
                            NemoFile = NemoFile,
                        };

                        _visualizerService.Show("ScenarioSimulatorOptionsView", vm, true, null);
                    }));
            }
        }

        SimpleCommand<object, object> _runScenario;
        #endregion

        #region RunScenarioGUICommand
        public SimpleCommand<object, object> RunScenarioGUICommand
        {
            get
            {
                return _runScenarioGUI ?? (_runScenarioGUI = new SimpleCommand<object, object>(
                    delegate { return IsRunScenarioGUICommandEnabled; },
                    delegate { RunScenarioGUIHandler(); }));
            }
        }

        SimpleCommand<object, object> _runScenarioGUI;

        bool IsRunScenarioGUICommandEnabled
        {
            get { return ((Globals.AppSettings.NAEMOTools.ScenarioExecutablePath != null)
                        && File.Exists(Globals.AppSettings.NAEMOTools.ScenarioExecutablePath)
                        && (Globals.AppSettings.NAEMOTools.JavaExecutablePath != null)
                        && File.Exists(Globals.AppSettings.NAEMOTools.JavaExecutablePath) && IsScenarioLoaded); }
        }

        void RunScenarioGUIHandler()
        {
            var commandArgs = CommandArgs;
            new Process
            {
                StartInfo =
                {
                    WorkingDirectory = Path.GetDirectoryName(Globals.AppSettings.NAEMOTools.ScenarioExecutablePath),
                    FileName = Globals.AppSettings.NAEMOTools.JavaExecutablePath,
                    Arguments = commandArgs,
                    
                },
            }.Start();
        }
        string CommandArgs
        {
            get
            {
                var sb = new StringBuilder();
                sb.Append(string.Format("-jar \"{0}\" ", Globals.AppSettings.NAEMOTools.ScenarioExecutablePath));
                sb.Append(string.Format("-s \"{0}\" ", NemoFile));
                return sb.ToString();
            }
        }
        #endregion

        #region public bool CanPlaceAnalysisPoint { get; set; }

        public bool CanPlaceAnalysisPoint { get { return ScenarioMetadata != null && NemoFile != null && RangeComplexes.IsEnvironmentFullySpecified; } }

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
        string _mainWindowTitle = Configuration.IsUnclassifiedModel ? "ESME WorkBench 2011 (public): <No scenario loaded>" : "ESME WorkBench 2011: <No scenario loaded>";

        #endregion

        #region NewScenarioCommand
        public SimpleCommand<object, object> NewScenarioCommand
        {
            get { return _newScenario ?? (_newScenario = new SimpleCommand<object, object>(delegate { return IsNewScenarioCommandEnabled; }, delegate { NewScenarioHandler(); })); }
        }

        SimpleCommand<object, object> _newScenario;

        bool IsNewScenarioCommandEnabled
        {
            get { return true; }
        }

        void NewScenarioHandler() { }
        #endregion

        readonly List<Tuple<IHaveProperties, Window>> _openPropertyWindows = new List<Tuple<IHaveProperties, Window>>();
        [MediatorMessageSink(MediatorMessage.ShowProperties)]
        public void ShowProperties(IHaveProperties propertyViewModel)
        {
            var target = _openPropertyWindows.Find(property => property.Item1 == propertyViewModel);
            if (target == null)
            {
                var window = _visualizerService.ShowWindow(propertyViewModel.PropertyViewName, propertyViewModel, true, (s, e) => _openPropertyWindows.Remove(_openPropertyWindows.Find(property => property.Item1 == (IHaveProperties)e.State)));
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
            //ScenarioMetadata.PlaceAnalysisPoint(MouseEarthCoordinate);
            throw new NotImplementedException();
        }

        [MediatorMessageSink(MediatorMessage.EditAnalysisPoint)]
        public void EditAnalysisPoint(AnalysisPoint analysisPoint)
        {
            var analysisPointPropertiesViewModel = new AnalysisPointPropertiesViewModel(analysisPoint);
            var settingsResult = _visualizerService.ShowDialog("AnalysisPointPropertiesView", analysisPointPropertiesViewModel);
            if (settingsResult.HasValue && settingsResult.Value)
            {
                //ScenarioMetadata.MapLayers.DisplayAnalysisPoint(analysisPoint);
                MediatorMessage.Send(MediatorMessage.RefreshMap, true);
            }
            analysisPoint.Validate();
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
        }
    }
}
