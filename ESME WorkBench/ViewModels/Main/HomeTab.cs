﻿using System;
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
using ESME.Mapping;
using ESME.Metadata;
using ESME.Model;
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

        #region public NAEMOScenarioMetadata ScenarioMetadata { get; set; }

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
                    _scenarioMetadata.RangeComplexDescriptors = RangeComplexDescriptors;
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

        static readonly PropertyChangedEventArgs ScenarioMetadataChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.ScenarioMetadata);
        NAEMOScenarioMetadata _scenarioMetadata;

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
                if (!_isScenarioLoaded) MainWindowTitle = "ESME WorkBench 2011: <No scenario loaded>";
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
            ScenarioMetadata = null;
            RecentFiles.InsertFile(fileName);
            //NemoScenario.Test(fileName);
            try
            {
                ScenarioMetadata = NAEMOScenarioMetadata.Load(NAEMOMetadataBase.MetadataFilename(fileName)) ?? new NAEMOScenarioMetadata { Filename = NAEMOMetadataBase.MetadataFilename(fileName) };
                _dispatcher.InvokeIfRequired(() =>
                {
                    ScenarioMetadata.ScenarioFilename = fileName;
                    MainWindowTitle = string.Format("ESME WorkBench 2011{0}: {1} [{2}]", Configuration.IsUnclassifiedModel ? " (public)" : "", ScenarioMetadata.NemoFile.Scenario.EventName, ScenarioMetadata.NemoFile.Scenario.TimeFrame);
                });
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
                ScenarioMetadata = null;
            }
        }
        #endregion

        #region CloseScenarioCommand
        public SimpleCommand<object, object> CloseScenarioCommand
        {
            get { return _closeScenario ?? (_closeScenario = new SimpleCommand<object, object>(delegate { return IsScenarioLoaded; }, delegate { CloseScenarioHandler(); })); }
        }

        SimpleCommand<object, object> _closeScenario;

        void CloseScenarioHandler()
        {
            ScenarioMetadata = null;
        }
        #endregion

        #region ConfigureAcousticModelsCommand
        public SimpleCommand<object, object> ConfigureAcousticModelsCommand
        {
            get { return _configureAcousticModels ?? (_configureAcousticModels = new SimpleCommand<object, object>(delegate { ConfigureAcousticModelsHandler(); })); }
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
                return _exportAnalysisPoints ??
                       (_exportAnalysisPoints =
                        new SimpleCommand<object, object>(delegate { return ScenarioMetadata != null && ScenarioMetadata.SelectedEnvironment != null; },
                                                          delegate { ExportAnalysisPointsHandler(); }));
            }
        }

        SimpleCommand<object, object> _exportAnalysisPoints;

        void ExportAnalysisPointsHandler() { ScenarioMetadata.ExportAnalysisPoints(); }
        #endregion

        #region RunScenarioCommand
        public SimpleCommand<object, object> RunScenarioCommand
        {
            get
            {
                return _runScenario ??
                       (_runScenario =
                        new SimpleCommand<object, object>(delegate { return IsRunScenarioCommandEnabled; },
                                                          delegate { RunScenarioHandler(); }));
            }
        }

        SimpleCommand<object, object> _runScenario;

        bool IsRunScenarioCommandEnabled
        {
            get
            {
                return IsScenarioLoaded;
            }
        }

        void RunScenarioHandler()
        {
            var vm = new ScenarioSimulatorOptionsViewModel
            {
                    ScenarioSimulatorSettings = Globals.AppSettings.ScenarioSimulatorSettings,
                    NemoFile = ScenarioMetadata.NemoFile,
            };

            _visualizerService.Show("ScenarioSimulatorOptionsView", vm, true, null);

        }
        #endregion

        #region RunScenarioGUICommand
        public SimpleCommand<object, object> RunScenarioGUICommand
        {
            get
            {
                return _runScenarioGUI ??
                       (_runScenarioGUI =
                        new SimpleCommand<object, object>(delegate { return IsRunScenarioGUICommandEnabled; },
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
                sb.Append(string.Format("-s \"{0}\" ", ScenarioMetadata.NemoFile));
                return sb.ToString();
            }
        }
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
        string _mainWindowTitle = Configuration.IsUnclassifiedModel ? "ESME WorkBench 2011 (public): <No scenario loaded>" : "ESME WorkBench 2011: <No scenario loaded>";

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
            ScenarioMetadata.PlaceAnalysisPoint(MouseEarthCoordinate);
        }

        [MediatorMessageSink(MediatorMessage.EditAnalysisPoint)]
        public void EditAnalysisPoint(AnalysisPoint analysisPoint)
        {
            var analysisPointPropertiesViewModel = new AnalysisPointPropertiesViewModel(analysisPoint);
            var settingsResult = _visualizerService.ShowDialog("AnalysisPointPropertiesView", analysisPointPropertiesViewModel);
            if (settingsResult.HasValue && settingsResult.Value)
            {
                ScenarioMetadata.MapLayers.DisplayAnalysisPoint(analysisPoint);
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
