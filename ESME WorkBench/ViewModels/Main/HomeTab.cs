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
using ESMEWorkbench.ViewModels.Map;
using HRC.Aspects;
using AnalysisPoint = ESME.TransmissionLoss.AnalysisPoint;

namespace ESMEWorkbench.ViewModels.Main
{
    public partial class MainViewModel
    {
        public MapLayerCollection ScenarioMapLayers { get; set; }

        [Initialize] public LayerTreeViewModel LayerTreeViewModel { get; set; }
        public MapViewModel MapViewModel { get; set; }

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