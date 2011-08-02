using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Windows;
using System.Windows.Media;
using Cinch;
using ESME;
using ESME.Mapping;
using ESME.NEMO;
using ESME.NEMO.Overlay;
using ESMEWorkBench.Properties;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Map;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        public MapLayerCollection HomeTabMapLayers { get; set; }

        #region public NemoFile Scenario { get; set; }

        public NemoFile Scenario
        {
            get { return _scenario; }
            set
            {
                if (_scenario == value) return;
                if (_scenario != null) ClearScenario(_scenario);
                
                _scenario = value;

                _rangeComplexPath = Path.Combine(ESME.Globals.AppSettings.ScenarioDataDirectory, _scenario.Scenario.SimAreaName);
                _areasPath = Path.Combine(_rangeComplexPath, "Areas");
                _bathymetryPath = Path.Combine(_rangeComplexPath, "Bathymetry");
                _environmentPath = Path.Combine(_rangeComplexPath, "Environment");
                _imagesPath = Path.Combine(_rangeComplexPath, "Images");

                if (_scenario != null) DisplayScenario(_scenario);
                NotifyPropertyChanged(ScenarioChangedEventArgs);
                IsScenarioLoaded = _scenario != null;
            }
        }

        static readonly PropertyChangedEventArgs ScenarioChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.Scenario);
        NemoFile _scenario;
        string _rangeComplexPath;
        string _areasPath;
        string _bathymetryPath;
        string _environmentPath;
        string _imagesPath;

        void ClearScenario(NemoFile scenario)
        {
            foreach (var platform in scenario.Scenario.Platforms)
            {
                if (platform.Trackdefs.Count == 1) HomeTabMapLayers.Remove(string.Format("{0} OpArea", platform.Name));
                else
                    for (var trackIndex = 0; trackIndex < platform.Trackdefs.Count; trackIndex++)
                        HomeTabMapLayers.Remove(string.Format("{0} OpArea{1}", platform.Name, trackIndex + 1));
                HomeTabMapLayers.Remove(string.Format("{0} track", platform.Name));
            }
        }

        void DisplayScenario(NemoFile scenario)
        {
            foreach (var platform in scenario.Scenario.Platforms)
            {
                if (platform.Trackdefs.Count == 1)
                {
                    HomeTabMapLayers.DisplayOverlayShapes(string.Format("{0} OpArea", platform.Name), LayerType.OpArea, Colors.Transparent, platform.Trackdefs[0].OverlayFile.Shapes);
                    platform.CalculateBehavior();
                    if (platform.BehaviorModel != null && platform.BehaviorModel.CourseOverlay != null)
                    {
                        var track = HomeTabMapLayers.DisplayOverlayShapes(string.Format("{0} track", platform.Name), LayerType.Track, Colors.Transparent,
                                                              new List<OverlayShape> {platform.BehaviorModel.CourseOverlay}, 1, PointSymbolType.Circle, true, new CustomStartEndLineStyle(PointSymbolType.Circle, Colors.Green, 5, PointSymbolType.Square, Colors.Red, 5, Colors.DarkGray, 1));
                    }
                }
                else
                    for (var trackIndex = 0; trackIndex < platform.Trackdefs.Count; trackIndex++)
                    {
                        HomeTabMapLayers.DisplayOverlayShapes(string.Format("{0} OpArea{1}", platform.Name, trackIndex + 1), LayerType.OpArea, Colors.Transparent,
                                                              platform.Trackdefs[0].OverlayFile.Shapes);
                    }
            }
        }

        #endregion

        #region public string ScenarioFilename { get; set; }

        public string ScenarioFilename
        {
            get { return _scenarioFilename; }
            set
            {
                if (_scenarioFilename == value) return;
                _scenarioFilename = value;
                NotifyPropertyChanged(ScenarioFilenameChangedEventArgs);
                Scenario = _scenarioFilename != null ? new NemoFile(_scenarioFilename, ESME.Globals.AppSettings.ScenarioDataDirectory) : null;
            }
        }

        static readonly PropertyChangedEventArgs ScenarioFilenameChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.ScenarioFilename);
        string _scenarioFilename;

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
            ScenarioFilename = fileName;
        }
        #endregion

        #region CloseScenarioCommand
        public SimpleCommand<object, object> CloseScenarioCommand
        {
            get { return _closeScenario ?? (_closeScenario = new SimpleCommand<object, object>(delegate { return IsScenarioLoaded; }, delegate { CloseScenarioHandler(); })); }
        }

        SimpleCommand<object, object> _closeScenario;

        void CloseScenarioHandler() { ScenarioFilename = null; }
        #endregion

    }
}
