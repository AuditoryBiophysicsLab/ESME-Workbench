using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Media;
using System.Windows.Threading;
using Cinch;
using ESME;
using ESME.Environment.Descriptors;
using ESME.Environment.NAVO;
using ESME.Mapping;
using ESME.NEMO;
using ESME.NEMO.Overlay;
using ESMEWorkBench.Properties;
using ESMEWorkBench.ViewModels.Layers;
using HRC.Navigation;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        public MapLayerCollection HomeTabMapLayers { get; set; }

        #region public NemoFile NemoFile { get; set; }

        public NemoFile NemoFile
        {
            get { return _nemoFile; }
            set
            {
                if (_nemoFile == value) return;
                if (_nemoFile != null) ClearScenario(_nemoFile);
                
                _nemoFile = value;
                if (_nemoFile != null)
                {
                    _rangeComplexPath = Path.Combine(ESME.Globals.AppSettings.ScenarioDataDirectory, _nemoFile.Scenario.SimAreaName);
                    _areasPath = Path.Combine(_rangeComplexPath, "Areas");
                    _bathymetryPath = Path.Combine(_rangeComplexPath, "Bathymetry");
                    _environmentPath = Path.Combine(_rangeComplexPath, "Environment");
                    _imagesPath = Path.Combine(_rangeComplexPath, "Images");
                    DisplayScenario(_nemoFile);
                    _rangeComplexDescriptor = (RangeComplexDescriptor)RangeComplexDescriptors[_nemoFile.Scenario.SimAreaName];
                    var curTimePeriod = (NAVOTimePeriod)Enum.Parse(typeof (NAVOTimePeriod), _nemoFile.Scenario.TimeFrame);
                    AvailableEnvironments = new NAEMOEnvironmentDescriptors();
                    AvailableEnvironments.AddRange(from environment in _rangeComplexDescriptor.NAEMOEnvironmentDescriptors
                                                   where (environment.Value != null) && (environment.Value.Metadata.TimePeriod == curTimePeriod) && (!string.IsNullOrEmpty(environment.Value.Metadata.BathymetryName))
                                                   select environment);
                }
                else
                {
                    _rangeComplexPath = _areasPath = _bathymetryPath = _environmentPath = _imagesPath = null;
                    _rangeComplexDescriptor = null;
                    _scenarioBounds = null;
                    AvailableEnvironments = null;
                }
                    
                NotifyPropertyChanged(NemoFileChangedEventArgs);
                IsScenarioLoaded = _nemoFile != null;
            }
        }

        static readonly PropertyChangedEventArgs NemoFileChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.NemoFile);
        NemoFile _nemoFile;
        string _rangeComplexPath;
        string _areasPath;
        string _bathymetryPath;
        string _environmentPath;
        string _imagesPath;
        RangeComplexDescriptor _rangeComplexDescriptor;
        GeoRect _scenarioBounds;

        void ClearScenario(NemoFile nemoFile)
        {
            var layersToDelete = from layer in HomeTabMapLayers
                                 where layer.LayerType != LayerType.BaseMap
                                 select layer;
            foreach (var layer in layersToDelete) HomeTabMapLayers.Remove(layer);
        }

        void DisplayScenario(NemoFile nemoFile)
        {
            if (nemoFile.Scenario.OverlayFile != null) HomeTabMapLayers.DisplayOverlayShapes(string.Format("{0} sim area", nemoFile.Scenario.SimAreaName), LayerType.SimArea, Colors.Transparent, nemoFile.Scenario.OverlayFile.Shapes);
            foreach (var platform in nemoFile.Scenario.Platforms)
            {
                if (platform.Trackdefs.Count == 1)
                {
                    HomeTabMapLayers.DisplayOverlayShapes(string.Format("Platform: {0} op area", platform.Name), LayerType.OpArea, Colors.Transparent, platform.Trackdefs[0].OverlayFile.Shapes);
                    if (_scenarioBounds == null) _scenarioBounds = new GeoRect(platform.Trackdefs[0].OverlayFile.Shapes[0].BoundingBox);
                    else _scenarioBounds.Union(platform.Trackdefs[0].OverlayFile.Shapes[0].BoundingBox);
                    platform.CalculateBehavior();
                    if (platform.BehaviorModel != null && platform.BehaviorModel.CourseOverlay != null)
                        HomeTabMapLayers.DisplayOverlayShapes(string.Format("Platform: {0} track", platform.Name), LayerType.Track, Colors.Transparent,
                                                              new List<OverlayShape> {platform.BehaviorModel.CourseOverlay}, 1, PointSymbolType.Circle, true, new CustomStartEndLineStyle(PointSymbolType.Circle, Colors.Green, 5, PointSymbolType.Square, Colors.Red, 5, Colors.DarkGray, 1));
                }
                else
                    for (var trackIndex = 0; trackIndex < platform.Trackdefs.Count; trackIndex++)
                    {
                        HomeTabMapLayers.DisplayOverlayShapes(string.Format("Platform: {0} OpArea{1}", platform.Name, trackIndex + 1), LayerType.OpArea, Colors.Transparent,
                                                              platform.Trackdefs[0].OverlayFile.Shapes);
                        if (_scenarioBounds == null) _scenarioBounds = new GeoRect(platform.Trackdefs[trackIndex].OverlayFile.Shapes[0].BoundingBox);
                        else _scenarioBounds.Union(platform.Trackdefs[trackIndex].OverlayFile.Shapes[0].BoundingBox);
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
                NemoFile = _scenarioFilename != null ? new NemoFile(_scenarioFilename, ESME.Globals.AppSettings.ScenarioDataDirectory) : null;
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

        #region public NAEMOEnvironmentDescriptors AvailableEnvironments { get; set; }

        public NAEMOEnvironmentDescriptors AvailableEnvironments
        {
            get { return _availableEnvironments; }
            set
            {
                if (_availableEnvironments == value) return;
                _availableEnvironments = value;
                NotifyPropertyChanged(AvailableEnvironmentsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AvailableEnvironmentsChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.AvailableEnvironments);
        NAEMOEnvironmentDescriptors _availableEnvironments;

        #endregion

        #region public NAEMOEnvironmentDescriptor SelectedEnvironment { get; set; }

        public NAEMOEnvironmentDescriptor SelectedEnvironment
        {
            get { return _selectedEnvironment; }
            set
            {
                if (_selectedEnvironment == value) return;
                _selectedEnvironment = value;
                NotifyPropertyChanged(SelectedEnvironmentChangedEventArgs);
                Task.Factory.StartNew(() => _dispatcher.InvokeIfRequired(DisplaySelectedEnvironment, DispatcherPriority.Normal));
            }
        }

        static readonly PropertyChangedEventArgs SelectedEnvironmentChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedEnvironment);
        NAEMOEnvironmentDescriptor _selectedEnvironment;

        void DisplaySelectedEnvironment()
        {
            if ((!_allViewModelsAreReady) || (!_viewIsActivated) || (_selectedEnvironment == null)) return;
            var samplePoints = _selectedEnvironment.Data.Locations.Select(samplePoint => new OverlayPoint(samplePoint));
            //_selectedEnvironment.Metadata.BathymetryName
            var bathyBitmapLayer = HomeTabMapLayers.DisplayBathymetryRaster("Bathymetry", Path.Combine(_imagesPath, _selectedEnvironment.Metadata.BathymetryName + ".bmp"), true, false, true, ((NAEMOBathymetryDescriptor)(_rangeComplexDescriptor.NAEMOBathymetryDescriptors[ _selectedEnvironment.Metadata.BathymetryName])).Metadata.Bounds);
            //MediatorMessage.Send(MediatorMessage.MoveLayerToBottom, bathyBitmapLayer);
            HomeTabMapLayers.DisplayOverlayShapes("Sound Speed", LayerType.SoundSpeed, Colors.Transparent, samplePoints, 0, PointSymbolType.Circle, false, null, false);
            HomeTabMapLayers.DisplayOverlayShapes("Wind", LayerType.WindSpeed, Colors.Transparent, samplePoints, 0, PointSymbolType.Diamond, false, null, false);
            foreach (var sedimentType in _selectedEnvironment.Data.SedimentTypes)
            {
                samplePoints = sedimentType.Value.Select(samplePoint => new OverlayPoint(samplePoint));
                HomeTabMapLayers.DisplayOverlayShapes(string.Format("Sediment: {0}", sedimentType.Key.ToLower()), LayerType.BottomType, Colors.Transparent, samplePoints, 0, PointSymbolType.Diamond, false, null, false);
            }
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }

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

        #region ZoomToScenarioCommand
        public SimpleCommand<object, object> ZoomToScenarioCommand
        {
            get { return _zoomToScenario ?? (_zoomToScenario = new SimpleCommand<object, object>(delegate { return IsScenarioLoaded; }, delegate { ZoomToScenarioHandler(); })); }
        }

        SimpleCommand<object, object> _zoomToScenario;

        void ZoomToScenarioHandler()
        {
            var mapExtent = new RectangleShape(_scenarioBounds.West, _scenarioBounds.North, _scenarioBounds.East, _scenarioBounds.South);
            MediatorMessage.Send(MediatorMessage.SetCurrentExtent, mapExtent);
        }
        #endregion

        #region EditScenarioCommand
        public SimpleCommand<object, object> EditScenarioCommand
        {
            get { return _editScenario ?? (_editScenario = new SimpleCommand<object, object>(delegate { return IsScenarioLoaded; }, delegate { EditScenarioHandler(); })); }
        }

        SimpleCommand<object, object> _editScenario;

        void EditScenarioHandler()
        {
            string arguments;

            if ((ScenarioFilename == null) || (!File.Exists(ScenarioFilename))) arguments = null;
            else arguments = "\"" + ScenarioFilename + "\"";
            new Process
            {
                StartInfo =
                        {
                            FileName = Globals.AppSettings.NAEMOTools.ScenarioEditorExecutablePath,
                            WorkingDirectory = Path.GetDirectoryName(Globals.AppSettings.NAEMOTools.ScenarioEditorExecutablePath),
                            Arguments = arguments,
                        }
            }.Start();
        }
        #endregion
    }
}
