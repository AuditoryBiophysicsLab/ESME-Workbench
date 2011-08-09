using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Threading;
using System.Xml.Serialization;
using Cinch;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Environment.NAVO;
using ESME.Mapping;
using ESME.Model;
using ESME.NEMO;
using ESME.NEMO.Overlay;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.CASS;
using HRC.Navigation;
using HRC.Utility;
using ThinkGeo.MapSuite.Core;

namespace ESME.Metadata
{
    public class NAEMOScenarioMetadata : NAEMOMetadataBase
    {
        new internal static readonly List<Type> ReferencedTypes = new List<Type>(NAEMOMetadataBase.ReferencedTypes) {typeof(NemoModeToAcousticModelNameMap)};

        public static NAEMOScenarioMetadata Load(string metaDataFilename)
        {
            var result = Load<NAEMOScenarioMetadata>(metaDataFilename);
            // Any other initialization code goes here
            return result;
        }

        public void Save(string filename = null) { Save(this, ReferencedTypes, filename); }

        #region Properties that MUST be initialized before setting the ScenarioFilename property
        [XmlIgnore]
        public MapLayerCollection MapLayers { get; set; }
        [XmlIgnore]
        public Dispatcher Dispatcher { get; set; }
        [XmlIgnore]
        public RangeComplexDescriptors RangeComplexDescriptors { get; set; }
        [XmlIgnore]
        public IUIVisualizerService VisualizerService { get; set; }
        #endregion

        #region public NemoFile NemoFile { get; set; }
        [XmlIgnore]
        public NemoFile NemoFile
        {
            get { return _nemoFile; }
            set
            {
                if (_nemoFile == value) return;
                _nemoFile = value;
                if (_nemoFile != null)
                {
                    _rangeComplexPath = Path.Combine(Globals.AppSettings.ScenarioDataDirectory,
                                                     _nemoFile.Scenario.SimAreaName);
                    _areasPath = Path.Combine(_rangeComplexPath, "Areas");
                    _bathymetryPath = Path.Combine(_rangeComplexPath, "Bathymetry");
                    _environmentPath = Path.Combine(_rangeComplexPath, "Environment");
                    _imagesPath = Path.Combine(_rangeComplexPath, "Images");
                    _scenarioPath = Path.GetDirectoryName(_nemoFile.FileName);
                    _propagationPath = Path.Combine(_scenarioPath, "Propagation");
                    _pressurePath = Path.Combine(_scenarioPath, "Pressure");
                    DisplayScenario();
                    _rangeComplexDescriptor = (RangeComplexDescriptor)RangeComplexDescriptors[_nemoFile.Scenario.SimAreaName];
                    var curTimePeriod = (NAVOTimePeriod)Enum.Parse(typeof (NAVOTimePeriod), _nemoFile.Scenario.TimeFrame);
                    AvailableEnvironments = new NAEMOEnvironmentDescriptors();
                    AvailableEnvironments.AddRange(from environment in _rangeComplexDescriptor.NAEMOEnvironmentDescriptors
                                                   where (environment.Value != null) && 
                                                         (environment.Value.Metadata.TimePeriod == curTimePeriod) && 
                                                         (!string.IsNullOrEmpty(environment.Value.Metadata.BathymetryName))
                                                   select environment);
                    if (EnvironmentName != null) SelectedEnvironment = (NAEMOEnvironmentDescriptor)AvailableEnvironments[EnvironmentName];
                    if (NemoModeToAcousticModelNameMap == null) NemoModeToAcousticModelNameMap = new NemoModeToAcousticModelNameMap(_nemoFile.Scenario.DistinctModePSMNames, TransmissionLossAlgorithm.CASS);
                    else NemoModeToAcousticModelNameMap.UpdateModes(_nemoFile.Scenario.DistinctModePSMNames, TransmissionLossAlgorithm.CASS);
                }
                else
                {
                    _rangeComplexPath = _areasPath = _bathymetryPath = _environmentPath = _imagesPath = null;
                    _rangeComplexDescriptor = null;
                    _scenarioBounds = null;
                    NemoModeToAcousticModelNameMap = null;
                    AvailableEnvironments = null;
                }
                NotifyPropertyChanged(NemoFileChangedEventArgs);
                NotifyPropertyChanged(CanPlaceAnalysisPointChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NemoFileChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.NemoFile);
        NemoFile _nemoFile;

        string _rangeComplexPath;
        string _areasPath;
        string _bathymetryPath;
        string _environmentPath;
        string _imagesPath;
        string _scenarioPath;
        string _propagationPath;
        string _pressurePath;
        RangeComplexDescriptor _rangeComplexDescriptor;
        GeoRect _scenarioBounds;

        void DisplayScenario()
        {
            if (_nemoFile.Scenario.OverlayFile != null) MapLayers.DisplayOverlayShapes(string.Format("{0} sim area", _nemoFile.Scenario.SimAreaName), LayerType.SimArea, Colors.Transparent, _nemoFile.Scenario.OverlayFile.Shapes);
            foreach (var platform in _nemoFile.Scenario.Platforms)
            {
                if (platform.Trackdefs.Count == 1)
                {
                    MapLayers.DisplayOverlayShapes(string.Format("Platform: {0} op area", platform.Name), LayerType.OpArea, Colors.Transparent, platform.Trackdefs[0].OverlayFile.Shapes);
                    if (_scenarioBounds == null) _scenarioBounds = new GeoRect(platform.Trackdefs[0].OverlayFile.Shapes[0].BoundingBox);
                    else _scenarioBounds.Union(platform.Trackdefs[0].OverlayFile.Shapes[0].BoundingBox);
                    platform.CalculateBehavior();
                    if (platform.BehaviorModel != null && platform.BehaviorModel.CourseOverlay != null)
                        MapLayers.DisplayOverlayShapes(string.Format("Platform: {0} track", platform.Name), LayerType.Track, Colors.Transparent,
                                                              new List<OverlayShape> { platform.BehaviorModel.CourseOverlay }, 0, PointSymbolType.Circle, true, new CustomStartEndLineStyle(PointSymbolType.Circle, Colors.Green, 5, PointSymbolType.Square, Colors.Red, 5, Colors.DarkGray, 1));
                }
                else
                    for (var trackIndex = 0; trackIndex < platform.Trackdefs.Count; trackIndex++)
                    {
                        MapLayers.DisplayOverlayShapes(string.Format("Platform: {0} OpArea{1}", platform.Name, trackIndex + 1), LayerType.OpArea, Colors.Transparent,
                                                              platform.Trackdefs[0].OverlayFile.Shapes);
                        if (_scenarioBounds == null) _scenarioBounds = new GeoRect(platform.Trackdefs[trackIndex].OverlayFile.Shapes[0].BoundingBox);
                        else _scenarioBounds.Union(platform.Trackdefs[trackIndex].OverlayFile.Shapes[0].BoundingBox);
                    }
            }
        }

        #endregion

        #region public string ScenarioFilename { get; set; }
        [XmlIgnore]
        public string ScenarioFilename
        {
            get { return _scenarioFilename; }
            set
            {
                if (_scenarioFilename == value) return;
                _scenarioFilename = value;
                NotifyPropertyChanged(ScenarioFilenameChangedEventArgs);
                NemoFile = _scenarioFilename != null ? new NemoFile(_scenarioFilename, Globals.AppSettings.ScenarioDataDirectory) : null;
                DisplayExistingAnalysisPoints();
            }
        }

        static readonly PropertyChangedEventArgs ScenarioFilenameChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.ScenarioFilename);
        string _scenarioFilename;

        #endregion

        #region public string EnvironmentName { get; set; }

        public string EnvironmentName
        {
            get { return _environmentName; }
            set
            {
                if (_environmentName == value) return;
                _environmentName = value;
                NotifyPropertyChanged(EnvironmentNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EnvironmentNameChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.EnvironmentName);
        string _environmentName;

        #endregion

        #region public NemoModeToAcousticModelNameMap NemoModeToAcousticModelNameMap { get; set; }

        public NemoModeToAcousticModelNameMap NemoModeToAcousticModelNameMap
        {
            get { return _nemoModeToAcousticModelNameMap; }
            set
            {
                if (_nemoModeToAcousticModelNameMap == value) return;
                _nemoModeToAcousticModelNameMap = value;
                NotifyPropertyChanged(NemoModeToAcousticModelNameMapChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NemoModeToAcousticModelNameMapChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.NemoModeToAcousticModelNameMap);
        NemoModeToAcousticModelNameMap _nemoModeToAcousticModelNameMap;

        #endregion

        #region public NAEMOEnvironmentDescriptors AvailableEnvironments { get; set; }
        [XmlIgnore]
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

        static readonly PropertyChangedEventArgs AvailableEnvironmentsChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.AvailableEnvironments);
        NAEMOEnvironmentDescriptors _availableEnvironments;

        #endregion

        #region public NAEMOEnvironmentDescriptor SelectedEnvironment { get; set; }
        [XmlIgnore]
        public NAEMOEnvironmentDescriptor SelectedEnvironment
        {
            get { return _selectedEnvironment; }
            set
            {
                if (_selectedEnvironment == value) return;
                _selectedEnvironment = value;
                NotifyPropertyChanged(SelectedEnvironmentChangedEventArgs);
                NotifyPropertyChanged(CanPlaceAnalysisPointChangedEventArgs);
                if (_selectedEnvironment != null) EnvironmentName = Path.GetFileNameWithoutExtension(_selectedEnvironment.Metadata.Filename);
                Task.Factory.StartNew(() => Dispatcher.InvokeIfRequired(DisplaySelectedEnvironment, DispatcherPriority.Normal));
            }
        }

        static readonly PropertyChangedEventArgs SelectedEnvironmentChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.SelectedEnvironment);
        NAEMOEnvironmentDescriptor _selectedEnvironment;

        void DisplaySelectedEnvironment()
        {
            if (_selectedEnvironment == null) return;
            var samplePoints = _selectedEnvironment.Data.Locations.Select(samplePoint => new OverlayPoint(samplePoint));
            SelectedBathymetry = ((NAEMOBathymetryDescriptor)(_rangeComplexDescriptor.NAEMOBathymetryDescriptors[_selectedEnvironment.Metadata.BathymetryName]));
            var bathymetryBounds = SelectedBathymetry.Metadata.Bounds;
            _scenarioBounds.Union(bathymetryBounds);
            var bathyBitmapLayer = MapLayers.DisplayBathymetryRaster("Bathymetry", Path.Combine(_imagesPath, _selectedEnvironment.Metadata.BathymetryName + ".bmp"), true, false, true, bathymetryBounds);
            MediatorMessage.Send(MediatorMessage.MoveLayerToBottom, bathyBitmapLayer);
            MapLayers.DisplayOverlayShapes("Sound Speed", LayerType.SoundSpeed, Colors.Transparent, samplePoints, 0, PointSymbolType.Circle, false, null, false);
            MapLayers.DisplayOverlayShapes("Wind", LayerType.WindSpeed, Colors.Transparent, samplePoints, 0, PointSymbolType.Diamond, false, null, false);
            foreach (var sedimentType in _selectedEnvironment.Data.SedimentTypes)
            {
                samplePoints = sedimentType.Value.Select(samplePoint => new OverlayPoint(samplePoint));
                MapLayers.DisplayOverlayShapes(string.Format("Sediment: {0}", sedimentType.Key.ToLower()), LayerType.BottomType, Colors.Transparent, samplePoints, 0, PointSymbolType.Diamond, false, null, false);
            }
            ZoomToScenarioHandler();
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }

        #endregion

        #region public NAEMOBathymetryDescriptor SelectedBathymetry { get; set; }
        [XmlIgnore]
        public NAEMOBathymetryDescriptor SelectedBathymetry
        {
            get { return _selectedBathymetry; }
            set
            {
                if (_selectedBathymetry == value) return;
                _selectedBathymetry = value;
                NotifyPropertyChanged(SelectedBathymetryChangedEventArgs);
                NotifyPropertyChanged(CanPlaceAnalysisPointChangedEventArgs);
                _bathymetry = _selectedBathymetry == null ? null : new WeakReference<Bathymetry>(_selectedBathymetry.Data);
                if ((_selectedBathymetry != null) && (AnalysisPoints != null)) SetBathymetryForAnalysisPoints();
            }
        }

        static readonly PropertyChangedEventArgs SelectedBathymetryChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.SelectedBathymetry);
        NAEMOBathymetryDescriptor _selectedBathymetry;
        WeakReference<Bathymetry> _bathymetry;

        #endregion

        #region ZoomToScenarioCommand
        public SimpleCommand<object, object> ZoomToScenarioCommand
        {
            get { return _zoomToScenario ?? (_zoomToScenario = new SimpleCommand<object, object>(delegate { ZoomToScenarioHandler(); })); }
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
            get { return _editScenario ?? (_editScenario = new SimpleCommand<object, object>(delegate { EditScenarioHandler(); })); }
        }

        SimpleCommand<object, object> _editScenario;

        void EditScenarioHandler()
        {
            var arguments = "\"" + ScenarioFilename + "\"";
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

        #region public bool IsInAnalysisPointMode { get; set; }
        [XmlIgnore]
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

        static readonly PropertyChangedEventArgs IsInAnalysisPointModeChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.IsInAnalysisPointMode);
        bool _isInAnalysisPointMode;

        #endregion

        #region public bool CanPlaceAnalysisPoint { get; set; }

        [XmlIgnore]
        public bool CanPlaceAnalysisPoint { get { return (NemoFile != null) && (SelectedBathymetry != null) && (SelectedEnvironment != null); } }
        static readonly PropertyChangedEventArgs CanPlaceAnalysisPointChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.CanPlaceAnalysisPoint);

        #endregion

        public void PlaceAnalysisPoint(EarthCoordinate location)
        {
            if (AnalysisPoints == null) AnalysisPoints = new ObservableCollection<AnalysisPoint>();
            var analysisPoint = new AnalysisPoint(location);
            var distinctModes = (from platform in NemoFile.Scenario.Platforms
                                 from source in platform.Sources
                                 from mode in source.Modes
                                 select mode).Distinct();
            foreach (var mode in distinctModes) analysisPoint.SoundSources.Add(new SoundSource(analysisPoint, mode, 16));
            AnalysisPoints.Add(analysisPoint);
            MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);
        }

        #region public ObservableCollection<AnalysisPoint> AnalysisPoints { get; set; }

        public ObservableCollection<AnalysisPoint> AnalysisPoints
        {
            get { return _analysisPoints; }
            set
            {
                if (_analysisPoints == value) return;
                if (_analysisPoints != null) _analysisPoints.CollectionChanged -= AnalysisPointsCollectionChanged;
                _analysisPoints = value;
                if (_analysisPoints != null) _analysisPoints.CollectionChanged += AnalysisPointsCollectionChanged;
                NotifyPropertyChanged(AnalysisPointsChangedEventArgs);
                SetBathymetryForAnalysisPoints();
            }
        }

        void SetBathymetryForAnalysisPoints()
        {
            if ((AnalysisPoints == null) || (AnalysisPoints.Count == 0) || (SelectedBathymetry == null) || (SelectedBathymetry.Data == null)) return;
            if ((_bathymetry == null) || (!_bathymetry.IsAlive)) _bathymetry = new WeakReference<Bathymetry>(SelectedBathymetry.Data);
            foreach (var analysisPoint in AnalysisPoints) analysisPoint.Bathymetry = _bathymetry;
        }

        void DisplayExistingAnalysisPoints()
        {
            if (AnalysisPoints == null || MapLayers == null) return;
            foreach (var analysisPoint in AnalysisPoints) MapLayers.DisplayAnalysisPoint(analysisPoint);
        }

        void AnalysisPointsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            NotifyPropertyChanged(AnalysisPointsChangedEventArgs);
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null)
                    {
                        foreach (var newPoint in e.NewItems)
                        {
                            if (MapLayers != null) MapLayers.DisplayAnalysisPoint((AnalysisPoint)newPoint);
                            if ((SelectedBathymetry == null) || (SelectedBathymetry.Data == null)) continue;
                            if ((_bathymetry == null) || (!_bathymetry.IsAlive)) _bathymetry = new WeakReference<Bathymetry>(SelectedBathymetry.Data);
                            ((AnalysisPoint)newPoint).Bathymetry = _bathymetry;
                        }
                    }
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
                case NotifyCollectionChangedAction.Remove:
                    if (e.OldItems != null) { }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    break;
            }
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }

        static readonly PropertyChangedEventArgs AnalysisPointsChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.AnalysisPoints);
        ObservableCollection<AnalysisPoint> _analysisPoints;

        #endregion

        public void ExportAnalysisPoints()
        {
            var propagationTimePath = Path.Combine(_propagationPath, NemoFile.Scenario.TimeFrame);
            var pressureTimePath = Path.Combine(_pressurePath, NemoFile.Scenario.TimeFrame);
            Directory.CreateDirectory(propagationTimePath);
            //Directory.CreateDirectory(pressureTimePath);
            var rangeComplex = ((RangeComplexDescriptor)RangeComplexDescriptors[NemoFile.Scenario.SimAreaName]).Data;
            SelectedEnvironment.Data.EnvironmentInformation.Bathymetry = SelectedBathymetry.Data;
            CASSFiles.WriteAcousticSimulatorFiles(Globals.AppSettings, new List<string> {NemoFile.Scenario.TimeFrame},
                                                  AnalysisPoints, NemoFile,
                                                  SelectedBathymetry.DataFilename, SelectedEnvironment.DataFilename,
                                                  NemoModeToAcousticModelNameMap,
                                                  SelectedEnvironment.Data.EnvironmentInformation, rangeComplex);
            Globals.WorkDirectories.Add(propagationTimePath);
        }
    }
}
