using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
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
using Cursors = System.Windows.Input.Cursors;
using TreeNode = ESME.Mapping.TreeNode;

namespace ESME.Metadata
{
    public class NAEMOScenarioMetadata : NAEMOMetadataBase
    {
        new internal static readonly List<Type> ReferencedTypes = new List<Type>(NAEMOMetadataBase.ReferencedTypes) {typeof(NemoModeToAcousticModelNameMap)};

        public NAEMOScenarioMetadata()
        {
            TreeViewRootNodes = new TreeNodeList();
        }

        public static NAEMOScenarioMetadata Load(string metaDataFilename)
        {
            var result = Load<NAEMOScenarioMetadata>(metaDataFilename);
            // Any other initialization code goes here
            return result;
        }

        public void Save(string filename = null) { Save(this, ReferencedTypes, filename); }

        #region Properties that MUST be initialized before setting the ScenarioFilename property

        #region public MapLayerCollection MapLayers { get; set; }
        [XmlIgnore]
        public MapLayerCollection MapLayers
        {
            get { return _mapLayers; }
            set
            {
                if (_mapLayers == value) return;
                if (_mapLayers != null) _mapLayers.CollectionChanged -= MapLayersCollectionChanged;
                _mapLayers = value;
                if (_mapLayers != null) _mapLayers.CollectionChanged += MapLayersCollectionChanged;
                NotifyPropertyChanged(MapLayersChangedEventArgs);
            }
        }

        void MapLayersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null)
                        foreach (MapLayerViewModel item in e.NewItems)
                        {
                            PlaceMapLayerInTree(item);
                        }
                    break;
                case NotifyCollectionChangedAction.Move:
                    Debug.WriteLine("NotifyCollectionChangedAction.Move");
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (MapLayerViewModel item in e.OldItems)
                    {
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    foreach (MapLayerViewModel item in e.OldItems)
                    {
                    }
                    foreach (MapLayerViewModel item in e.NewItems)
                    {
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    Debug.WriteLine("NotifyCollectionChangedAction.Reset");
                    break;
            }
        }
        static readonly PropertyChangedEventArgs MapLayersChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.MapLayers);
        [XmlIgnore]
        MapLayerCollection _mapLayers;

        #endregion
   
        [XmlIgnore]
        public Dispatcher Dispatcher { get; set; }
        [XmlIgnore]
        public RangeComplexDescriptors RangeComplexDescriptors { get; set; }
        [XmlIgnore]
        public IUIVisualizerService VisualizerService { get; set; }
        #endregion

        #region public CASSOutputs CASSOutputs { get; set; }
        [XmlIgnore]
        public CASSOutputs CASSOutputs
        {
            get { return _cassOutputs; }
            set
            {
                if (_cassOutputs == value) return;
                _cassOutputs = value;
                NotifyPropertyChanged(CASSOutputsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CASSOutputsChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.CASSOutputs);
        CASSOutputs _cassOutputs;

        void CASSOutputsChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (CASSOutput newItem in args.NewItems)
                    {
                        Debug.WriteLine("New CASSOutput: {0}|{1}|{2}", newItem.PlatformName, newItem.SourceName, newItem.ModeName);
                        if ((_bathymetry == null) || (!_bathymetry.IsAlive)) _bathymetry = new WeakReference<Bathymetry>(SelectedBathymetry.Data);
                        newItem.Bathymetry = _bathymetry;
                        newItem.ThresholdRadiusChanged += (s, e) => Dispatcher.InvokeIfRequired(() => MapLayers.DisplayPropagationPoint(newItem));
                        Dispatcher.InvokeIfRequired(() => MapLayers.DisplayPropagationPoint(newItem));
                        Task.Factory.StartNew(() =>
                        {
                            newItem.CheckThreshold(120, Dispatcher);
                        });
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (CASSOutput oldItem in args.OldItems)
                    {
                        Debug.WriteLine("Removed CASSOutput: {0}|{1}|{2}", oldItem.PlatformName, oldItem.SourceName, oldItem.ModeName);
                        Dispatcher.InvokeIfRequired(() => MapLayers.RemovePropagationPoint(oldItem));
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    Debug.WriteLine("CASSOutputs has been cleared");
                    break;
            }
        }

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
                    _propagationPath = Path.Combine(_scenarioPath, "Propagation", _nemoFile.Scenario.TimeFrame);
                    _pressurePath = Path.Combine(_scenarioPath, "Pressure", _nemoFile.Scenario.TimeFrame);
                    if (_nemoFile.Scenario.DistinctModes != null)
                    {
                        _distinctModeProperties = new List<AcousticProperties>();
                        foreach (var mode in _nemoFile.Scenario.DistinctModes)
                            _distinctModeProperties.Add(mode.AcousticProperties);
                    }
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
        List<AcousticProperties> _distinctModeProperties;

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
                UpdateScenarioTreeRoot();
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
            Dispatcher.InvokeIfRequired(() => MediatorMessage.Send(MediatorMessage.MoveLayerToBottom, bathyBitmapLayer));
            MapLayers.DisplayOverlayShapes("Sound Speed", LayerType.SoundSpeed, Colors.Transparent, samplePoints, 0, PointSymbolType.Circle, false, null, false);
            MapLayers.DisplayOverlayShapes("Wind", LayerType.WindSpeed, Colors.Transparent, samplePoints, 0, PointSymbolType.Diamond, false, null, false);
            foreach (var sedimentType in _selectedEnvironment.Data.SedimentTypes)
            {
                samplePoints = sedimentType.Value.Select(samplePoint => new OverlayPoint(samplePoint));
                MapLayers.DisplayOverlayShapes(string.Format("Sediment: {0}", sedimentType.Key.ToLower()), LayerType.BottomType, Colors.Transparent, samplePoints, 0, PointSymbolType.Diamond, false, null, false);
            }
            ZoomToScenarioHandler();
            Dispatcher.InvokeIfRequired(() => MediatorMessage.Send(MediatorMessage.RefreshMap, true));
            // Get a list of transmission loss files that match the modes in the current scenario
            if (CASSOutputs == null) CASSOutputs = new CASSOutputs(_propagationPath, "*.bin", CASSOutputsChanged, _distinctModeProperties);
            else CASSOutputs.RefreshInBackground();
            UpdateEnvironmentTreeRoot();
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
            Dispatcher.InvokeIfRequired(() => MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow));
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
            if (Dispatcher != null) Dispatcher.InvokeIfRequired(() => MediatorMessage.Send(MediatorMessage.RefreshMap, true));
        }

        static readonly PropertyChangedEventArgs AnalysisPointsChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.AnalysisPoints);
        ObservableCollection<AnalysisPoint> _analysisPoints;

        #endregion

        #region public TreeNodeList TreeViewRootNodes { get; set; }
        [XmlIgnore]
        public TreeNodeList TreeViewRootNodes
        {
            get { return _treeViewRootNodes; }
            set
            {
                if (_treeViewRootNodes == value) return;
                _treeViewRootNodes = value;
                NotifyPropertyChanged(TreeViewRootNodesChangedEventArgs);
                MediatorMessage.Send(MediatorMessage.SetTreeRoots, TreeViewRootNodes);
            }
        }

        static readonly PropertyChangedEventArgs TreeViewRootNodesChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.TreeViewRootNodes);
        TreeNodeList _treeViewRootNodes;

        void PlaceMapLayerInTree(MapLayerViewModel mapLayer)
        {
            foreach (var tree in TreeViewRootNodes)
                if (tree.FindNodeForMapLayer(mapLayer) != null) return;
        }

        void UpdateEnvironmentTreeRoot()
        {
            TreeViewRootNodes.RemoveAll(new Regex(@"Environment: [\s\S]+$"));
            var environmentRoot = new TreeNode("Environment: {0}", Path.GetFileNameWithoutExtension(_selectedEnvironment.DataFilename));
            TreeViewRootNodes.Add(environmentRoot);
            environmentRoot.MapLayers.Add(MapLayers.Find(LayerType.BaseMap, "Base Map").FirstOrDefault());
            environmentRoot.MapLayers.Add(MapLayers.Find(LayerType.BathymetryRaster, "Bathymetry").FirstOrDefault());
            environmentRoot.MapLayers.Add(MapLayers.Find(LayerType.SoundSpeed, "Sound Speed").FirstOrDefault());
            environmentRoot.MapLayers.Add(MapLayers.Find(LayerType.WindSpeed, "Wind").FirstOrDefault());
            environmentRoot.MapLayers.AddRange(MapLayers.Find(LayerType.WindSpeed, new Regex(@"Sediment: \S+$", RegexOptions.Singleline)));
        }

        void UpdateScenarioTreeRoot()
        {
            var scenarioRoot = TreeViewRootNodes.Find(node => node is ScenarioNode);
            if (scenarioRoot == null)
            {
                scenarioRoot = new ScenarioNode(NemoFile.Scenario);
                TreeViewRootNodes.Add(scenarioRoot);
            }
        }

        void UpdateAnimalsTreeRoot()
        {
            TreeViewRootNodes.RemoveAll(new Regex(@"Animals: [\s\S]+$"));
        }

        #endregion



        public void ExportAnalysisPoints()
        {
            Directory.CreateDirectory(_propagationPath);
            //Directory.CreateDirectory(pressureTimePath);
            var rangeComplex = ((RangeComplexDescriptor)RangeComplexDescriptors[NemoFile.Scenario.SimAreaName]).Data;
            SelectedEnvironment.Data.EnvironmentInformation.Bathymetry = SelectedBathymetry.Data;
            CASSFiles.WriteAcousticSimulatorFiles(Globals.AppSettings, new List<string> {NemoFile.Scenario.TimeFrame},
                                                  AnalysisPoints, NemoFile,
                                                  SelectedBathymetry.DataFilename, SelectedEnvironment.DataFilename,
                                                  NemoModeToAcousticModelNameMap,
                                                  SelectedEnvironment.Data.EnvironmentInformation, rangeComplex);
            Globals.WorkDirectories.Add(_propagationPath, true);
        }
    }
}
