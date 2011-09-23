﻿using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
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
    [Serializable]
    public class ScenarioMetadata : PropertyChangedBase
    {
        protected static readonly List<Type> ReferencedTypes = new List<Type> { typeof(string), typeof(DateTime), typeof(NAVOTimePeriod), typeof(NAEMOEnvironmentLocation), typeof(NAEMOEnvironmentFile), typeof(NemoModeToAcousticModelNameMap) };

        public ScenarioMetadata()
        {
            TreeViewRootNodes = new ObservableList<TreeNode>();
            Creator = System.Environment.UserName;
            CreationDateTime = DateTime.Now;
        }

        public static ScenarioMetadata Load(string metaDataFilename, RangeComplexes rangeComplexes)
        {
            if (!File.Exists(metaDataFilename)) return null;
            var result = XmlSerializer<ScenarioMetadata>.Load(metaDataFilename, ReferencedTypes);
            result.Filename = metaDataFilename;
            result.RangeComplexes = rangeComplexes;
            result.RangeComplexes.PropertyChanged += result.RangeComplexesPropertyChanged;
            // Any other initialization code goes here

            return result;
        }

        public void Save(string filename = null)
        {
            if (string.IsNullOrEmpty(filename)) filename = Filename;
            var serializer = new XmlSerializer<ScenarioMetadata> { Data = this };
            serializer.Save(filename, ReferencedTypes);
        }

        void RangeComplexesPropertyChanged(object sender, PropertyChangedEventArgs args)
        {
            switch (args.PropertyName)
            {
                case "IsEnvironmentLoaded":
                    if (RangeComplexes.IsEnvironmentLoaded)
                    {
                        _bathymetry = new WeakReference<Bathymetry>(((Task<Bathymetry>)RangeComplexes.EnvironmentData[EnvironmentDataType.Bathymetry]).Result);

                    }
                    else
                    {
                        _bathymetry = null;
                    }
                    break;
            }
        }

        #region Properties that MUST be initialized before setting the ScenarioFilename property

        #region public MapLayerCollection MapLayers { get; set; }
        [XmlIgnore]
        public MapLayerCollection CurrentMapLayers
        {
            get { return _currentMapLayers; }
            set
            {
                if (_currentMapLayers == value) return;
                if (_currentMapLayers != null) _currentMapLayers.CollectionChanged -= CurrentMapLayersCollectionChanged;
                _currentMapLayers = value;
                if (_currentMapLayers != null) _currentMapLayers.CollectionChanged += CurrentMapLayersCollectionChanged;
                NotifyPropertyChanged(MapLayersChangedEventArgs);
            }
        }

        void CurrentMapLayersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
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
                        foreach (var tree in TreeViewRootNodes) tree.RemoveMapLayer(item);
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    foreach (MapLayerViewModel item in e.OldItems)
                    {
                        foreach (var tree in TreeViewRootNodes) tree.RemoveMapLayer(item);
                    }
                    foreach (MapLayerViewModel item in e.NewItems)
                    {
                        PlaceMapLayerInTree(item);
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    Debug.WriteLine("NotifyCollectionChangedAction.Reset");
                    break;
            }
            NotifyPropertyChanged(MapLayersChangedEventArgs);
        }
        static readonly PropertyChangedEventArgs MapLayersChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.CurrentMapLayers);
        [XmlIgnore]
        MapLayerCollection _currentMapLayers;

        #endregion
        [XmlIgnore]
        public Dispatcher Dispatcher { get; set; }
        [XmlIgnore]
        public RangeComplexes RangeComplexes { get; set; }
        [XmlIgnore]
        public IUIVisualizerService VisualizerService { get; set; }
        [XmlIgnore]
        public IMessageBoxService MessageBoxService { get; set; }
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

        static readonly PropertyChangedEventArgs CASSOutputsChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.CASSOutputs);
        CASSOutputs _cassOutputs;

        void CASSOutputsChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (CASSOutput newItem in args.NewItems)
                    {
                        Debug.WriteLine("New CASSOutput: {0}|{1}|{2}", newItem.PlatformName, newItem.SourceName, newItem.ModeName);
                        newItem.Bathymetry = _bathymetry;
                        Dispatcher.InvokeIfRequired(() => CurrentMapLayers.DisplayPropagationPoint(newItem));
                        _cassFileQueue.Post(newItem);
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (CASSOutput oldItem in args.OldItems)
                    {
                        Debug.WriteLine("Removed CASSOutput: {0}|{1}|{2}", oldItem.PlatformName, oldItem.SourceName, oldItem.ModeName);
                        Dispatcher.InvokeIfRequired(() => CurrentMapLayers.RemovePropagationPoint(oldItem));
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
                    var scenarioPath = Path.GetDirectoryName(_nemoFile.FileName);
                    _propagationPath = Path.Combine(scenarioPath, "Propagation", _nemoFile.Scenario.TimeFrame);
                    _pressurePath = Path.Combine(scenarioPath, "Pressure", _nemoFile.Scenario.TimeFrame);
                    if (_nemoFile.Scenario.DistinctModes != null)
                    {
                        _distinctModeProperties = new List<AcousticProperties>();
                        foreach (var mode in _nemoFile.Scenario.DistinctModes)
                            _distinctModeProperties.Add(mode.AcousticProperties);
                    }

                    UpdateScenarioTreeRoot();

                    // Display any animal layers on the map asynchronously
                    if (_nemoFile.Scenario.Animals != null)
                        foreach (var animal in _nemoFile.Scenario.Animals)
                            foreach (var species in animal.Species)
                            {
                                try
                                {
                                    species.AnimatDataTask.Start();
                                    var localSpecies = species;
                                    species.AnimatDataTask.ContinueWith(task => Dispatcher.InvokeInBackgroundIfRequired(() => CurrentMapLayers.DisplaySpecies(localSpecies.SpeciesName, localSpecies.AnimatDataTask.Result)));
                                }
                                catch (Exception e)
                                {
                                    MessageBoxService.ShowError("Error loading animats: " + e.Message);
                                }
                            }



                    DisplayScenario();
                    _selectedRangeComplex = RangeComplexes.RangeComplexCollection[_nemoFile.Scenario.SimAreaName];
                    RangeComplexes.SelectedTimePeriod = (NAVOTimePeriod)Enum.Parse(typeof(NAVOTimePeriod), _nemoFile.Scenario.TimeFrame);
                    if (NemoModeToAcousticModelNameMap == null) NemoModeToAcousticModelNameMap = new NemoModeToAcousticModelNameMap(_nemoFile.Scenario.DistinctModePSMNames, TransmissionLossAlgorithm.CASS);
                    else NemoModeToAcousticModelNameMap.UpdateModes(_nemoFile.Scenario.DistinctModePSMNames, TransmissionLossAlgorithm.CASS);
                    _cassOutputProcessor = new ActionBlock<CASSOutput>(newItem =>
                    {
                        Debug.WriteLine("New CASSOutput: {0}|{1}|{2}", newItem.PlatformName, newItem.SourceName, newItem.ModeName);
                        //newItem.Bathymetry = new WeakReference<Bathymetry>(RangeComplexes.SelectedBathymetry.DataTask.Result);
                        newItem.CheckThreshold(Globals.AppSettings.TransmissionLossContourThreshold, Dispatcher);
                        Dispatcher.InvokeInBackgroundIfRequired(() => CurrentMapLayers.DisplayPropagationPoint(newItem));
                    },
                    new ExecutionDataflowBlockOptions
                    {
                        TaskScheduler = TaskScheduler.Default,
                        BoundedCapacity = 4,
                        MaxDegreeOfParallelism = 4,
                    });
                    _cassFileQueue = new BufferBlock<CASSOutput>();
                    _cassFileQueue.LinkTo(_cassOutputProcessor);
                    _cassFileQueue.Completion.ContinueWith(task =>
                    {
                        _cassOutputProcessor.Complete();
                        _cassOutputProcessor.Completion.ContinueWith(t => { _cassOutputProcessor = null; });
                        _cassFileQueue = null;
                    });

                }
                else
                {
                    _selectedRangeComplex = null;
                    _scenarioBounds = null;
                    NemoModeToAcousticModelNameMap = null;
                }
                NotifyPropertyChanged(NemoFileChangedEventArgs);
                NotifyPropertyChanged(CanPlaceAnalysisPointChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NemoFileChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.NemoFile);
        NemoFile _nemoFile;
        List<AcousticProperties> _distinctModeProperties;

        ActionBlock<CASSOutput> _cassOutputProcessor;
        BufferBlock<CASSOutput> _cassFileQueue;

        string _propagationPath;
        string _pressurePath;
        NewRangeComplex _selectedRangeComplex;
        GeoRect _scenarioBounds;
        WeakReference<Bathymetry> _bathymetry;

        void DisplayScenario()
        {
            if (_nemoFile.Scenario.OverlayFile != null) CurrentMapLayers.DisplayOverlayShapes(string.Format("{0} sim area", _nemoFile.Scenario.SimAreaName), LayerType.SimArea, Colors.Transparent, _nemoFile.Scenario.OverlayFile.Shapes);
            foreach (var platform in _nemoFile.Scenario.Platforms)
            {
                if (platform.Trackdefs.Count == 1)
                {
                    CurrentMapLayers.DisplayOverlayShapes(string.Format("Platform: {0} op area", platform.Name), LayerType.OpArea, Colors.Transparent, platform.Trackdefs[0].OverlayFile.Shapes, canBeRemoved: false);
                    if (_scenarioBounds == null) _scenarioBounds = new GeoRect(platform.Trackdefs[0].OverlayFile.Shapes[0].BoundingBox);
                    else _scenarioBounds.Union(platform.Trackdefs[0].OverlayFile.Shapes[0].BoundingBox);
                    platform.CalculateBehavior();
                    if (platform.BehaviorModel != null && platform.BehaviorModel.CourseOverlay != null)
                        CurrentMapLayers.DisplayOverlayShapes(string.Format("Platform: {0} track", platform.Name), LayerType.Track, Colors.Transparent,
                                                              new List<OverlayShape> { platform.BehaviorModel.CourseOverlay }, 0, PointSymbolType.Circle, true, new CustomStartEndLineStyle(PointSymbolType.Circle, Colors.Green, 5, PointSymbolType.Square, Colors.Red, 5, Colors.DarkGray, 1), false, true, false);
                }
                else
                    for (var trackIndex = 0; trackIndex < platform.Trackdefs.Count; trackIndex++)
                    {
                        CurrentMapLayers.DisplayOverlayShapes(string.Format("Platform: {0} OpArea{1}", platform.Name, trackIndex + 1), LayerType.OpArea, Colors.Transparent,
                                                              platform.Trackdefs[0].OverlayFile.Shapes, canBeRemoved: false);
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

        static readonly PropertyChangedEventArgs ScenarioFilenameChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.ScenarioFilename);
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

        static readonly PropertyChangedEventArgs EnvironmentNameChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.EnvironmentName);
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

        static readonly PropertyChangedEventArgs NemoModeToAcousticModelNameMapChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.NemoModeToAcousticModelNameMap);
        NemoModeToAcousticModelNameMap _nemoModeToAcousticModelNameMap;

        #endregion

        #region EditEnvironmentCommand
        public SimpleCommand<object, object> EditEnvironmentCommand
        {
            get
            {
                return _editEnvironment ??
                       (_editEnvironment =
                        new SimpleCommand<object, object>(delegate { return IsEditEnvironmentCommandEnabled; },
                                                          delegate { EditEnvironmentHandler(); }));
            }
        }

        SimpleCommand<object, object> _editEnvironment;

        bool IsEditEnvironmentCommandEnabled
        {
            get { return true; }
        }

        void EditEnvironmentHandler() { }
        #endregion

        #region ZoomToScenarioCommand
        public SimpleCommand<object, object> ZoomToScenarioCommand
        {
            get { return _zoomToScenario ?? (_zoomToScenario = new SimpleCommand<object, object>(delegate { ZoomToScenarioHandler(); })); }
        }

        SimpleCommand<object, object> _zoomToScenario;

        public void ZoomToScenarioHandler()
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

        static readonly PropertyChangedEventArgs IsInAnalysisPointModeChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.IsInAnalysisPointMode);
        bool _isInAnalysisPointMode;

        #endregion

        #region public bool CanPlaceAnalysisPoint { get; set; }

        [XmlIgnore]
        public bool CanPlaceAnalysisPoint { get { return (NemoFile != null) && (RangeComplexes.IsEnvironmentFullySpecified); } }
        static readonly PropertyChangedEventArgs CanPlaceAnalysisPointChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.CanPlaceAnalysisPoint);

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
                if (_analysisPoints != null)
                {
                    _analysisPoints.CollectionChanged -= AnalysisPointsCollectionChanged;

                    TreeViewRootNodes.RemoveAll(item => item.Name == "Analysis points");
                }
                _analysisPoints = value;
                if (_analysisPoints != null)
                {
                    _analysisPoints.CollectionChanged += AnalysisPointsCollectionChanged;
                }
                NotifyPropertyChanged(AnalysisPointsChangedEventArgs);
                SetBathymetryForAnalysisPoints();
            }
        }

        void SetBathymetryForAnalysisPoints()
        {
            if ((AnalysisPoints == null) || (AnalysisPoints.Count == 0) || (!RangeComplexes.IsEnvironmentLoaded)) return;
            foreach (var analysisPoint in AnalysisPoints) analysisPoint.Bathymetry = _bathymetry;
        }

        void DisplayExistingAnalysisPoints()
        {
            if (AnalysisPoints == null || CurrentMapLayers == null) return;
            TreeViewRootNodes.RemoveAll(item => item.Name == "Analysis points");
            var analysisPointNode = new AnalysisPointNode("Analysis points");
            TreeViewRootNodes.Add(analysisPointNode);
            foreach (var analysisPoint in AnalysisPoints) CurrentMapLayers.DisplayAnalysisPoint(analysisPoint);
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
                            if (CurrentMapLayers != null) CurrentMapLayers.DisplayAnalysisPoint((AnalysisPoint)newPoint);
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

        static readonly PropertyChangedEventArgs AnalysisPointsChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.AnalysisPoints);
        ObservableCollection<AnalysisPoint> _analysisPoints;

        #endregion

        #region public ObservableList<TreeNode> TreeViewRootNodes { get; set; }
        [XmlIgnore]
        public ObservableList<TreeNode> TreeViewRootNodes
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

        static readonly PropertyChangedEventArgs TreeViewRootNodesChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.TreeViewRootNodes);
        ObservableList<TreeNode> _treeViewRootNodes;

        void PlaceMapLayerInTree(MapLayerViewModel mapLayer) { foreach (var tree in TreeViewRootNodes) tree.AddMapLayer(mapLayer); }

        void UpdateEnvironmentTreeRoot()
        {
            var environmentRoot = new TreeNode("Environment");
            TreeViewRootNodes.Add(environmentRoot);
            environmentRoot.MapLayers.Add(CurrentMapLayers.Find(LayerType.BaseMap, "Base Map").FirstOrDefault());
            environmentRoot.MapLayers.Add(CurrentMapLayers.Find(LayerType.BathymetryRaster, "Bathymetry").FirstOrDefault());
            environmentRoot.MapLayers.Add(CurrentMapLayers.Find(LayerType.SoundSpeed, "Sound Speed").FirstOrDefault());
            environmentRoot.MapLayers.Add(CurrentMapLayers.Find(LayerType.WindSpeed, "Wind").FirstOrDefault());
            environmentRoot.MapLayers.Add(CurrentMapLayers.Find(LayerType.BottomType, "Bottom Loss").FirstOrDefault());
            environmentRoot.MapLayers.AddRange(CurrentMapLayers.Find(LayerType.BottomType, new Regex(@"Sediment: \S+$", RegexOptions.Singleline)));
        }

        void UpdateScenarioTreeRoot()
        {
            var scenarioRoot = TreeViewRootNodes.Find(node => node is ScenarioNode);
            if (scenarioRoot != null) return;
            scenarioRoot = new ScenarioNode(NemoFile.Scenario);
            TreeViewRootNodes.Add(scenarioRoot);
        }

        void UpdateAnimalsTreeRoot()
        {
            var regex = new Regex(@"Animals: [\s\S]+$");
            TreeViewRootNodes.RemoveAll(item => regex.IsMatch(item.Name));
        }

        #endregion

        public void ExportAnalysisPoints()
        {
            Directory.CreateDirectory(_propagationPath);
            CASSFiles.WriteAcousticSimulatorFiles(Globals.AppSettings, new List<string> { NemoFile.Scenario.TimeFrame },
                                                  AnalysisPoints, NemoFile,
                                                  NemoModeToAcousticModelNameMap,
                                                  RangeComplexes.EnvironmentData, RangeComplexes);
            Globals.WorkDirectories.Add(_propagationPath, true);
        }

        public static string MetadataFilename(string sourceFilename)
        {
            var metadataPath = Path.GetDirectoryName(sourceFilename);
            var metadataFile = Path.GetFileNameWithoutExtension(sourceFilename);
            return Path.Combine(metadataPath, metadataFile + ".xml");
        }

        #region public string OverlayFilename { get; set; }

        public string OverlayFilename
        {
            get { return _overlayFilename; }
            set
            {
                if (_overlayFilename == value) return;
                _overlayFilename = Path.GetFileNameWithoutExtension(value);
                if (!string.IsNullOrEmpty(_overlayFilename) && !string.IsNullOrEmpty(Filename))
                    if (!OverlayFileExists) _overlayFilename = null;
                NotifyPropertyChanged(OverlayNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OverlayNameChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.OverlayFilename);
        string _overlayFilename;

        bool OverlayFileExists { get { return File.Exists(Path.Combine(Path.GetDirectoryName(Path.GetDirectoryName(Filename)), "Areas", OverlayFilename + ".ovr")); } }

        #endregion

        #region public string Creator { get; set; }

        public string Creator
        {
            get { return _creator; }
            set
            {
                if (_creator == value) return;
                _creator = value;
                NotifyPropertyChanged(CreatorChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CreatorChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.Creator);
        string _creator;

        #endregion

        #region public DateTime CreationDateTime { get; set; }

        public DateTime CreationDateTime
        {
            get { return _creationDateTime; }
            set
            {
                if (_creationDateTime == value) return;
                _creationDateTime = value;
                NotifyPropertyChanged(CreationDateChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CreationDateChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.CreationDateTime);
        DateTime _creationDateTime;

        #endregion

        #region public GeoRect Bounds { get; set; }

        public GeoRect Bounds
        {
            get { return _bounds; }
            set
            {
                if (_bounds == value) return;
                _bounds = value;
                NotifyPropertyChanged(BoundsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BoundsChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.Bounds);
        GeoRect _bounds;

        #endregion

        #region public string Filename { get; set; }

        public string Filename
        {
            get { return _filename; }
            set
            {
                if (_filename == value) return;
                _filename = value;
                if (!string.IsNullOrEmpty(OverlayFilename) && !string.IsNullOrEmpty(_filename))
                    if (!OverlayFileExists) OverlayFilename = null;
                NotifyPropertyChanged(FilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FilenameChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.Filename);
        string _filename;

        #endregion
    }
}
