using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading;
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
using ESME.TransmissionLoss.REFMS;
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

        public ScenarioMetadata(string metaDataFilename, RangeComplexes rangeComplexes)
            : this()
        {
            Filename = metaDataFilename;
            RangeComplexes = rangeComplexes;
            RangeComplexes.PropertyChanged += RangeComplexesPropertyChanged;
            PropertyChanged += (s, e) => Save();
        }

        public static ScenarioMetadata Load(string metaDataFilename, RangeComplexes rangeComplexes)
        {
            if (!File.Exists(metaDataFilename)) return new ScenarioMetadata(metaDataFilename, rangeComplexes);
            var retry = 10;
            ScenarioMetadata result = null;
            Exception exception = null;
            while (--retry > 0)
            {
                try
                {
                    exception = null;
                    result = XmlSerializer<ScenarioMetadata>.Load(metaDataFilename, ReferencedTypes);
                }
                catch (Exception e)
                {
                    exception = e;
                    Thread.Sleep(100);
                }
            }
            if (exception != null) throw exception;
            result.Filename = metaDataFilename;
            result.RangeComplexes = rangeComplexes;
            result.RangeComplexes.PropertyChanged += result.RangeComplexesPropertyChanged;
            result.PropertyChanged += (s, e) => result.Save();
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
                        Debug.WriteLine("{0} Bathymetry weak reference is set", DateTime.Now);
                        NotifyPropertyChanged(CanPlaceAnalysisPointChangedEventArgs);
                    }
                    else
                    {
                        _bathymetry = null;
                        Debug.WriteLine("{0} Bathymetry weak reference is null", DateTime.Now);
                    }
                    Task.Factory.StartNew(() =>
                    {
                        SetBathymetryAndValidate();
                        ProcessCassOutputs();
                    });
                    break;
                case "SelectedArea":
                    SelectedAreaName = RangeComplexes.SelectedArea == null ? null : RangeComplexes.SelectedArea.Name;
                    break;
                case "SelectedBathymetry":
                    SelectedBathymetryName = RangeComplexes.SelectedBathymetry == null ? null : RangeComplexes.SelectedBathymetry.FileName;
                    break;
            }
        }

        [XmlIgnore]
        public Dispatcher Dispatcher { get; set; }
        [XmlIgnore]
        public RangeComplexes RangeComplexes { get; set; }
        [XmlIgnore]
        public IUIVisualizerService VisualizerService { get; set; }
        [XmlIgnore]
        public IMessageBoxService MessageBoxService { get; set; }

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
                                    species.AnimatDataTask.ContinueWith(task => Dispatcher.InvokeInBackgroundIfRequired(() =>
                                    {
                                        CurrentMapLayers.DisplaySpecies(localSpecies.SpeciesName,
                                                                        localSpecies.AnimatDataTask.Result);
                                        UpdateAnimalsTreeRoot();
                                    }));
                                }
                                catch (Exception e)
                                {
                                    MessageBoxService.ShowError("Error loading animats: " + e.Message);
                                }
                            }

                    DisplayScenario();

                    if (!RangeComplexes.RangeComplexCollection.ContainsKey(_nemoFile.Scenario.SimAreaName))
                        throw new ApplicationException(
                                string.Format("The range complex specified by this scenario ({0}) was not found",
                                              _nemoFile.Scenario.SimAreaName));
                        
                    // Set the selected range complex
                    RangeComplexes.SelectedRangeComplex = RangeComplexes.RangeComplexCollection[_nemoFile.Scenario.SimAreaName];
                    _selectedRangeComplex = RangeComplexes.SelectedRangeComplex;
                    RangeComplexes.SelectedTimePeriod = (NAVOTimePeriod)Enum.Parse(typeof(NAVOTimePeriod), _nemoFile.Scenario.TimeFrame);
                    
                    if ((SelectedAreaName != null) && (RangeComplexes.SelectedRangeComplex.AreaCollection.ContainsKey(SelectedAreaName)))
                        RangeComplexes.SelectedArea = RangeComplexes.SelectedRangeComplex.AreaCollection[SelectedAreaName];
                    else SelectedAreaName = null;

                    if ((SelectedBathymetryName != null) && (RangeComplexes.SelectedArea.BathymetryFiles[SelectedBathymetryName] != null))
                        RangeComplexes.SelectedBathymetry = RangeComplexes.SelectedArea.BathymetryFiles[SelectedBathymetryName];
                    else SelectedBathymetryName = null;

                    if (NemoModeToAcousticModelNameMap == null) NemoModeToAcousticModelNameMap = new NemoModeToAcousticModelNameMap(_nemoFile.Scenario.DistinctModePSMNames, TransmissionLossAlgorithm.CASS);
                    else NemoModeToAcousticModelNameMap.UpdateModes(_nemoFile.Scenario.DistinctModePSMNames, TransmissionLossAlgorithm.CASS);
                    NemoModeToAcousticModelNameMap.CollectionChanged += (s, e) => Save();
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

        void ProcessCassOutputs()
        {
            if (_cassOutputProcessor == null)
            {
                _cassOutputProcessor = new ActionBlock<CASSOutput>(newItem =>
                {
                    Debug.WriteLine("New CASSOutput: {0}|{1}|{2}", newItem.PlatformName, newItem.SourceName,
                                    newItem.ModeName);
                    newItem.Bathymetry = _bathymetry;
                    newItem.CheckThreshold(Globals.AppSettings.TransmissionLossContourThreshold);
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
                AwaitCassOutputProcessorCompletion();
            }
            if (CASSOutputs == null) CASSOutputs = new CASSOutputs(_propagationPath, "*.bin", CASSOutputsChanged, _distinctModeProperties);
            else CASSOutputs.RefreshInBackground();
        }

        async void AwaitCassOutputProcessorCompletion()
        {
            try
            {
                await _cassOutputProcessor.Completion;
            }
            catch (Exception e)
            {
                Debug.WriteLine("Caught exception from CASS Output processor: " + e.Message);
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
            if (RangeComplexes.SelectedArea != null) _scenarioBounds.Union(RangeComplexes.SelectedArea.GeoRect);
        }

        public void RemoveScenarioDisplay()
        {
            CurrentMapLayers.RemoveAll(
                                       layer =>
                                       layer.LayerType == LayerType.OpArea && layer.Name.StartsWith("Platform:"));
            CurrentMapLayers.RemoveAll(
                                       layer =>
                                       layer.LayerType == LayerType.Track && layer.Name.StartsWith("Platform:"));
            _scenarioBounds = RangeComplexes.SelectedArea == null ? null : RangeComplexes.SelectedArea.GeoRect;
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
                DisplayExistingExplosivePoints();
            }
        }

        static readonly PropertyChangedEventArgs ScenarioFilenameChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.ScenarioFilename);
        string _scenarioFilename;

        #endregion

        #region public string SelectedAreaName { get; set; }

        public string SelectedAreaName
        {
            get { return _selectedAreaName; }
            set
            {
                if (_selectedAreaName == value) return;
                _selectedAreaName = value;
                NotifyPropertyChanged(SelectedAreaNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedAreaNameChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.SelectedAreaName);
        string _selectedAreaName;

        #endregion

        #region public string SelectedBathymetryName { get; set; }

        public string SelectedBathymetryName
        {
            get { return _selectedBathymetryName; }
            set
            {
                if (_selectedBathymetryName == value) return;
                _selectedBathymetryName = value;
                NotifyPropertyChanged(SelectedBathymetryNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedBathymetryNameChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.SelectedBathymetryName);
        string _selectedBathymetryName;

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
        public bool CanPlaceAnalysisPoint
        {
            get { return (NemoFile != null) && (RangeComplexes.IsEnvironmentLoaded); }
        }
        static readonly PropertyChangedEventArgs CanPlaceAnalysisPointChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.CanPlaceAnalysisPoint);

        #endregion

        public void PlaceAnalysisPoint(EarthCoordinate location)
        {
            if (AnalysisPoints == null) AnalysisPoints = new ObservableCollection<AnalysisPoint>();
            if (ExplosivePoints == null) ExplosivePoints = new ObservableCollection<ExplosivePoint>();

            var analysisPoint = new AnalysisPoint(location);
            ExplosivePoint explosivePoint = null;
            var distinctModes = (from platform in NemoFile.Scenario.Platforms
                                 from source in platform.Sources
                                 from mode in source.Modes
                                 select new
                                 {
                                     Platform = platform,
                                     Source = source,
                                     Mode = mode,
                                 }).Distinct().ToList();
            foreach (var mode in distinctModes)
            {
                if (mode.Mode.Name.ToLower() != "explosive") analysisPoint.SoundSources.Add(new SoundSource(analysisPoint, mode.Mode, 16));
                else
                {
                    if (explosivePoint == null) explosivePoint = new ExplosivePoint(location, mode.Platform, mode.Mode, 1.0f);
                    else explosivePoint.SoundSources.Add(new SoundSource(location, mode.Mode, 1));
                }
            }
            if (analysisPoint.SoundSources.Count > 0) AnalysisPoints.Add(analysisPoint);
            if ((explosivePoint != null) && (explosivePoint.SoundSources.Count > 0)) ExplosivePoints.Add(explosivePoint);
            Dispatcher.InvokeIfRequired(() => MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow));
        }

        #region public ObservableCollection<ExplosivePoint> ExplosivePoints { get; set; }

        public ObservableCollection<ExplosivePoint> ExplosivePoints
        {
            get { return _explosivePoints; }
            set
            {
                if (_explosivePoints == value) return;
                if (_explosivePoints != null)
                {
                    _explosivePoints.CollectionChanged -= ExplosivePointsCollectionChanged;
                    TreeViewRootNodes.RemoveAll(item => item.Name == "Analysis points");
                }
                _explosivePoints = value;
                if (_explosivePoints != null)
                {
                    _explosivePoints.CollectionChanged += ExplosivePointsCollectionChanged;
                }
                NotifyPropertyChanged(ExplosivePointsChangedEventArgs);
            }
        }

        void DisplayExistingExplosivePoints()
        {
            if (AnalysisPoints == null || CurrentMapLayers == null) return;
            foreach (var explosivePoint in ExplosivePoints) CurrentMapLayers.DisplayExplosivePoint(explosivePoint);
        }

        void ExplosivePointsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            NotifyPropertyChanged(ExplosivePointsChangedEventArgs);
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null)
                    {
                        foreach (var newPoint in e.NewItems)
                        {
                            if (CurrentMapLayers != null) CurrentMapLayers.DisplayExplosivePoint((ExplosivePoint)newPoint);
                            ((ExplosivePoint)newPoint).Bathymetry = _bathymetry;
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


        static readonly PropertyChangedEventArgs ExplosivePointsChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.ExplosivePoints);
        ObservableCollection<ExplosivePoint> _explosivePoints;

        #endregion

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
                SetBathymetryAndValidate();
            }
        }

        void SetBathymetryAndValidate()
        {
            if ((AnalysisPoints != null) && (AnalysisPoints.Count != 0) && (RangeComplexes.IsEnvironmentLoaded))
            {
                foreach (var analysisPoint in AnalysisPoints)
                {
                    analysisPoint.Bathymetry = _bathymetry;
                    analysisPoint.Validate();
                }
            }
            if ((ExplosivePoints != null) && (ExplosivePoints.Count != 0) && (RangeComplexes.IsEnvironmentLoaded))
            {
                foreach (var explosivePoint in ExplosivePoints)
                {
                    explosivePoint.Bathymetry = _bathymetry;
                    explosivePoint.Validate();
                }
            }
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
            if (AnalysisPoints != null && AnalysisPoints.Count() > 0)
            {
                CASSFiles.WriteAcousticSimulatorFiles(Globals.AppSettings,
                                                      new List<string> {NemoFile.Scenario.TimeFrame},
                                                      AnalysisPoints, NemoFile,
                                                      NemoModeToAcousticModelNameMap,
                                                      RangeComplexes.EnvironmentData, RangeComplexes);
            }
            else
            {
                foreach (var point in ExplosivePoints)
                {
                    
                }
            }
            Globals.WorkDirectories.Add(_propagationPath, true);
        }

        public static string MetadataFilename(string sourceFilename)
        {
            var metadataPath = Path.GetDirectoryName(sourceFilename);
            var metadataFile = Path.GetFileNameWithoutExtension(sourceFilename);
            return Path.Combine(metadataPath, metadataFile + ".xml");
        }

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
                NotifyPropertyChanged(FilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FilenameChangedEventArgs = ObservableHelper.CreateArgs<ScenarioMetadata>(x => x.Filename);
        string _filename;

        #endregion
    }
}
