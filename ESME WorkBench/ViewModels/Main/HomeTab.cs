using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Diagnostics;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Serialization;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Threading;
using System.Xml.Serialization;
using Cinch;
using ESME;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Environment.NAVO;
using ESME.Mapping;
using ESME.Metadata;
using ESME.Model;
using ESME.NEMO;
using ESME.NEMO.Overlay;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.CASS;
using ESME.Views.AcousticBuilder;
using ESME.Views.TransmissionLoss;
using ESMEWorkBench.Properties;
using ESMEWorkBench.ViewModels.NAVO;
using HRC.Navigation;
using HRC.Utility;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
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
                // Load the NEMO file, and check if we have the range complex that it specifies
                NemoFile = new NemoFile(fileName, Globals.AppSettings.ScenarioDataDirectory);
                if (!RangeComplexes.RangeComplexCollection.ContainsKey(NemoFile.Scenario.SimAreaName))
                {
                    _messageBoxService.ShowError(string.Format("This scenario specifies a range complex \"{0}\" that does not exist on this computer", NemoFile.Scenario.SimAreaName));
                    NemoFile = null;
                    return;
                }
                if (NemoFile.Scenario == null)
                {
                    _messageBoxService.ShowError("This file does not contain a scenario");
                    return;
                }

                // If the metadata file is not found, it will be constructed and returned by the Load static method
                ScenarioMetadata = NAEMOScenarioMetadata.LoadOrCreate(fileName);

                RangeComplexes.SelectedRangeComplex = RangeComplexes.RangeComplexCollection[NemoFile.Scenario.SimAreaName];
                RangeComplexes.SelectedTimePeriod = (NAVOTimePeriod)Enum.Parse(typeof(NAVOTimePeriod), NemoFile.Scenario.TimeFrame);

                // Initialize the scenario metadata with the list of distinct mode names from the scenario file
                ScenarioMetadata.Initialize(NemoFile.Scenario.DistinctModePSMNames);

                // If the previously-selected area does not exist, set the name to null
                if ((ScenarioMetadata.SelectedAreaName != null) && (!RangeComplexes.SelectedRangeComplex.AreaCollection.ContainsKey(ScenarioMetadata.SelectedAreaName))) ScenarioMetadata.SelectedAreaName = null;

                // Use the selected area if it exists, otherwise use the sim area
                RangeComplexes.SelectedArea = ScenarioMetadata.SelectedAreaName != null ? RangeComplexes.SelectedRangeComplex.AreaCollection[ScenarioMetadata.SelectedAreaName] : RangeComplexes.SelectedRangeComplex.SimArea;

                // Create the list of TreeView nodes that will hold the roots of the tree-structured view of the scenario
                TreeViewRootNodes = new ObservableList<TreeNode> {new ScenarioNode(NemoFile.Scenario)};

                // If an area name has been selected, and the selected range complex has an area of that same name, select it as the current area
                if (!string.IsNullOrEmpty(ScenarioMetadata.SelectedAreaName) && (SelectedRangeComplex.AreaCollection[ScenarioMetadata.SelectedAreaName] != null))
                    RangeComplexes.SelectedArea = SelectedRangeComplex.AreaCollection[ScenarioMetadata.SelectedAreaName];

                // If an resolution has been selected, and the selected area has cached bathymetry of that resolution, select it
                if (!string.IsNullOrEmpty(ScenarioMetadata.SelectedResolutionName) && (SelectedArea != RangeComplexArea.None) &&
                    (SelectedArea.BathymetryFiles[ScenarioMetadata.SelectedResolutionName] != null) && SelectedArea.BathymetryFiles[ScenarioMetadata.SelectedResolutionName].IsCached)
                    RangeComplexes.SelectedBathymetry = (BathymetryFile)SelectedArea.BathymetryFiles[ScenarioMetadata.SelectedResolutionName];

                // Display any animal layers on the map asynchronously
                if (NemoFile.Scenario.Animals != null)
                    foreach (var animal in _nemoFile.Scenario.Animals)
                        foreach (var species in animal.Species)
                        {
                            try
                            {
                                species.AnimatDataTask.Start();
                                var localSpecies = species;
                                species.AnimatDataTask.ContinueWith(task => _dispatcher.InvokeInBackgroundIfRequired(() => CurrentMapLayers.DisplaySpecies(localSpecies.SpeciesName, localSpecies.AnimatDataTask.Result)));
                            }
                            catch (Exception e)
                            {
                                _messageBoxService.ShowError("Error loading animats");
                            }
                        }

                // Display the scenario overlays and tracks on the map
                if (_nemoFile.Scenario.OverlayFile != null) CurrentMapLayers.DisplayOverlayShapes(string.Format("{0} sim area", NemoFile.Scenario.SimAreaName), LayerType.SimArea, Colors.Transparent, _nemoFile.Scenario.OverlayFile.Shapes);
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
                CloseScenarioHandler();
            }
        }

        GeoRect _scenarioBounds;

        #endregion

        #region CloseScenarioCommand
        public SimpleCommand<object, object> CloseScenarioCommand
        {
            get
            {
                return _closeScenario ?? (_closeScenario = new SimpleCommand<object, object>(
                    delegate { return IsScenarioLoaded; },
                    delegate { CloseScenarioHandler(); }));
            }
        }

        SimpleCommand<object, object> _closeScenario;

        void CloseScenarioHandler()
        {
            if (ScenarioMetadata != null) ScenarioMetadata.Save();
            ScenarioMetadata = null;
            TreeViewRootNodes = null;
            NemoFile = null;
            var scenarioLayerTypes = new List<LayerType> { LayerType.AnalysisPoint, LayerType.Animal, LayerType.Pressure, LayerType.Propagation, LayerType.Track };
            scenarioLayerTypes.ForEach(layerType => CurrentMapLayers.RemoveAll(layer => layer.LayerType == layerType));
            CurrentMapLayers.RemoveAll(layer => layer.Name.StartsWith("Platform:"));
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
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
                NotifyPropertyChanged(IsScenarioNotLoadedChangedEventArgs);
                NotifyPropertyChanged(IsTimePeriodSelectionEnabledChangedEventArgs);

                if (_nemoFile != null) ScenarioLoadedToolTip = "Switching range complexes and time periods is disabled while a scenario is loaded";
                MainWindowTitle = _nemoFile != null ? string.Format("ESME WorkBench 2011{0}: {1} [{2}]", Configuration.IsUnclassifiedModel ? " (public)" : "", NemoFile.Scenario.EventName, NemoFile.Scenario.TimeFrame) : string.Format("ESME WorkBench 2011{0}: <No scenario loaded>", Configuration.IsUnclassifiedModel ? " (public)" : "");
            }
        }

        public bool IsScenarioLoaded { get { return NemoFile != null; } }
        public bool IsScenarioNotLoaded { get { return !IsScenarioLoaded; } }
        static readonly PropertyChangedEventArgs IsScenarioLoadedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsScenarioLoaded);
        static readonly PropertyChangedEventArgs IsScenarioNotLoadedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsScenarioNotLoaded);
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

        #region public CASSOutputs CASSOutputs { get; set; }
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

        static readonly PropertyChangedEventArgs CASSOutputsChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.CASSOutputs);
        CASSOutputs _cassOutputs;

        void CASSOutputsChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (CASSOutput newItem in args.NewItems)
                    {
                        Debug.WriteLine("New CASSOutput: {0}|{1}|{2}", newItem.PlatformName, newItem.SourceName, newItem.ModeName);
                        newItem.Bathymetry = new WeakReference<Bathymetry>(SelectedBathymetry.DataTask.Result);
                        newItem.ThresholdRadiusChanged += (s, e) => _dispatcher.InvokeIfRequired(() => MapLayers.DisplayPropagationPoint(newItem));
                        _dispatcher.InvokeIfRequired(() => MapLayers.DisplayPropagationPoint(newItem));
                        Task.Factory.StartNew(() =>
                        {
                            newItem.CheckThreshold(120, _dispatcher);
                        });
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (CASSOutput oldItem in args.OldItems)
                    {
                        Debug.WriteLine("Removed CASSOutput: {0}|{1}|{2}", oldItem.PlatformName, oldItem.SourceName, oldItem.ModeName);
                        _dispatcher.InvokeIfRequired(() => MapLayers.RemovePropagationPoint(oldItem));
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    Debug.WriteLine("CASSOutputs has been cleared");
                    break;
            }
        }

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
            var arguments = "\"" + NemoFile.FileName + "\"";
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
        static readonly PropertyChangedEventArgs MapLayersChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.MapLayers);
        MapLayerCollection _mapLayers;

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

        static readonly PropertyChangedEventArgs TreeViewRootNodesChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.TreeViewRootNodes);
        ObservableList<TreeNode> _treeViewRootNodes;

        void PlaceMapLayerInTree(MapLayerViewModel mapLayer) { foreach (var tree in TreeViewRootNodes) tree.AddMapLayer(mapLayer); }

#if false
        void DisplaySelectedEnvironment()
        {
            var regex = new Regex(@"Environment: [\s\S]+$");
            TreeViewRootNodes.RemoveAll(item => regex.IsMatch(item.Name));
            var environmentRoot = new EnvironmentNode("Environment");
            TreeViewRootNodes.Add(environmentRoot);
            foreach (var layer in _mapLayers) PlaceMapLayerInTree(layer);

            var samplePoints = _selectedEnvironment.Data.Locations.Select(samplePoint => new OverlayPoint(samplePoint)).ToList();
            var bathymetryBounds = SelectedBathymetry.GeoRect;
            _scenarioBounds.Union(bathymetryBounds);
            var bathyBitmapLayer = MapLayers.DisplayBathymetryRaster("Bathymetry", Path.Combine(_imagesPath, _selectedEnvironment.Metadata.BathymetryName + ".bmp"), true, false, true, bathymetryBounds);
            Dispatcher.InvokeIfRequired(() => MediatorMessage.Send(MediatorMessage.MoveLayerToBottom, bathyBitmapLayer));
            MapLayers.DisplayOverlayShapes("Sound Speed", LayerType.SoundSpeed, Colors.Transparent, samplePoints, 0, PointSymbolType.Circle, false, null, false);
            MapLayers.DisplayOverlayShapes("Wind", LayerType.WindSpeed, Colors.Transparent, samplePoints, 0, PointSymbolType.Diamond, false, null, false);
            foreach (var sedimentType in _selectedEnvironment.Data.SedimentTypes)
            {
                samplePoints = sedimentType.Value.Select(samplePoint => new OverlayPoint(samplePoint)).ToList();
                MapLayers.DisplayOverlayShapes(string.Format("Sediment: {0}", sedimentType.Key.ToLower()), LayerType.BottomType, Colors.Transparent, samplePoints, 0, PointSymbolType.Diamond, false, null, false);
            }
            ZoomToScenarioHandler();
            Dispatcher.InvokeIfRequired(() => MediatorMessage.Send(MediatorMessage.RefreshMap, true));
            // Get a list of transmission loss files that match the modes in the current scenario
            if (CASSOutputs == null) CASSOutputs = new CASSOutputs(_propagationPath, "*.bin", CASSOutputsChanged, _distinctModeProperties);
            else CASSOutputs.RefreshInBackground();
            //UpdateEnvironmentTreeRoot();
        }
#endif

        void UpdateEnvironmentTreeRoot()
        {
            var regex = new Regex(@"Environment: [\s\S]+$");
            TreeViewRootNodes.RemoveAll(item => regex.IsMatch(item.Name));
            var environmentRoot = new TreeNode("Environment");
            TreeViewRootNodes.Add(environmentRoot);
            environmentRoot.MapLayers.Add(CurrentMapLayers.Find(LayerType.BaseMap, "Base Map").FirstOrDefault());
            environmentRoot.MapLayers.Add(CurrentMapLayers.Find(LayerType.BathymetryRaster, "Bathymetry").FirstOrDefault());
            environmentRoot.MapLayers.Add(CurrentMapLayers.Find(LayerType.SoundSpeed, "Sound Speed").FirstOrDefault());
            environmentRoot.MapLayers.Add(CurrentMapLayers.Find(LayerType.WindSpeed, "Wind").FirstOrDefault());
            environmentRoot.MapLayers.AddRange(CurrentMapLayers.Find(LayerType.WindSpeed, new Regex(@"Sediment: \S+$", RegexOptions.Singleline)));
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
    }
}
