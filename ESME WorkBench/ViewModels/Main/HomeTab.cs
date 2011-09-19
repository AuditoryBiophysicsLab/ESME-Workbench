﻿using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Diagnostics;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;
using System.Xml.Serialization;
using Cinch;
using ESME;
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
                RangeComplexes.SelectedRangeComplex = RangeComplexes.RangeComplexCollection[NemoFile.Scenario.SimAreaName];
                RangeComplexes.SelectedTimePeriod = (NAVOTimePeriod)Enum.Parse(typeof(NAVOTimePeriod), NemoFile.Scenario.TimeFrame);

                // Load the metadata file if it exists, or create one if it doesn't
                var metadataFileName = Path.Combine(Path.GetDirectoryName(fileName), Path.GetFileNameWithoutExtension(fileName) + ".emf");
                if (File.Exists(metadataFileName)) ScenarioMetadata = NAEMOScenarioMetadata.Load(metadataFileName);
                else ScenarioMetadata = new NAEMOScenarioMetadata(NemoFile.Scenario.DistinctModePSMNames)
                {
                    Filename = metadataFileName,
                    NemoFileName = fileName,
                };

                // If the previously-selected area does not exist, set the name to null
                if ((ScenarioMetadata.SelectedAreaName != null) && (!RangeComplexes.SelectedRangeComplex.AreaCollection.ContainsKey(ScenarioMetadata.SelectedAreaName))) ScenarioMetadata.SelectedAreaName = null;

                // Use the selected area if it exists, otherwise use the sim area
                RangeComplexes.SelectedArea = ScenarioMetadata.SelectedAreaName != null ? RangeComplexes.SelectedRangeComplex.AreaCollection[ScenarioMetadata.SelectedAreaName] : RangeComplexes.SelectedRangeComplex.SimArea;

                // Check if the chosen resolution (if any) is valid.  If so, use it.  If not, use the 2 minute bathymetry from the current area
                if ((ScenarioMetadata.SelectedResolutionName != null) && 
                    (RangeComplexes.SelectedArea.BathymetryFiles[ScenarioMetadata.SelectedResolutionName] != null) && 
                    (RangeComplexes.SelectedArea.BathymetryFiles[ScenarioMetadata.SelectedResolutionName].IsCached)) 
                    RangeComplexes.SelectedBathymetry = (BathymetryFile)RangeComplexes.SelectedArea.BathymetryFiles[ScenarioMetadata.SelectedResolutionName];
                else RangeComplexes.SelectedBathymetry = (BathymetryFile)RangeComplexes.SelectedArea.BathymetryFiles["2.00min"];

                // Create the list of TreeView nodes that will hold the roots of the tree-structured view of the scenario
                TreeViewRootNodes = new ObservableList<TreeNode> {new ScenarioNode(NemoFile.Scenario)};

                // Display any animal layers on the map
                if (NemoFile.Scenario.Animals != null)
                    foreach (var species in _nemoFile.Scenario.Animals.SelectMany(animal => animal.Species))
                        CurrentMapLayers.DisplaySpecies(species);

                // Display the scenario layers on the map
                DisplayScenario();
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

        void DisplayScenario()
        {
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
        GeoRect _scenarioBounds;

        #endregion

        #region CloseScenarioCommand
        public SimpleCommand<object, object> CloseScenarioCommand
        {
            get
            {
                return _closeScenario ?? (_closeScenario = new SimpleCommand<object, object>(
                    delegate { return IsScenarioLoaded; },
                    delegate
                    {
                        NemoFile = null;
                        ScenarioMetadata = null;
                    }));
            }
        }

        SimpleCommand<object, object> _closeScenario;
        #endregion


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

                MainWindowTitle = _nemoFile != null ? string.Format("ESME WorkBench 2011{0}: {1} [{2}]", Configuration.IsUnclassifiedModel ? " (public)" : "", NemoFile.Scenario.EventName, NemoFile.Scenario.TimeFrame) : string.Format("ESME WorkBench 2011{0}: <No scenario loaded>", Configuration.IsUnclassifiedModel ? " (public)" : "");
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

        void UpdateEnvironmentTreeRoot()
        {
            var regex = new Regex(@"Environment: [\s\S]+$");
            TreeViewRootNodes.RemoveAll(item => regex.IsMatch(item.Name));
            var environmentRoot = new TreeNode("Environment: ");
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
