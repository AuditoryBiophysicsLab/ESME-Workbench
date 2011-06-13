﻿using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Threading;
using System.Xml.Serialization;
using Cinch;
using ESME;
using ESME.Data;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Model;
using ESME.NEMO;
using ESME.Overlay;
using ESME.TransmissionLoss;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Map;
using HRC.Navigation;
using HRC.Utility;
using ThinkGeo.MapSuite.Core;
using FileFormatException = ESME.Model.FileFormatException;
using LineStyle = ESME.Overlay.LineStyle;

namespace ESMEWorkBench.Data
{
    [Serializable]
    public partial class Experiment : SerializableData<Experiment>
    {
        static Type[] _referencedTypes;
        static readonly PropertyChangedEventArgs TransmissionLossFieldsChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.TransmissionLossFields);
        public static IUIVisualizerService VisualizerService { get; set; }

        #region public string Comments { get; set; }

        [XmlIgnore] static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.Comments);
        [XmlIgnore] string _comments;

        [XmlElement]
        public string Comments
        {
            get { return _comments; }
            set
            {
                if (_comments == value) return;
                _comments = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        #endregion

        #region public string Author { get; set; }

        [XmlIgnore] static readonly PropertyChangedEventArgs AuthorChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.Author);
        [XmlIgnore] string _author;

        [XmlElement]
        public string Author
        {
            get { return _author; }
            set
            {
                if (_author == value) return;
                _author = value;
                NotifyPropertyChanged(AuthorChangedEventArgs);
            }
        }

        #endregion

        #region public DateTime Created { get; set; }

        [XmlIgnore] static readonly PropertyChangedEventArgs CreatedChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.Created);
        [XmlIgnore] DateTime _created;

        [XmlElement]
        public DateTime Created
        {
            get { return _created; }
            set
            {
                if (_created == value) return;
                _created = value;
                NotifyPropertyChanged(CreatedChangedEventArgs);
            }
        }

        #endregion

        #region public DateTime LastModified { get; set; }

        [XmlIgnore] static readonly PropertyChangedEventArgs LastModifiedChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.LastModified);
        [XmlIgnore] DateTime _lastModified;

        [XmlElement]
        public DateTime LastModified
        {
            get { return _lastModified; }
            set
            {
                if (_lastModified == value) return;
                _lastModified = value;
                NotifyPropertyChanged(LastModifiedChangedEventArgs);
            }
        }

        #endregion

        #region public string ModifiedBy { get; set; }

        [XmlIgnore] static readonly PropertyChangedEventArgs ModifiedByChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.ModifiedBy);
        [XmlIgnore] string _modifiedBy;

        [XmlElement]
        public string ModifiedBy
        {
            get { return _modifiedBy; }
            set
            {
                if (_modifiedBy == value) return;
                _modifiedBy = value;
                NotifyPropertyChanged(ModifiedByChangedEventArgs);
            }
        }

        #endregion

        #region public string WindSpeedFileName { get; set; }

        [XmlIgnore] static readonly PropertyChangedEventArgs WindSpeedFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.WindSpeedFileName);
        [XmlIgnore] string _windSpeedFileName;

        [XmlElement]
        public string WindSpeedFileName
        {
            get { return _windSpeedFileName; }
            set
            {
                if (_windSpeedFileName == value) return;
                _windSpeedFileName = value;
                NotifyPropertyChanged(WindSpeedFileNameChangedEventArgs);
                WindSpeed = null;
                InitializeEnvironment(false);
            }
        }

        #endregion

        #region public string SoundSpeedFileName { get; set; }

        [XmlIgnore] static readonly PropertyChangedEventArgs SoundSpeedFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.SoundSpeedFileName);
        [XmlIgnore] string _soundSpeedFileName;

        [XmlElement]
        public string SoundSpeedFileName
        {
            get { return _soundSpeedFileName; }
            set
            {
                if (_soundSpeedFileName == value) return;
                _soundSpeedFileName = value;
                NotifyPropertyChanged(SoundSpeedFileNameChangedEventArgs);
                SoundSpeedField = null;
                InitializeEnvironment(false);
            }
        }

        #endregion

        #region public string SedimentFileName { get; set; }

        [XmlIgnore]
        static readonly PropertyChangedEventArgs SedimentFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.SedimentFileName);
        [XmlIgnore] string _sedimentFileName;

        [XmlElement]
        public string SedimentFileName
        {
            get { return _sedimentFileName; }
            set
            {
                if (_sedimentFileName == value) return;
                _sedimentFileName = value;
                NotifyPropertyChanged(SedimentFileNameChangedEventArgs);
                Sediment = null;
                InitializeEnvironment(false);
            }
        }

        #endregion

        #region public string BathymetryFileName { get; set; }

        [XmlIgnore] static readonly PropertyChangedEventArgs BathymetryFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.BathymetryFileName);
        [XmlIgnore] string _bathymetryFileName;

        [XmlElement]
        public string BathymetryFileName
        {
            get { return _bathymetryFileName; }
            set
            {
                if (_bathymetryFileName == value) return;
                _bathymetryFileName = value;
                NotifyPropertyChanged(BathymetryFileNameChangedEventArgs);
                Bathymetry = null;
                InitializeEnvironment(false);
            }
        }

        #endregion

        #region public string ScenarioFileName { get; set; }

        [XmlIgnore] static readonly PropertyChangedEventArgs ScenarioFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.ScenarioFileName);
        [XmlIgnore] string _scenarioFileName;

        [XmlElement]
        public string ScenarioFileName
        {
            get { return _scenarioFileName; }
            set
            {
                if (_scenarioFileName == value) return;
                _scenarioFileName = value;
                LoadScenarioFile();
                NotifyPropertyChanged(ScenarioFileNameChangedEventArgs);
                InitializeEnvironment(false);
            }
        }

        void LoadScenarioFile()
        {
            if ((_scenarioFileName != null) && (Globals.AppSettings.ScenarioDataDirectory != null) && File.Exists(_scenarioFileName) && Directory.Exists(Globals.AppSettings.ScenarioDataDirectory))
            {
                NemoFile = new NemoFile(_scenarioFileName, Globals.AppSettings.ScenarioDataDirectory);
            }
        }

        [XmlIgnore]
        public NemoFile NemoFile { get; private set; }

        #endregion

        #region public ObservableCollection<MapLayerViewModel> MapLayers { get; set; }

        static readonly PropertyChangedEventArgs MapLayersChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.MapLayers);
        ObservableCollection<MapLayerViewModel> _mapLayers;

        public ObservableCollection<MapLayerViewModel> MapLayers
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
                    if (e.NewItems != null) {}
                    break;
                case NotifyCollectionChangedAction.Remove:
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    break;
            }
            NotifyPropertyChanged(MapLayersChangedEventArgs);
        }

        #endregion

        #region public ObservableCollection<AnalysisPoint> AnalysisPoints { get; set; }

        static readonly PropertyChangedEventArgs AnalysisPointsChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.AnalysisPoints);
        ObservableCollection<AnalysisPoint> _analysisPoints;

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
            }
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
                            DisplayAnalysisPoint((AnalysisPoint)newPoint);
                    }
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
                case NotifyCollectionChangedAction.Remove:
                    if (e.OldItems != null) {}
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    break;
            }
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }

#if false
        void AddContextMenuToAnalysisPoint(AnalysisPoint analysisPoint)
        {
            marker.ContextMenu = new ContextMenu();
            marker.ContextMenu.Items.Add(new MenuItem
                                         {
                                             Header = "View...",
                                             Command = ViewAnalysisPointCommand,
                                             CommandParameter = analysisPoint,
                                         });
            marker.ContextMenu.Items.Add(new MenuItem
                                         {
                                             Header = "Recalculate...",
                                             Command = RecalculateAnalysisPointCommand,
                                             CommandParameter = analysisPoint,
                                         });
            marker.ContextMenu.Items.Add(new MenuItem
                                         {
                                             Header = "Delete",
                                             Command = DeleteAnalysisPointCommand,
                                             CommandParameter = analysisPoint,
                                         });
        }
#endif

        public TransmissionLossField NearestMatchingTransmissionLoss(NemoMode nemoMode, EarthCoordinate location)
        {
            TransmissionLossField nearestMatch = null;
            foreach (var analysisPoint in AnalysisPoints)
                foreach (var transmissionLossField in analysisPoint.TransmissionLossFields)
                    if (transmissionLossField.IsAcousticMatchFor(nemoMode))
                    {
                        if (nearestMatch == null) nearestMatch = transmissionLossField;
                        else if (location.DistanceTo(nearestMatch.EarthCoordinate) > location.DistanceTo(transmissionLossField.EarthCoordinate)) nearestMatch = transmissionLossField;
                    }
            return nearestMatch;
        }

        #endregion

        #region public ObservableCollection<string> AnimalPopulationFiles { get; set; }

        static readonly PropertyChangedEventArgs AnimalPopulationFilesChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.AnimalPopulationFiles);
        ObservableCollection<string> _animalPopulationFiles;

        public ObservableCollection<string> AnimalPopulationFiles
        {
            get { return _animalPopulationFiles; }
            set
            {
                if (_animalPopulationFiles == value) return;
                if (_animalPopulationFiles != null) _animalPopulationFiles.CollectionChanged -= AnimalPopulationFilesCollectionChanged;
                _animalPopulationFiles = value;
                if (_animalPopulationFiles != null) _animalPopulationFiles.CollectionChanged += AnimalPopulationFilesCollectionChanged;
                NotifyPropertyChanged(AnimalPopulationFilesChangedEventArgs);
            }
        }

        void AnimalPopulationFilesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null)
                    {
                        foreach (var item in e.NewItems)
                        {
                            var animatPopulationFile = (string) item;
                            if (!File.Exists(animatPopulationFile))
                            {
                                var result = MessageBoxService.ShowYesNo("This experiment references an animal population file that cannot be located on this computer.  The file referenced is:\n" + animatPopulationFile + "\nProceed with opening this experiment?  If yes, all existing animal population information will be removed.", CustomDialogIcons.Question);
                                if (result == CustomDialogResults.Yes)
                                {
                                    _deleteAllSpeciesLayersOnInitialize = true;
                                    AnimalPopulationFiles.Remove(animatPopulationFile);
                                    return;
                                }
                                throw new UserCanceledOperationException("The operation was canceled by user request");
                            }
                            if (AnimatInterface == null) AnimatInterface = AnimatInterface.Create(animatPopulationFile);

                            //for each species...
                            foreach (var species in AnimatInterface.AnimatList.SpeciesList)
                            {
                                var speciesName = species.SpeciesName;
                                var animatsInSpecies = AnimatInterface.AnimatList.FindAll(a => a.SpeciesName == speciesName);
                                var layerName = "Species: " + speciesName.Replace('_', ' ');
                                var layer = FindOverlayShapeMapLayer(LayerType.Animal, layerName) ?? new OverlayShapeMapLayer
                                                                                                     {
                                                                                                         Name = layerName,
                                                                                                         CanBeRemoved = false,
                                                                                                         CanBeReordered = true,
                                                                                                         CanChangeLineColor = true,
                                                                                                         CanChangeLineWidth = true,
                                                                                                         LayerType = LayerType.Animal,
                                                                                                     };
                                foreach (var animat in animatsInSpecies) layer.Add(new OverlayPoint(animat.Location));
                                layer.Done();
                                if (MapLayers.IndexOf(layer) == -1) MapLayers.Add(layer);
                            }
                        }
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    break;
            }
            NotifyPropertyChanged(MapLayersChangedEventArgs);
        }

        #endregion

        #region public GeoRect OpArea { get; set; }

        public GeoRect OpArea
        {
            get { return _opArea; }
            set
            {
                if (_opArea == value) return;
                _opArea = value;
                NotifyPropertyChanged(OpAreaChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OpAreaChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.OpArea);
        GeoRect _opArea;

        #endregion

        #region public GeoRect SimArea { get; set; }

        public GeoRect SimArea
        {
            get { return _simArea; }
            set
            {
                if (_simArea == value) return;
                _simArea = value;
                NotifyPropertyChanged(SimAreaChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SimAreaChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.SimArea);
        GeoRect _simArea;

        #endregion

        #region public ScenarioSimulatorSettings ScenarioSimulatorSettings { get; set; }

        public ScenarioSimulatorSettings ScenarioSimulatorSettings
        {
            get { return _scenarioSimulatorSettings; }
            set
            {
//                if (_scenarioSimulatorSettings == value) return;
                _scenarioSimulatorSettings = value;
                NotifyPropertyChanged(ScenarioSimulatorSettingsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ScenarioSimulatorSettingsChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.ScenarioSimulatorSettings);
        ScenarioSimulatorSettings _scenarioSimulatorSettings;

        #endregion

        #region public ulong NextObjectID { get; set; }

        static readonly PropertyChangedEventArgs NextObjectIDChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.NextObjectID);
        ulong _nextObjectID = 1;
        bool _nextObjectIDSetLocked;

        public ulong NextObjectID
        {
            get
            {
                lock (this)
                {
                    NotifyPropertyChanged(NextObjectIDChangedEventArgs);
                    return _nextObjectID++;
                }
            }
            set
            {
                if (_nextObjectIDSetLocked) throw new ApplicationException("Write access to NextObjectID is forbidden.  Read access is permitted");
                if (_nextObjectID == value) return;
                _nextObjectID = value;
                _nextObjectIDSetLocked = true;
            }
        }

        #endregion

        bool _deleteAllSpeciesLayersOnInitialize;
        [XmlIgnore] bool _isInitialized;
        ObservableCollection<TransmissionLossField> _transmissionLossFields;

        public Experiment()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nExperiment: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
            Author = Environment.UserName;
            Created = DateTime.Now;
            PropertyChanged += delegate(object s, PropertyChangedEventArgs e) { if (e.PropertyName != "IsChanged") IsChanged = true; };
            CurrentExtent = "POLYGON((-173.84765625 123.442822265625,169.98046875 123.442822265625,169.98046875 -165.555615234375,-173.84765625 -165.555615234375,-173.84765625 123.442822265625))";
            CurrentScale = 147647947.5;
            PropertyChanged += LocalPropertyChanged;
            TransmissionLossFields = new ObservableCollection<TransmissionLossField>();
            AnimalPopulationFiles = new ObservableCollection<string>();
        }

        public static Type[] ReferencedTypes
        {
            get
            {
                return _referencedTypes ?? (_referencedTypes = new[]
                                                               {
                                                                   typeof (MapLayerViewModel), typeof (ShapefileMapLayer), typeof (OverlayShapeMapLayer), typeof (OverlayFileMapLayer), typeof (MarkerLayerViewModel), typeof(RasterMapLayer), typeof(AnalysisPoint), typeof(SoundSource), typeof(GeoRect), typeof(NemoModeToAcousticModelNameMap), typeof(NAVOTimePeriod),
                                                               });
            }
        }

        [XmlIgnore]
        public AnimatInterface AnimatInterface { get; set; }

        [XmlElement]
        public string CurrentExtent { get; set; }

        [XmlElement]
        public double CurrentScale { get; set; }

        [XmlIgnore]
        public static IMessageBoxService MessageBoxService { private get; set; }

        [XmlIgnore]
        public Environment2DData WindSpeed { get; private set; }

        [XmlIgnore]
        public SoundSpeedField SoundSpeedField { get; private set; }

        [XmlIgnore]
        public Sediment Sediment { get; private set; }

        #region public Environment2DData Bathymetry { get; set; }

        [XmlIgnore]
        public Environment2DData Bathymetry
        {
            get { return _bathymetry; }
            set
            {
                if (_bathymetry == value) return;
                _bathymetry = value;
                NotifyPropertyChanged(BathymetryChangedEventArgs);
                AnalysisPoint.Bathymetry.Target = _bathymetry;
                SoundSource.Bathymetry.Target = _bathymetry;
                if ((_bathymetry == null) || (AnalysisPoints == null)) return;
                foreach (var analysisPoint in AnalysisPoints)
                    analysisPoint.Validate();
            }
        }

        static readonly PropertyChangedEventArgs BathymetryChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.Bathymetry);
        Environment2DData _bathymetry;

        #endregion

        [XmlIgnore]
        Timer ScenarioReloadTimer { get; set; }

        #region public bool CanSaveAs { get; set; }
        [XmlIgnore]
        public bool CanSaveAs
        {
            get
            {
                if ((FileName == null) || string.IsNullOrEmpty(ScenarioFileName)) return false;
                return true;
            }
        }

        #endregion

        #region public bool CanSave { get; set; }
        [XmlIgnore]
        public bool CanSave
        {
            get
            {
                if (string.IsNullOrEmpty(ScenarioFileName)) return false;
                return true;
            }
        }

        #endregion

        #region public string LocalStorageRoot { get; set; }

        [XmlIgnore]
        public string LocalStorageRoot
        {
            get
            {
                if (FileName == null) throw new ApplicationException("The new experiment must be saved before performing this operation");
                var localStorageRoot = Path.Combine(Path.GetDirectoryName(FileName), Path.GetFileNameWithoutExtension(FileName));
                if (!Directory.Exists(localStorageRoot))
                {
                    Directory.CreateDirectory(localStorageRoot);
                    File.WriteAllText(Path.Combine(localStorageRoot, "_README.TXT"), "This directory and all its contents are managed by the ESME WorkBench.\r\n\r\nMoving, deleting, renaming or otherwise changing the contents of this directory or any subdirectory WILL cause the ESME WorkBench to function improperly and/or to give invalid results.\r\n\r\nYOU HAVE BEEN WARNED!");
                }

                return localStorageRoot;
            }
        }

        #endregion

        #region public string EnvironmentRoot { get; set; }

        public string EnvironmentRoot
        {
            get
            {
                var environmentRoot = Path.Combine(LocalStorageRoot, "Environment");
                if (!Directory.Exists(environmentRoot)) Directory.CreateDirectory(environmentRoot);
                return environmentRoot;
            }
        }

        #endregion

        #region public string TransmissionLossJobRoot { get; set; }

        public string TransmissionLossJobRoot
        {
            get
            {
                var transmissionLossJobRoot = Path.Combine(LocalStorageRoot, "Transmission Loss\\Jobs");
                if (!Directory.Exists(transmissionLossJobRoot)) Directory.CreateDirectory(transmissionLossJobRoot);
                return transmissionLossJobRoot;
            }
        }

        #endregion

        #region public string TransmissionLossFileRoot { get; set; }

        public string TransmissionLossFileRoot
        {
            get
            {
                var transmissionLossFileRoot = Path.Combine(LocalStorageRoot, "Transmission Loss\\Files");
                if (!Directory.Exists(transmissionLossFileRoot)) Directory.CreateDirectory(transmissionLossFileRoot);
                return transmissionLossFileRoot;
            }
        }

        #endregion
        
        #region public ObservableCollection<TransmissionLossField> TransmissionLossFields { get; set; }

        [XmlIgnore]

        public ObservableCollection<TransmissionLossField> TransmissionLossFields
        {
            get { return _transmissionLossFields; }
            set
            {
                if (_transmissionLossFields == value) return;
                if (_transmissionLossFields != null) _transmissionLossFields.CollectionChanged -= TransmissionLossFieldsCollectionChanged;
                _transmissionLossFields = value;
                if (_transmissionLossFields != null) _transmissionLossFields.CollectionChanged += TransmissionLossFieldsCollectionChanged;
            }
        }

        #endregion

        #region public List<NAVOTimePeriod> AvailableTimePeriods { get; set; }

        public List<NAVOTimePeriod> AvailableTimePeriods
        {
            get { return _availableTimePeriods; }
            set
            {
                if (_availableTimePeriods == value) return;
                _availableTimePeriods = value;
                NotifyPropertyChanged(AvailableTimePeriodsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AvailableTimePeriodsChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.AvailableTimePeriods);
        List<NAVOTimePeriod> _availableTimePeriods;

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

        static readonly PropertyChangedEventArgs NemoModeToAcousticModelNameMapChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.NemoModeToAcousticModelNameMap);
        NemoModeToAcousticModelNameMap _nemoModeToAcousticModelNameMap;

        #endregion


        [XmlIgnore]
        FileSystemWatcher TransmissionLossFileWatcher { get; set; }

        [XmlIgnore]
        FileSystemWatcher ScenarioFileWatcher { get; set; }

        #region public bool IsChanged { get; set; }

        [XmlIgnore] static readonly PropertyChangedEventArgs IsChangedChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.IsChanged);
        [XmlIgnore] bool _isChanged;

        [XmlIgnore]
        public bool IsChanged
        {
            get { return _isChanged; }
            set
            {
                if (_isChanged == value) return;
                _isChanged = value;
                NotifyPropertyChanged(IsChangedChangedEventArgs);
            }
        }

        #endregion

        void TransmissionLossFieldsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null)
                        foreach (var newItem in e.NewItems.Cast<TransmissionLossField>())
                        {
                            Console.WriteLine("Added tlf: " + newItem.Name);
                        }
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
                case NotifyCollectionChangedAction.Remove:
                    if (e.OldItems != null) foreach (var oldItem in e.OldItems.Cast<TransmissionLossField>()) {}
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    break;
            }
            NotifyPropertyChanged(TransmissionLossFieldsChangedEventArgs);
        }

        static void LocalPropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            switch (e.PropertyName)
            {
                case "FileName":
                    break;
            }
        }

        public void Save() { SaveAs(FileName); }

        public void Save(string fileName)
        {
            FileName = fileName;
            SaveAs(FileName);
        }

        public void SaveAs(string fileName)
        {
            LastModified = DateTime.Now;
            ModifiedBy = Environment.UserName;
            SaveAs(fileName, ReferencedTypes);
            IsChanged = false;
        }

        public new void Close()
        {
            if (ScenarioFileWatcher != null)
            {
                ScenarioFileWatcher.EnableRaisingEvents = false;
                ScenarioFileWatcher = null;
            }
            if (TransmissionLossFileWatcher != null)
            {
                TransmissionLossFileWatcher.EnableRaisingEvents = false;
                TransmissionLossFileWatcher = null;
            }
            base.Close();
        }

        public static void CopyAllPrivateFiles(DirectoryInfo source, DirectoryInfo target)
        {
            // Check if the target directory exists, if not, create it.
            if (Directory.Exists(target.FullName) == false) Directory.CreateDirectory(target.FullName);

            // Copy each file into it’s new directory.
            foreach (var fi in source.GetFiles()) fi.CopyTo(Path.Combine(target.ToString(), fi.Name), true);

            // Copy each subdirectory using recursion.
            foreach (var diSourceSubDir in source.GetDirectories())
            {
                var nextTargetSubDir = target.CreateSubdirectory(diSourceSubDir.Name);
                CopyAllPrivateFiles(diSourceSubDir, nextTargetSubDir);
            }
        }


        public void InitializeIfViewModelsReady() { if (_mainViewModelInitialized && _mapViewModelInitialized && _layerListViewModelInitialized) Initialize(); }

        void Initialize()
        {
            Globals.AppSettings.VerifyExperimentsStillExist();

            // if (CurrentExtent != null) MediatorMessage.Send(MediatorMessage.SetCurrentExtent, new RectangleShape(CurrentExtent)););
            if (CurrentScale != 0) MediatorMessage.Send(MediatorMessage.SetCurrentScale, CurrentScale);
            if (MapLayers == null)
            {
                var appPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
                MapLayers = new ObservableCollection<MapLayerViewModel>
                            {
                                new ShapefileMapLayer
                                {
                                    LineColor = Colors.Beige,
                                    AreaStyle = AreaStyles.Country2,
                                    CanBeRemoved = false,
                                    CanBeReordered = true,
                                    CanChangeAreaColor = true,
                                    CanChangeLineColor = true,
                                    ShapefileName = Path.Combine(appPath, @"Sample GIS Data\Countries02.shp"),
                                    Name = "Base Map",
                                    LayerType = LayerType.BaseMap,
                                },
                            };
            }
            if (FileName != null)
            {
                var backgroundWorker = new BackgroundWorker();
                backgroundWorker.DoWork += delegate { ProcessTransmissionLossFieldFiles(LocalStorageRoot); };
                backgroundWorker.RunWorkerAsync();
            }
            MapLayerViewModel.Layers = MapLayers;
            InitializeEnvironment(true);
            AddScenarioFileCommand(ScenarioFileName);
            IsChanged = false;
            MediatorMessage.Send(MediatorMessage.SetExperiment, this);
            foreach (var transmissionLossField in TransmissionLossFields) MatchTransmissionLossFieldToAnalysisPoints(transmissionLossField);
            if (_deleteAllSpeciesLayersOnInitialize)
            {
                var layersToRemove = MapLayers.Where(layer => layer.LayerType == LayerType.Animal).ToList();
                foreach (var layer in layersToRemove) MapLayers.Remove(layer);
            }
            _isInitialized = true;
            CommandManager.InvalidateRequerySuggested();
        }

        void DisplayAnalysisPoint(AnalysisPoint curPoint)
        {
            var analysisPointName = string.Format("Analysis Point: [{0:0.###}, {1:0.###}]", curPoint.Latitude, curPoint.Longitude);
            var analysisPointLayer = (OverlayShapeMapLayer) MapLayers.FirstOrDefault(curLayer => curLayer.Name == analysisPointName);
            if (analysisPointLayer == null)
            {
                analysisPointLayer = new OverlayShapeMapLayer
                                     {
                                         Name = analysisPointName,
                                         LayerType = LayerType.AnalysisPoint,
                                         LineWidth = 1,
                                     };
                MapLayers.Add(analysisPointLayer);
            }
            
            analysisPointLayer.AnalysisPoint = curPoint;
            analysisPointLayer.CanBeRemoved = true;
            analysisPointLayer.CanBeReordered = true;
            analysisPointLayer.HasSettings = true;
            analysisPointLayer.CanChangeLineColor = true;
            analysisPointLayer.CanChangeLineWidth = true;
            analysisPointLayer.Validate();

            var sourcePoints = new List<EarthCoordinate>();
            analysisPointLayer.Clear();
            foreach (var soundSource in curPoint.SoundSources)
            {
                if (soundSource.ShouldBeCalculated)
                {
                    sourcePoints.Add(curPoint);
                    foreach (var radialBearing in soundSource.RadialBearings)
                    {
                        var endPoint = new EarthCoordinate(curPoint);
                        endPoint.Move(radialBearing, soundSource.Radius);
                        sourcePoints.Add(endPoint);
                        sourcePoints.Add(curPoint);
                    }
                    analysisPointLayer.Add(new OverlayLineSegments(sourcePoints.ToArray(), Colors.Red, 5, LineStyle.Solid));
                }
                analysisPointLayer.Done();
            }
            MediatorMessage.Send(MediatorMessage.RefreshLayer, analysisPointLayer);
        }

        void ScenarioFileChanged(object sender, FileSystemEventArgs e)
        {
            if ((e.ChangeType & WatcherChangeTypes.Created) == WatcherChangeTypes.Created) { }
            if ((e.ChangeType & WatcherChangeTypes.Deleted) == WatcherChangeTypes.Deleted) { }
            if ((e.ChangeType & WatcherChangeTypes.Changed) == WatcherChangeTypes.Changed) {}
            //ProcessTransmissionLossFieldFile(e.FullPath);}
            if ((e.ChangeType & WatcherChangeTypes.Renamed) == WatcherChangeTypes.Renamed) { }
            if ((e.ChangeType & WatcherChangeTypes.All) == WatcherChangeTypes.All) { }
            Debug.WriteLine("File: " + e.Name + " " + e.ChangeType);
            if (ScenarioReloadTimer == null)
                ScenarioReloadTimer = new Timer(ReloadScenarioFile, null, 1000, Timeout.Infinite);
        }

        void ReloadScenarioFile(object state)
        {
            ScenarioReloadTimer = null;
            LoadScenarioFile();
            _mainViewModelDispatcher.Invoke(DispatcherPriority.Background, (Action)(() => AddScenarioFileCommand(ScenarioFileName)));
        }

        void TransmissionLossFieldFileChanged(object sender, FileSystemEventArgs e)
        {
            if ((e.ChangeType & WatcherChangeTypes.Created) == WatcherChangeTypes.Created) { }
            if ((e.ChangeType & WatcherChangeTypes.Deleted) == WatcherChangeTypes.Deleted) { }
            if ((e.ChangeType & WatcherChangeTypes.Changed) == WatcherChangeTypes.Changed)
            {
                ProcessTransmissionLossFieldFile(e.FullPath);
            }
            if ((e.ChangeType & WatcherChangeTypes.Renamed) == WatcherChangeTypes.Renamed) { }
            if ((e.ChangeType & WatcherChangeTypes.All) == WatcherChangeTypes.All) { }
            //Debug.WriteLine("File: " + e.Name + " " + e.ChangeType);
        }

        void ProcessTransmissionLossFieldFiles(string directoryName) { foreach (var file in Directory.GetFiles(directoryName, "*.tlf")) ProcessTransmissionLossFieldFile(file); }

        void ProcessTransmissionLossFieldFile(string fileName)
        {
            var newField = TransmissionLossField.LoadHeader(fileName);
            if (TransmissionLossFields.Any(field => field.IDField == newField.IDField)) return;
            TransmissionLossFields.Add(newField);
            if (_isInitialized) MatchTransmissionLossFieldToAnalysisPoints(newField);
        }

        void MatchTransmissionLossFieldToAnalysisPoints(TransmissionLossField transmissionLossField)
        {
            foreach (var analysisPoint in AnalysisPoints.Where(analysisPoint => transmissionLossField.EarthCoordinate.Equals(analysisPoint)))
            {
                analysisPoint.TransmissionLossFields.Add(transmissionLossField);
                Console.WriteLine(string.Format("Matched TL Field @({0}, {1}) to analysis point @({2}, {3})", transmissionLossField.Latitude, transmissionLossField.Longitude, analysisPoint.Latitude, analysisPoint.Longitude));
                return;
            }
        }

        public void ClearAnalysisPoints()
        {
            MediatorMessage.Send(MediatorMessage.CancelCurrentTransmissionLossCalculation, true);
            var files = Directory.GetFiles(LocalStorageRoot, "*.tlf");
            foreach (var file in files) File.Delete(file);
            files = Directory.GetFiles(LocalStorageRoot, "*.bellhop");
            foreach (var file in files) File.Delete(file);
            files = Directory.GetFiles(LocalStorageRoot, "*.ram");
            foreach (var file in files) File.Delete(file);
            AnalysisPoints.Clear();
        }

        public void InitializeEnvironment(bool isFromInitialize)
        {
            if (!isFromInitialize && !_isInitialized) return;

#if false
            // Graham - here's how to get the physical memory size for the current system
            var info = new Microsoft.VisualBasic.Devices.ComputerInfo();
            var bytes = info.TotalPhysicalMemory;
            var megs = bytes >> 20;
#endif

            if (ScenarioFileWatcher != null) ScenarioFileWatcher.Dispose();
            if (ScenarioFileName != null)
            {
                ScenarioFileWatcher = new FileSystemWatcher(Path.GetDirectoryName(ScenarioFileName), "*.nemo");
                ScenarioFileWatcher.Changed += ScenarioFileChanged;
                ScenarioFileWatcher.EnableRaisingEvents = true;
            }

            if (TransmissionLossFileWatcher != null) TransmissionLossFileWatcher.Dispose();
            if (FileName != null)
            {
                TransmissionLossFileWatcher = new FileSystemWatcher(TransmissionLossFileRoot)
                {
                    NotifyFilter = NotifyFilters.LastAccess | NotifyFilters.LastWrite | NotifyFilters.FileName | NotifyFilters.DirectoryName,
                    Filter = "*.tlf",
                };
                TransmissionLossFileWatcher.Changed += TransmissionLossFieldFileChanged;
                TransmissionLossFileWatcher.Deleted += TransmissionLossFieldFileChanged;
                TransmissionLossFileWatcher.EnableRaisingEvents = true;
            }

            if (NemoFile == null) return;

            if (NemoModeToAcousticModelNameMap == null) NemoModeToAcousticModelNameMap = new NemoModeToAcousticModelNameMap(NemoFile.Scenario.DistinctModePSMNames, "CASS");
            else NemoModeToAcousticModelNameMap.UpdateModes(NemoFile.Scenario.DistinctModePSMNames, "CASS");

            if (NemoFile.Scenario.OverlayFile != null) OpArea = new GeoRect(NemoFile.Scenario.OverlayFile.Shapes[0].BoundingBox);
            else foreach (var trackdef in NemoFile.Scenario.Platforms.SelectMany(platform => platform.Trackdefs))
                    if (OpArea == null) OpArea = new GeoRect(trackdef.OverlayFile.Shapes[0].BoundingBox);
                    else OpArea.Union(trackdef.OverlayFile.Shapes[0].BoundingBox);
            if (SimArea == null) SimArea = new GeoRect(OpArea);

            if ((WindSpeedFileName != null) && (File.Exists(WindSpeedFileName)))
            {
                if (WindSpeedFileName.EndsWith(".eeb")) WindSpeed = Environment2DData.FromEEB(WindSpeedFileName, "windspeed", SimArea);
                else if (WindSpeedFileName.EndsWith(".txt")) WindSpeed = SurfaceMarineGriddedClimatologyDatabase.Parse(WindSpeedFileName);
            }

            if (WindSpeed != null)
            {
                const string windName = "Wind";
                var windLayer = (OverlayShapeMapLayer)MapLayers.FirstOrDefault(curLayer => curLayer.Name == windName);
                if (windLayer == null)
                {
                    windLayer = new OverlayShapeMapLayer
                    {
                        Name = windName,
                        CanBeReordered = true,
                        CanChangeLineColor = true,
                        CanChangeLineWidth = true,
                        CanBeRemoved = false,
                        LayerType = LayerType.WindSpeed,
                        IsChecked = false,
                    };
                    MapLayers.Add(windLayer);
                }
                windLayer.Clear();
                foreach (var lon in WindSpeed.Longitudes)
                    foreach (var lat in WindSpeed.Latitudes)
                        windLayer.Add(new OverlayPoint(new EarthCoordinate(lat, lon)));
                windLayer.Done();
            }

            if ((SoundSpeedFileName != null) && (File.Exists(SoundSpeedFileName)))
            {
                if (SoundSpeedFileName.EndsWith(".eeb")) SoundSpeedField = new SoundSpeedField(SoundSpeedFileName, SimArea);
                else if (SoundSpeedFileName.EndsWith(".xml")) 
                {
                    var rawSoundSpeeds = SerializedOutput.Load(SoundSpeedFileName, GeneralizedDigitalEnvironmentModelDatabase.ReferencedTypes);
                    SoundSpeedField = new SoundSpeedField(rawSoundSpeeds, NemoFile.Scenario.TimeFrame);
                }
            }
            if (SoundSpeedField != null)
            {
                const string soundSpeedName = "Sound Speed";
                var soundSpeedLayer = (OverlayShapeMapLayer)MapLayers.FirstOrDefault(curLayer => curLayer.Name == soundSpeedName);
                if (soundSpeedLayer == null)
                {
                    soundSpeedLayer = new OverlayShapeMapLayer
                                      {
                                          Name = soundSpeedName,
                                          CanBeReordered = true,
                                          CanChangeLineColor = true,
                                          CanChangeLineWidth = true,
                                          CanBeRemoved = false,
                                          LayerType = LayerType.WindSpeed,
                                          IsChecked = false,
                                      };
                    MapLayers.Add(soundSpeedLayer);
                }
                soundSpeedLayer.Clear();
                foreach (var soundSpeedProfile in SoundSpeedField.SoundSpeedProfiles) soundSpeedLayer.Add(new OverlayPoint(soundSpeedProfile));
                soundSpeedLayer.Done();
            }

            if ((BathymetryFileName != null) && (File.Exists(BathymetryFileName)))
            {
                if (BathymetryFileName.EndsWith(".eeb")) Bathymetry = Environment2DData.FromEEB(BathymetryFileName, "bathymetry", SimArea);
                else if (BathymetryFileName.EndsWith(".chb"))
                    Bathymetry = Environment2DData.FromCHB(BathymetryFileName, -1);
                else if (BathymetryFileName.EndsWith(".yxz"))
                    Bathymetry = Environment2DData.FromYXZ(BathymetryFileName, -1);
            }
            //Bathymetry = Environment2DData.ReadChrtrBinaryFile(@"C:\Users\Dave Anderson\Desktop\test.chb");
            if (Bathymetry != null)
            {
                const string bathyBoundsName = "Bathymetry: Boundary";
                var bathyBoundsLayer = (OverlayShapeMapLayer)MapLayers.FirstOrDefault(curLayer => curLayer.Name == bathyBoundsName);
                if (bathyBoundsLayer == null)
                {
                    bathyBoundsLayer = new OverlayShapeMapLayer
                    {
                        Name = bathyBoundsName,
                        CanBeReordered = true,
                        CanChangeLineColor = true,
                        CanChangeLineWidth = true,
                        CanBeRemoved = false,
                        LayerType = LayerType.WindSpeed,
                    };
                    MapLayers.Add(bathyBoundsLayer);
                }
                bathyBoundsLayer.Clear();
                bathyBoundsLayer.Add(Bathymetry.BoundingBox);
                bathyBoundsLayer.Done();

                const string bathyBitmapName = "Bathymetry: Bitmap";
                var colormap = new DualColormap(Colormap.Summer, Colormap.Jet)
                {
                    Threshold = 0,
                };
                var bathysize = Math.Max(Bathymetry.Longitudes.Count, Bathymetry.Latitudes.Count);
                var screenSize = Math.Min(SystemParameters.PrimaryScreenWidth, SystemParameters.PrimaryScreenHeight);
                Bitmap displayBitmap;
                float horizontalResolution;
                if (bathysize > screenSize)
                {
                    var scaleFactor = screenSize/bathysize;
                    var decimatedValues = Decimator2D.Decimate(Bathymetry.FieldData, (int)(Bathymetry.Longitudes.Count * scaleFactor), (int)(Bathymetry.Latitudes.Count * scaleFactor));
                    horizontalResolution = (float)(Bathymetry.LongitudinalResolution / ((double)decimatedValues.GetLength(0) / Bathymetry.Longitudes.Count));
                    displayBitmap = colormap.ToBitmap(decimatedValues, Bathymetry.Minimum.Data, Bathymetry.Maximum.Data < 0 ? Bathymetry.Maximum.Data : 8000);
                }
                else
                {
                    displayBitmap = colormap.ToBitmap(Bathymetry.FieldData, Bathymetry.Minimum.Data, Bathymetry.Maximum.Data < 0 ? Bathymetry.Maximum.Data : 8000);
                    horizontalResolution = (float) Bathymetry.LongitudinalResolution;
                }
                displayBitmap.Save(Path.Combine(LocalStorageRoot, "bathy.bmp"), ImageFormat.Bmp);
                var bathyBitmapLayer = (RasterMapLayer)MapLayers.FirstOrDefault(curLayer => curLayer.Name == bathyBitmapName);
                if (bathyBitmapLayer == null)
                {
                    bathyBitmapLayer = new RasterMapLayer
                    {
                        Name = bathyBitmapName,
                        CanBeReordered = true,
                        CanChangeLineColor = false,
                        CanChangeLineWidth = false,
                        CanBeRemoved = false,
                        LayerType = LayerType.BathymetryRaster,
                    };
                    MapLayers.Add(bathyBitmapLayer);
                }
                bathyBitmapLayer.PixelSize = horizontalResolution;
                bathyBitmapLayer.North = (float)Bathymetry.Latitudes.Last();
                bathyBitmapLayer.South = (float)Bathymetry.Latitudes.First();
                bathyBitmapLayer.East = (float)Bathymetry.Longitudes.Last();
                bathyBitmapLayer.West = (float)Bathymetry.Longitudes.First();
                bathyBitmapLayer.RasterFilename = Path.Combine(LocalStorageRoot, "bathy.bmp");
                MediatorMessage.Send(MediatorMessage.MoveLayerToBottom, bathyBitmapLayer);
            }
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }

        #region ViewAnalysisPointCommand

        SimpleCommand<object, AnalysisPoint> _viewAnalysisPointCommand;

        public SimpleCommand<object, AnalysisPoint> ViewAnalysisPointCommand
        {
            get { return _viewAnalysisPointCommand ?? (_viewAnalysisPointCommand = new SimpleCommand<object, AnalysisPoint>(analysisPoint => MediatorMessage.Send(MediatorMessage.ViewAnalysisPoint, analysisPoint))); }
        }

        #endregion

        #region RecalculateAnalysisPointCommand

        SimpleCommand<object, object> _recalculateAnalysisPoint;

        public SimpleCommand<object, object> RecalculateAnalysisPointCommand
        {
            get { return _recalculateAnalysisPoint ?? (_recalculateAnalysisPoint = new SimpleCommand<object, object>(analysisPoint => MediatorMessage.Send(MediatorMessage.CalculateAnalysisPoint, analysisPoint))); }
        }

        #endregion

        #region DeleteAnalysisPointCommand

        SimpleCommand<object, AnalysisPoint> _deleteAnalysisPointCommand;

        public SimpleCommand<object, AnalysisPoint> DeleteAnalysisPointCommand
        {
            get { return _deleteAnalysisPointCommand ?? (_deleteAnalysisPointCommand = new SimpleCommand<object, AnalysisPoint>(ap => MediatorMessage.Send(MediatorMessage.RemoveAnalysisPoint, ap))); }
        }

        #endregion
    }
}