using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Xml.Serialization;
using Cinch;
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

namespace ESMEWorkBench.Data
{
    [Serializable]
    public partial class Experiment : SerializableData<Experiment>
    {
        static Type[] _referencedTypes;
        static readonly PropertyChangedEventArgs TransmissionLossFieldsChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.TransmissionLossFields);

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
                InitializeEnvironment(false);
            }
        }

        #endregion

        #region public string TemperatureFileName { get; set; }

        public string TemperatureFileName
        {
            get { return _temperatureFileName; }
            set
            {
                if (_temperatureFileName == value) return;
                _temperatureFileName = value;
                NotifyPropertyChanged(TemperatureFileNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TemperatureFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.TemperatureFileName);
        string _temperatureFileName;

        #endregion

        #region public string SalinityFileName { get; set; }

        public string SalinityFileName
        {
            get { return _salinityFileName; }
            set
            {
                if (_salinityFileName == value) return;
                _salinityFileName = value;
                NotifyPropertyChanged(SalinityFileNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SalinityFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.SalinityFileName);
        string _salinityFileName;

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
                InitializeEnvironment(false);
            }
        }

        #endregion

        #region public string SedimentFileName { get; set; }

        [XmlIgnore] static readonly PropertyChangedEventArgs BottomTypeFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.SedimentFileName);
        [XmlIgnore] string _sedimentFileName;

        [XmlElement]
        public string SedimentFileName
        {
            get { return _sedimentFileName; }
            set
            {
                if (_sedimentFileName == value) return;
                _sedimentFileName = value;
                NotifyPropertyChanged(BottomTypeFileNameChangedEventArgs);
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
                if ((_scenarioFileName != null) && (Globals.AppSettings.ScenarioDataDirectory != null) && File.Exists(_scenarioFileName) && Directory.Exists(Globals.AppSettings.ScenarioDataDirectory)) NemoFile = new NemoFile(_scenarioFileName, Globals.AppSettings.ScenarioDataDirectory);
                NotifyPropertyChanged(ScenarioFileNameChangedEventArgs);
                InitializeEnvironment(false);
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
                    if (e.NewItems != null)
                    {
                        foreach (var item in e.NewItems)
                        {
                            var layer = (MapLayerViewModel) item;
                            if (layer.LayerType == LayerType.AnalysisPoint)
                            {
                                if (AnalysisPointLayer == null)
                                {
                                    AnalysisPointLayer = (MarkerLayerViewModel) layer;
                                    AnalysisPointLayer.MarkerImageUri = new Uri("pack://application:,,,/ESME WorkBench;component/Images/AQUA.png");
                                    if (AnalysisPoints != null)
                                    {
                                        foreach (var ap in AnalysisPoints) AddContextMenuToAnalysisPoint(ap);
                                    }
                                }
                                else throw new ApplicationException("Experiment error: Analysis point layer already exists!");
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
                        if (AnalysisPointLayer == null) return;
                        foreach (var item in e.NewItems) AddContextMenuToAnalysisPoint((AnalysisPoint) item);
                    }
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
                case NotifyCollectionChangedAction.Remove:
                    if (e.OldItems != null)
                    {
                        if (AnalysisPointLayer == null) return;
                        foreach (var item in e.OldItems) AnalysisPointLayer.RemoveMarker(item);
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    AnalysisPointLayer.Clear();
                    break;
            }
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }

        void AddContextMenuToAnalysisPoint(AnalysisPoint analysisPoint)
        {
            var marker = AnalysisPointLayer.AddMarker(analysisPoint.EarthCoordinate, analysisPoint);
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

        public TransmissionLossField NearestMatchingTransmissionLoss(NemoMode nemoMode, EarthCoordinate location)
        {
            TransmissionLossField nearestMatch = null;
            foreach (var analysisPoint in AnalysisPoints)
                foreach (var transmissionLossField in analysisPoint.TransmissionLossFields)
                    if (transmissionLossField.IsAcousticMatchFor(nemoMode))
                    {
                        if (nearestMatch == null) nearestMatch = transmissionLossField;
                        else if (location.GetDistanceTo_Meters(nearestMatch.EarthCoordinate) > location.GetDistanceTo_Meters(transmissionLossField.EarthCoordinate)) nearestMatch = transmissionLossField;
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

        #region public double BellhopRangeCellSize { get; set; }

        static readonly PropertyChangedEventArgs BellhopRangeCellSizeChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.BellhopRangeCellSize);
        double _bellhopRangeCellSize = 50.0;

        public double BellhopRangeCellSize
        {
            get { return _bellhopRangeCellSize; }
            set
            {
                if (_bellhopRangeCellSize == value) return;
                _bellhopRangeCellSize = value;
                NotifyPropertyChanged(BellhopRangeCellSizeChangedEventArgs);
            }
        }

        #endregion

        #region public double BellhopDepthCellSize { get; set; }

        static readonly PropertyChangedEventArgs BellhopDepthCellSizeChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.BellhopDepthCellSize);
        double _bellhopDepthCellSize = 50.0;

        public double BellhopDepthCellSize
        {
            get { return _bellhopDepthCellSize; }
            set
            {
                if (_bellhopDepthCellSize == value) return;
                _bellhopDepthCellSize = value;
                NotifyPropertyChanged(BellhopDepthCellSizeChangedEventArgs);
            }
        }

        #endregion

        #region public float OpAreaBufferZoneSize { get; set; }
        /// <summary>
        /// Size of the buffer zone around operational area bounding box that will be used to extract environmental data
        /// from the selected environmental data source
        /// Units: Degrees
        /// Default value: 2.0
        /// </summary>
        public float OpAreaBufferZoneSize
        {
            get { return _opAreaBufferZoneSize; }
            set
            {
                if (_opAreaBufferZoneSize == value) return;
                _opAreaBufferZoneSize = value;
                NotifyPropertyChanged(OpAreaBufferZoneSizeChangedEventArgs);
                // TODO: If/when this value changes, we will need to do the following
                //     - Compute the new bounding box we will be using
                //     - Verify that all AnalysisPoints are completely contained inside the new bounding box, including the endpoints of every radial
                //     - If that verification fails, throw an exception containing descriptive error text about exactly what has failed
                //     - Re-extract the environmental data from the selected environmental data source
                //     - Re-initialize the environmental data
            }
        }

        static readonly PropertyChangedEventArgs OpAreaBufferZoneSizeChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.OpAreaBufferZoneSize);
        float _opAreaBufferZoneSize = 2.5f;

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
                                                                   typeof (MapLayerViewModel), typeof (ShapefileMapLayer), typeof (OverlayShapeMapLayer), typeof (OverlayFileMapLayer), typeof (MarkerLayerViewModel), typeof(RasterMapLayer),
                                                               });
            }
        }

        [XmlIgnore]
        public float North { get; private set; }

        [XmlIgnore]
        public float South { get; private set; }

        [XmlIgnore]
        public float East { get; private set; }

        [XmlIgnore]
        public float West { get; private set; }

        [XmlIgnore]
        public AnimatInterface AnimatInterface { get; set; }

        [XmlIgnore]
        public MarkerLayerViewModel AnalysisPointLayer { get; private set; }

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

        [XmlIgnore]
        public Environment2DData Bathymetry { get; private set; }

        [XmlIgnore]

        #region public string LocalStorageRoot { get; set; }

        public string LocalStorageRoot
        {
            get
            {
                if (FileName == null) throw new ApplicationException("The new experiment must be saved before performing this operation");
                var localStorageRoot = Path.Combine(Path.GetDirectoryName(FileName), Path.GetFileNameWithoutExtension(FileName));
                if (!Directory.Exists(localStorageRoot))
                {
                    var directoryInfo = Directory.CreateDirectory(localStorageRoot);
                    directoryInfo.Attributes = FileAttributes.Directory | FileAttributes.Hidden;
                }
                if (FileSystemWatcher == null)
                {
                    FileSystemWatcher = new FileSystemWatcher(localStorageRoot)
                                        {
                                            NotifyFilter = NotifyFilters.LastAccess | NotifyFilters.LastWrite | NotifyFilters.FileName | NotifyFilters.DirectoryName,
                                            Filter = "*.tlf",
                                        };
                    FileSystemWatcher.Changed += TransmissionLossFieldFileChanged;
                    FileSystemWatcher.Deleted += TransmissionLossFieldFileChanged;
                    FileSystemWatcher.EnableRaisingEvents = true;
                }
                return localStorageRoot;
            }
        }

        #endregion

        [XmlIgnore]

        #region public ObservableCollection<TransmissionLossField> TransmissionLossFields { get; set; }

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

        [XmlIgnore]
        FileSystemWatcher FileSystemWatcher { get; set; }

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
            if (FileSystemWatcher != null)
            {
                FileSystemWatcher.EnableRaisingEvents = false;
                FileSystemWatcher = null;
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
            // if (CurrentExtent != null) MediatorMessage.Send(MediatorMessage.SetCurrentExtent, new RectangleShape(CurrentExtent));
            if (CurrentScale != 0) MediatorMessage.Send(MediatorMessage.SetCurrentScale, CurrentScale);
            if (AnalysisPointLayer == null)
                AnalysisPointLayer = new MarkerLayerViewModel
                                     {
                                         Name = "Analysis Points",
                                         CanBeRemoved = false,
                                         CanBeReordered = true,
                                         CanChangeAreaColor = false,
                                         CanChangeLineColor = false,
                                         LayerType = LayerType.AnalysisPoint,
                                         MarkerImageUri = new Uri("pack://application:,,,/ESME WorkBench;component/Images/AQUA.png"),
                                     };
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
                                AnalysisPointLayer,
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

        void TransmissionLossFieldFileChanged(object sender, FileSystemEventArgs e)
        {
            if ((e.ChangeType & WatcherChangeTypes.Created) == WatcherChangeTypes.Created) {}
            if ((e.ChangeType & WatcherChangeTypes.Deleted) == WatcherChangeTypes.Deleted) {}
            if ((e.ChangeType & WatcherChangeTypes.Changed) == WatcherChangeTypes.Changed)
            {
                ProcessTransmissionLossFieldFile(e.FullPath);
            }
            if ((e.ChangeType & WatcherChangeTypes.Renamed) == WatcherChangeTypes.Renamed) {}
            if ((e.ChangeType & WatcherChangeTypes.All) == WatcherChangeTypes.All) {}
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
            foreach (var analysisPoint in AnalysisPoints.Where(analysisPoint => transmissionLossField.EarthCoordinate.Equals(analysisPoint.EarthCoordinate)))
            {
                analysisPoint.TransmissionLossFields.Add(transmissionLossField);
                Console.WriteLine(string.Format("Matched TL Field @({0}, {1}) to analysis point @({2}, {3})", transmissionLossField.Latitude, transmissionLossField.Longitude, analysisPoint.EarthCoordinate.Latitude_degrees, analysisPoint.EarthCoordinate.Longitude_degrees));
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

        void InitializeEnvironment(bool isFromInitialize)
        {
            if (!isFromInitialize && !_isInitialized) return;

            if (NemoFile == null) return;
            var boundingBox = new Rect();
            if (NemoFile.Scenario.OverlayFile != null) boundingBox = NemoFile.Scenario.OverlayFile.Shapes[0].BoundingBox;
            else
            {
                foreach (var platform in NemoFile.Scenario.Platforms)
                    foreach (var trackdef in platform.Trackdefs)
                    {
                        if ((boundingBox.Width == 0) && (boundingBox.Height == 0)) boundingBox = trackdef.OverlayFile.Shapes[0].BoundingBox;
                        else boundingBox.Union(trackdef.OverlayFile.Shapes[0].BoundingBox);
                    }
            }

            North = (float)boundingBox.Bottom + OpAreaBufferZoneSize;
            West = (float)boundingBox.Left - OpAreaBufferZoneSize;
            South = (float)boundingBox.Top - OpAreaBufferZoneSize;
            East = (float)boundingBox.Right + OpAreaBufferZoneSize;

            if ((WindSpeedFileName != null) && (File.Exists(WindSpeedFileName)))
            {
                if (WindSpeedFileName.EndsWith(".eeb")) WindSpeed = new Environment2DData(WindSpeedFileName, "windspeed", North, West, South, East);
                else if (WindSpeedFileName.EndsWith(".txt")) WindSpeed = SurfaceMarineGriddedClimatologyDatabase.Parse(WindSpeedFileName);
            }

            if (WindSpeed != null)
            {
                const string windName = "Wind";
                var layerExists = false;
                foreach (var windLayer in MapLayers.Where(curLayer => curLayer.Name == windName).Cast<OverlayShapeMapLayer>())
                {
                    for (var lonIndex = 0; lonIndex < WindSpeed.Longitudes.Length; lonIndex++)
                        for (var latIndex = 0; latIndex < WindSpeed.Latitudes.Length; latIndex++)
                            windLayer.Add(new OverlayPoint(new EarthCoordinate(WindSpeed.Latitudes[latIndex], WindSpeed.Longitudes[lonIndex])));
                    windLayer.Done();
                    layerExists = true;
                }
                if (!layerExists)
                {
                    var windLayer = new OverlayShapeMapLayer
                    {
                        Name = windName,
                        CanBeReordered = true,
                        CanChangeLineColor = true,
                        CanChangeLineWidth = true,
                        CanBeRemoved = false,
                        LayerType = LayerType.WindSpeed,
                    };
                    for (var lonIndex = 0; lonIndex < WindSpeed.Longitudes.Length; lonIndex++)
                        for (var latIndex = 0; latIndex < WindSpeed.Latitudes.Length; latIndex++)
                            windLayer.Add(new OverlayPoint(new EarthCoordinate(WindSpeed.Latitudes[latIndex], WindSpeed.Longitudes[lonIndex])));
                    windLayer.Done();
                    MapLayers.Add(windLayer);
                    windLayer.IsChecked = false;
                }
            }

            if ((SedimentFileName != null) && (File.Exists(SedimentFileName)))
            {
                if (SedimentFileName.EndsWith(".eeb")) Sediment = Sediment.ReadESMEEnvironmentBinaryFile(SedimentFileName, North, South, East, West);
                else if (SedimentFileName.EndsWith(".chb")) Sediment = Sediment.ReadChrtrBinaryFile(SedimentFileName);
            }
            if (Sediment != null)
            {
                const string bottomTypeName = "Bottom Type";
                var bottomTypeLayerExists = false;
                foreach (var bottomTypeLayer in MapLayers.Where(curLayer => curLayer.Name == bottomTypeName).Cast<OverlayShapeMapLayer>())
                {
                    foreach (var sample in Sediment.SedimentSamples)
                        bottomTypeLayer.Add(new OverlayPoint(sample));
                    bottomTypeLayer.Done();
                    bottomTypeLayerExists = true;
                }
                if (!bottomTypeLayerExists)
                {
                    var bottomTypeLayer = new OverlayShapeMapLayer
                    {
                        Name = bottomTypeName,
                        CanBeReordered = true,
                        CanChangeLineColor = true,
                        CanChangeLineWidth = true,
                        CanBeRemoved = false,
                        LayerType = LayerType.BottomType,
                    };
                    foreach (var sample in Sediment.SedimentSamples)
                        bottomTypeLayer.Add(new OverlayPoint(sample));
                    bottomTypeLayer.Done();
                    MapLayers.Add(bottomTypeLayer);
                    bottomTypeLayer.IsChecked = false;
                }
            }

            if ((SoundSpeedFileName != null) && (File.Exists(SoundSpeedFileName)))
            {
                if (SoundSpeedFileName.EndsWith(".eeb")) SoundSpeedField = new SoundSpeedField(SoundSpeedFileName, North, West, South, East);
                else if (SoundSpeedFileName.EndsWith(".xml")) 
                {
                    var rawSoundSpeeds = SerializedOutput.Load(SoundSpeedFileName, null);
                    SoundSpeedField = new SoundSpeedField(rawSoundSpeeds, NemoFile.Scenario.TimeFrame);
                    if (Bathymetry != null)
                        SoundSpeedField.ExtendProfilesToDepth(Bathymetry.MaxValue, SerializedOutput.Load(TemperatureFileName, null), SerializedOutput.Load(SalinityFileName, null));
                }
            }
            if (SoundSpeedField != null)
            {
                const string soundSpeedName = "Sound Speed";
                var soundSpeedLayerExists = false;
                foreach (var soundSpeedLayer in MapLayers.Where(curLayer => curLayer.Name == soundSpeedName).Cast<OverlayShapeMapLayer>())
                {
                    foreach (var soundSpeedProfile in SoundSpeedField.SoundSpeedProfiles) soundSpeedLayer.Add(new OverlayPoint(soundSpeedProfile));
                    soundSpeedLayer.Done();
                    soundSpeedLayerExists = true;
                }
                if (!soundSpeedLayerExists)
                {
                    var soundSpeedLayer = new OverlayShapeMapLayer
                    {
                        Name = soundSpeedName,
                        CanBeReordered = true,
                        CanChangeLineColor = true,
                        CanChangeLineWidth = true,
                        CanBeRemoved = false,
                        LayerType = LayerType.SoundSpeed,
                    };
                    foreach (var soundSpeedProfile in SoundSpeedField.SoundSpeedProfiles) soundSpeedLayer.Add(new OverlayPoint(soundSpeedProfile));
                    soundSpeedLayer.Done();
                    MapLayers.Add(soundSpeedLayer);
                }
                //SoundSpeedField.Save(Path.Combine(LocalStorageRoot, "ssf.xml"));
                //var testSSF = SoundSpeedField.Load(Path.Combine(LocalStorageRoot, "ssf.xml"));
            }

            if ((BathymetryFileName != null) && (File.Exists(BathymetryFileName)))
            {
                if (BathymetryFileName.EndsWith(".eeb")) Bathymetry = new Environment2DData(BathymetryFileName, "bathymetry", North, West, South, East);
                else if (BathymetryFileName.EndsWith(".chb"))
                {
                    Bathymetry = Environment2DData.ReadChrtrBinaryFile(BathymetryFileName, -1);
                    if (SoundSpeedField != null)
                        SoundSpeedField.ExtendProfilesToDepth(Math.Abs(Bathymetry.MaxValue), SerializedOutput.Load(TemperatureFileName, null), SerializedOutput.Load(SalinityFileName, null));
                }
            }
            //Bathymetry = Environment2DData.ReadChrtrBinaryFile(@"C:\Users\Dave Anderson\Desktop\test.chb");
            if (Bathymetry != null)
            {
                const string bathyBoundsName = "Bathymetry: Boundary";
                var boundsLayerExists = false;
                foreach (var bathyBoundsLayer in MapLayers.Where(curLayer => curLayer.Name == bathyBoundsName).Cast<OverlayShapeMapLayer>())
                {
                    bathyBoundsLayer.Add(Bathymetry.BoundingBox);
                    bathyBoundsLayer.Done();
                    boundsLayerExists = true;
                }
                if (!boundsLayerExists)
                {
                    var layer = new OverlayShapeMapLayer
                                {
                                    Name = bathyBoundsName,
                                    CanBeReordered = true,
                                    CanChangeLineColor = true,
                                    CanChangeLineWidth = true,
                                    CanBeRemoved = false,
                                    LayerType = LayerType.Bathymetry,
                                };
                    layer.Add(Bathymetry.BoundingBox);
                    layer.Done();
                    MapLayers.Add(layer);
                }

                const string bathyBitmapName = "Bathymetry: Bitmap";
                var bitmapLayerExists = false;
                var colormap = new DualColormap(Colormap.Summer, Colormap.Jet)
                {
                    Threshold = 0,
                };
                var bathysize = Math.Max(Bathymetry.Values.GetLength(0), Bathymetry.Values.GetLength(1));
                var screenSize = Math.Min(SystemParameters.PrimaryScreenWidth, SystemParameters.PrimaryScreenHeight);
                Bitmap displayBitmap;
                float horizontalResolution;
                if (bathysize > screenSize)
                {
                    var scaleFactor = screenSize/bathysize;
                    var decimatedValues = Decimator2D.Decimate(Bathymetry.Values, (int)(Bathymetry.Values.GetLength(0) *scaleFactor), (int)(Bathymetry.Values.GetLength(1) * scaleFactor));
                    horizontalResolution = (float)(Bathymetry.HorizontalResolution / ((double)decimatedValues.GetLength(0) / Bathymetry.Values.GetLength(0)));
                    displayBitmap = colormap.ToBitmap(decimatedValues, Bathymetry.MinValue, Bathymetry.MaxValue < 0 ? Bathymetry.MaxValue : 8000);
                }
                else
                {
                    displayBitmap = colormap.ToBitmap(Bathymetry.Values, Bathymetry.MinValue, Bathymetry.MaxValue < 0 ? Bathymetry.MaxValue : 8000);
                    horizontalResolution = (float) Bathymetry.HorizontalResolution;
                }
                displayBitmap.Save(Path.Combine(LocalStorageRoot, "bathy.bmp"), ImageFormat.Bmp);

                foreach (var bathyBitmapLayer in MapLayers.Where(curLayer => curLayer.Name == bathyBitmapName).Cast<RasterMapLayer>())
                {
                    bathyBitmapLayer.RasterFilename = Path.Combine(LocalStorageRoot, "bathy.bmp");
                    bathyBitmapLayer.PixelSize = horizontalResolution;
                    bathyBitmapLayer.North = (float)Bathymetry.Latitudes.Last();
                    bathyBitmapLayer.South = (float)Bathymetry.Latitudes.First();
                    bathyBitmapLayer.East = (float)Bathymetry.Longitudes.Last();
                    bathyBitmapLayer.West = (float)Bathymetry.Longitudes.First();
                    bitmapLayerExists = true;
                }
                if (!bitmapLayerExists)
                {
                    var rasterLayer = new RasterMapLayer
                                      {
                                          Name = bathyBitmapName,
                                          CanBeReordered = true,
                                          CanChangeLineColor = false,
                                          CanChangeLineWidth = false,
                                          CanBeRemoved = false,
                                          LayerType = LayerType.BathymetryRaster,
                                          PixelSize = horizontalResolution,
                                          North = (float)Bathymetry.Latitudes.Last(),
                                          South = (float)Bathymetry.Latitudes.First(),
                                          East = (float)Bathymetry.Longitudes.Last(),
                                          West = (float)Bathymetry.Longitudes.First(),
                                          RasterFilename = Path.Combine(LocalStorageRoot, "bathy.bmp"),
                                      };
                    MapLayers.Add(rasterLayer);
                    //Also send the layer to the back.
                    MediatorMessage.Send(MediatorMessage.MoveLayerToBottom, rasterLayer);
                }
            }
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
            get { return _deleteAnalysisPointCommand ?? (_deleteAnalysisPointCommand = new SimpleCommand<object, AnalysisPoint>(ap => MediatorMessage.Send(MediatorMessage.DeleteAnalysisPoint, ap))); }
        }

        #endregion
    }
}