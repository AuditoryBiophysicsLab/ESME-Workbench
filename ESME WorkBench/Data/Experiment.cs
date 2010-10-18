using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Xml.Serialization;
using Cinch;
using ESME.Environment;
using ESME.Model;
using ESME.NEMO;
using ESME.Overlay;
using ESME.TransmissionLoss;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.Main;
using ESMEWorkBench.ViewModels.Map;
using ThinkGeo.MapSuite.Core;
using MapShapeLayer = ESMEWorkBench.ViewModels.Map.MapShapeLayer;

namespace ESMEWorkBench.Data
{
    [Serializable]
    public partial class Experiment : SerializableData<Experiment>
    {
        public static Type[] ReferencedTypes
        {
            get
            {
                return _referencedTypes ?? (_referencedTypes = new[]
                                                               {
                                                                   typeof (MapLayerViewModel), typeof (ShapefileMapLayer), typeof (OverlayShapeMapLayer), typeof (OverlayFileMapLayer), typeof (MarkerLayerViewModel)
                                                               });
            }
        }

        static Type[] _referencedTypes;
        #region public string Comments { get; set; }

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

        [XmlIgnore] static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.Comments);
        [XmlIgnore] string _comments;

        #endregion

        #region public string Author { get; set; }

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

        [XmlIgnore] static readonly PropertyChangedEventArgs AuthorChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.Author);
        [XmlIgnore] string _author;

        #endregion

        #region public DateTime Created { get; set; }

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

        [XmlIgnore] static readonly PropertyChangedEventArgs CreatedChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.Created);
        [XmlIgnore] DateTime _created;

        #endregion

        #region public DateTime LastModified { get; set; }

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

        [XmlIgnore] static readonly PropertyChangedEventArgs LastModifiedChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.LastModified);
        [XmlIgnore] DateTime _lastModified;

        #endregion

        #region public string ModifiedBy { get; set; }

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

        [XmlIgnore] static readonly PropertyChangedEventArgs ModifiedByChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.ModifiedBy);
        [XmlIgnore] string _modifiedBy;

        #endregion

        #region public string WindSpeedFileName { get; set; }

        [XmlElement]
        public string WindSpeedFileName
        {
            get { return _windSpeedFileName; }
            set
            {
                if (_windSpeedFileName == value) return;
                _windSpeedFileName = value;
                NotifyPropertyChanged(WindSpeedFileNameChangedEventArgs);
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs WindSpeedFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.WindSpeedFileName);
        [XmlIgnore] string _windSpeedFileName;

        #endregion

        #region public string SoundSpeedFileName { get; set; }

        [XmlElement]
        public string SoundSpeedFileName
        {
            get { return _soundSpeedFileName; }
            set
            {
                if (_soundSpeedFileName == value) return;
                _soundSpeedFileName = value;
                NotifyPropertyChanged(SoundSpeedFileNameChangedEventArgs);
                InitializeEnvironment();
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs SoundSpeedFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.SoundSpeedFileName);
        [XmlIgnore] string _soundSpeedFileName;

        #endregion

        #region public string BottomTypeFileName { get; set; }

        [XmlElement]
        public string BottomTypeFileName
        {
            get { return _bottomTypeFileName; }
            set
            {
                if (_bottomTypeFileName == value) return;
                _bottomTypeFileName = value;
                NotifyPropertyChanged(BottomTypeFileNameChangedEventArgs);
                InitializeEnvironment();
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs BottomTypeFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.BottomTypeFileName);
        [XmlIgnore] string _bottomTypeFileName;

        #endregion

        #region public string BathymetryFileName { get; set; }

        [XmlElement]
        public string BathymetryFileName
        {
            get { return _bathymetryFileName; }
            set
            {
                if (_bathymetryFileName == value) return;
                _bathymetryFileName = value;
                NotifyPropertyChanged(BathymetryFileNameChangedEventArgs);
                InitializeEnvironment();
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs BathymetryFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.BathymetryFileName);
        [XmlIgnore] string _bathymetryFileName;

        #endregion

        #region public string ScenarioFileName { get; set; }

        [XmlElement]
        public string ScenarioFileName
        {
            get { return _scenarioFileName; }
            set
            {
                if (_scenarioFileName == value) return;
                _scenarioFileName = value;
                if ((_scenarioFileName != null) && (Globals.AppSettings.ScenarioDataDirectory != null) && File.Exists(_scenarioFileName) && Directory.Exists(Globals.AppSettings.ScenarioDataDirectory)) 
                    NemoFile = new NemoFile(_scenarioFileName, Globals.AppSettings.ScenarioDataDirectory);
                NotifyPropertyChanged(ScenarioFileNameChangedEventArgs);
                InitializeEnvironment();
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs ScenarioFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.ScenarioFileName);
        [XmlIgnore] string _scenarioFileName;

        [XmlIgnore]
        public NemoFile NemoFile { get; private set; }

        #endregion

        #region public ObservableCollection<MapLayerViewModel> MapLayers { get; set; }

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
        static readonly PropertyChangedEventArgs MapLayersChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.MapLayers);
        ObservableCollection<MapLayerViewModel> _mapLayers;

        #endregion

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
                        foreach (var item in e.NewItems)
                        {
                            AnalysisPointLayer.AddMarker(((AnalysisPoint) item).Location, item);
                        }
                    }
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
                case NotifyCollectionChangedAction.Remove:
                    if (e.OldItems != null)
                    {
                        if (AnalysisPointLayer == null) return;
                        foreach (var item in e.OldItems)
                        {
                            AnalysisPointLayer.RemoveMarker(item);
                        }
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    break;
            }
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        static readonly PropertyChangedEventArgs AnalysisPointsChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.AnalysisPoints);
        ObservableCollection<AnalysisPoint> _analysisPoints;

        #endregion

        [XmlIgnore]
        public MarkerLayerViewModel AnalysisPointLayer { get; private set; }

        #region public bool IsChanged { get; set; }

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

        [XmlIgnore] static readonly PropertyChangedEventArgs IsChangedChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.IsChanged);
        [XmlIgnore] bool _isChanged;

        #endregion

        [XmlIgnore]
        bool _isInitialized;

        [XmlElement]
        public string CurrentExtent { get; set; }

        [XmlElement]
        public double CurrentScale { get; set; }

        [XmlIgnore]
        public IMessageBoxService MessageBoxService { private get; set; }

        [XmlIgnore]
        public Environment2DData Bathymetry { get; private set; }

        [XmlIgnore]
        public SoundSpeedField SoundSpeedField { get; private set; }

        [XmlIgnore]
        public string LocalStorageRoot { get; private set; }

        public Experiment()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine("***********\nExperiment: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
            Author = Environment.UserName;
            Created = DateTime.Now;
            PropertyChanged += delegate(object s, PropertyChangedEventArgs e) { if (e.PropertyName != "IsChanged") IsChanged = true; };
            CurrentExtent = "POLYGON((-173.84765625 123.442822265625,169.98046875 123.442822265625,169.98046875 -165.555615234375,-173.84765625 -165.555615234375,-173.84765625 123.442822265625))";
            CurrentScale = 147647947.5;
        }

        public Experiment(string fileName)
            : this()
        {
            FileName = fileName;
        }

        public Experiment(Experiment that) { CopyFrom(that); }

        public void Save()
        {
            SaveAs(FileName);
        }

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

        public void InitializeIfViewModelsReady()
        {
            if (_mainViewModelInitialized && _mapViewModelInitialized && _layerListViewModelInitialized)
                Initialize();
        }

        void Initialize()
        {
            if (CurrentExtent != null) MediatorMessage.Send(MediatorMessage.SetCurrentExtent, new RectangleShape(CurrentExtent));
            if (CurrentScale != 0) MediatorMessage.Send(MediatorMessage.SetCurrentScale, CurrentScale);
            if (MapLayers == null)
            {
                var appPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
                AnalysisPointLayer = new MarkerLayerViewModel
                                     {
                                         Name="Analysis Points",
                                         CanBeRemoved = false,
                                         CanBeReordered = true,
                                         CanChangeAreaColor = false,
                                         CanChangeLineColor = false,
                                         LayerType = LayerType.AnalysisPoint,
                                     };
                MapLayers = new ObservableCollection<MapLayerViewModel>
                            {
                                new ShapefileMapLayer
                                {
                                    LineColor = Colors.Tan,
                                    AreaColor = Colors.LightGreen,
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
            foreach (var layer in MapLayers.Where(layer => layer.Name == "Analysis Points"))
            {
                AnalysisPointLayer = (MarkerLayerViewModel) layer;
                AnalysisPointLayer.MarkerImageUri = new Uri("pack://application:,,,/ESME WorkBench;component/Images/AQUA.png");
                if (AnalysisPoints != null)
                {
                    foreach (var ap in AnalysisPoints)
                    {
                        var marker = AnalysisPointLayer.AddMarker(ap.Location, ap);
                        marker.ContextMenu = new ContextMenu();
                        marker.ContextMenu.Items.Add(new MenuItem
                        {
                            Header = "Run transmission loss...",
                            Command = RunTransmissionLossCommand,
                            CommandParameter = ap,
                        });
                        marker.ContextMenu.Items.Add(new MenuItem
                        {
                            Header = "Delete",
                            Command = DeleteAnalysisPointCommand,
                            CommandParameter = ap,
                        });
                    }
                }
            }
            if (FileName != null)
            {
                LocalStorageRoot = Path.Combine(Path.GetDirectoryName(FileName), Path.GetFileNameWithoutExtension(FileName));
                if (!Directory.Exists(LocalStorageRoot)) Directory.CreateDirectory(LocalStorageRoot);
            }
            MapLayerViewModel.Layers = MapLayers;
            InitializeEnvironment();
            AddScenarioFileCommand(ScenarioFileName);
            IsChanged = false;
            _isInitialized = true;
            MediatorMessage.Send(MediatorMessage.SetExperiment, this);
        }

        void InitializeEnvironment()
        {
            if (NemoFile == null) return;
            var boundingBox = new Rect();
            if (NemoFile.Scenario.OverlayFile != null)
                boundingBox = NemoFile.Scenario.OverlayFile.Shapes[0].BoundingBox;
            else
            {
                foreach (var platform in NemoFile.Scenario.Platforms)
                    foreach (var trackdef in platform.Trackdefs)
                    {
                        if ((boundingBox.Width == 0) && (boundingBox.Height == 0)) 
                            boundingBox = trackdef.OverlayFile.Shapes[0].BoundingBox;
                        else 
                            boundingBox.Union(trackdef.OverlayFile.Shapes[0].BoundingBox);
                    }
                        
            }
            var north = (float)boundingBox.Bottom + 2;
            var west = (float)boundingBox.Left - 2;
            var south = (float)boundingBox.Top - 2;
            var east = (float)boundingBox.Right + 2;
            if ((BathymetryFileName != null) && (File.Exists(BathymetryFileName)))
                Bathymetry = new Environment2DData(BathymetryFileName, north, west, south, east);
            if ((SoundSpeedFileName != null) && (File.Exists(SoundSpeedFileName)))
                SoundSpeedField = new SoundSpeedField(SoundSpeedFileName, north, west, south, east);
        }

        #region RunTransmissionLossCommand

        public SimpleCommand<object, object> RunTransmissionLossCommand
        {
            get { return _runTransmissionLossCommand ?? (_runTransmissionLossCommand = new SimpleCommand<object, object>(delegate
                                                                                                                         {
                                                                                                                             MediatorMessage.Send(MediatorMessage.DoNothing);
                                                                                                                         })); }
        }

        SimpleCommand<object, object> _runTransmissionLossCommand;

        #endregion

        #region DeleteAnalysisPointCommand

        public SimpleCommand<object, AnalysisPoint> DeleteAnalysisPointCommand
        {
            get { return _deleteAnalysisPointCommand ?? (_deleteAnalysisPointCommand = new SimpleCommand<object, AnalysisPoint>(ap => MediatorMessage.Send(MediatorMessage.DeleteAnalysisPoint, ap))); }
        }

        SimpleCommand<object, AnalysisPoint> _deleteAnalysisPointCommand;

        #endregion
    }
}