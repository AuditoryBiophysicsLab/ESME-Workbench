using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.IO;
using System.Linq;
using ESME.Database;
using ESME.Environment;
using ESME.Locations;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Scenarios
{
    [NotifyPropertyChanged]
    public class Scenario : IHaveGuid, INotifyPropertyChanged, IMouseOverAware
    {
        public Scenario()
        {
            StorageDirectory = Path.Combine("scenarios", MasterDatabaseService.RandomFilenameWithoutExension);
            var duration = new TimeSpan(0, 1, 0, 0);
            Duration.Ticks = duration.Ticks;
        }

        [Key, Initialize]
        public Guid Guid { get; set; }

        public string Name { get; set; }
        public string Comments { get; set; }
        public string StorageDirectory { get; set; }
        public bool ShowAllAnalysisPoints
        {
            get { return _showAllAnalysisPoints; }
            set
            {
                _showAllAnalysisPoints = value;
                foreach (var analysisPoint in AnalysisPoints) analysisPoint.LayerSettings.IsChecked = _showAllAnalysisPoints;
            }
        }
        bool _showAllAnalysisPoints;

        public bool ShowAllPerimeters
        {
            get { return _showAllPerimeters; }
            set
            {
                _showAllPerimeters = value;
                foreach (var perimeter in Perimeters) perimeter.LayerSettings.IsChecked = _showAllPerimeters;
            }
        }
        bool _showAllPerimeters;

        public bool ShowAllSpecies
        {
            get { return _showAllSpecies; }
            set
            {
                _showAllSpecies = value;
                foreach (var species in ScenarioSpecies) species.LayerSettings.IsChecked = _showAllSpecies;
            }
        }
        bool _showAllSpecies;

        [Initialize] public DbTimeSpan StartTime { get; set; }
        [Initialize] public DbTimeSpan Duration { get; set; }
        [Initialize] public DbTimePeriod TimePeriod { get; set; }

        public virtual Location Location { get; set; }

        public virtual EnvironmentalDataSet Wind { get; set; }
        public virtual EnvironmentalDataSet SoundSpeed { get; set; }
        public virtual EnvironmentalDataSet Sediment { get; set; }
        public virtual EnvironmentalDataSet Bathymetry { get; set; }

        [Initialize] public virtual ObservableList<Platform> Platforms { get; set; }
        [Initialize] public virtual ObservableList<ScenarioSpecies> ScenarioSpecies { get; set; }
        [Initialize] public virtual ObservableList<AnalysisPoint> AnalysisPoints { get; set; }
        [Initialize] public virtual ObservableList<Perimeter> Perimeters { get; set; }
        [Initialize] public virtual ObservableList<LogEntry> Logs { get; set; }

        public void Add(Platform platform) { Platforms.Add(platform); }

        [NotMapped] public Wind WindData { get { return ((Wind)Cache[Wind].Result); } }
        [NotMapped] public SoundSpeed SoundSpeedData { get { return ((SoundSpeed)Cache[SoundSpeed].Result); } }
        [NotMapped] public Bathymetry BathymetryData { get { return ((Bathymetry)Cache[Bathymetry].Result); } }
        [NotMapped] public Sediment SedimentData { get { return ((Sediment)Cache[Sediment].Result); } }

        [NotMapped] public string StorageDirectoryPath
        {
            get
            {
                if (_storageDirectoryPath != null) return _storageDirectoryPath;
                if (Database == null || Database.MasterDatabaseDirectory == null) throw new ApplicationException("Database or Database.MasterDatabaseDirectory is null");
                _storageDirectoryPath = Path.Combine(Database.MasterDatabaseDirectory, StorageDirectory);
                if (!Directory.Exists(_storageDirectoryPath)) Directory.CreateDirectory(_storageDirectoryPath);
                return _storageDirectoryPath;
            }
        }
        string _storageDirectoryPath;

        [NotMapped] public static IMasterDatabaseService Database { get; set; }
        [NotMapped] public static EnvironmentalCacheService Cache { get; set; }
        [NotMapped] public bool IsNew { get; set; }
        [NotMapped] public object LayerControl
        {
            get { return _layerControl; }
            set
            {
                _layerControl = value;
                MediatorMessage.Send(MediatorMessage.ScenarioBoundToLayer, this);
            }
        }
        object _layerControl;


        public void CreateMapLayers()
        {
            if (Platforms != null) foreach (var platform in Platforms) platform.CreateMapLayers();
            foreach (var analysisPoint in AnalysisPoints.ToList()) analysisPoint.CreateMapLayers();
            foreach (var species in ScenarioSpecies.ToList()) species.CreateMapLayers();
            foreach (var perimeter in Perimeters.ToList()) perimeter.CreateMapLayers();
        }

        public void RemoveMapLayers()
        {
            if (Wind != null) Wind.RemoveMapLayers();
            if (SoundSpeed != null) SoundSpeed.RemoveMapLayers();
            if (Bathymetry != null) Bathymetry.RemoveMapLayers();
            if (Sediment != null) Sediment.RemoveMapLayers();
            if (Platforms != null) foreach (var platform in Platforms) platform.RemoveMapLayers();
            foreach (var analysisPoint in AnalysisPoints) analysisPoint.RemoveMapLayers();
            foreach (var species in ScenarioSpecies.ToList()) species.RemoveMapLayers();
            foreach (var perimeter in Perimeters.ToList()) perimeter.RemoveMapLayers();
        }

        public event PropertyChangedEventHandler PropertyChanged;

        [NotMapped] public bool IsMouseOver { get; set; }
        [NotMapped] public GeoRect GeoRect { get { return Location.GeoRect; } }
        #region AddPlatformCommand
        public SimpleCommand<object, EventToCommandArgs> AddPlatformCommand { get { return _addPlatform ?? (_addPlatform = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.AddPlatform, this))); } }
        SimpleCommand<object, EventToCommandArgs> _addPlatform;
        #endregion

        #region ViewScenarioPropertiesCommand
        public SimpleCommand<object, EventToCommandArgs> ViewScenarioPropertiesCommand { get { return _viewScenarioProperties ?? (_viewScenarioProperties = new SimpleCommand<object, EventToCommandArgs>(o =>MediatorMessage.Send(MediatorMessage.ViewScenarioProperties,this))); } }
        SimpleCommand<object, EventToCommandArgs> _viewScenarioProperties;
        #endregion
        public void Delete()
        {
            RemoveMapLayers();
            Location.Scenarios.Remove(this);
            foreach (var platform in Platforms.ToList()) platform.Delete();
            foreach (var analysisPoint in AnalysisPoints.ToList()) analysisPoint.Delete();
            foreach (var species in ScenarioSpecies.ToList()) species.Delete();
            foreach (var perimeter in Perimeters.ToList()) perimeter.Delete();
            Database.Context.Scenarios.Remove(this);
            Directory.Delete(StorageDirectoryPath, true);
        }
        #region LoadScenarioCommand
        public SimpleCommand<object, EventToCommandArgs> LoadScenarioCommand { get { return _loadScenario ?? (_loadScenario = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.LoadScenario, this))); } }
        SimpleCommand<object, EventToCommandArgs> _loadScenario;
        #endregion

        #region DeleteScenarioCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteScenarioCommand { get { return _deleteScenario ?? (_deleteScenario = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeleteScenario, this))); } }
        SimpleCommand<object, EventToCommandArgs> _deleteScenario;
        #endregion

        #region ScenarioPropertiesCommand
        public SimpleCommand<object, EventToCommandArgs> ScenarioPropertiesCommand { get { return _scenarioProperties ?? (_scenarioProperties = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.ScenarioProperties, this))); } }
        SimpleCommand<object, EventToCommandArgs> _scenarioProperties;
        #endregion
    }

    public static class ScenarioExensions
    {
        public static IEnumerable<Mode> GetAllModes(this Scenario scenario) 
        {
            return scenario.Platforms.SelectMany(platform => platform.Sources.SelectMany(source => source.Modes));
        }
    }
}
