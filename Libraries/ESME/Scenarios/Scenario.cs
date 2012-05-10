using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.IO;
using System.Linq;
using System.Transactions;
using ESME.Behaviors;
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
    public class Scenario : IHaveGuid, IHaveLayerSettings, INotifyPropertyChanged, IMouseOverAware
    {
        [Key, Initialize]
        public Guid Guid { get; set; }

        public string Name { get; set; }
        public string Comments { get; set; }
        public string StorageDirectory { get; set; }
        [Initialize] public DbTimeSpan StartTime { get; set; }
        [Initialize] public DbTimeSpan Duration { get; set; }
        [Initialize] public DbTimePeriod TimePeriod { get; set; }

        public virtual Location Location { get; set; }
        public virtual LayerSettings LayerSettings { get; set; }

        public virtual EnvironmentalDataSet Wind { get; set; }
        public virtual EnvironmentalDataSet SoundSpeed { get; set; }
        public virtual EnvironmentalDataSet Sediment { get; set; }
        public virtual EnvironmentalDataSet Bathymetry { get; set; }

        [Initialize]
        public virtual ObservableList<Platform> Platforms { get; set; }
        [Initialize]
        public virtual ObservableList<ScenarioSpecies> ScenarioSpecies { get; set; }
        [Initialize]
        public virtual ObservableList<AnalysisPoint> AnalysisPoints { get; set; }
        [Initialize]
        public virtual ObservableList<LogEntry> Logs { get; set; }

        [NotMapped] public Wind WindData { get { return ((Wind)Cache[Wind].Result); } }
        [NotMapped] public SoundSpeed SoundSpeedData { get { return ((SoundSpeed)Cache[SoundSpeed].Result); } }
        [NotMapped] public Bathymetry BathymetryData { get { return ((Bathymetry)Cache[Bathymetry].Result); } }
        [NotMapped] public Sediment SedimentData { get { return ((Sediment)Cache[Sediment].Result); } }

        [NotMapped] public string StorageDirectoryPath { get { return Path.Combine(Database.MasterDatabaseDirectory, StorageDirectory); } }
        
        #region Importer for NEMO files
        public static Scenario FromNemoFile(IMasterDatabaseService masterDatabase, Location location, string nemoFilePath, string scenarioDataDirectory)
        {
            var nemoFile = new NEMO.NemoFile(nemoFilePath, scenarioDataDirectory);
            Scenario scenario;
            masterDatabase.Add(scenario = new Scenario
            {
                Location = location,
                Name = Path.GetFileNameWithoutExtension(nemoFilePath),
                Comments = nemoFile.Scenario.Description,
                StartTime = nemoFile.Scenario.StartTime.TimeOfDay,
                Duration = nemoFile.Scenario.Duration,
                TimePeriod = (TimePeriod)Enum.Parse(typeof(TimePeriod), nemoFile.Scenario.TimeFrame),
            });
            foreach (var nemoPlatform in nemoFile.Scenario.Platforms)
            {
                Perimeter perimeter = null;
                if (nemoPlatform.Trackdefs.Count <= 0) continue;
                var trackType = TrackType.Stationary;
                switch (nemoPlatform.Trackdefs[0].TrackType.ToLower())
                {
                    case "perimeter_bounce":
                        trackType = TrackType.PerimeterBounce;
                        break;
                    case "stationary":
                        trackType = TrackType.Stationary;
                        break;
                    case "straight_line":
                        trackType = TrackType.StraightLine;
                        break;
                }
                if (nemoPlatform.Trackdefs[0].LimitFileName != null)
                {
                    var perimeterName = Path.GetFileNameWithoutExtension(nemoPlatform.Trackdefs[0].LimitFileName);
                    perimeter = (from p in masterDatabase.Context.Perimeters
                                 where p.Name == perimeterName
                                 select p).FirstOrDefault();
                    if (perimeter == null)
                    {
                        masterDatabase.Add(perimeter = new Perimeter
                        {
                            Name = perimeterName,
                            Scenario = scenario,
                        });
                        for (var i = 0; i < nemoPlatform.Trackdefs[0].OverlayFile.Shapes[0].Geos.Count; i++)
                        {
                            masterDatabase.Add(new PerimeterCoordinate
                            {
                                Order = i,
                                Geo = nemoPlatform.Trackdefs[0].OverlayFile.Shapes[0].Geos[i],
                                Perimeter = perimeter,
                            });
                        }
                    }
                }
                Platform platform;
                masterDatabase.Add(platform = new Platform
                {
                    Description = nemoPlatform.Description,
                    //Launches = nemoPlatform.Launcher
                    //PSMPlatformGuid = nemoPlatform.
                    PlatformName = nemoPlatform.Name,
                    PlatformType = nemoPlatform.Type,
                    RepeatCount = nemoPlatform.RepeatCount,
                    Scenario = scenario,
                    //Tows = nemoPlatform.Towwer
                    //Duration = nemoPlatform.Trackdefs[0].Duration,
                    Course = nemoPlatform.Trackdefs[0].InitialCourse,
                    Depth = -1 * nemoPlatform.Trackdefs[0].InitialHeight,
                    Geo = new Geo(nemoPlatform.Trackdefs[0].InitialLatitude, nemoPlatform.Trackdefs[0].InitialLongitude),
                    Speed = nemoPlatform.Trackdefs[0].InitialSpeed,
                    //OpsBounds = nemoPlatform.Trackdefs[0].OpsBounds,
                    //OpsTimes = nemoPlatform.Trackdefs[0].OpsTimes,
                    IsRandom = nemoPlatform.Trackdefs[0].Random,
                    //StartTime = nemoPlatform.Trackdefs[0].StartTime.TimeOfDay,
                    TrackType = trackType,
                    Perimeter = perimeter,
                });
                foreach (var nemoSource in nemoPlatform.Sources)
                {
                    Source source;
                    masterDatabase.Add(source = new Source
                    {
                        //PSMSourceGuid = 
                        Platform = platform,
                        SourceName = nemoSource.Name,
                        SourceType = nemoSource.Type,
                    });
                    foreach (var nemoMode in nemoSource.Modes)
                    {
                        masterDatabase.Add(new Mode
                        {
                            ActiveTime = nemoMode.ActiveTime,
                            DepressionElevationAngle = nemoMode.DepressionElevationAngle,
                            Depth = nemoMode.DepthOffset,
                            HighFrequency = nemoMode.HighFrequency,
                            HorizontalBeamWidth = nemoMode.HorizontalBeamWidth,
                            LowFrequency = nemoMode.LowFrequency,
                            MaxPropagationRadius = nemoMode.Radius,
                            ModeName = nemoMode.Name,
                            ModeType = nemoMode.Type,
                            //PSMModeGuid = 
                            PulseInterval = new TimeSpan(nemoMode.PulseInterval.Ticks),
                            PulseLength = new TimeSpan(nemoMode.PulseLength.Ticks),
                            RelativeBeamAngle = nemoMode.RelativeBeamAngle,
                            Source = source,
                            SourceLevel = nemoMode.SourceLevel,
                            VerticalBeamWidth = nemoMode.VerticalBeamWidth,
                        });
                    }
                }
            }
            foreach (var nemoAnimals in nemoFile.Scenario.Animals)
            {
                foreach (var nemoSpecies in nemoAnimals.Species)
                {
                    nemoSpecies.AnimatDataTask.Start();
                    var result = nemoSpecies.AnimatDataTask.Result;
                    masterDatabase.Add(new ScenarioSpecies
                    {
                        LatinName = result.LatinName,
                        SpeciesFile = result.Filename,
                        Scenario = scenario,
                    });
                }
            }
            using (var transaction = new TransactionScope())
            {
                transaction.Complete();
            }
            return scenario;
        }
        #endregion

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
            if (LayerSettings == null) LayerSettings = new LayerSettings();
            if (Platforms != null) foreach (var platform in Platforms) platform.CreateMapLayers();
            foreach (var analysisPoint in AnalysisPoints) analysisPoint.CreateMapLayers();
        }

        public void RemoveMapLayers()
        {
            Wind.RemoveMapLayers();
            SoundSpeed.RemoveMapLayers();
            Bathymetry.RemoveMapLayers();
            Sediment.RemoveMapLayers();
            if (Platforms != null) foreach (var platform in Platforms) platform.RemoveMapLayers();
            var transmissionLosses = (from tl in Database.Context.TransmissionLosses
                                      where tl.AnalysisPoint.Scenario.Guid == Guid
                                      select tl).ToList();
            foreach (var transmissionLoss in transmissionLosses) transmissionLoss.RemoveMapLayers();
        }

        public event PropertyChangedEventHandler PropertyChanged;

        [NotMapped] public bool IsMouseOver { get; set; }
        [NotMapped] public GeoRect GeoRect { get { return Location.GeoRect; } }
        #region AddPlatformCommand
        public SimpleCommand<object, EventToCommandArgs> AddPlatformCommand { get { return _addPlatform ?? (_addPlatform = new SimpleCommand<object, EventToCommandArgs>(AddPlatformHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _addPlatform;

        void AddPlatformHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
            MediatorMessage.Send(MediatorMessage.AddPlatform, this);
        }
        #endregion
        public void Delete()
        {
            foreach (var platform in Platforms.ToArray()) platform.Delete();
            Database.Context.Scenarios.Remove(this);
        }
    }

    public static class ScenarioExensions
    {
        public static IEnumerable<Mode> GetAllModes(this Scenario scenario) 
        {
            return scenario.Platforms.SelectMany(platform => platform.Sources.SelectMany(source => source.Modes));
        }
    }
}
