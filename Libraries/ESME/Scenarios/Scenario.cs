using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Transactions;
using ESME.Behaviors;
using ESME.Database;
using ESME.Environment;
using ESME.Locations;
using HRC.Aspects;
using HRC.Navigation;

namespace ESME.Scenarios
{
    public class Scenario : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }

        public string Name { get; set; }
        public string Comments { get; set; }
        public DbTimeSpan StartTime { get; set; }
        public DbTimeSpan Duration { get; set; }
        public DbTimePeriod TimePeriod { get; set; }

        public virtual Location Location { get; set; }
        public virtual EnvironmentalDataSet Wind { get; set; }
        public virtual EnvironmentalDataSet SoundSpeed { get; set; }
        public virtual EnvironmentalDataSet Sediment { get; set; }
        public virtual EnvironmentalDataSet Bathymetry { get; set; }

        public virtual ICollection<Platform> Platforms { get; set; }
        public virtual ICollection<ScenarioSpecies> ScenarioSpecies { get; set; }
        public virtual ICollection<LogEntry> Logs { get; set; }

        #region Importer for NEMO files
        public static Scenario FromNemoFile(MasterDatabaseService masterDatabase, Location location, string nemoFilePath, string scenarioDataDirectory)
        {
            
            var nemoFile = new NEMO.NemoFile(nemoFilePath, scenarioDataDirectory);
            var scenario = masterDatabase.CreateScenario(Path.GetFileNameWithoutExtension(nemoFilePath),
                                                          nemoFile.Scenario.Description,
                                                          nemoFile.Scenario.StartTime.TimeOfDay,
                                                          nemoFile.Scenario.Duration,
                                                          (TimePeriod)Enum.Parse(typeof(TimePeriod), nemoFile.Scenario.TimeFrame),
                                                          location);
            foreach (var nemoPlatform in nemoFile.Scenario.Platforms)
            {
                var platform = new Platform
                {
                    Description = nemoPlatform.Description,
                    //Launches = nemoPlatform.Launcher
                    //PSMPlatformGuid = nemoPlatform.
                    PlatformName = nemoPlatform.Name,
                    PlatformType = nemoPlatform.Type,
                    RepeatCount = nemoPlatform.RepeatCount,
                    Scenario = scenario,
                    //Tows = nemoPlatform.Towwer
                };
                masterDatabase.Context.Platforms.Add(platform);
                foreach (var nemoSource in nemoPlatform.Sources)
                {
                    var source = new Source
                    {
                        //PSMSourceGuid = 
                        Platform = platform,
                        SourceName = nemoSource.Name,
                        SourceType = nemoSource.Type,
                    };
                    masterDatabase.Context.Sources.Add(source);
                    foreach (var nemoMode in nemoSource.Modes)
                    {
                        var mode = new Mode
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
                            PulseInterval = (float)nemoMode.PulseInterval.TotalSeconds,
                            PulseLength = (float)nemoMode.PulseLength.TotalSeconds * 1000,
                            RelativeBeamAngle = nemoMode.RelativeBeamAngle,
                            Source = source,
                            SourceLevel = nemoMode.SourceLevel,
                            VerticalBeamWidth = nemoMode.VerticalBeamWidth,
                        };
                        masterDatabase.Context.Modes.Add(mode);
                    }
                }
                if (nemoPlatform.Trackdefs.Count > 0)
                {
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
                    Perimeter perimeter = null;
                    if (nemoPlatform.Trackdefs[0].LimitFileName != null)
                    {
                        var perimeterName = Path.GetFileNameWithoutExtension(nemoPlatform.Trackdefs[0].LimitFileName);
                        perimeter = (from p in masterDatabase.Context.Perimeters
                                     where p.Name == perimeterName
                                     select p).FirstOrDefault();
                        if (perimeter == null)
                        {
                            perimeter = new Perimeter
                            {
                                Name = perimeterName,
                                Scenario = scenario,
                            };
                            masterDatabase.Context.Perimeters.Add(perimeter);
                        }
                        for (var i = 0; i < nemoPlatform.Trackdefs[0].OverlayFile.Shapes[0].Geos.Count; i++)
                        {
                            masterDatabase.Context.PerimeterCoordinates.Add(new PerimeterCoordinate
                            {
                                Order = i,
                                Geo = nemoPlatform.Trackdefs[0].OverlayFile.Shapes[0].Geos[i],
                                Perimeter = perimeter,
                            });
                        }
                    }
                    var trackDefinition = new TrackDefinition
                    {
                        Duration = nemoPlatform.Trackdefs[0].Duration,
                        InitialCourse = nemoPlatform.Trackdefs[0].InitialCourse,
                        InitialDepth = -1 * nemoPlatform.Trackdefs[0].InitialHeight,
                        InitialLatitude = nemoPlatform.Trackdefs[0].InitialLatitude,
                        InitialLongitude = nemoPlatform.Trackdefs[0].InitialLongitude,
                        InitialSpeed = nemoPlatform.Trackdefs[0].InitialSpeed,
                        OpsBounds = nemoPlatform.Trackdefs[0].OpsBounds,
                        OpsTimes = nemoPlatform.Trackdefs[0].OpsTimes,
                        Random = nemoPlatform.Trackdefs[0].Random,
                        StartTime = nemoPlatform.Trackdefs[0].StartTime.TimeOfDay,
                        TrackType = trackType,
                        Perimeter = perimeter,
                    };
                    platform.TrackDefinition = trackDefinition;
                    masterDatabase.Context.TrackDefinitions.Add(trackDefinition);
                }
            }
            foreach (var nemoAnimals in nemoFile.Scenario.Animals)
            {
                foreach (var nemoSpecies in nemoAnimals.Species)
                {
                    nemoSpecies.AnimatDataTask.Start();
                    TaskEx.WhenAll(nemoSpecies.AnimatDataTask).Wait();
                    var result = nemoSpecies.AnimatDataTask.Result;
                    var species = new ScenarioSpecies
                    {
                        LatinName = result.LatinName,
                        SpeciesFile = nemoSpecies.SpeciesFile,
                        Scenario = scenario,
                    };
                    masterDatabase.Context.ScenarioSpecies.Add(species);
                    masterDatabase.Context.SaveChanges();
                    foreach (var startPoint in result.AnimatStartPoints)
                    {
                        var animat = new AnimatLocation
                        {
                            Geo = new Geo(startPoint.Latitude, startPoint.Longitude),
                            Depth = startPoint.Data,
                            ScenarioSpecies = species,
                        };
                        masterDatabase.Context.AnimatLocations.Add(animat);
                    }
                }
            }
            using (var transaction = new TransactionScope())
            {
                masterDatabase.Context.SaveChanges();
                transaction.Complete();
            }
            return scenario;
        }
        #endregion
    }
}
