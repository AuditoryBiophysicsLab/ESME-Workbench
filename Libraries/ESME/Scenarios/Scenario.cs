using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.IO;
using System.Linq;
using System.Transactions;
using ESME.Behaviors;
using ESME.Database;
using ESME.Environment;
using ESME.Locations;
using HRC.Aspects;

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
            var scenario = new Scenario
            {
                Location = location,
                Name = Path.GetFileNameWithoutExtension(nemoFilePath),
                Comments = nemoFile.Scenario.Description,
                StartTime = nemoFile.Scenario.StartTime.TimeOfDay,
                Duration = nemoFile.Scenario.Duration,
                TimePeriod = (TimePeriod)Enum.Parse(typeof (TimePeriod), nemoFile.Scenario.TimeFrame),
            };
            masterDatabase.Add(scenario);
            foreach (var nemoPlatform in nemoFile.Scenario.Platforms)
            {
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
                            PulseInterval = (float)nemoMode.PulseInterval.TotalSeconds,
                            PulseLength = (float)nemoMode.PulseLength.TotalSeconds * 1000,
                            RelativeBeamAngle = nemoMode.RelativeBeamAngle,
                            Source = source,
                            SourceLevel = nemoMode.SourceLevel,
                            VerticalBeamWidth = nemoMode.VerticalBeamWidth,
                        });
                    }
                }
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
                Perimeter perimeter = null;
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
                    }
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
                TrackDefinition trackDefinition;
                masterDatabase.Add(trackDefinition = new TrackDefinition
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
                });
                platform.TrackDefinition = trackDefinition;
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
                masterDatabase.Context.SaveChanges();
                transaction.Complete();
            }
            return scenario;
        }
        #endregion
    }
}
