using System;
using System.Data;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Transactions;
using HRC.Navigation;

namespace ESME.Database.Importers
{
    public static class NemoFile
    {
        public static void Import(string nemoFilePath, string scenarioDataDirectory, ScenarioContext scenarioContext)
        {
            var nemoFile = new NEMO.NemoFile(nemoFilePath, scenarioDataDirectory);
            var scenario = new Scenario
            {
                BuilderVersion = nemoFile.Scenario.BuilderVersion,
                AnalystName = nemoFile.Scenario.AnalystName,
                CreationTime = nemoFile.Scenario.CreationTime,
                Description = nemoFile.Scenario.Description,
                Duration = nemoFile.Scenario.Duration,
                EventName = nemoFile.Scenario.EventName,
                SimAreaName = nemoFile.Scenario.SimAreaName,
                StartTime = nemoFile.Scenario.StartTime.TimeOfDay,
                TimeFrame = nemoFile.Scenario.TimeFrame,
            };
            scenarioContext.Scenarios.Add(scenario);
            scenarioContext.SaveChanges();
            foreach (var nemoPlatform in nemoFile.Scenario.Platforms)
            {
                var platform = new ScenarioPlatform
                {
                    PlatformID = int.Parse(nemoPlatform.Id),
                    PlatformName = nemoPlatform.Name,
                    Description = nemoPlatform.Description,
                    Launches = false,
                    Tows = false,
                    Scenario = scenario,
                };
                scenarioContext.ScenarioPlatforms.Add(platform);
                scenarioContext.SaveChanges();
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
                    var trackDefinition = new TrackDefinition
                    {
                        Duration = nemoPlatform.Trackdefs[0].Duration,
                        InitialCourse = nemoPlatform.Trackdefs[0].InitialCourse,
                        InitialDepth = -1 * nemoPlatform.Trackdefs[0].InitialHeight,
                        InitialLatitude = nemoPlatform.Trackdefs[0].InitialLatitude,
                        InitialLongitude = nemoPlatform.Trackdefs[0].InitialLongitude,
                        InitialSpeed = nemoPlatform.Trackdefs[0].InitialSpeed,
                        LimitFileName = nemoPlatform.Trackdefs[0].LimitFileName,
                        OpsBounds = nemoPlatform.Trackdefs[0].OpsBounds,
                        OpsTimes = nemoPlatform.Trackdefs[0].OpsTimes,
                        Random = nemoPlatform.Trackdefs[0].Random,
                        StartTime = nemoPlatform.Trackdefs[0].StartTime.TimeOfDay,
                        TrackType = (int)trackType,
                    };
                    scenarioContext.TrackDefinitions.Add(trackDefinition);
                    platform.TrackDefinition = trackDefinition;
                    scenarioContext.SaveChanges();
                    if (trackDefinition.LimitFileName != null)
                    {
                        var perimeterName = Path.GetFileNameWithoutExtension(trackDefinition.LimitFileName);
                        var existingPerimeter = (from perimeter in scenarioContext.Perimeters
                                                 where perimeter.Name == perimeterName
                                                 select perimeter).FirstOrDefault();
                        // If there is no perimeter with the current filename that already exists
                        if (existingPerimeter == null)
                        {
                            var perimeter = new Perimeter
                            {
                                Name = Path.GetFileNameWithoutExtension(trackDefinition.LimitFileName),
                            };
                            scenarioContext.Perimeters.Add(perimeter);
                            scenarioContext.SaveChanges();
                            foreach (var geo in nemoPlatform.Trackdefs[0].OverlayFile.Shapes[0].Geos)
                                scenarioContext.PerimeterCoordinates.Add(new PerimeterCoordinate
                                {
                                    Geo = geo,
                                    Perimeter = perimeter,
                                });
                            scenarioContext.SaveChanges();
                        }
                    }
                }
                foreach (var nemoSource in nemoPlatform.Sources)
                {
                    var source = new ScenarioSource
                    {
                        SourceID = int.Parse(nemoSource.Id),
                        SourceName = nemoSource.Name,
                        Description = nemoSource.Description,
                        Platform = platform,
                    };
                    scenarioContext.ScenarioSources.Add(source);
                    scenarioContext.SaveChanges();
                    foreach (var nemoMode in nemoSource.Modes)
                    {
                        var mode = new ScenarioMode
                        {
                            //ModeID = int.Parse(nemoMode.Id),
                            ActiveTime = nemoMode.ActiveTime,
                            ClusterCount = nemoMode.ClusterCount,
                            DepressionElevationAngle = nemoMode.DepressionElevationAngle,
                            Depth = nemoMode.DepthOffset,
                            HighFrequency = nemoMode.HighFrequency,
                            LowFrequency = nemoMode.LowFrequency,
                            HorizontalBeamWidth = nemoMode.HorizontalBeamWidth,
                            Linked = nemoMode.Linked,
                            MaxPropagationRadius = nemoMode.Radius,
                            ModeName = nemoMode.Name,
                            PulseInterval = (float)nemoMode.PulseInterval.TotalSeconds,
                            PulseLength = (float)nemoMode.PulseLength.TotalSeconds * 1000,
                            RelativeBeamAngle = nemoMode.RelativeBeamAngle,
                            SourceLevel = nemoMode.SourceLevel,
                            State = nemoMode.State,
                            VerticalBeamWidth = nemoMode.VerticalBeamWidth,
                            Source = source,
                        };
                        scenarioContext.ScenarioModes.Add(mode);
                        scenarioContext.SaveChanges();
                    }
                }
            }
            foreach (var nemoAnimals in nemoFile.Scenario.Animals)
            {
                foreach (var nemoSpecies in nemoAnimals.Species)
                {
                    var species = new ScenarioSpecies
                    {
                        Scenario = scenario,
                    };
                    scenarioContext.ScenarioSpecies.Add(species);
                    scenarioContext.SaveChanges();
                    using (var transaction = new TransactionScope())
                    {
                        nemoSpecies.AnimatDataTask.Start();
                        TaskEx.WhenAll(nemoSpecies.AnimatDataTask).Wait();
                        var result = nemoSpecies.AnimatDataTask.Result;
                        species.Name = result.LatinName;
                        var locationIndex = 1;
                        foreach (var location in result.AnimatStartPoints)
                        {
                            locationIndex++;
                            Console.Write("{0} Adding animat {1} of {2}\r", species.Name, locationIndex, result.AnimatStartPoints.Count);
                            scenarioContext.AnimatLocations.Add(new AnimatLocation
                            {
                                Geo = new Geo(location.Latitude, location.Longitude),
                                Depth = location.Data,
                                ScenarioSpecies = species,
                            });
                        }
                        scenarioContext.Entry(species).State = EntityState.Modified;
                        scenarioContext.SaveChanges();
                        transaction.Complete();
                    }
                }
            }
            scenarioContext.Database.ExecuteSqlCommand("VACUUM;");
        }
    }
}
