using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using ESME.Behaviors;
using ESME.Environment;
using ESME.Locations;
using HRC.Navigation;

namespace ESME.Database.Importers
{
    public static class NemoFile
    {
        public static Scenario Import(MasterDatabaseService masterDatabase, Location location, string nemoFilePath, string scenarioDataDirectory)
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
                var platform = new PSMPlatform
                {
                    PlatformName = nemoPlatform.Name,
                    PlatformType = nemoPlatform.Type,
                    PSMSources = new Collection<PSMSource>(),
                };
                foreach (var nemoSource in nemoPlatform.Sources)
                {
                    var source = new PSMSource
                    {
                        SourceName = nemoSource.Name,
                        SourceType = nemoSource.Type,
                        PSMModes = new Collection<PSMMode>(),
                    };
                    platform.PSMSources.Add(source);
                    foreach (var nemoMode in nemoSource.Modes)
                    {
                        var mode = new PSMMode
                        {
                            ModeName = nemoMode.Name,
                            ModeType = nemoMode.Type,
                            ActiveTime = nemoMode.ActiveTime,
                            DepressionElevationAngle = nemoMode.DepressionElevationAngle,
                            Depth = nemoMode.DepthOffset,
                            HighFrequency = nemoMode.HighFrequency,
                            LowFrequency = nemoMode.LowFrequency,
                            HorizontalBeamWidth = nemoMode.HorizontalBeamWidth,
                            MaxPropagationRadius = nemoMode.Radius,
                            PulseInterval = (float)nemoMode.PulseInterval.TotalSeconds,
                            PulseLength = (float)nemoMode.PulseLength.TotalSeconds * 1000,
                            RelativeBeamAngle = nemoMode.RelativeBeamAngle,
                            SourceLevel = nemoMode.SourceLevel,
                            VerticalBeamWidth = nemoMode.VerticalBeamWidth,
                        };
                        source.PSMModes.Add(mode);
                    }
                }
                var curPlatform = masterDatabase.AddPlatform(scenario, platform, nemoPlatform.Description);
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
                        var order = 0;
                        var perimeterCoordinates = nemoPlatform.Trackdefs[0].OverlayFile.Shapes[0].Geos.Select(geo => new PerimeterCoordinate
                        {
                            Order = order++,
                            Geo = geo,
                            Perimeter = perimeter,
                        });
                        var perimeterName = Path.GetFileNameWithoutExtension(nemoPlatform.Trackdefs[0].LimitFileName);
                        perimeter = masterDatabase.AddOrGetPerimeter(scenario, perimeterName, perimeterCoordinates);
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
                    masterDatabase.SetTrackDefinition(curPlatform, trackDefinition);
                }
            }
            foreach (var nemoAnimals in nemoFile.Scenario.Animals)
            {
                foreach (var nemoSpecies in nemoAnimals.Species)
                {
                    nemoSpecies.AnimatDataTask.Start();
                    TaskEx.WhenAll(nemoSpecies.AnimatDataTask).Wait();
                    var result = nemoSpecies.AnimatDataTask.Result;
                    ScenarioSpecies species = null;
                    if (scenario.Species != null)
                        species = (from s in scenario.Species
                                   where s.LatinName == result.LatinName
                                   select s).FirstOrDefault();
                    if (species != null) Console.WriteLine("Species with name \"{0}\" already exists in scenario \"{1}\", replacing with current data", nemoSpecies.AnimatDataTask.Result.LatinName, scenario.Name);
                    else species = new ScenarioSpecies {LatinName = nemoSpecies.AnimatDataTask.Result.LatinName};
                    var animats = new List<AnimatLocation>();
                    foreach (var startPoint in result.AnimatStartPoints)
                    {
                        animats.Add(new AnimatLocation
                        {
                            Geo = new Geo(startPoint.Latitude, startPoint.Longitude),
                            Depth = startPoint.Data,
                            ScenarioSpecies = species,
                        });
                    }
                    masterDatabase.AddOrReplaceSpecies(scenario, species, animats);
                }
            }
            return scenario;
        }
    }
}
