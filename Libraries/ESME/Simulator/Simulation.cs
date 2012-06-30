using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using ESME.Behaviors;
using ESME.Environment;
using ESME.Scenarios;
using HRC.Navigation;

namespace ESME.Simulator
{
    public class Simulation
    {
        public List<Actor> GetActors() { return _database.Actors.ToList(); }
        public Scenario Scenario { get { return _database.Scenarios.First(); } }

        public static Simulation Create(Scenario scenario, string simulationDirectory)
        {
            Directory.CreateDirectory(simulationDirectory);
            var result = new Simulation(scenario, simulationDirectory);
            return result;
        }

        Simulation(Scenario scenario, string simulationDirectory)
        {
            _simulationDirectory = simulationDirectory;
            _database = SimulationContext.OpenOrCreate(Path.Combine(_simulationDirectory, "simulation.db"));
            _scenario = _database.ImportScenario(scenario);
        }

        readonly Scenario _scenario;
        readonly string _simulationDirectory;
        readonly SimulationContext _database;
        readonly List<PlatformBehavior> _platformBehaviors = new List<PlatformBehavior>();
        readonly List<PlatformState[]> _platformStates = new List<PlatformState[]>();
        SimulationLog _log;

        public void Start(TimeSpan timeStepSize)
        {
            var actorID = 0;
            var sourceActorModeID = 0;
            var timeStepCount = (int)Math.Round(((TimeSpan)_scenario.Duration).TotalSeconds / timeStepSize.TotalSeconds);
            var actors = new List<Actor>();
            foreach (var platform in _scenario.Platforms)
            {
                platform.ActorID = actorID++;
                var platformBehavior = new PlatformBehavior(platform, timeStepSize, timeStepCount);
                _platformBehaviors.Add(platformBehavior);
                _platformStates.Add(platformBehavior.PlatformStates.ToArray());
                var actor = new Actor { ID = platform.ActorID, Platform = platform };
                actors.Add(actor);
                _database.Actors.Add(actor);
                foreach (var mode in platform.Sources.SelectMany(source => source.Modes)) mode.SourceActorModeID = sourceActorModeID++;
            }
            foreach (var species in _scenario.ScenarioSpecies)
            {
                var animats = Animat.Load(species, species.SpeciesFilePath);
                species.StartActorID = actorID;
                foreach (var location in animats.Locations) _database.Actors.Add(new Actor { ID = actorID++, AnimatLocation = new AnimatLocation { Location = location, ScenarioSpecies = species } });
            }
            var actorCount = actorID;
            _log = SimulationLog.Create(Path.Combine(_simulationDirectory, "simulation.log"), timeStepCount, timeStepSize);
            for (var timeStepIndex = 0; timeStepIndex < timeStepCount; timeStepIndex++)
            {
                var timeStepRecord = new SimulationTimeStepRecord();
                var actorPositionRecords = new ActorPositionRecord[actorCount];
                var broadcastBlock = new BroadcastBlock<ActorPositionRecord[]>(a => a);
                foreach (var platform in _scenario.Platforms)
                {
                    var platformState = _platformStates[actorID][timeStepIndex];
                    actorPositionRecords[platform.ActorID] = new ActorPositionRecord((float)platformState.PlatformLocation.Location.Latitude,
                                                                                     (float)platformState.PlatformLocation.Location.Longitude,
                                                                                     platformState.PlatformLocation.Depth);
                    foreach (var activeMode in platformState.ModeActiveTimes.Keys)
                    {
                        var mode = activeMode;
                        var platformLocation = platformState.PlatformLocation.Location;
                        var course = Geo.DegreesToRadians(platformState.PlatformLocation.Course);
                        var thisPlatform = platform;
                        var actionBlock = new ActionBlock<ActorPositionRecord[]>(records =>
                                                                                 {
                                                                                     var geoArc = new GeoArc(platformLocation,
                                                                                                             course + mode.RelativeBeamAngle,
                                                                                                             mode.HorizontalBeamWidth,
                                                                                                             mode.MaxPropagationRadius);
                                                                                     for (var i = 0; i < records.Length; i++)
                                                                                     {
                                                                                         // Don't expose a platform to itself
                                                                                         if (thisPlatform.ActorID == i) continue;

                                                                                         var record = records[i];
                                                                                         var actorGeo = new Geo(record.Latitude, record.Longitude);
                                                                                         var radiusToActor = platformLocation.DistanceRadians(actorGeo);
                                                                                         var azimuthToActor = platformLocation.Azimuth(actorGeo);
                                                                                         if (!geoArc.Contains(radiusToActor, azimuthToActor)) continue;
                                                                                         // At this point we know the actor will be exposed to this mode
                                                                                         // so we want to find the nearest radial, load it if necessary
                                                                                         // then look up the TL value at the actor's range and depth cell
                                                                                         var rangeToActor = Geo.RadiansToMeters(radiusToActor);
                                                                                         var actorDepth = record.Depth;
                                                                                         var peakSPL = 1f;
                                                                                         var energy = 2f;
                                                                                         record.Exposures.Add(new ActorExposureRecord(mode.SourceActorModeID, peakSPL, energy));
                                                                                     }
                                                                                 },
                        new ExecutionDataflowBlockOptions { BoundedCapacity = -1, MaxDegreeOfParallelism = -1 });
                        broadcastBlock.LinkTo(actionBlock);
                    }
#if false
                    activeModes.AddRange(platformState[timeStepIndex].ModeActiveTimes.Keys);
                        timeStepRecord.ActorPositionRecords.Add(new ActorPositionRecord((float)animat.Location.Latitude,
                                                                                        (float)animat.Location.Longitude,
                                                                                        animat.Location.Data));
#endif
                }
            }
        }
    }

    [Serializable]
    public class SimulationException : Exception
    {
        //
        // For guidelines regarding the creation of new exception types, see
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/cpgenref/html/cpconerrorraisinghandlingguidelines.asp
        // and
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dncscol/html/csharp07192001.asp
        //

        public SimulationException() { }
        public SimulationException(string message) : base(message) { }
        public SimulationException(string message, Exception inner) : base(message, inner) { }

        protected SimulationException(
            SerializationInfo info,
            StreamingContext context) : base(info, context) { }
    }
}
