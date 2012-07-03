using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using ESME.Behaviors;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using HRC.Navigation;

namespace ESME.Simulator
{
    public class Simulation
    {
        public List<Actor> GetActors() { return _database.Actors.ToList(); }
        public Scenario Scenario { get { return _database.Scenarios.First(); } }
        readonly TransmissionLossCache _transmissionLossCache;

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
            _transmissionLossCache = new TransmissionLossCache("RadialCache", new NameValueCollection
            {
                { "physicalMemoryLimitPercentage", "50" }, 
                { "pollingInterval", "00:05:00" }
            });

        }

        readonly Scenario _scenario;
        readonly string _simulationDirectory;
        readonly SimulationContext _database;
        readonly List<PlatformBehavior> _platformBehaviors = new List<PlatformBehavior>();
        readonly List<PlatformState[]> _platformStates = new List<PlatformState[]>();
        SimulationLog _log;

        public async void Start(TimeSpan timeStepSize)
        {
            var actorCount = 0;
            var sourceActorModeID = 0;
            var timeStepCount = (int)Math.Round(((TimeSpan)_scenario.Duration).TotalSeconds / timeStepSize.TotalSeconds);
            var actors = new List<Actor>();
            foreach (var platform in _scenario.Platforms)
            {
                platform.ActorID = actorCount++;
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
                species.StartActorID = actorCount;
                foreach (var location in species.Animat.Locations) _database.Actors.Add(new Actor { ID = actorCount++, Species = species });
            }
            _log = SimulationLog.Create(Path.Combine(_simulationDirectory, "simulation.log"), timeStepCount, timeStepSize);
            for (var timeStepIndex = 0; timeStepIndex < timeStepCount; timeStepIndex++)
            {
                var actorPositionRecords = new ActorPositionRecord[actorCount];
                var broadcastBlock = new BroadcastBlock<int>(a => a);
                var actionBlocks = new List<Task>();
                foreach (var platform in _scenario.Platforms)
                {
                    var platformState = _platformStates[platform.ActorID][timeStepIndex];
                    actorPositionRecords[platform.ActorID] = new ActorPositionRecord((float)platformState.PlatformLocation.Location.Latitude,
                                                                                     (float)platformState.PlatformLocation.Location.Longitude,
                                                                                     platformState.PlatformLocation.Depth);
                    foreach (var activeMode in platformState.ModeActiveTimes.Keys)
                    {
                        var mode = activeMode;
                        var platformLocation = platformState.PlatformLocation.Location;
                        var course = Geo.DegreesToRadians(platformState.PlatformLocation.Course);
                        var thisPlatform = platform;
                        var actionBlock = new ActionBlock<int>(async index =>
                                                               {
                                                                   var geoArc = new GeoArc(platformLocation,
                                                                                           course + mode.RelativeBeamAngle,
                                                                                           mode.HorizontalBeamWidth,
                                                                                           mode.MaxPropagationRadius);
                                                                   // Don't expose a platform to itself
                                                                   if (thisPlatform.ActorID == index) return;

                                                                   var record = actorPositionRecords[index];
                                                                   var actorGeo = new Geo(record.Latitude, record.Longitude);
                                                                   var radiusToActor = platformLocation.DistanceRadians(actorGeo);
                                                                   var azimuthToActor = platformLocation.Azimuth(actorGeo);
                                                                   if (!geoArc.Contains(radiusToActor, azimuthToActor)) return;
                                                                   // At this point we know the actor will be exposed to this mode
                                                                   // Find the nearest radial
                                                                   var closestRadial = Scenario.ClosestTransmissionLoss(platformLocation, mode)
                                                                       .ClosestRadial(Geo.RadiansToDegrees(azimuthToActor));
                                                                   // Load it into the cache if it's not already there
                                                                   var tlTask = _transmissionLossCache[closestRadial];
                                                                   await tlTask;
                                                                   // Look up the TL value at the actor's range and depth
                                                                   var peakSPL = tlTask.Result.TransmissionLossRadial[Geo.RadiansToMeters(radiusToActor), record.Depth];
                                                                   record.Exposures.Add(new ActorExposureRecord(mode.SourceActorModeID, peakSPL, peakSPL));
                                                               },
                                                               new ExecutionDataflowBlockOptions { BoundedCapacity = -1, MaxDegreeOfParallelism = -1 });
                        actionBlocks.Add(actionBlock.Completion);
                        broadcastBlock.LinkTo(actionBlock);
                    }
                    foreach (var species in _scenario.ScenarioSpecies)
                    {
                        for (var animatIndex = 0; animatIndex < species.Animat.Locations.Count; animatIndex++)
                        {
                            var actorID = species.StartActorID + animatIndex;
                            actorPositionRecords[actorID] = new ActorPositionRecord((float)species.Animat.Locations[animatIndex].Latitude,
                                                                                    (float)species.Animat.Locations[animatIndex].Longitude,
                                                                                    species.Animat.Locations[animatIndex].Data);
                        }
                    }
                    for (var actorID = 0; actorID < actorCount; actorID++) broadcastBlock.Post(actorID);
                    broadcastBlock.Complete();
                    await TaskEx.WhenAll(actionBlocks);
                    Debug.WriteLine(string.Format("{0}: Completed time step {1} of {2}", DateTime.Now, timeStepIndex, timeStepCount));
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
