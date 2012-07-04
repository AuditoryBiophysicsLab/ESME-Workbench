using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;
using System.Threading;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using ESME.Behaviors;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Simulator
{
    [NotifyPropertyChanged]
    public class Simulation
    {
        //public List<Actor> GetActors() { return _database.Actors.ToList(); }
        //public Scenario Scenario { get { return _database.Scenarios.First(); } }
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
            //_database = SimulationContext.OpenOrCreate(Path.Combine(_simulationDirectory, "simulation.db"));
            //_scenario = _database.ImportScenario(scenario);
            _scenario = scenario;
            _transmissionLossCache = new TransmissionLossCache("RadialCache", new NameValueCollection
            {
                { "physicalMemoryLimitPercentage", "50" }, 
                { "pollingInterval", "00:05:00" }
            });
        }

        readonly Scenario _scenario;
        readonly string _simulationDirectory;
        //readonly SimulationContext _database;
        readonly List<PlatformBehavior> _platformBehaviors = new List<PlatformBehavior>();
        readonly List<PlatformState[]> _platformStates = new List<PlatformState[]>();
        public List<Actor> Actors { get; private set; }
        public PercentProgress<Simulation> PercentProgress { get; private set; }
        public SimulationLog SimulationLog { get; private set; }
        CancellationTokenSource _cancellationTokenSource;
        public void Cancel() { _cancellationTokenSource.Cancel(); }

        public Task Start(TimeSpan timeStepSize)
        {
            _cancellationTokenSource = new CancellationTokenSource();
            var task = new Task(() => Run(timeStepSize, _cancellationTokenSource.Token));
            task.Start();
            return task;
        }

        void Run(TimeSpan timeStepSize, CancellationToken token)
        {
            var actorCount = 0;
            var sourceActorModeID = 0;
            var timeStepCount = (int)Math.Round(((TimeSpan)_scenario.Duration).TotalSeconds / timeStepSize.TotalSeconds);
            PercentProgress = new PercentProgress<Simulation>(this) { MinimumValue = 0, MaximumValue = timeStepCount - 1 };
            Actors = new List<Actor>();
            foreach (var platform in _scenario.Platforms)
            {
                platform.ActorID = actorCount++;
                var platformBehavior = new PlatformBehavior(platform, timeStepSize, timeStepCount);
                _platformBehaviors.Add(platformBehavior);
                _platformStates.Add(platformBehavior.PlatformStates.ToArray());
                var actor = new Actor { ID = platform.ActorID, Platform = platform };
                Actors.Add(actor);
                //_database.Actors.Add(actor);
                foreach (var mode in platform.Sources.SelectMany(source => source.Modes)) mode.SourceActorModeID = sourceActorModeID++;
            }
            foreach (var species in _scenario.ScenarioSpecies)
            {
                species.StartActorID = actorCount;
                for (var i = 0; i < species.Animat.Locations.Count; i++) Actors.Add(new Actor { ID = actorCount + i, Species = species });
                actorCount += species.Animat.Locations.Count;
            }

            SimulationLog = SimulationLog.Create(Path.Combine(_simulationDirectory, "simulation.log"), timeStepCount, timeStepSize);
            
            var logBlock = new ActionBlock<SimulationTimeStepRecord>(block => SimulationLog.Add(block), new ExecutionDataflowBlockOptions { BoundedCapacity = 1, MaxDegreeOfParallelism = 1 });
            logBlock.Completion.ContinueWith(t => SimulationLog.Close());

            var logBuffer = new BufferBlock<SimulationTimeStepRecord>();
            logBuffer.LinkTo(logBlock);
            logBuffer.Completion.ContinueWith(t => logBlock.Complete());

            for (var timeStepIndex = 0; timeStepIndex < timeStepCount; timeStepIndex++)
            {
                var actorPositionRecords = new ActorPositionRecord[actorCount];
                var actionBlockCompletions = new List<Task>();
                var bufferBlocks = new List<BufferBlock<int>>();
                foreach (var platform in _scenario.Platforms)
                {
                    var platformState = _platformStates[platform.ActorID][timeStepIndex];
                    actorPositionRecords[platform.ActorID] = new ActorPositionRecord(platformState.PlatformLocation.Location, platformState.PlatformLocation.Depth);
                    foreach (var activeMode in platformState.ModeActiveTimes.Keys)
                    {
                        var mode = activeMode;
                        var platformLocation = platformState.PlatformLocation.Location;
                        var thisPlatform = platform;
                        var scenario = _scenario;
                        var geoArc = new GeoArc(platformLocation,
                                                Geo.DegreesToRadians(platformState.PlatformLocation.Course + mode.RelativeBeamAngle),
                                                Geo.DegreesToRadians(mode.HorizontalBeamWidth),
                                                Geo.MetersToRadians(mode.MaxPropagationRadius));
                        //var exposedCount = 0;
                        var actionBlock = new ActionBlock<int>(async index =>
                        {
                            // Don't expose a platform to itself
                            if (thisPlatform.ActorID == index) return;

                            var record = actorPositionRecords[index];
                            var actorGeo = new Geo(record.Latitude, record.Longitude);
                            var radiansToActor = platformLocation.DistanceRadians(actorGeo);
                            var azimuthToActor = platformLocation.Azimuth(actorGeo);
                            if (!geoArc.Contains(radiansToActor, azimuthToActor)) return;
                            // At this point we know the actor will be exposed to this mode
                            // Find the nearest radial
                            var closestRadial = scenario.ClosestTransmissionLoss(platformLocation, mode)
                                .ClosestRadial(Geo.RadiansToDegrees(azimuthToActor));
                            // Load it into the cache if it's not already there
                            var tlTask = _transmissionLossCache[closestRadial];
                            await tlTask;
                            // Look up the TL value at the actor's range and depth
                            var peakSPL = tlTask.Result.TransmissionLossRadial[Geo.RadiansToMeters(radiansToActor), -record.Depth];
                            record.Exposures.Add(new ActorExposureRecord(mode.SourceActorModeID, peakSPL, peakSPL));
                            //Interlocked.Increment(ref exposedCount);
                        }, new ExecutionDataflowBlockOptions { BoundedCapacity = -1, MaxDegreeOfParallelism = -1 });
                        var bufferBlock = new BufferBlock<int>();
                        bufferBlock.LinkTo(actionBlock);
                        bufferBlocks.Add(bufferBlock);
                        actionBlockCompletions.Add(actionBlock.Completion);
                        bufferBlock.Completion.ContinueWith(t => actionBlock.Complete());
                    }
                }
                foreach (var species in _scenario.ScenarioSpecies)
                {
                    for (var animatIndex = 0; animatIndex < species.Animat.Locations.Count; animatIndex++)
                    {
                        var actorID = species.StartActorID + animatIndex;
                        actorPositionRecords[actorID] = new ActorPositionRecord(species.Animat.Locations[animatIndex]);
                    }
                }
                foreach (var bufferBlock in bufferBlocks)
                {
                    //Debug.WriteLine("Sending actor IDs to an active mode");
                    for (var actorID = 0; actorID < actorCount; actorID++) bufferBlock.Post(actorID);
                    bufferBlock.Complete();
                }
                //Debug.WriteLine("Actor IDs sent.  Waiting for completion.");
                TaskEx.WhenAll(actionBlockCompletions).Wait();
                PercentProgress.Report(timeStepIndex);
                //Debug.WriteLine(string.Format("{0}: Finished time step {1} of {2}: {3:0%} complete", DateTime.Now, timeStepIndex, timeStepCount, (float)timeStepIndex / timeStepCount));
                var timeStepRecord = new SimulationTimeStepRecord();
                timeStepRecord.ActorPositionRecords.AddRange(actorPositionRecords);
                logBuffer.Post(timeStepRecord);
                // todo: when we have 3MB support, this is where we do the animat movement
                // MoveAnimats();
                if (token.IsCancellationRequested) break;
            }
            logBuffer.Complete();
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
