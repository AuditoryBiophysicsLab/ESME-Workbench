using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;
using System.Threading;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using ESME.Behaviors;
using ESME.NEMO;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using ESME.SimulationAnalysis;
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
            Scenario = scenario;
            _transmissionLossCache = new TransmissionLossCache("RadialCache", new NameValueCollection
            {
                { "physicalMemoryLimitPercentage", "50" }, 
                { "pollingInterval", "00:05:00" }
            });
        }

        public Scenario Scenario { get; private set; }
        readonly string _simulationDirectory;
        //readonly SimulationContext _database;
        readonly List<PlatformBehavior> _platformBehaviors = new List<PlatformBehavior>();
        readonly List<PlatformState[]> _platformStates = new List<PlatformState[]>();
        public List<Actor> Actors { get; private set; }
        public PercentProgress<Simulation> PercentProgress { get; private set; }
        public SimulationLog SimulationLog { get; private set; }
        CancellationTokenSource _cancellationTokenSource;
        public void Cancel() { _cancellationTokenSource.Cancel(); }
        public TimeSpan TimeStepSize { get; private set; }
        public ModeThresholdHistogram ModeThresholdHistogram { get; set; }
        //public SpeciesThresholdHistogram SpeciesThresholdHistogram { get; set; }

        public Task Start(TimeSpan timeStepSize)
        {
            TimeStepSize = timeStepSize;
            _cancellationTokenSource = new CancellationTokenSource();
            var task = new Task(() => Run(timeStepSize, _cancellationTokenSource.Token));
            task.Start();
            return task;
        }

        int _totalExposureCount;
        void Run(TimeSpan timeStepSize, CancellationToken token)
        {
            var actorCount = 0;
            var sourceActorModeID = 0;
            var timeStepCount = (int)Math.Round(((TimeSpan)Scenario.Duration).TotalSeconds / timeStepSize.TotalSeconds);
            PercentProgress = new PercentProgress<Simulation>(this) { MinimumValue = 0, MaximumValue = timeStepCount - 1 };
            Actors = new List<Actor>();
            foreach (var platform in Scenario.Platforms)
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
            foreach (var species in Scenario.ScenarioSpecies)
            {
                species.StartActorID = actorCount;
                for (var i = 0; i < species.Animat.Locations.Count; i++) Actors.Add(new Actor { ID = actorCount + i, Species = species });
                actorCount += species.Animat.Locations.Count;
            }
            ModeThresholdHistogram = new ModeThresholdHistogram
            {
                ModeBinnedExposureDictionary =
                {
                    Filter1 = SpeciesIndexFromActorExposureRecord,
                    Filter2 = record => record.SourceActorModeID,
                },
            };
            //SpeciesThresholdHistogram = new SpeciesThresholdHistogram(this);

            SimulationLog = SimulationLog.Create(Path.Combine(_simulationDirectory, "simulation.log"), timeStepSize, Scenario);
            
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
                foreach (var platform in Scenario.Platforms)
                {
                    var platformState = _platformStates[platform.ActorID][timeStepIndex];
                    actorPositionRecords[platform.ActorID] = new ActorPositionRecord(platformState.PlatformLocation.Location, platformState.PlatformLocation.Depth);
                    foreach (var activeMode in platformState.ModeActiveTimes.Keys)
                    {
                        var mode = activeMode;
                        var platformLocation = platformState.PlatformLocation.Location;
                        var thisPlatform = platform;
                        var scenario = Scenario;
                        var geoArc = new GeoArc(platformLocation,
                                                Geo.DegreesToRadians(platformState.PlatformLocation.Course + mode.RelativeBeamAngle),
                                                Geo.DegreesToRadians(mode.HorizontalBeamWidth),
                                                Geo.MetersToRadians(mode.MaxPropagationRadius));
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
                            var peakSPL = mode.SourceLevel - tlTask.Result.TransmissionLossRadial[Geo.RadiansToMeters(radiansToActor), -record.Depth];
                            record.Exposures.Add(new ActorExposureRecord(index, mode, peakSPL, peakSPL));
                            Interlocked.Increment(ref _totalExposureCount);
                        }, new ExecutionDataflowBlockOptions { BoundedCapacity = -1, MaxDegreeOfParallelism = -1 });
                        var bufferBlock = new BufferBlock<int>();
                        bufferBlock.LinkTo(actionBlock);
                        bufferBlocks.Add(bufferBlock);
                        actionBlockCompletions.Add(actionBlock.Completion);
                        bufferBlock.Completion.ContinueWith(t => actionBlock.Complete());
                    }
                }
                foreach (var species in Scenario.ScenarioSpecies)
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
                ModeThresholdHistogram.Process(timeStepRecord);
                //SpeciesThresholdHistogram.Process(timeStepRecord);
                logBuffer.Post(timeStepRecord);
                // todo: when we have 3MB support, this is where we do the animat movement
                // MoveAnimats();
                if (token.IsCancellationRequested) break;
            }
            logBuffer.Complete();
            logBlock.Completion.Wait();
            Debug.WriteLine(string.Format("{0}: Simulation complete. Exposure count: {1}", DateTime.Now, _totalExposureCount));
            Debug.WriteLine("Species by Mode");
            ModeThresholdHistogram.Display(speciesIndex => Scenario.ScenarioSpecies[speciesIndex].LatinName, modeID => (from platform in Scenario.Platforms from source in platform.Sources from mode in source.Modes where mode.SourceActorModeID == modeID select mode.ModeName).FirstOrDefault());
            //SpeciesThresholdHistogram.Display();
        }

        int SpeciesIndexFromActorID(int actorID)
        {
            if (actorID < 0) throw new ParameterOutOfRangeException("actorID must be non-negative");
            if (actorID < Scenario.ScenarioSpecies[0].StartActorID) throw new ParameterOutOfRangeException("actorID must be greater than the highest platform number");
            for (var i = 0; i < Scenario.ScenarioSpecies.Count - 1; i++)
                if (Scenario.ScenarioSpecies[i].StartActorID <= actorID && actorID < Scenario.ScenarioSpecies[i + 1].StartActorID) return i;
            return Scenario.ScenarioSpecies.Count - 1;
        }

        int? SpeciesIndexFromActorExposureRecord(ActorExposureRecord record)
        {
            if (record.ActorID < 0) return null;
            if (record.ActorID < Scenario.ScenarioSpecies[0].StartActorID) return null;
            for (var i = 0; i < Scenario.ScenarioSpecies.Count - 1; i++)
                if (Scenario.ScenarioSpecies[i].StartActorID <= record.ActorID && record.ActorID < Scenario.ScenarioSpecies[i + 1].StartActorID) return i;
            return Scenario.ScenarioSpecies.Count - 1;
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
