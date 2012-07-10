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
using System.Windows.Threading;
using ESME.Behaviors;
using ESME.Model;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using ESME.SimulationAnalysis;
using HRC.Aspects;
using HRC.Collections;
using HRC.Navigation;
using HRC.Utility;
using HRC.WPF;
using mbs;

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
        public Dispatcher Dispatcher { get; set; }
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
            SimulationLog = SimulationLog.Create(Path.Combine(_simulationDirectory, "simulation.log"), timeStepSize, Scenario);
            ModeThresholdHistogram = new ModeThresholdHistogram(SimulationLog);
            //SpeciesThresholdHistogram = new SpeciesThresholdHistogram(this);
            return TaskEx.Run(() => Run(timeStepSize, _cancellationTokenSource.Token));
        }

        int _totalExposureCount;
        int[] _exposuresBySpecies;
        int[] _speciesActorIDStart;
        int[] _speciesActorIDEnd;
        void Run(TimeSpan timeStepSize, CancellationToken token)
        {
            Geo<float> firstAnimatPosition = null;
            var timeStepCount = (int)Math.Round(((TimeSpan)Scenario.Duration).TotalSeconds / timeStepSize.TotalSeconds);
            Initialize3MB();
            PercentProgress = new PercentProgress<Simulation>(this) { MinimumValue = 0, MaximumValue = timeStepCount - 1 };
            Actors = new List<Actor>();
            foreach (var platform in Scenario.Platforms)
            {
                var platformBehavior = new PlatformBehavior(platform, timeStepSize, timeStepCount);
                _platformBehaviors.Add(platformBehavior);
                _platformStates.Add(platformBehavior.PlatformStates.ToArray());
                var actor = new Actor { ID = platform.ActorID, Platform = platform };
                Actors.Add(actor);
            }
            _exposuresBySpecies = new int[Scenario.ScenarioSpecies.Count];
            _speciesActorIDStart = new int[Scenario.ScenarioSpecies.Count];
            _speciesActorIDEnd = new int[Scenario.ScenarioSpecies.Count];
            for (var j = 0; j < Scenario.ScenarioSpecies.Count; j++)
            {
                var species = Scenario.ScenarioSpecies[j];
                _speciesActorIDStart[j] = species.StartActorID;
                _speciesActorIDEnd[j] = species.StartActorID + species.Animat.Locations.Count - 1;
                if (firstAnimatPosition == null) firstAnimatPosition = new Geo<float>(species.Animat.Locations[0].Latitude, species.Animat.Locations[0].Longitude) { Data = species.Animat.Locations[0].Data };
                for (var i = 0; i < species.Animat.Locations.Count; i++) Actors.Add(new Actor { ID = species.StartActorID + i, Species = species });
            }
            var actorCount = Scenario.ScenarioSpecies.Last().StartActorID + Scenario.ScenarioSpecies.Last().Animat.Locations.Count;
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
                            for (var i = 0; i < Scenario.ScenarioSpecies.Count; i++)
                                if (_speciesActorIDStart[i] <= index && index <= _speciesActorIDEnd[i]) Interlocked.Increment(ref _exposuresBySpecies[i]);
                            //var actorRecord = SimulationLog.RecordFromActorID(index) as SpeciesNameGuid;
                            //if (actorRecord != null) Interlocked.Increment(ref _exposuresBySpecies[SimulationLog.SpeciesRecords.IndexOf(actorRecord)]);
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
                var timeStepRecord = new SimulationTimeStepRecord();
                timeStepRecord.ActorPositionRecords.AddRange(actorPositionRecords);
                ModeThresholdHistogram.Process(timeStepRecord);
                //SpeciesThresholdHistogram.Process(timeStepRecord);
                logBuffer.Post(timeStepRecord);
                // todo: when we have 3MB support, this is where we do the animat movement
                MoveAnimats();
                var distance = Scenario.ScenarioSpecies[0].Animat.Locations[0].DistanceKilometers(firstAnimatPosition);
                if (distance > 0.01) Debug.WriteLine(string.Format("{0}: First animat has moved {1:0.##} km from initial location", DateTime.Now, distance));
                Debug.WriteLine(string.Format("{0}: Finished time step {1} of {2}: {3:0%} complete", DateTime.Now, timeStepIndex, timeStepCount, Math.Round((float)timeStepIndex / timeStepCount, 3)));
                if (Dispatcher != null)
                    Dispatcher.InvokeIfRequired(() =>
                                                {
                                                    foreach (var species in Scenario.ScenarioSpecies)
                                                    {
                                                        species.RemoveMapLayers();
                                                        species.CreateMapLayers();
                                                    }
                                                });
                if (token.IsCancellationRequested) break;
            }
            logBuffer.Complete();
            logBlock.Completion.Wait();
            Shutdown3MB();
            Debug.WriteLine(string.Format("{0}: Simulation complete. Exposure count: {1}", DateTime.Now, _totalExposureCount));
            Debug.WriteLine(string.Format("{0}: Exposures by species:", DateTime.Now));
            for (var i = 0; i < _exposuresBySpecies.Length; i++) Debug.WriteLine(string.Format("{0}: Species: {1}, Exposures: {2}", DateTime.Now, Scenario.ScenarioSpecies[i].LatinName, _exposuresBySpecies[i]));
            ModeThresholdHistogram.Display();
            ModeThresholdHistogram.Write(Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "Simulation Test",Scenario.Name+".xml"), Scenario.Name, Scenario.Location.Name);
            WriteMatlabFiles();
            //SpeciesThresholdHistogram.Display();
        }

        void WriteMatlabFiles()
        {
            using (var w = new StringWriter())
            {
                var xmlPath = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "Simulation Test", Scenario.Name + ".xml");
                w.WriteLine("simulatorOutputStruct = plotSpeciesModeHistograms('{0}',true);",xmlPath);
                File.WriteAllText(Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "Simulation Test", "batch.m"),w.ToString());
            }

        }

        class AnimatContext
        {
            public ScenarioSpecies Species { get; set; }
            public int SpeciesInstanceIndex { get; set; }
            public int SpeciesAnimatIndex { get; set; }
        }

        C3mbs[] _mbs;
        AnimatContext[][] _animatContext;
        readonly Dictionary<string, int> _speciesNameToIndex = new Dictionary<string, int>();
        //double[] _animatSoundExposure;
        void Initialize3MB()
        {
            var nextSpeciesIndex = 0;
            var yxzFileName = Path.Combine(Path.GetTempPath(), Path.GetFileNameWithoutExtension(Path.GetTempFileName()) + ".txt");
            Scenario.BathymetryData.ToYXZ(yxzFileName, -1);
            var contexts = new List<AnimatContext>();
            foreach (var species in Scenario.ScenarioSpecies)
            {
                if (!_speciesNameToIndex.ContainsKey(species.SpeciesDefinitionFilePath)) 
                    _speciesNameToIndex.Add(species.SpeciesDefinitionFilePath, nextSpeciesIndex++);
                var speciesInstanceIndex = _speciesNameToIndex[species.SpeciesDefinitionFilePath];
                var curSpecies = species;
                contexts.AddRange(species.Animat.Locations.Select((t, speciesIndex) => new AnimatContext
                {
                    Species = curSpecies, 
                    SpeciesInstanceIndex = speciesInstanceIndex, 
                    SpeciesAnimatIndex = speciesIndex
                }));
            }
            var splitContext = contexts.Split(1).ToList();
            _mbs = new C3mbs[splitContext.Count];
            _animatContext = new AnimatContext[splitContext.Count][];
            for (var mbsIndex = 0; mbsIndex < splitContext.Count; mbsIndex++)
            {
                mbsRESULT result;
                _mbs[mbsIndex] = new C3mbs();
                
                // set the output directory
                if (mbsRESULT.OK != (result = _mbs[mbsIndex].SetOutputDirectory(_simulationDirectory))) 
                    throw new AnimatInterfaceMMBSException("SetOutputDirectory Error:" + _mbs[mbsIndex].ResultToTc(result));

                var config = _mbs[mbsIndex].GetConfiguration();
                config.seedValue = (uint)mbsIndex;  // Probably wise to set each 3mb instance to a unique randomizer seed value.
                config.enabled = false;             // binary output enabled/disabled
                config.durationLess = true;         // make sure we're in durationless mode.
                _mbs[mbsIndex].SetConfiguration(config);
                result = _mbs[mbsIndex].LoadBathymetryFromTextFile(yxzFileName);
                if (mbsRESULT.OK != result) throw new AnimatInterfaceMMBSException("Bathymetry failed to load: " + _mbs[mbsIndex].ResultToTc(result));

                foreach (var speciesFilePath in _speciesNameToIndex.Keys)
                {
                    result = _mbs[mbsIndex].AddSpecies(speciesFilePath);
                    if (mbsRESULT.OK != result) throw new AnimatInterfaceMMBSException(string.Format("C3mbs::AddSpecies FATAL error {0} for species {1}", _mbs[mbsIndex].ResultToTc(result), speciesFilePath));
                }
                var context = splitContext[mbsIndex].ToList();
                _animatContext[mbsIndex] = new AnimatContext[context.Count];
                //_animatPositions[mbsIndex] = new mbsPosition[context.Count];
                //Debug.WriteLine("Thread {0} contains {1} animats", mbsIndex, context.Count);
                for (var animatIndex = 0; animatIndex < context.Count; animatIndex++)
                //for (var animatIndex = 0; animatIndex < 1; animatIndex++)
                    {
                    var species = context[animatIndex].Species;
                    var geo = species.Animat.Locations[context[animatIndex].SpeciesAnimatIndex];
                    _animatContext[mbsIndex][animatIndex] = new AnimatContext { Species = species, SpeciesAnimatIndex = context[animatIndex].SpeciesAnimatIndex };
                    result = _mbs[mbsIndex].AddIndividualAnimat(context[animatIndex].SpeciesInstanceIndex, new mbsPosition { latitude = geo.Latitude, longitude = geo.Longitude, depth = 0 });
                    if (mbsRESULT.OK != result) throw new AnimatInterfaceMMBSException(string.Format("C3mbs::AddIndividualAnimat FATAL error {0}", _mbs[mbsIndex].ResultToTc(result)));
                    //Debug.WriteLine("Animat {0} lat: {1:0.####} lon: {2:0.####} depth: {3:0.#}, bathy: {4:0.#}", animatIndex, geo.Latitude, geo.Longitude, geo.Data, Scenario.BathymetryData.Samples.GetNearestPoint(geo).Data);
                }
                _mbs[mbsIndex].SetDuration((int)((TimeSpan)Scenario.Duration).TotalSeconds);
                result = _mbs[mbsIndex].SaveScenario(Path.Combine(_simulationDirectory, "test.sce"));
                if (mbsRESULT.OK != result) throw new AnimatInterfaceMMBSException(string.Format("C3mbs::SaveScenario FATAL error {0}", _mbs[mbsIndex].ResultToTc(result)));
#if true
                //----------------------------------------------------------------------//
                // Initialize the scenario.
                //----------------------------//
                result = _mbs[mbsIndex].InitializeRun();
                if (mbsRESULT.OK == result)
                {
                    // Wait for reach 3mb instance to finish initialzing (run state wont
                    // be mbsRUNSTATE.INITIALIZING) before initializing the instance.
                    mbsRUNSTATE runState;
                    do
                    {
                        Thread.Sleep(20);
                        runState = _mbs[mbsIndex].GetRunState();
                    } while (mbsRUNSTATE.INITIALIZING == runState);
                }
                else
                {
                    throw new AnimatInterfaceMMBSException("C3mbs::Initialize FATAL error " + _mbs[mbsIndex].ResultToTc(result));
                }
#else
                if (mbsRESULT.OK != (result = _mbs[mbsIndex].RunScenarioNumIterations(0))) throw new AnimatInterfaceMMBSException("C3mbs::RunScenarioNumIterations FATAL error " + _mbs[mbsIndex].ResultToTc(result));
                while (_mbs[mbsIndex].GetRunState() == mbsRUNSTATE.INITIALIZING) Thread.Sleep(1);
#endif
            }
            UpdateAnimatPositions();
        }

        void UpdateAnimatPositions()
        {
            for (var mbsIndex = 0; mbsIndex < _mbs.Length; mbsIndex++)
            {
                mbsRESULT result;
                var positions = new mbsPosition[_animatContext[mbsIndex].Length];
                if (mbsRESULT.OK != (result = _mbs[mbsIndex].GetAnimatCoordinates(positions)))
                {
                    _mbs[mbsIndex].AbortRun(); // kills the thread.
                    throw new AnimatInterfaceMMBSException("C3mbs::GetAnimatCoordinates FATAL error " + _mbs[mbsIndex].ResultToTc(result));
                }
                for (var contextIndex = 0; contextIndex < _animatContext[mbsIndex].Length; contextIndex++)
                {
                    var species = _animatContext[mbsIndex][contextIndex].Species;
                    var position = positions[contextIndex];
                    species.Animat.Locations[_animatContext[mbsIndex][contextIndex].SpeciesAnimatIndex] = new Geo<float>(position.latitude, position.longitude) { Data = -(float)position.depth };
                }
            }
        }

        void MoveAnimats()
        {
            for (var mbsIndex = 0; mbsIndex < _mbs.Length; mbsIndex++)
            {
                var result = _mbs[mbsIndex].RunScenarioNumIterations((int)Math.Round(TimeStepSize.TotalSeconds));
                if (result != mbsRESULT.OK)
                {
                    _mbs[mbsIndex].AbortRun();
                    throw new AnimatInterfaceMMBSException("C3mbs::RunScenarioNumIterations FATAL error: " + _mbs[mbsIndex].ResultToTc(result));
                }
                while (mbsRUNSTATE.RUNNING == _mbs[mbsIndex].GetRunState()) Thread.Sleep(1);
            }

            UpdateAnimatPositions();
        }

        void Shutdown3MB()
        {
            foreach (var mbs in _mbs)
            {
                var result = mbs.FinishRun();
                if (mbsRESULT.OK != result) throw new AnimatInterfaceMMBSException("C3mbs::FinishRun FATAL error " + mbs.ResultToTc(result)); 
            }
        }

#if false
        void UpdateAnimatSoundExposure()
        {
            mbsRESULT result;

            const double curSPL = 0.0;
            const double lat = 0.0;
            const double lon = 0.0;

            for (var i = 0; i < Animat.Count; i++) _animatSoundExposure[i] = curSPL;

            if (mbsRESULT.OK != (result = _mmmbs.SetAnimatAcousticExposure(lat, lon, _mbsSoundExposure)))
            {
                _mmmbs.AbortRun();
                throw new AnimatInterfaceMMBSException("C3mbs::SetAnimatAcousticExposure FATAL error " + _mmmbs.MbsResultToString(result));
            }
        }
#endif

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
