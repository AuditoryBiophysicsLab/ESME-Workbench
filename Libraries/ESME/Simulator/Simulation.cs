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
using System.Windows.Media;
using System.Windows.Threading;
using ESME.Behaviors;
using ESME.Mapping;
using ESME.Model;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using ESME.SimulationAnalysis;
using HRC;
using HRC.Aspects;
using HRC.Collections;
using HRC.Navigation;
using HRC.Utility;
using HRC.WPF;
using mbs;

namespace ESME.Simulator
{
    [NotifyPropertyChanged]
    public class Simulation : IHistogramSource
    {
        //public List<Actor> GetActors() { return _database.Actors.ToList(); }
        //public Scenario Scenario { get { return _database.Scenarios.First(); } }
        readonly TransmissionLossCache _transmissionLossCache;

        public static Simulation Create(Scenario scenario, string simulationDirectory, Dispatcher dispatcher)
        {
            Directory.CreateDirectory(simulationDirectory);
            var result = new Simulation(scenario, simulationDirectory, dispatcher)
            {
                MovingAnimats = false,
                AnimateSimulation = true,
                TimeStepSize = (from p in scenario.Platforms
                                from s in p.Sources
                                from m in s.Modes
                                select (TimeSpan)m.PulseInterval).Max()
            };
            return result;
        }

        Simulation(Scenario scenario, string simulationDirectory, Dispatcher dispatcher)
        {
            _simulationDirectory = simulationDirectory;
            Dispatcher = dispatcher;
            //_database = SimulationContext.OpenOrCreate(Path.Combine(_simulationDirectory, "simulation.db"));
            //_scenario = _database.ImportScenario(scenario);
            Scenario = scenario;
            _transmissionLossCache = new TransmissionLossCache("RadialCache", new NameValueCollection
            {
                { "physicalMemoryLimitPercentage", "50" }, 
                { "pollingInterval", "00:05:00" }
            });
            foreach (var species in scenario.ScenarioSpecies)
                GuidToColorMap.Add(species.Guid, species.LayerSettings.LineOrSymbolColor);
        }

        public Scenario Scenario { get; private set; }
        public Dispatcher Dispatcher { get; private set; }
        readonly string _simulationDirectory;
        //readonly SimulationContext _database;
        readonly List<PlatformState[]> _platformStates = new List<PlatformState[]>();
        readonly List<OverlayShapeMapLayer[]> _modeFootprintMapLayers = new List<OverlayShapeMapLayer[]>();
        public List<Actor> Actors { get; private set; }
        public PercentProgress<Simulation> PercentProgress { get; private set; }
        public SimulationLog SimulationLog { get; private set; }
        CancellationTokenSource _cancellationTokenSource;
        public void Cancel() { _cancellationTokenSource.Cancel(); }
        public TimeSpan TimeStepSize { get; private set; }

        public ModeThresholdHistogram ModeThresholdHistogram { get; set; }
        //public SpeciesThresholdHistogram SpeciesThresholdHistogram { get; set; }
        public bool AnimateSimulation { get; set; }
        public bool MovingAnimats { get; set; }
        public Task Start(TimeSpan timeStepSize)
        {
            TimeStepSize = timeStepSize;
            _cancellationTokenSource = new CancellationTokenSource();
            SimulationLog = SimulationLog.Create(Path.Combine(_simulationDirectory, "simulation.exposures"), TimeStepSize, Scenario);
            ModeThresholdHistogram = new ModeThresholdHistogram(this, SimulationLog, 100.0, 10.0, 10);
            //SpeciesThresholdHistogram = new SpeciesThresholdHistogram(this);
            return TaskEx.Run(() => Run(TimeStepSize, _cancellationTokenSource.Token));
        }

        int _totalExposureCount;
        int[] _exposuresBySpecies;
        int[] _speciesActorIDStart;
        int[] _speciesActorIDEnd;
        async void Run(TimeSpan timeStepSize, CancellationToken token)
        {
            Geo<float> firstAnimatPosition = null;
            Task<bool> processTask = null;
            var timeStepCount = (int)Math.Round(((TimeSpan)Scenario.Duration).TotalSeconds / timeStepSize.TotalSeconds);
            if (MovingAnimats) Initialize3MB();
            PercentProgress = new PercentProgress<Simulation>(this) { MinimumValue = 0, MaximumValue = timeStepCount - 1 };
            Actors = new List<Actor>();
            foreach (var platform in Scenario.Platforms)
            {
                platform.PlatformBehavior = new PlatformBehavior(platform, timeStepSize, timeStepCount);
                var behaviors = platform.PlatformBehavior.PlatformStates.ToArray();

                _platformStates.Add(behaviors);
                var curPlatform = platform;
                Dispatcher.InvokeIfRequired(() =>
                                            {
                                                curPlatform.RemoveMapLayers();
                                                curPlatform.CreateMapLayers();
                                                var mapLayers = CreateFootprintMapLayers(curPlatform, behaviors[0]).ToArray();
                                                _modeFootprintMapLayers.Add(mapLayers);
                                                foreach (var layer in mapLayers)
                                                {
                                                    MediatorMessage.Send(MediatorMessage.AddMapLayer, layer);
                                                    MediatorMessage.Send(MediatorMessage.HideMapLayer, layer);
                                                }
                                            });
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
            Task moveTask = null;
            for (var timeStepIndex = 0; timeStepIndex < timeStepCount; timeStepIndex++)
            {

                if (MovingAnimats) moveTask = MoveAnimatsAsync();
                var actorPositionRecords = new ActorPositionRecord[actorCount];
                var actionBlockCompletions = new List<Task>();
                var bufferBlocks = new List<BufferBlock<int>>();
                foreach (var platform in Scenario.Platforms)
                {
                    var platformState = _platformStates[platform.ActorID][timeStepIndex];
                    actorPositionRecords[platform.ActorID] = new ActorPositionRecord(platformState.PlatformLocation.Location, platformState.PlatformLocation.Depth);
                    foreach (var activeMode in platformState.ModeActiveTimes.Keys)
                    {
                        var platformModeLayers = _modeFootprintMapLayers[Scenario.Platforms.IndexOf(platform)];
                        var activeModeLayerName = string.Format("{0}-footprint", activeMode.Guid);
                        var curModeLayer = (from l in platformModeLayers
                                            where l.Name == activeModeLayerName
                                            select l).FirstOrDefault();
                        if (curModeLayer != null)
                        {
                            UpdateFootprintMapLayer(activeMode, platformState, curModeLayer);
                            var isActive = platformState.ModeActiveTimes[activeMode].Ticks > 0;
                            Dispatcher.InvokeIfRequired(() =>
                                                        {
                                                            MediatorMessage.Send(AnimateSimulation && isActive ? MediatorMessage.ShowMapLayer : MediatorMessage.HideMapLayer, curModeLayer);
                                                            MediatorMessage.Send(MediatorMessage.RefreshMapLayer, curModeLayer);
                                                        });
                        }
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
                            //Debug.Assert(platformState != null, "platformState != null");
                            //Debug.Assert(platformState.ModeActiveTimes != null, "platformState.ModeActiveTimes != null");
                            //Debug.Assert(platformState.ModeActiveTimes.ContainsKey(mode), "platformState.ModeActiveTimes does not contain key");
                            //Debug.Assert(record != null, "record != null");
                            //Debug.Assert(record.Exposures != null, "record.Exposures != null");
                            var energy = (float)(peakSPL + (10 * Math.Log10(platformState.ModeActiveTimes[mode].TotalSeconds)));
                            record.Expose(new ActorExposureRecord(index, mode, peakSPL, energy));
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
                if (processTask != null) await processTask;
                processTask = ModeThresholdHistogram.Process(timeStepRecord, Dispatcher);
                if (timeStepIndex % 10 == 0)
                {
                    await processTask;
                    Dispatcher.InvokeIfRequired(UpdateHistogramDisplay);
                }
                //SpeciesThresholdHistogram.Process(timeStepRecord);
                logBuffer.Post(timeStepRecord);
                if (moveTask != null)
                {
                    // Wait for 3MB to finish moving the animats
                    moveTask.Wait();
                    // Pull in updated animat positions from 3MB for the next time step
                    UpdateAnimatPositions();
                }
                //var distance = Scenario.ScenarioSpecies[0].Animat.Locations[0].DistanceKilometers(firstAnimatPosition);
                //if (distance > 0.01) Debug.WriteLine(string.Format("{0}: First animat has moved {1:0.##} km from initial location", DateTime.Now, distance));
                //Debug.WriteLine(string.Format("{0}: Finished time step {1} of {2}: {3:0%} complete", DateTime.Now, timeStepIndex, timeStepCount, Math.Round((float)timeStepIndex / timeStepCount, 3)));
                if (MovingAnimats & AnimateSimulation) Dispatcher.InvokeIfRequired(() => { foreach (var species in Scenario.ScenarioSpecies) species.UpdateMapLayers(); });
                if (token.IsCancellationRequested) break;
            }
            if (processTask != null) await processTask;
            Dispatcher.InvokeIfRequired(UpdateHistogramDisplay);
            foreach (var layer in _modeFootprintMapLayers.SelectMany(layerSet => layerSet))
            {
                var curLayer = layer;
                Dispatcher.InvokeIfRequired(() => MediatorMessage.Send(MediatorMessage.RemoveMapLayer, curLayer));
            }
            Dispatcher.InvokeIfRequired(() => MediatorMessage.Send(MediatorMessage.RefreshMap, true));
            logBuffer.Complete();
            logBlock.Completion.Wait();
            if (MovingAnimats) Shutdown3MB();
            Debug.WriteLine(string.Format("{0}: Simulation complete. Exposure count: {1}", DateTime.Now, _totalExposureCount));
            Debug.WriteLine(string.Format("{0}: Exposures by species:", DateTime.Now));
            for (var i = 0; i < _exposuresBySpecies.Length; i++) Debug.WriteLine(string.Format("{0}: Species: {1}, Exposures: {2}", DateTime.Now, Scenario.ScenarioSpecies[i].LatinName, _exposuresBySpecies[i]));
            //SpeciesThresholdHistogram.Display();
            //NewModeThresholdHistogram.DebugDisplay();
        }

        void UpdateHistogramDisplay()
        {
            var handlers = GraphicsUpdate;
            if (handlers == null) return;
            foreach (EventHandler<EventArgs> handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                {
                    var localHandler = handler;
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(this, new EventArgs()));
                }
                else
                    handler(this, new EventArgs());
            }
        }

        static List<OverlayShapeMapLayer> CreateFootprintMapLayers(Platform platform, PlatformState state)
        {
            var result = new List<OverlayShapeMapLayer>();
            foreach (var mode in from source in platform.Sources
                                 from mode in source.Modes
                                 orderby mode.ModeID
                                 select mode)
            {
                var mapLayer = new OverlayShapeMapLayer
                {
                    Name = string.Format("{0}-footprint", mode.Guid),
                    LineColor = Colors.Red,
                    AreaColor = Color.FromArgb(64, 255, 0, 0),
                    LineWidth = 2f,
                };
                UpdateFootprintMapLayer(mode, state, mapLayer);
                result.Add(mapLayer);
            }
            return result;
        }

        static void UpdateFootprintMapLayer(Mode mode, PlatformState state, OverlayShapeMapLayer mapLayer)
        {
            mapLayer.Clear();
            var initialGeo = state.PlatformLocation.Location;
            var footprintArcStartBearing = state.PlatformLocation.Course + mode.RelativeBeamAngle - (Math.Abs(mode.HorizontalBeamWidth / 2));
            var footprintArcEndBearing = state.PlatformLocation.Course + mode.RelativeBeamAngle + (Math.Abs(mode.HorizontalBeamWidth / 2));
            var geos = new List<Geo>();
            if (mode.HorizontalBeamWidth < 360) geos.Add(initialGeo);
            for (var arcPointBearing = footprintArcStartBearing; arcPointBearing < footprintArcEndBearing; arcPointBearing++) geos.Add(initialGeo.Offset(Geo.MetersToRadians(mode.MaxPropagationRadius), Geo.DegreesToRadians(arcPointBearing)));
            geos.Add(initialGeo.Offset(Geo.MetersToRadians(mode.MaxPropagationRadius), Geo.DegreesToRadians(footprintArcEndBearing)));
            if (mode.HorizontalBeamWidth < 360) geos.Add(initialGeo);
            mapLayer.AddPolygon(geos);
            mapLayer.Done();
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
            var yxzFileName = Path.Combine(Path.GetTempPath(), Path.GetFileNameWithoutExtension(Path.GetRandomFileName()) + ".txt");
            Scenario.BathymetryData.ToYXZ(yxzFileName, -1);
            var contexts = new List<AnimatContext>();
            foreach (var species in Scenario.ScenarioSpecies)
            {
                if (!_speciesNameToIndex.ContainsKey(species.SpeciesDefinitionFilePath)) 
                    _speciesNameToIndex.Add(species.SpeciesDefinitionFilePath, nextSpeciesIndex++);
                var speciesInstanceIndex = _speciesNameToIndex[species.SpeciesDefinitionFilePath];
                var curSpecies = species;
                species.ReloadOrReseedAnimats();
                contexts.AddRange(species.Animat.Locations.Select((t, speciesIndex) => new AnimatContext
                {
                    Species = curSpecies, 
                    SpeciesInstanceIndex = speciesInstanceIndex, 
                    SpeciesAnimatIndex = speciesIndex
                }));
            }
            //var splitContext = contexts.Split(System.Environment.ProcessorCount).ToList();
            var splitContext = contexts.Split(1).ToList();
            _mbs = new C3mbs[splitContext.Count];
            _animatContext = new AnimatContext[splitContext.Count][];
            for (var mbsIndex = 0; mbsIndex < splitContext.Count; mbsIndex++)
            {
                mbsRESULT result;
                var mbs = new C3mbs();
                _mbs[mbsIndex] = mbs;
                
                // set the output directory
                if (mbsRESULT.OK != (result = mbs.SetOutputDirectory(_simulationDirectory)))
                    throw new AnimatInterfaceMMBSException("SetOutputDirectory Error:" + mbs.ResultToTc(result));

                var config = mbs.GetConfiguration();
                config.seedValue = (uint)mbsIndex;  // Probably wise to set each 3mb instance to a unique randomizer seed value.
                config.enabled = false;             // binary output enabled/disabled
                config.durationLess = true;         // make sure we're in durationless mode.
                mbs.SetConfiguration(config);
                result = mbs.LoadBathymetryFromTextFile(yxzFileName);
                if (mbsRESULT.OK != result) throw new AnimatInterfaceMMBSException("Bathymetry failed to load: " + mbs.ResultToTc(result));

                foreach (var speciesFilePath in _speciesNameToIndex.Keys)
                {
                    result = mbs.AddSpecies(speciesFilePath);
                    if (mbsRESULT.OK != result) throw new AnimatInterfaceMMBSException(string.Format("C3mbs::AddSpecies FATAL error {0} for species {1}", mbs.ResultToTc(result), speciesFilePath));
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
                    result = mbs.AddIndividualAnimat(context[animatIndex].SpeciesInstanceIndex, new mbsPosition { latitude = geo.Latitude, longitude = geo.Longitude, depth = 0 });
                    if (mbsRESULT.OK != result) throw new AnimatInterfaceMMBSException(string.Format("C3mbs::AddIndividualAnimat FATAL error {0}", mbs.ResultToTc(result)));
                    //Debug.WriteLine("Animat {0} lat: {1:0.####} lon: {2:0.####} depth: {3:0.#}, bathy: {4:0.#}", animatIndex, geo.Latitude, geo.Longitude, geo.Data, Scenario.BathymetryData.Samples.GetNearestPoint(geo).Data);
                }
                mbs.SetDuration((int)((TimeSpan)Scenario.Duration).TotalSeconds);
                result = mbs.SaveScenario(Path.Combine(_simulationDirectory, "test.sce"));
                if (mbsRESULT.OK != result) throw new AnimatInterfaceMMBSException(string.Format("C3mbs::SaveScenario FATAL error {0}", mbs.ResultToTc(result)));
                //----------------------------------------------------------------------//
                // Initialize the scenario.
                //----------------------------//
                if (mbsRESULT.OK == result) while (mbsRUNSTATE.INITIALIZING == mbs.GetRunState()) Thread.Sleep(1);
                else throw new AnimatInterfaceMMBSException("C3mbs::Initialize FATAL error " + mbs.ResultToTc(result));
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

        async Task MoveAnimatsAsync()
        {
            var actionBlock = new ActionBlock<C3mbs>(mbs =>
            {
                var result = mbs.RunScenarioNumIterations((int)Math.Round(TimeStepSize.TotalSeconds));
                if (result != mbsRESULT.OK)
                {
                    mbs.AbortRun();
                    throw new AnimatInterfaceMMBSException("C3mbs::RunScenarioNumIterations FATAL error: " + mbs.ResultToTc(result));
                }
                while (mbsRUNSTATE.RUNNING == mbs.GetRunState()) TaskEx.Yield();
            }, new ExecutionDataflowBlockOptions { BoundedCapacity = -1, MaxDegreeOfParallelism = -1 });
            foreach (var mbs in _mbs) actionBlock.Post(mbs);
            actionBlock.Complete();
            await actionBlock.Completion;
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

        public event EventHandler<EventArgs> GraphicsUpdate;
        [Initialize, UsedImplicitly] public Dictionary<Guid, Color> GuidToColorMap { get; private set; }
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

    public interface IHistogramSource
    {
        event EventHandler<EventArgs> GraphicsUpdate;
        Dictionary<Guid, Color> GuidToColorMap { get; }
    }
}
