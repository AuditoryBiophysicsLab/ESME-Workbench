using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using ESME.Behaviors;
using ESME.Environment;
using ESME.Locations;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.Simulator;
using HRC.Navigation;
using HRC.Utility;
using NUnit.Framework;

namespace ESME.Tests.Simulator
{
    public class SimulationLogTests
    {
        public static readonly string SimulationDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "Simulation Log Tests");

        [Test]
        public void CreateSimulationLogFromBehaviors()
        {
            if (!Directory.Exists(SimulationDirectory)) Directory.CreateDirectory(SimulationDirectory);

            var jaxOpsArea = new GeoArray(new Geo(29.3590, -79.2195),
                                          new Geo(31.1627, -79.2195),
                                          new Geo(31.1627, -81.2789),
                                          new Geo(30.1627, -81.2789),
                                          new Geo(29.3590, -80.8789),
                                          new Geo(29.3590, -79.2195));
            var platformStates = new List<List<PlatformState>>();
            var actorID = 0;
            var sourceActorModeID = 0;
            Location.Database = new DummyDatabaseService();
            Scenario.Database = Location.Database;
            Location.Cache = new DummyCacheService();
            Scenario.Cache = Location.Cache;
            var scenario = new Scenario
            {
                Name = "Test Scenario",
                Location = new Location
                {
                    GeoRect = new GeoRect(1, -1, 1, -1),
                },
            };
            for (var i = 0; i < 10; i++)
            {
                var platform = new Platform
                {
                    PlatformName = "Test Platform",
                    Perimeter = jaxOpsArea,
                    Depth = 0,
                    IsRandom = true,
                    TrackType = TrackType.PerimeterBounce,
                    Sources = new ObservableList<Source>(),
                    Speed = 20,
                    ActorID = actorID++,
                };
                scenario.Platforms.Add(platform);
                var source = new Source
                {
                    SourceName = "Test Source",
                    Modes = new ObservableList<Mode>(),
                    Platform = platform,
                };
                platform.Sources.Add(source);
                var mode = new Mode
                {
                    ModeName = "Test Mode",
                    PulseInterval = new TimeSpan(0, 0, 0, 10),
                    PulseLength = new TimeSpan(0, 0, 0, 0, 500),
                    Depth = 5,
                    HighFrequency = 1000,
                    LowFrequency = 1000,
                    DepressionElevationAngle = 10,
                    VerticalBeamWidth = 90,
                    SourceLevel = 200,
                    Source = source,
                    ModeID = sourceActorModeID++,
                };
                source.Modes.Add(mode);
                var behavior = new PlatformBehavior(platform, new TimeSpan(0, 0, 0, 1), 86400);
                platformStates.Add(behavior.PlatformStates.ToList());
            }
            var species = new ScenarioSpecies { StartActorID = actorID++, Scenario = scenario, PopulationDensity = 0.01f };
            scenario.ScenarioSpecies.Add(species);
            species.Animat.Locations.Add(new Geo<float>(0, 0, 10));
            const int timeStepCount = 86400;
            Debug.WriteLine(string.Format("{0}: Beginning write phase of test", DateTime.Now));
            using (var writeLog = SimulationLog.Create(Path.Combine(SimulationDirectory, "simulation.log"), new TimeSpan(0, 0, 0, 1), scenario))
            {
                Assert.AreEqual(timeStepCount, writeLog.TimeStepCount);
                Assert.AreEqual(new TimeSpan(0, 0, 0, 1), writeLog.TimeStepSize);
                for (var writeTimeStepIndex = 0; writeTimeStepIndex < timeStepCount; writeTimeStepIndex++)
                {
                    var cur = new SimulationTimeStepRecord();
                    for (var actorIndex = 0; actorIndex < platformStates.Count; actorIndex++)
                    {
                        var platformState = platformStates[actorIndex];
                        var platformLocation = platformState[writeTimeStepIndex].PlatformLocation;
                        cur.ActorPositionRecords.Add(new ActorPositionRecord(platformLocation.Location, platformLocation.Depth));
                        for (var activeSource = 0; activeSource < platformStates.Count; activeSource++)
                        {
                            // dja: This test may be bad.  Rewrite it to take into account the active mode in the current step, if any.
                            // there should be another loop here for mode
                            var mode = platformState[writeTimeStepIndex].ModeActiveTimes.FirstOrDefault();
                            if (mode.Key != null) cur.ActorPositionRecords[actorIndex].Exposures.Add(new ActorExposureRecord(actorIndex, mode.Key, activeSource * 1000, activeSource * 2000));
                        }
                    }
                    writeLog.Add(cur);
                    if (writeTimeStepIndex % 1000 == 0) Debug.WriteLine(string.Format("{0}: Wrote record for time step {1}", DateTime.Now, writeTimeStepIndex));
                }
            }

            Debug.WriteLine(string.Format("{0}: Beginning read phase of test", DateTime.Now));
            using (var readLog = SimulationLog.Open(Path.Combine(SimulationDirectory, "simulation.log")))
            {
                Assert.AreEqual(timeStepCount, readLog.TimeStepCount);
                Assert.AreEqual(new TimeSpan(0, 0, 0, 1), readLog.TimeStepSize);
                for (var readTimeStepIndex = 0; readTimeStepIndex < timeStepCount; readTimeStepIndex++)
                {
                    var cur = readLog[readTimeStepIndex];
                    cur.ReadAll();
                    Assert.AreEqual(platformStates.Count, cur.ActorPositionRecords.Count);
                    for (var actorIndex = 0; actorIndex < cur.ActorPositionRecords.Count; actorIndex++)
                    {
                        var actorPosition = cur.ActorPositionRecords[actorIndex];
                        var platformState = platformStates[actorIndex];
                        Assert.AreEqual((float)platformState[readTimeStepIndex].PlatformLocation.Location.Latitude, actorPosition.Latitude, 0.00001);
                        Assert.AreEqual((float)platformState[readTimeStepIndex].PlatformLocation.Location.Longitude, actorPosition.Longitude, 0.00001);
                        Assert.AreEqual(platformState[readTimeStepIndex].PlatformLocation.Depth, actorPosition.Depth, 0.00001);
                        Assert.AreEqual(platformStates.Count, actorPosition.Exposures.Count);
                        foreach (var exposure in actorPosition.Exposures)
                        {
                            var exposureIndex = actorPosition.Exposures.ToList().IndexOf(exposure);
                            var mode = platformState[readTimeStepIndex].ModeActiveTimes.FirstOrDefault();
                            if (mode.Key == null) continue;
                            //Assert.AreEqual(exposureIndex, exposure.ModeID);
                            Assert.AreEqual(exposureIndex * 1000, exposure.PeakSPL);
                            Assert.AreEqual(exposureIndex * 2000, exposure.Energy);
                        }
                    }
                    if (readTimeStepIndex % 1000 == 0) Debug.WriteLine(string.Format("{0}: Read record for time step {1}", DateTime.Now, readTimeStepIndex));
                }
            }
        }
    }

    class DummyDatabaseService : IMasterDatabaseService 
    {
        public void Dispose() { throw new NotImplementedException(); }
        public string MasterDatabaseDirectory { get { return SimulationLogTests.SimulationDirectory; } set { throw new NotImplementedException(); } }

        public LocationContext Context { get { throw new NotImplementedException(); } }

        public void Refresh() { throw new NotImplementedException(); }
        public void Add(Perimeter perimeter) { throw new NotImplementedException(); }
        public void Add(PerimeterCoordinate coordinate, bool replaceExisting = false) { throw new NotImplementedException(); }
        public void Add(ScenarioSpecies species) { throw new NotImplementedException(); }
        public void Add(AnalysisPoint analysisPoint, Bathymetry bathymetry) { throw new NotImplementedException(); }
        public EnvironmentalDataSet LoadOrCreateEnvironmentalDataSet(Location location, float resolution, TimePeriod timePeriod, PluginIdentifier sourcePlugin) { throw new NotImplementedException(); }
        public void SaveChanges() { throw new NotImplementedException(); }
    }

    class DummyCacheService : IEnvironmentalCacheService 
    {
        public PercentProgressList<PercentProgressList<Location>> ImportMissingDatasets() { throw new NotImplementedException(); }
        public PercentProgressList<Location> ImportLocationDatasets(Location location) { throw new NotImplementedException(); }
        public PercentProgress<EnvironmentalDataSet> ImportDataset(EnvironmentalDataSet dataSet) { throw new NotImplementedException(); }
        public void ImportDatasetTest(EnvironmentalDataSet dataSet) { throw new NotImplementedException(); }
        public int BusyCount { get { throw new NotImplementedException(); } }

        public void ToBitmap<T>(EnvironmentData<T> data, string fileName, Func<T, float> valueFunc, Func<float[,], float, float, uint[,]> toPixelValuesFunc) where T : Geo, new() { throw new NotImplementedException(); }
        public bool IsCached(EnvironmentalDataSet dataSet) { throw new NotImplementedException(); }

        public Task<EnvironmentDataSetBase> this[EnvironmentalDataSet dataSet]
        {
            get
            {
                if (dataSet.SourcePlugin.PluginSubtype != PluginSubtype.Bathymetry) throw new NotImplementedException();
                return new Task<EnvironmentDataSetBase>(() =>
                {
                    var result = new Bathymetry();
                    result.Samples.Add(new Geo<float>(0, 0, -500));
                    return result;
                });
            }
        }
    }
}
