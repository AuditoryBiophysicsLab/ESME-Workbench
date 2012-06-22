using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using ESME.Behaviors;
using ESME.Scenarios;
using ESME.Simulator;
using HRC.Navigation;
using HRC.Utility;
using NUnit.Framework;

namespace ESME.Tests.Simulator
{
    public class SimulationLogTests
    {
        readonly string _simulationDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "Simulation Log Tests");

        [Test]
        public void CreateSimulationLogFromBehaviors()
        {
            if (!Directory.Exists(_simulationDirectory)) Directory.CreateDirectory(_simulationDirectory);

            var jaxOpsArea = new GeoArray(new Geo(29.3590, -79.2195),
                                          new Geo(31.1627, -79.2195),
                                          new Geo(31.1627, -81.2789),
                                          new Geo(30.1627, -81.2789),
                                          new Geo(29.3590, -80.8789),
                                          new Geo(29.3590, -79.2195));
            var platformStates = new List<List<PlatformState>>();
            var sourceActorModeID = 0;
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
                };
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
                    SourceActorModeID = sourceActorModeID++,
                };
                source.Modes.Add(mode);
                var behavior = new PlatformBehavior(platform, new TimeSpan(0, 0, 0, 1), 86400);
                platformStates.Add(behavior.PlatformStates.ToList());
            }
            const int timeStepCount = 86400;
            Debug.WriteLine(string.Format("{0}: Beginning write phase of test", DateTime.Now));
            using (var writeLog = SimulationLog.Create(Path.Combine(_simulationDirectory, "simulation.log"), timeStepCount, new TimeSpan(0, 0, 0, 1)))
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
                        cur.ActorPositionRecords.Add(new ActorPositionRecord((float)platformLocation.Location.Latitude,
                                                                             (float)platformLocation.Location.Longitude,
                                                                             platformLocation.Depth));
                        for (var activeSource = 0; activeSource < platformStates.Count; activeSource++)
                        {
                            cur.ActorPositionRecords[actorIndex].Exposures.Add(new ActorExposureRecord(activeSource, activeSource * 1000, activeSource * 2000));
                        }
                    }
                    writeLog.Add(cur);
                    if (writeTimeStepIndex % 1000 == 0) Debug.WriteLine(string.Format("{0}: Wrote record for time step {1}", DateTime.Now, writeTimeStepIndex));
                }
            }

            Debug.WriteLine(string.Format("{0}: Beginning read phase of test", DateTime.Now));
            using (var readLog = SimulationLog.Open(Path.Combine(_simulationDirectory, "simulation.log")))
            {
                Assert.AreEqual(timeStepCount, readLog.TimeStepCount);
                Assert.AreEqual(new TimeSpan(0, 0, 0, 1), readLog.TimeStepSize);
                for (var readTimeStepIndex = 0; readTimeStepIndex < timeStepCount; readTimeStepIndex++)
                {
                    var cur = readLog[readTimeStepIndex].ReadAll();
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
                            var exposureIndex = actorPosition.Exposures.IndexOf(exposure);
                            Assert.AreEqual(exposureIndex, exposure.SourceActorModeID);
                            Assert.AreEqual(exposureIndex * 1000, exposure.PeakSPL);
                            Assert.AreEqual(exposureIndex * 2000, exposure.Energy);
                        }
                    }
                    if (readTimeStepIndex % 1000 == 0) Debug.WriteLine(string.Format("{0}: Read record for time step {1}", DateTime.Now, readTimeStepIndex));
                }
            }
        }

        [Test]
        public void CreateSimpleSimulationLog()
        {
            using (var writeLog = SimulationLog.Create(Path.Combine(_simulationDirectory, "simulation.log"), 100, new TimeSpan(0, 0, 0, 1)))
            {
                Assert.AreEqual(100, writeLog.TimeStepCount);
                Assert.AreEqual(new TimeSpan(0, 0, 0, 1), writeLog.TimeStepSize);
                for (var writeTimeStepIndex = 0; writeTimeStepIndex < writeLog.TimeStepCount; writeTimeStepIndex++)
                {
                    var cur = new SimulationTimeStepRecord();
                    for (var actorIndex = 0; actorIndex < 101; actorIndex++)
                    {
                        cur.ActorPositionRecords.Add(new ActorPositionRecord(42, -70, 10));
                        for (var activeSource = 0; activeSource < 11; activeSource++)
                        {
                            cur.ActorPositionRecords[actorIndex].Exposures.Add(new ActorExposureRecord(activeSource, activeSource * 1000, activeSource * 2000));
                        }
                    }
                    writeLog.Add(cur);
                }
            }

            using (var readLog = SimulationLog.Open(Path.Combine(_simulationDirectory, "simulation.log")))
            {
                Assert.AreEqual(100, readLog.TimeStepCount);
                Assert.AreEqual(new TimeSpan(0, 0, 0, 1), readLog.TimeStepSize);
                for (var readTimeStepIndex = 0; readTimeStepIndex < readLog.TimeStepCount; readTimeStepIndex++)
                {
                    var cur = readLog[readTimeStepIndex].ReadAll();
                    Assert.AreEqual(101, cur.ActorPositionRecords.Count);
                    foreach (var actorPosition in cur.ActorPositionRecords)
                    {
                        Assert.AreEqual(42, actorPosition.Latitude);
                        Assert.AreEqual(-70, actorPosition.Longitude);
                        Assert.AreEqual(10, actorPosition.Depth);
                        Assert.AreEqual(11, actorPosition.Exposures.Count);
                        foreach (var exposure in actorPosition.Exposures)
                        {
                            var exposureIndex = actorPosition.Exposures.IndexOf(exposure);
                            Assert.AreEqual(exposureIndex, exposure.SourceActorModeID);
                            Assert.AreEqual(exposureIndex * 1000, exposure.PeakSPL);
                            Assert.AreEqual(exposureIndex * 2000, exposure.Energy);
                        }
                    }
                }
            }
        }
    }
}
