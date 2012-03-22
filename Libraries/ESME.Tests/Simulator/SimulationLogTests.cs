using System;
using System.IO;
using ESME.Simulator;
using NUnit.Framework;

namespace ESME.Tests.Simulator
{
    public class SimulationLogTests
    {
        readonly string _simulationDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), @"ESME.Simulation Tests\Simulation Output");

        [Test]
        public void CreateNewSimulationLog()
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
