using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ESME.NEMO;
using ESME.TransmissionLoss;
using HRC.Navigation;
using NUnit.Framework;

namespace ESME.Tests.TransmissionLoss
{
    public class TransmissionLossRunFileTests
    {
        [Test]
        public void Construction()
        {
            var thirtySeconds = new TimeSpan(0, 0, 0, 30);
            var oneSecond = new TimeSpan(0, 0, 0, 1);
            var oneHour = new TimeSpan(0, 1, 0, 0);
            var startTime = DateTime.Now;
            var testMode = new NemoMode
            {
                ModeID = 1,
                State = "state",
                Linked = "linked",
                ActiveTime = 0,
                DepthOffset = 10,
                SourceLevel = 200,
                SourceDepth = 10,
                LowFrequency = 1000,
                HighFrequency = 1000,
                PulseInterval = thirtySeconds,
                PulseLength = oneSecond,
                HorizontalBeamWidth = 90,
                VerticalBeamWidth = 90,
                DepressionElevationAngle = 0,
                RelativeBeamAngle = 0,
                Radius = 20000,
                ClusterCount = 1,
                StartTime = startTime,
                Duration = oneHour,
                EndTime = startTime + oneHour,
                Type = "testType",
                Id = "testID",
                Name = "testMode",
                PSMId = "testPSMId",
                PSMName = "testPSMName",
                Priority = 1,
            };
            //var result = TransmissionLossRunFile.Create(TransmissionLossAlgorithm.Bellhop,
            //                                            new SoundSource(new Geo(39, -71), testMode, 16),
            //);
        }
    }
}
