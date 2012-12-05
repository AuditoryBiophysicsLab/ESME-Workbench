using System;
using ESME.Behaviors;
using NUnit.Framework;

namespace ESME.Tests.Behaviors
{
    public class ActivityTimelineTests
    {
        [Test]
        public void AlwaysOn()
        {
            var timeline = new ActivityTimeline();
            var testDuration = new TimeSpan(0, 1, 0, 0);
            var timeStep = new TimeSpan(0, 0, 0, 1);
            var onDuration = new TimeSpan(0, 0, 0, 0, 33);
            var offDuration = new TimeSpan(0, 0, 0, 0);
            while (timeline.Duration < testDuration)
            {
                timeline.AddActivity(true, onDuration);
                timeline.AddActivity(false, offDuration);
            }
            Assert.GreaterOrEqual(timeline.Duration, testDuration);
            var activeTimes = timeline.GetActiveTimes(timeStep);
            foreach (var activeTime in activeTimes)
                Assert.AreEqual(timeStep, activeTime);
        }

        [Test]
        public void AlwaysOff()
        {
            var timeline = new ActivityTimeline();
            var testDuration = new TimeSpan(0, 1, 0, 0);
            var timeStep = new TimeSpan(0, 0, 0, 1);
            var onDuration = new TimeSpan(0, 0, 0, 0, 0);
            var offDuration = new TimeSpan(0, 0, 0, 17);
            while (timeline.Duration < testDuration)
            {
                timeline.AddActivity(true, onDuration);
                timeline.AddActivity(false, offDuration);
            }
            Assert.GreaterOrEqual(timeline.Duration, testDuration);
            var activeTimes = timeline.GetActiveTimes(timeStep);
            foreach (var activeTime in activeTimes)
                Assert.AreEqual(TimeSpan.Zero, activeTime);
        }

        [Test]
        public void TwoStateOnOff()
        {
            var timeline = new ActivityTimeline();
            var testDuration = new TimeSpan(0, 1, 0, 0);
            var timeStep = new TimeSpan(0, 0, 0, 1);
            var onDuration = new TimeSpan(0, 0, 0, 0, 500);
            var offDuration = new TimeSpan(0, 0, 0, 0, 1500);
            while (timeline.Duration < testDuration)
            {
                timeline.AddActivity(true, onDuration);
                timeline.AddActivity(false, offDuration);
            }
            Assert.GreaterOrEqual(timeline.Duration, testDuration);
            var activeTimes = timeline.GetActiveTimes(timeStep);
            var isActive = true;
            foreach (var activeTime in activeTimes)
            {
                Assert.AreEqual(activeTime, isActive ? onDuration : TimeSpan.Zero);
                isActive = !isActive;
            }
        }

        [Test]
        public void ThreeStateOnOnOff()
        {
            var timeline = new ActivityTimeline();
            var testDuration = new TimeSpan(0, 1, 0, 0);
            var timeStep = new TimeSpan(0, 0, 0, 1);
            var halfStep = new TimeSpan(timeStep.Ticks / 2);
            var onDuration = new TimeSpan(0, 0, 0, 0, 1500);
            var offDuration = new TimeSpan(0, 0, 0, 0, 1500);
            while (timeline.Duration < testDuration)
            {
                timeline.AddActivity(true, onDuration);
                timeline.AddActivity(false, offDuration);
            }
            Assert.GreaterOrEqual(timeline.Duration, testDuration);
            var activeTimes = timeline.GetActiveTimes(timeStep);
            var state = 0;
            foreach (var activeTime in activeTimes)
            {
                switch(state)
                {
                    case 0:
                        Assert.AreEqual(timeStep, activeTime);
                        break;
                    case 1:
                        Assert.AreEqual(halfStep, activeTime);
                        break;
                    case 2:
                        Assert.AreEqual(0, activeTime.Ticks);
                        break;
                    default:
                        throw new ApplicationException("Should never see this");
                }
                state = (state + 1) % 3;
            }
        }
    }
}
