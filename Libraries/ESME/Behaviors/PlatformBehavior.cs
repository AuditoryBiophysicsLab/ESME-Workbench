using System;
using System.Collections.Generic;
using System.Linq;
using ESME.Scenarios;
using HRC.Navigation;

namespace ESME.Behaviors
{
    public class PlatformBehavior
    {
        public PlatformBehavior(Platform platform, TimeSpan timeStep, int timeStepCount)
        {
            Platform = platform;
            _timeStep = timeStep;
            _timeStepCount = timeStepCount;
            _duration = new TimeSpan(_timeStep.Ticks * (timeStepCount + 1));
            try
            {
                _modeActiveTimes = new Dictionary<Mode, IList<TimeSpan>>();
                foreach (var source in Platform.Sources)
                    foreach (var mode in source.Modes)
                    {
                        var modeTimeline = new ActivityTimeline();
                        while (modeTimeline.Duration < _duration)
                        {
                            modeTimeline.AddActivity(true, mode.PulseLength);
                            modeTimeline.AddActivity(false, (TimeSpan)mode.PulseInterval - mode.PulseLength);
                        }
                        _modeActiveTimes.Add(mode, modeTimeline.GetActiveTimes(_timeStep).ToList());
                    }
            }
            catch (Exception e)
            {
                throw new PlatformBehaviorException(string.Format("Error initializing behavior for {0}", Platform.PlatformName), e);
            }
        }
        readonly TimeSpan _timeStep;
        readonly TimeSpan _duration;
        readonly int _timeStepCount;
        readonly Dictionary<Mode, IList<TimeSpan>> _modeActiveTimes;
        
        public Platform Platform { get; private set; }
        public bool Display { get; set; }

        public IEnumerable<PlatformState> PlatformStates
        {
            get
            {
                var simulationTime = new TimeSpan(0);
                var platformLocations = PlatformLocations.GetEnumerator();
                platformLocations.MoveNext();
                for (var timeStep = 0; timeStep < _timeStepCount; timeStep++)
                {
                    yield return new PlatformState
                    {
                        SimulationTime = simulationTime,
                        PlatformLocation = platformLocations.Current,
                        ModeActiveTimes = _modeActiveTimes.Keys.ToDictionary(key => key, key => _modeActiveTimes[key][timeStep]),
                    };
                    simulationTime += _timeStep;
                    platformLocations.MoveNext();
                }
            }
        }

        IEnumerable<PlatformLocation> PlatformLocations
        {
            get
            {
                Geo location;
                Course course;
                GeoArray perimeter = null;
                GeoArray bounceTrack = null;
                var trackType = (TrackType)Platform.TrackType;

                if (trackType == TrackType.PerimeterBounce && Platform.Perimeter == null) throw new PerimeterInvalidException("Must have a perimeter specified for PerimeterBounce behavior");
                if (Platform.IsRandom && Platform.Perimeter == null) throw new PerimeterInvalidException("Must have a perimeter specified for random start point behavior");
                if (Platform.Perimeter != null)
                {
                    perimeter = Platform.Perimeter;
                    var reasons = new List<string>();
                    if (!perimeter.IsClosed) reasons.Add("Perimeter is not closed");
                    if (perimeter.HasCrossingSegments) reasons.Add("Perimeter is not a simple polygon (segments cross each other)");
                    if (reasons.Count != 0) throw new PerimeterInvalidException(string.Format("The perimeter specified in the track definition is not valid: {0}", string.Join(", ", reasons)));
                }
                if (Platform.IsRandom)
                {
                    location = perimeter.RandomLocationWithinPerimeter();
                    course = Course.RandomCourse;
                }
                else
                {
                    location = new Geo(Platform.Geo);
                    course = new Course(Platform.Course);
                }
                var metersPerSecond = Platform.Speed * 0.514444444f;
                var metersPerTimeStep = (metersPerSecond * _timeStep.TotalSeconds);

                switch (trackType)
                {
                    default:
                        throw new PlatformBehaviorException(string.Format("Unknown track type {0}", trackType));
                    case TrackType.Stationary:
                        for (var timeStep = 0; timeStep < _timeStepCount; timeStep++) 
                            yield return new PlatformLocation
                            {
                                Location = new Geo(location), 
                                Course = 0, 
                                Speed = 0, 
                                Depth = Platform.Depth,
                            };
                        break;
                    case TrackType.StraightLine:
                        // straight line navigation code
                        if (Platform.Speed == 0) throw new PlatformMovementException("Speed cannot be 0 for StraightLine behavior");
                        for (var timeStep = 0; timeStep < _timeStepCount; timeStep++)
                        {
                            yield return new PlatformLocation
                            {
                                Location = location, 
                                Course = (float)course.Degrees, 
                                Speed = metersPerSecond, 
                                Depth = Platform.Depth,
                            };
                            location = location.Offset(Geo.MetersToRadians(metersPerTimeStep), course.Radians);
                        }
                        break;
                    case TrackType.PerimeterBounce:
                        // perimeter bounce navigation code here
                        if (Platform.Speed == 0) throw new PlatformMovementException("Speed cannot be 0 for PerimeterBounce behavior");
                        while (bounceTrack == null)
                        {
                            try
                            {
                                bounceTrack = perimeter.PerimeterBounce(location, course.Radians, 1e6);
                                location = bounceTrack.Geos.First();
                            }
                            catch (PerimeterBounceException) {}
                        }
                        var timeStepsRemaining = _timeStepCount;
                        foreach (var segment in bounceTrack.Segments)
                        {
                            var segmentLength = Geo.RadiansToMeters(segment.LengthRadians);
                            var stepsInSegment = Math.Round(segmentLength / metersPerTimeStep);
                            for (double curStep = 0; curStep < stepsInSegment; curStep++)
                            {
                                var oldLocation = location;
                                location = segment.Slerp(curStep / stepsInSegment);
                                yield return new PlatformLocation
                                {
                                    Location = location,
                                    Course = (float)course.Degrees,
                                    Speed = metersPerSecond,
                                    Depth = Platform.Depth,
                                };
                                timeStepsRemaining--;
                                if (timeStepsRemaining <= 0) break;
                                course = new Course(Geo.RadiansToDegrees(oldLocation.Azimuth(location)));
                            }
                            if (timeStepsRemaining <= 0) break;
                        }
                        break;
                }
            }
        }
    }

    public class PlatformState
    {
        public TimeSpan SimulationTime { get; internal set; }
        public PlatformLocation PlatformLocation { get; internal set; }
        public Dictionary<Mode, TimeSpan> ModeActiveTimes { get; internal set; }
    }

    public class ActivityTimeline
    {
        public ActivityTimeline() { Duration = new TimeSpan(0); }
        public void AddActivity(bool isActive, TimeSpan timeSpan)
        {
            _items.Add(new TimelineItem(isActive, timeSpan));
            Duration += timeSpan;
        }

        readonly List<TimelineItem> _items = new List<TimelineItem>();
        public TimeSpan Duration { get; private set; }

        class TimelineItem
        {
            public TimelineItem(bool isActive, TimeSpan timeSpan)
            {
                IsActive = isActive;
                TimeSpan = new TimeSpan(timeSpan.Ticks);
            }
            public TimelineItem(TimelineItem item)
            {
                IsActive = item.IsActive;
                TimeSpan = new TimeSpan(item.TimeSpan.Ticks);
            }
            public bool IsActive { get; private set; }
            public TimeSpan TimeSpan { get; set; }
        }

        public IEnumerable<TimeSpan> GetActiveTimes(TimeSpan timeStep)
        {
            if (_items.Count == 0) yield break;

            // Make a local copy of the items in case we need to call GetActiveItems again
            var itemQueue = new Queue<TimelineItem>();
            foreach (var curItem in _items) itemQueue.Enqueue(new TimelineItem(curItem));

            var item = itemQueue.Dequeue();
            var onTime = new TimeSpan(0);
            var remainingTime = new TimeSpan(timeStep.Ticks);
            while (item.TimeSpan.Ticks > 0)
            {
                if (item.TimeSpan > remainingTime)
                {
                    item.TimeSpan -= remainingTime;
                    if (item.IsActive) onTime += remainingTime;
                    remainingTime -= remainingTime;
                }
                else
                {
                    if (item.IsActive) onTime += item.TimeSpan;
                    remainingTime -= item.TimeSpan;
                    if (itemQueue.Count > 0) item = itemQueue.Dequeue();
                    else yield break;
                }
                if (remainingTime.Ticks > 0) continue;
                yield return onTime;
                onTime = new TimeSpan(0);
                remainingTime = new TimeSpan(timeStep.Ticks);
            }
        }
    }
}
