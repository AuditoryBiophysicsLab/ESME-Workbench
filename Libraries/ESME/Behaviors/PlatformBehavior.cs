using System;
using System.Collections.Generic;
using System.Linq;
using ESME.NEMO.Overlay;
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
                _platformLocations = PlatformLocations;
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
        readonly List<PlatformLocation> _platformLocations;
        
        public Platform Platform { get; private set; }
        public bool Display { get; set; }

        public IEnumerable<PlatformState> PlatformStates
        {
            get
            {
                var simulationTime = new TimeSpan(0);
                for (var timeStep = 0; timeStep < _timeStepCount; timeStep++)
                {
                    yield return new PlatformState
                    {
                        SimulationTime = simulationTime,
                        PlatformLocation = _platformLocations[timeStep],
                        ModeActiveTimes = _modeActiveTimes.Keys.ToDictionary(key => key, key => _modeActiveTimes[key][timeStep]),
                    };
                    simulationTime += _timeStep;
                }
            }
        }

        List<PlatformLocation> PlatformLocations
        {
            get
            {
                var result = new List<PlatformLocation>();
                var random = new Random();
                Geo location;
                Course course;
                OverlayLineSegments perimeter = null;
                GeoRect bounds = null;
                var trackType = (TrackType)Platform.TrackType;

                if (trackType == TrackType.PerimeterBounce && Platform.Perimeter == null) throw new PerimeterInvalidException("Must have a perimeter specified for PerimeterBounce behavior");
                if (Platform.IsRandom && Platform.Perimeter == null) throw new PerimeterInvalidException("Must have a perimeter specified for random start point behavior");
                if (Platform.Perimeter != null)
                {
                    var points = (from point in Platform.Perimeter.PerimeterCoordinates
                                  orderby point.Order
                                  select (Geo)point.Geo).ToList();
                    bounds = new GeoRect(points);
                    perimeter = new OverlayLineSegments(points);
                    if (!perimeter.IsUsableAsPerimeter)
                    {
                        var reasons = new List<string>();
                        if (!perimeter.IsClosed) reasons.Add("Perimeter is not closed");
                        if (perimeter.HasCrossingSegments) reasons.Add("Perimeter is not a simple polygon (segments cross each other)");
                        throw new PerimeterInvalidException(string.Format("The perimeter specified in the track definition is not valid: {0}", string.Join(", ", reasons)));
                    }
                }
                if (bounds == null) throw new ApplicationException("Bounds is null, should not happen!");
                if (Platform.IsRandom)
                {
                    location = new Geo(-90, 0);
                    while (!bounds.Contains(location)) location = new Geo(bounds.South + (random.NextDouble() * bounds.Height), bounds.West + (random.NextDouble() * bounds.Width));
                    course = new Course(random.NextDouble() * 360.0);
                }
                else
                {
                    location = new Geo(Platform.Geo);
                    course = new Course(Platform.Course);
                }
                var speed = Platform.Speed * 0.514444444f;

                for (var timeStep = 0; timeStep < _timeStepCount; timeStep++)
                {
                    switch (trackType)
                    {
                        default:
                            throw new PlatformBehaviorException(string.Format("Unknown track type {0}", trackType));
                        case TrackType.Stationary:
                            speed = 0;
                            break;
                        case TrackType.StraightLine:
                            // straight line navigation code
                            location = location.Offset(Geo.KilometersToRadians((speed * _timeStep.TotalSeconds) / 1000),
                                                       course.Radians);
                            break;
                        case TrackType.PerimeterBounce:
                            // perimeter bounce navigation code here
                            var proposedLocation = location.Offset(Geo.KilometersToRadians((speed * _timeStep.TotalSeconds) / 1000),
                                                                   course.Radians);
                            if (perimeter.Contains(proposedLocation)) location = proposedLocation;
                            else
                            {
                                //curLocation.Compare(proposedLocation);
                                Course proposedCourse;
                                Console.WriteLine("At time step {0}, {1} crossed the perimeter at ({2}, {3}) while on course {4}", timeStep, Platform.PlatformName, proposedLocation.Latitude, proposedLocation.Longitude, course);
                                proposedLocation = new Geo(perimeter.Reflect(location, proposedLocation, out proposedCourse));
                                Console.WriteLine("  Bounced location: ({0}, {1}) and course {2}", proposedLocation.Latitude, proposedLocation.Longitude, new Course(location, proposedLocation).Degrees);
                                if (!perimeter.Contains(proposedLocation))
                                {
                                    proposedLocation = new Geo(perimeter.Reflect(location, proposedLocation, out proposedCourse));
                                    Console.WriteLine("  Bounced location (2): ({0}, {1}) and course {2}", proposedLocation.Latitude, proposedLocation.Longitude, new Course(location, proposedLocation).Degrees);
                                    if (!perimeter.Contains(proposedLocation))
                                        throw new PlatformMovementException(
                                            "Two reflections failed to keep the platform inside the bounding region.  Please check the bounding region closely for small pockets or other irregularities");
                                }

                                var newCourse = new Course(location, proposedLocation);

                                course = newCourse;
                                location = new Geo(proposedLocation);
                                if (!perimeter.Contains(location)) throw new PlatformMovementException("Reflected position is outside the bounding region");
                            }
                            break;
                    }
                    // Put the current location, course, speed and time into the PlatformStates list
                    result.Add(new PlatformLocation
                    {
                        Location = new Geo(location),
                        Course = (float)course.Degrees,
                        Speed = speed,
                        Depth = Platform.Depth,
                    });
                }
                return result;
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
