using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using ESME.NEMO;
using ESME.Overlay;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Platform
{
    public class BehaviorModel
    {
        public BehaviorModel(NemoPlatform nemoPlatform)
        {
            NemoPlatform = nemoPlatform;
            Initialize();
        }

        public NemoPlatform NemoPlatform { get; private set; }
        public List<ActiveMode> ActiveModes { get; private set; }
        public PlatformState[] PlatformStates { get; private set; }
        public OverlayPoint CourseStart { get; private set; }
        public OverlayPoint CourseEnd { get; private set; }
        public OverlayLineSegments CourseOverlay { get; private set; }
        public bool Display { get; set; }

        private void Initialize()
        {
            ActiveModes = new List<ActiveMode>();

            if (NemoBase.SimulationStepTime.TotalSeconds == 0.0)
                throw new ApplicationException("ESME.Platform.BehaviorModel: NemoBase.SimulationStepTime is not set!");

            foreach (NemoSource source in NemoPlatform.Sources)
                foreach (NemoMode mode in source.Modes)
                    ActiveModes.Add(new ActiveMode {NemoMode = mode});

            //TBD: Low priority: Validate that none of the list of trackdefs overlaps in time with any others in the list
            //     Only do this after checking with NUWC to make sure it's necessary

            MovementModel();
        }

        private List<ActiveSourceState> GetActiveSources(DateTime simulationTime, float currentCourseDegrees)
        {
            var results = new List<ActiveSourceState>();
            // This routine will go through all ActiveModes, looking for modes that are active 
            // during the time step specified inside currentState.  If any are found, they will be added to
            // the currentState.ActiveModes list
            foreach (ActiveMode curMode in ActiveModes)
            {
                NemoMode nemoMode = curMode.NemoMode;
                if (!nemoMode.Contains(simulationTime)) continue;
                // How long after the start of the current mode is the simulation time
                TimeSpan frameStartOffset = simulationTime - nemoMode.StartTime;
                TimeSpan frameEndOffset;

                // Make sure the end of the current frame does not go beyond the active time for the current mode
                if (nemoMode.Contains(simulationTime + NemoBase.SimulationStepTime))
                    frameEndOffset = (simulationTime + NemoBase.SimulationStepTime) - nemoMode.StartTime;
                else
                    frameEndOffset = nemoMode.Duration;

                TimeSpan frameStartModulo = frameStartOffset.Modulo(nemoMode.PulseInterval);
                TimeSpan frameEndModulo = frameEndOffset.Modulo(nemoMode.PulseInterval);

                var activeTime = new TimeSpan(0);

                if (frameStartModulo < nemoMode.PulseLength)
                {
                    // At least some part of the active part of the cycle was captured at
                    // the beginning of the current frame
                    activeTime += nemoMode.PulseLength - frameStartModulo;
                }
                if (frameEndModulo < nemoMode.PulseLength)
                {
                    // At least some part of the active part of the cycle was captured at
                    // the end of the current frame
                    activeTime += frameEndModulo;
                }

                // Compute the fraction of the current frame where the current source is active
                // This fraction can be, at most, 1.0
                double fraction = Math.Min(1.0, activeTime.DivideBy(NemoBase.SimulationStepTime));

                var curSourceState = new ActiveSourceState
                                         {
                                             ActiveFraction = (float) fraction,
                                             Heading_degrees = currentCourseDegrees
                                         };

                results.Add(curSourceState);
            } // foreach
            return results;
        }

        // GetActiveSources()

        private void MovementModel()
        {
            if (NemoBase.SimulationStepTime.TotalSeconds == 0.0)
                throw new ApplicationException("ESME.Platform.MovementModel: NemoBase.SimulationStepTime is not set!");

            var timestepCount = (int) NemoPlatform.NemoScenario.Duration.DivideBy(NemoBase.SimulationStepTime);
            DateTime currentTime = NemoPlatform.NemoScenario.StartTime;
            PlatformStates = new PlatformState[timestepCount];
            NemoTrackdef curTrackdef = null;
            var curLocation = new EarthCoordinate3D(0, 0, 0);
            EarthCoordinate3D proposedLocation;
            double curCourseDegrees = 0;
            double curSpeedMetersPerSecond = 0;
            var overlayPoints = new List<EarthCoordinate>();
            OverlayShape curTrackBoundingRegion = null;

            overlayPoints.Add(new EarthCoordinate(NemoPlatform.Trackdefs[0].InitialLocation));
            // Put trackdefs in ascending start-time order, if they weren't already
            NemoPlatform.Trackdefs.Sort();

            for (var i = 0; i < PlatformStates.Length; i++, currentTime += NemoBase.SimulationStepTime)
            {
                // if we have a current trackdef we're processing, AND that current trackdef DOES NOT CONTAIN the current time
                // THEN we no longer have a current trackdef
                if ((curTrackdef != null) && (!curTrackdef.Contains(currentTime)))
                    curTrackdef = null;
                // if we don't have a current trackdef
                if (curTrackdef == null)
                {
                    // look through all of our trackdefs
                    foreach (NemoTrackdef trackdef in NemoPlatform.Trackdefs)
                        // If we find one that contains the current time
                        if (trackdef.Contains(currentTime))
                        {
                            // make this trackdef the current one
                            curTrackdef = trackdef;
                            curLocation = curTrackdef.InitialLocation;
                            curCourseDegrees = curTrackdef.InitialCourse;
                            curSpeedMetersPerSecond = curTrackdef.InitialSpeed*0.514444444f;
                                // Conversion factor for knots to meters per second
                            if (curTrackdef.OverlayFile != null)
                            {
                                if (curTrackdef.OverlayFile.Shapes.Count() != 1)
                                    throw new ApplicationException("MovementModel: Attempt to use an OverlayFile with multiple shapes as a bounding region.  Operation unsupported.");
                                curTrackBoundingRegion = curTrackdef.OverlayFile.Shapes[0];
                                if (!curTrackBoundingRegion.IsUsableAsPerimeter)
                                {
                                    var reasons = new StringBuilder();
                                    if (!curTrackBoundingRegion.IsClosed)
                                        reasons.Append("Bounding region is not closed, ");
                                    if (curTrackBoundingRegion.HasCrossingSegments)
                                        reasons.Append("Bounding region is not a simple polygon (segments cross each other), ");
                                    if (reasons.Length != 0)
                                        reasons.Remove(reasons.Length - 2, 2); // Remove the trailing ", "
                                    throw new ApplicationException("MovementModel: Specified OverlayFile is unsuitable to use as a bounding region: " + reasons);
                                }
                                if (!curTrackBoundingRegion.Contains(curLocation))
                                    throw new ApplicationException("MovementModel: Specified start location is not contained within the bounding region");
                            }
                            else
                            {
                                // Else, the current trackdef's overlay file IS null, and if the type is perimeter_bounce, that's a no-no
                                if (curTrackdef.TrackType.ToLower() == "perimeter_bounce")
                                    throw new ApplicationException("MovementModel: PERIMETER_BOUNCE trackdefs require a bounding region, none was supplied.");
                            }
                            break;
                        }
                }
                // If we have a current trackdef, use it, otherwise we don't update the location or course
                if (curTrackdef != null)
                {
                    switch (curTrackdef.TrackType.ToLower())
                    {
                        default:
                        case "stationary":
                            curSpeedMetersPerSecond = 0;
                            break;
                        case "straight_line":
                            // straight line navigation code
                            curLocation.Move(curCourseDegrees,
                                             curSpeedMetersPerSecond*NemoBase.SimulationStepTime.TotalSeconds);
                            break;
                        case "perimeter_bounce":
                            // perimeter bounce navigation code here
                            proposedLocation = new EarthCoordinate3D(curLocation);
                            proposedLocation.Move(curCourseDegrees,
                                                  curSpeedMetersPerSecond*NemoBase.SimulationStepTime.TotalSeconds);
                            if (curTrackBoundingRegion != null)
                            {
                                if (curTrackBoundingRegion.Contains(proposedLocation))
                                    curLocation = proposedLocation;
                                else
                                {
                                    //curLocation.Compare(proposedLocation);
                                    proposedLocation = new EarthCoordinate3D(curTrackBoundingRegion.Bounce(curLocation, proposedLocation));
                                    if (!curTrackBoundingRegion.Contains(proposedLocation))
                                    {
                                        proposedLocation = new EarthCoordinate3D(curTrackBoundingRegion.Bounce(curLocation, proposedLocation));
                                        if (!curTrackBoundingRegion.Contains(proposedLocation))
                                            throw new ApplicationException("MovementModel: Two reflections failed to keep the platform inside the bounding region.  Please check the bounding region closely for small pockets or other irregularities");
                                    }
                                    //curLocation.Compare(proposedLocation);
                                    proposedLocation.Elevation_meters = curLocation.Elevation_meters;
                                    curCourseDegrees = new Course(curLocation, proposedLocation).Degrees;

                                    curLocation = new EarthCoordinate3D(proposedLocation);
                                    if (!curTrackBoundingRegion.Contains(curLocation))
                                        throw new ApplicationException("MovementModel: Reflected position is outside the bounding region");
                                    overlayPoints.Add(new EarthCoordinate(curLocation));
                                }
                            }
                            break;
                    }
                }
                else
                    // If we don't have a current trackdef, that means our speed is zero
                    curSpeedMetersPerSecond = 0;

                // Put the current location, course, speed and time into the current index of the PlatformStates array
                PlatformStates[i] = new PlatformState
                                        {
                                            Location = curLocation,
                                            Course_degrees = (float) curCourseDegrees,
                                            Speed_meters_per_second = (float) curSpeedMetersPerSecond,
                                            SimulationTime = currentTime
                                        };
            }
            overlayPoints.Add(new EarthCoordinate(curLocation));
            CourseOverlay = new OverlayLineSegments(overlayPoints.ToArray(), Color.Orange, 1, LineStyle.Dot);
            CourseEnd = new OverlayPoint(curLocation, Color.Red, 2);
            CourseStart = new OverlayPoint(NemoPlatform.Trackdefs[0].InitialLocation, Color.Green, 2);
        }
    }
}