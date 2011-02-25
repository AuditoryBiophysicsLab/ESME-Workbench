using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Media;
using ESME.NEMO;
using ESME.Overlay;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Platform
{
    public partial class BehaviorModel
    {
        public BehaviorModel(NemoPlatform nemoPlatform)
        {
            NemoPlatform = nemoPlatform;
            try
            {
                Initialize();
            }
            catch (Exception e)
            {
                throw new PlatformBehaviorException("Error initializing platform behavior", e);
            }
        }

        public NemoPlatform NemoPlatform { get; private set; }
        public PlatformStates PlatformStates { get; private set; }
        public OverlayPoint CourseStart { get; private set; }
        public OverlayPoint CourseEnd { get; private set; }
        public List<CourseChangeDatum> CourseChangePoints { get; private set; }
        public OverlayLineSegments CourseOverlay { get; private set; }
        public bool Display { get; set; }

        void Initialize()
        {
            foreach (var source in NemoPlatform.Sources)
                foreach (var mode in source.Modes)
                    mode.CalculateActiveTimeSteps(NemoPlatform.NemoScenario);

            //TBD: Low priority: Validate that none of the list of trackdefs overlaps in time with any others in the list
            //     Only do this after checking with NUWC to make sure it's necessary

            MovementModel();
        }

        void MovementModel()
        {
            var timestepCount = (int) NemoPlatform.NemoScenario.Duration.DivideBy(NemoBase.SimulationStepTime);
            var currentTime = NemoPlatform.NemoScenario.StartTime;
            PlatformStates = new PlatformStates();
            NemoTrackdef curTrackdef = null;
            var curLocation = new EarthCoordinate3D(0, 0, 0);
            EarthCoordinate3D proposedLocation;
            double curCourseDegrees = 0;
            double curSpeedMetersPerSecond = 0;
            var overlayPoints = new List<EarthCoordinate>();
            OverlayShape curTrackBoundingRegion = null;
            
            CourseChangePoints = new List<CourseChangeDatum>();

            overlayPoints.Add(new EarthCoordinate(NemoPlatform.Trackdefs[0].InitialLocation));
            // Put trackdefs in ascending start-time order, if they weren't already
            NemoPlatform.Trackdefs.Sort();

            for (var i = 0; i < timestepCount; i++, currentTime += NemoBase.SimulationStepTime)
            {
                // if we have a current trackdef we're processing, AND that current trackdef DOES NOT CONTAIN the current time
                // THEN we no longer have a current trackdef
                if ((curTrackdef != null) && (!curTrackdef.Contains(currentTime))) curTrackdef = null;
                // if we don't have a current trackdef
                if (curTrackdef == null)
                {
                    // look through all of our trackdefs
                    foreach (var trackdef in NemoPlatform.Trackdefs) // If we find one that contains the current time
                        if (trackdef.Contains(currentTime))
                        {
                            // make this trackdef the current one
                            curTrackdef = trackdef;
                            curLocation = curTrackdef.InitialLocation;
                            curCourseDegrees = curTrackdef.InitialCourse;
                            curSpeedMetersPerSecond = curTrackdef.InitialSpeed*0.514444444f;
                            CourseChangePoints.Add(new CourseChangeDatum
                            {
                                IsStart = true,
                                Location = curLocation,
                                NewCourse = curCourseDegrees,
                            });

                            // Conversion factor for knots to meters per second
                            if (curTrackdef.OverlayFile != null)
                            {
                                if (curTrackdef.OverlayFile.Shapes.Count() != 1) throw new PlatformMovementException(string.Format("Specified overlay file {0} is unsuitable for use as a bounding region.\nReason(s): Overlay file contains multiple shapes, therefore the bounding shape is undefined", curTrackdef.OverlayFile.FileName));
                                curTrackBoundingRegion = curTrackdef.OverlayFile.Shapes[0];
#if false
                                if (!curTrackBoundingRegion.IsUsableAsPerimeter)
                                {
                                    var reasons = new StringBuilder();
                                    if (!curTrackBoundingRegion.IsClosed) reasons.Append("Bounding region is not closed, ");
                                    if (curTrackBoundingRegion.HasCrossingSegments) reasons.Append("Bounding region is not a simple polygon (segments cross each other), ");
                                    if (reasons.Length != 0) reasons.Remove(reasons.Length - 2, 2); // Remove the trailing ", "
                                    throw new PlatformMovementException(string.Format("Specified overlay file {0} is unsuitable for use as a bounding region.\nReason(s): {1}", curTrackdef.OverlayFile.FileName, reasons));
                                }
#endif
                                if (!curTrackBoundingRegion.Contains(curLocation)) throw new PlatformMovementException(string.Format("Specified start location ({0:0.####}, {1:0.####}) is not contained within the trackdef bounding region", curLocation.Latitude, curLocation.Longitude));
                            }
                            else
                            {
                                // Else, the current trackdef's overlay file IS null, and if the type is perimeter_bounce, that's a no-no
                                if (curTrackdef.TrackType.ToLower() == "perimeter_bounce") throw new PlatformMovementException("PERIMETER_BOUNCE trackdefs require a bounding region, none was supplied.");
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
                            curLocation.Move(curCourseDegrees, curSpeedMetersPerSecond*NemoBase.SimulationStepTime.TotalSeconds);
                            break;
                        case "perimeter_bounce":
                            // perimeter bounce navigation code here
                            proposedLocation = new EarthCoordinate3D(curLocation);
                            proposedLocation.Move(curCourseDegrees, curSpeedMetersPerSecond*NemoBase.SimulationStepTime.TotalSeconds);
                            if (curTrackBoundingRegion == null) throw new PlatformBehaviorException("Platform behavior is specified as Perimeter Bounce, but no bounding shape was specified");
                            if (curTrackBoundingRegion.Contains(proposedLocation)) curLocation = proposedLocation;
                            else
                            {
                                //curLocation.Compare(proposedLocation);
                                proposedLocation = new EarthCoordinate3D(curTrackBoundingRegion.Bounce(curLocation, proposedLocation));
                                if (!curTrackBoundingRegion.Contains(proposedLocation))
                                {
                                    proposedLocation = new EarthCoordinate3D(curTrackBoundingRegion.Bounce(curLocation, proposedLocation));
                                    if (!curTrackBoundingRegion.Contains(proposedLocation)) throw new PlatformMovementException("Two reflections failed to keep the platform inside the bounding region.  Please check the bounding region closely for small pockets or other irregularities");
                                }

                                var newCourseDegrees = new Course(curLocation, proposedLocation).Degrees;
                                CourseChangePoints.Add(new CourseChangeDatum
                                {
                                    Location = curLocation,
                                    OldCourse = curCourseDegrees,
                                    NewCourse = newCourseDegrees,
                                });

                                //curLocation.Compare(proposedLocation);
                                proposedLocation.Elevation = curLocation.Elevation;
                                curCourseDegrees = newCourseDegrees;
                                curLocation = new EarthCoordinate3D(proposedLocation);
                                if (!curTrackBoundingRegion.Contains(curLocation)) throw new PlatformMovementException("Reflected position is outside the bounding region");
                                overlayPoints.Add(new EarthCoordinate(curLocation));
                            }
                            break;
                    }
                }
                else
                    // If we don't have a current trackdef, that means our speed is zero
                    curSpeedMetersPerSecond = 0;

                // Put the current location, course, speed and time into the PlatformStates list
                PlatformStates.Add(new PlatformState
                                   {
                                       Location = curLocation,
                                       Course = (float) curCourseDegrees,
                                       Speed = (float) curSpeedMetersPerSecond,
                                       SimulationTime = currentTime
                                   });
            }
            overlayPoints.Add(new EarthCoordinate(curLocation));
            CourseOverlay = new OverlayLineSegments(overlayPoints.ToArray(), Colors.Orange, 1, LineStyle.Dot);
            CourseEnd = new OverlayPoint(curLocation, Colors.Red, 2);
            CourseChangePoints.Add(new CourseChangeDatum
            {
                IsEnd = true,
                Location = curLocation,
                OldCourse = curCourseDegrees,
            });

            CourseStart = new OverlayPoint(NemoPlatform.Trackdefs[0].InitialLocation, Colors.Green, 2);
        }
    }

    public class CourseChangeDatum
    {
        #region public constructor

        public CourseChangeDatum()
        {
            IsStart = false;
            IsEnd = false;
            OldCourse = 0;
            NewCourse = 0;
            Comments = null;
        }

        #endregion

        public EarthCoordinate Location { get; internal set; }
        public double OldCourse { get; internal set; }
        public double NewCourse { get; internal set; }
        public bool IsStart { get; internal set; }
        public bool IsEnd { get; internal set; }
        public string Comments { get; internal set; }
    }
}