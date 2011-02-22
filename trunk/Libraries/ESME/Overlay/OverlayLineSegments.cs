﻿//#define MATLAB_DEBUG_OUTPUT
using System;
using System.Collections.Generic;
using System.Windows.Media;
using System.Linq;
using System.Text;
using HRC.Navigation;

namespace ESME.Overlay
{
    public class OverlayLineSegments : OverlayShape
    {
        private readonly List<OverlayLineSegment> _segments = new List<OverlayLineSegment>();
        public override bool IsClosed { get; protected set; }
        public override bool HasCrossingSegments { get; protected set; }
        public Course[] Normals { get; private set; }
        public double North { get; private set; }
        public double South { get; private set; }
        public double East { get; private set; }
        public double West { get; private set; }

        public OverlayLineSegments(EarthCoordinate[] points, Color color, float size, LineStyle lineStyle)
            : base(color, size, lineStyle)
        {
            Add(points);
            CreateSegments();
            ComputeBoundingBox();
            CheckForClosure();
            CheckCrossingSegments();
        }

        private void CreateSegments()
        {
            for (var i = 0; i < Length - 1; i++)
                _segments.Add(new OverlayLineSegment(this[i], this[i + 1]));
        }
        /// <summary>
        /// Returns an OverlayLineSegments containing the verticies of the  rectangular bounding box of the contained polygon, starting and ending with the northwest corner.
        /// </summary>
        private void ComputeBoundingBox()
        {            
            North = East = double.MinValue;
            South = West = double.MaxValue;
         
            foreach (var cursegment in _segments)
            {
                North = Math.Max(North, cursegment.Location.Latitude_degrees);
                East = Math.Max(East, cursegment.Location.Longitude_degrees);
                South = Math.Min(South, cursegment.Location.Latitude_degrees);
                West = Math.Min(West, cursegment.Location.Longitude_degrees);
            }

        }

        public override bool IsUsableAsPerimeter
        {
            get
            {
                if (IsClosed && (!HasCrossingSegments))
                {
                    // If we're a shape that can be used as a perimeter to bounce within
                    Normals = new Course[Length];
                    // Calculate the "normal" vector to each side of our polygon.
                    // For now, the normal vector is defined as a coplanar vector perpendicular to the side in question,
                    // having a length of 100m, whose endpoint is inside the polygon
                    for (var i = 0; i < Length - 1; i++)
                    {
                        var edgeNormalCourse = new Course(this[i], this[i + 1]);
                        var trialCw = _segments[i].Midpoint;
                        edgeNormalCourse.Degrees += 90;
                        trialCw.Move(edgeNormalCourse.Degrees, 100);
                        if (Contains(trialCw))
                            Normals[i] = new Course(edgeNormalCourse.Degrees);
                        else
                            Normals[i] = new Course(edgeNormalCourse.Reciprocal);
                    }
                    return true;
                }
                return false;
            }
        }

#if MATLAB_DEBUG_OUTPUT
        private void MatlabDumpVertices()
        {
            Console.WriteLine("Vertices=zeros({0}, 2);", Length);
            Console.WriteLine("Intersects=zeros({0}, 2);", Length);
            for (var i = 0; i < Length; i++)
            {
                Console.WriteLine("Vertices({0},:)=[{1} {2}];", i + 1, this[i].Longitude_degrees, this[i].Latitude_degrees);
            }
        }
#endif

        /// <summary>
        /// Takes a start location and a proposed end location, and returns a 'bounced' end location
        /// If the proposed end location is outside of the current figure, the end location is reflected
        /// by the normal to the intersecting segment, and returned to the caller.  If the proposed end
        /// location is inside the current figure, then it is simply returned to the caller.
        /// </summary>
        /// <param name="startLocation">Start location for the current proposed move</param>
        /// <param name="proposedEndLocation">End location for the current proposed move</param>
        /// <returns>Actual end location that remains inside the figure, which may be reflected from the
        /// proposed end location provided if the proposed end location lies outside the figure</returns>
        public override EarthCoordinate Bounce(EarthCoordinate startLocation, EarthCoordinate proposedEndLocation)
        {
            //EarthCoordinate start = new EarthCoordinate(StartLocation);
            //EarthCoordinate end = new EarthCoordinate(ProposedEndLocation);
#if MATLAB_DEBUG_OUTPUT
            MatlabDumpVertices();
#endif
            if (Contains(proposedEndLocation))
                return proposedEndLocation;

            //start.Move(ProposedCourse.ReciprocalDegrees, 1000);
            //end.Move(ProposedCourse.Degrees, 1000);
#if MATLAB_DEBUG_OUTPUT
            Console.WriteLine("Course=zeros(2,2);\nCourse(1,:)=[{0} {1}];\nCourse(2,:)=[{2} {3}];",
                startLocation.Longitude_degrees, startLocation.Latitude_degrees, 
                proposedEndLocation.Longitude_degrees, proposedEndLocation.Latitude_degrees);
#endif
            var proposedCourse = new Course(startLocation, proposedEndLocation);
            var proposedCourseSegment = new OverlayLineSegment(startLocation, proposedEndLocation);
            for (var i = 0; i < _segments.Count(); i++)
            {
                var intersect = proposedCourseSegment.IntersectionPoint(_segments[i]);
#if MATLAB_DEBUG_OUTPUT
                Console.WriteLine("Intersects({0},:)=[{1} {2}];", i + 1, intersect.Longitude_degrees, intersect.Latitude_degrees);
#endif
                if (!proposedCourseSegment.Contains(intersect) || !_segments[i].Contains(intersect)) continue;
                proposedCourse.Reflect(Normals[i]);
                var result = new EarthCoordinate(startLocation);
                result.Move(proposedCourse, proposedEndLocation.GetDistanceTo_Meters(startLocation));
                return result;
            }
#if MATLAB_DEBUG_OUTPUT
            Console.WriteLine("figure;");
            Console.WriteLine("plot(Vertices(:, 1), Vertices(:, 2), 'g-*');");
            Console.WriteLine("hold on;");
            Console.WriteLine("plot(Course(:, 1), Course(:, 2), 'r-o');");
            Console.WriteLine("plot(Intersects(:, 1), Intersects(:, 2), 'bx');");
            Console.WriteLine("legend('Area Boundary', 'Course Segment under review', 'Calculated intersection points');");
#endif
            throw new GeometricException("The proposed course didn't intersect with any of the edges of the figure");
        }

        /// <summary>
        /// True if this figure has intersecting segments
        /// </summary>
        /// <returns></returns>
        private void CheckCrossingSegments()
        {
            // If the figure is open, the number of non-adjacent segments we will be checking is the number of 
            // segments minus 2 (for the current segment and the next adjoining segment)

            // If this is a polygon (the end joins the beginning) then we don't want to check the final segment because
            // it always joins to the first one (by definition)

            // Special case for the first segment - we stop at the second to last segment
            for (var j = 2; j < _segments.Count() - 1; j++)
            {
                if (_segments[0].Intersects(_segments[j]))
                {
                    HasCrossingSegments = true;
                    return;
                }
            }

            // Check the rest of the pairs, now stopping at the last segment
            for (var i = 1; i < _segments.Count() - 2; i++)
            {
                for (var j = i + 2; j < _segments.Count(); j++)
                {
                    if (_segments[i].Intersects(_segments[j]))
                    {
                        HasCrossingSegments = true;
                        return;
                    }
                }
            }
            // If we haven't previously found two segments which cross, there are NO crossing segments in this shape
            HasCrossingSegments = false;
        }

        private void CheckForClosure()
        {
            // Assume the current set of lines is not closed
            IsClosed = false;

            // If we have less than four points, it's not possible for this shape to be closed
            if (EarthCoordinates.Count < 4)
                return;

            // If the first point and the last point are identical, then this shape IS closed
            if ((EarthCoordinates[0].Longitude_degrees == EarthCoordinates[EarthCoordinates.Count - 1].Longitude_degrees) &&
                (EarthCoordinates[0].Latitude_degrees == EarthCoordinates[EarthCoordinates.Count - 1].Latitude_degrees))
                IsClosed = true;
        }

        public override bool Contains(EarthCoordinate coordinate)
        {
            if (!IsClosed)
                return false;

            // Andy: your point-within-a-polygon algorithm goes here
            //cn = 0;    % the crossing number counter
            var crossingNumber = 0;

            //% loop through all edges of the polygon
            //for i=1:n % edge from V(i) to V(i+1)
            for (var i = 0; i < (EarthCoordinates.Count - 1); i++)
            {
                // if (((V(i).y <= P.y) && (V(i+1).y > P.y)) ||    % an upward crossing
                //     ((V(i).y > P.y) && (V(i+1).y <= P.y)))      % a downward crossing
                //     vt = (P.y - V(i).y) / (V(i+1).y - V(i).y);  % compute the actual edge-ray intersect x-coordinate
                if (((EarthCoordinates[i].Latitude_degrees <= coordinate.Latitude_degrees) &&
                     (EarthCoordinates[i + 1].Latitude_degrees > coordinate.Latitude_degrees)) ||
                    ((EarthCoordinates[i].Latitude_degrees > coordinate.Latitude_degrees) &&
                     (EarthCoordinates[i + 1].Latitude_degrees <= coordinate.Latitude_degrees)))
                {
                    var vt = (coordinate.Latitude_degrees - EarthCoordinates[i].Latitude_degrees) /
                                (EarthCoordinates[i + 1].Latitude_degrees - EarthCoordinates[i].Latitude_degrees);
                    // if (P.x < (V(i).x + vt * (V(i+1).x - V(i).x))) % P.x < intersect
                    //     cn = cn + 1;   % a valid crossing of y=P.y right of P.x
                    if (coordinate.Longitude_degrees <
                        (EarthCoordinates[i].Longitude_degrees +
                         (vt * (EarthCoordinates[i + 1].Longitude_degrees - EarthCoordinates[i].Longitude_degrees))))
                        crossingNumber++;
                }
            }

            if ((crossingNumber & 1) == 1)
                return true;
            return false;
        }

        public override string WellKnownText
        {
            get
            {
                if (MyWellKnownText == null)
                {
                    var retval = new StringBuilder();
                    retval.Append("LINESTRING(");
                    foreach (var coord in EarthCoordinates)
                        retval.Append(string.Format("{0} {1}, ", coord.Longitude_degrees, coord.Latitude_degrees));
                    retval.Remove(retval.Length - 2, 2); // Lose the last comma and space
                    retval.Append(")");
                    MyWellKnownText = retval.ToString();
                }
                return MyWellKnownText;
            }
        }
    }
}