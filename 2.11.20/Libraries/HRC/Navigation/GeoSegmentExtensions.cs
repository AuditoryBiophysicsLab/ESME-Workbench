using System;
using System.Linq;
using HRC.Navigation;

namespace HRC.Navigation
{
    public static class GeoSegmentExtensions
    {
        /// <summary>
        /// Find the intersection of the great circle described by segment and the great circle normal 
        /// to it that passes through r. We assume the segment subtends less than 180 degrees.  If the
        /// segment happens to subtend exactly 180 degrees, the antipode of the returned point is also
        /// a valid intersection. Note that the returned point may not actually lie on the segment, if 
        /// the intersection point lies at a different location along the great circle than that 
        /// subtended by the segment. To check if the returned point is actually on the segment, use
        /// the Intersection extension method for the segment.
        /// </summary>
        /// <param name="segment"></param>
        /// <param name="r"></param>
        /// <returns></returns>
        public static Geo GreatCircleIntersection(this GeoSegment segment, Geo r)
        {
            var a = segment[0].Dot(r);
            var b = segment[1].Dot(r);
            var x = -b / (a - b);
            return segment[0].Scale(x) + segment[1].Scale(1.0 - x).Normalized;
        }

        /// <summary>
        /// Find the intersection of the great circle described by segment1 and the great circle described
        /// by segment2. We assume the segments both subtend less than 180 degrees.  If either segment 
        /// happens to subtend exactly 180 degrees, the antipode of the returned point is also a valid 
        /// intersection. Note that the returned point may not actually lie on either segment, if the 
        /// intersection point lies at a different location along their respective great circles than that 
        /// subtended by the segments themselves. To check if the returned point is actually on the segment,
        /// use the Intersection extension method for the segment.
        /// </summary>
        /// <param name="segment1"> </param>
        /// <param name="segment2"> </param>
        /// <returns></returns>
        public static Geo GreatCircleIntersection(this GeoSegment segment1, GeoSegment segment2)
        {
            return segment1[0].CrossNormalize(segment1[1]).CrossNormalize(segment2[0].CrossNormalize(segment2[1]));
        }

        /// <summary>
        /// Find the intersection of the great circle described by segment and the great circle normal 
        /// to it that passes through r, if that intersection point is contained within the segment.
        /// If it is not contained within the segment, null is returned.
        /// </summary>
        /// <param name="segment"></param>
        /// <param name="r"></param>
        /// <returns></returns>
        public static Geo Intersection(this GeoSegment segment, Geo r)
        {
            var geo = segment.GreatCircleIntersection(r);
            return geo.IsOn(segment) ? geo : null;
        }

        /// <summary>
        /// Find the intersection of the great circle described by segment1 and the great circle described
        /// by segment2, if that intersection point is contained within segment both segments. If it is not 
        /// contained within both segments, null is returned.
        /// </summary>
        /// <param name="segment1"> </param>
        /// <param name="segment2"> </param>
        /// <returns></returns>
        public static Geo Intersection(this GeoSegment segment1, GeoSegment segment2)
        {
            var geo = segment1.GreatCircleIntersection(segment2); 
            return geo.IsOn(segment1) && geo.IsOn(segment2) ? geo : geo.Antipode.IsOn(segment1) && geo.Antipode.IsOn(segment2) ? geo.Antipode : null;
        }

        /// <summary>
        /// Compute the result of a specular reflection between incidentSegment and edgeSegment.  
        /// If incidentSegment and edgeSegment do not intersect somewhere along both of their 
        /// lengths, null is returned.  The returned segment is one meter long, originating 
        /// at the point of reflection and terminating one meter along the reflected path.
        /// </summary>
        /// <param name="incidentSegment"></param>
        /// <param name="edgeSegment"></param>
        /// <returns>Angle of reflection, in radians</returns>
        public static GeoSegment Reflect(this GeoSegment incidentSegment, GeoSegment edgeSegment)
        {
            var intersectionPoint = incidentSegment.Intersection(edgeSegment);
            if (intersectionPoint == null) return null;
            var intersectionFraction1 = new GeoSegment(incidentSegment[0], intersectionPoint).LengthRadians / incidentSegment.LengthRadians;
            var intersectionFraction2 = new GeoSegment(edgeSegment[0], intersectionPoint).LengthRadians / edgeSegment.LengthRadians;
            var fiftyCm1 = 1 / (Geo.RadiansToKilometers(incidentSegment.LengthRadians) * 2000);
            var fiftyCm2 = 1 / (Geo.RadiansToKilometers(edgeSegment.LengthRadians) * 2000);
            // Create two one-meter-long segments, centered on the intersection point so we can pretend we're doing geometry on a plane
            var incidentUnit = incidentSegment.SubSegment(intersectionFraction1 - fiftyCm1, intersectionFraction1 + fiftyCm1);
            var edgeUnit = edgeSegment.SubSegment(intersectionFraction2 - fiftyCm2, intersectionFraction2 + fiftyCm2);
            var incidentVector = new SurfaceVector(incidentUnit);
            var edgeVector = new SurfaceVector(edgeUnit);
            var rightNormal = edgeVector.Rotate(-MoreMath.PiOverTwo);
            var isRightNormal = incidentVector.Dot(rightNormal) <= 0;
            var normal = isRightNormal ? rightNormal : edgeVector.Rotate(MoreMath.PiOverTwo);
            var reflectionVector = incidentVector - normal.Scale(2 * normal.Dot(incidentVector));
            return new GeoSegment(intersectionPoint, intersectionPoint.Offset(Geo.KilometersToRadians(0.001), reflectionVector.GeoAzimuth));
        }

        public static GeoSegment SubSegment(this GeoSegment segment, double startFraction, double endFraction)
        {
            return new GeoSegment(segment.Slerp(startFraction), segment.Slerp(endFraction));
        }

        public static Geo Slerp(this GeoSegment segment, double fraction)
        {
            var omega = Math.Acos(segment[0].Dot(segment[1]));
            var result = (segment[0].Scale(Math.Sin((1 - fraction) * omega) / Math.Sin(omega)) +
                          segment[1].Scale(Math.Sin(fraction * omega) / Math.Sin(omega))).Normalized;
            return result;
        }

        public static GeoSegment Scale(this GeoSegment segment, double scale)
        {
            var omega = Math.Acos(segment[0].Dot(segment[1]));
            var endPoint = (segment[0].Scale(Math.Sin((1 - scale)*omega)/Math.Sin(omega)) +
                            segment[1].Scale(Math.Sin(scale*omega)/Math.Sin(omega))).Normalized;
            return new GeoSegment(segment[0], endPoint);
        }

        /// <summary>
        /// Treating the passed in GeoSegments as planar vectors, return the dot product
        /// </summary>
        /// <param name="segment1"></param>
        /// <param name="segment2"></param>
        /// <returns></returns>
        public static double Dot(this GeoSegment segment1, GeoSegment segment2)
        {
            // Using Geo only for convenience, these are NOT Geos, really!
            // X is delta longitude, Y is delta latitude, Z is always zero.
            var vec1 = new Geo(segment1[1].Longitude - segment1[0].Longitude, segment1[1].Latitude - segment1[0].Latitude, 0).Normalized;
            var vec2 = new Geo(segment2[1].Longitude - segment2[0].Longitude, segment2[1].Latitude - segment2[0].Latitude, 0).Normalized;
            return vec1.Dot(vec2);
        }

        /// <summary>
        /// Treating the GeoSegment as a planar vector, rotate it through the specified angle.
        /// Positive angles are clockwise rotations.
        /// </summary>
        /// <param name="segment"></param>
        /// <param name="angle">Angle, in radians</param>
        /// <returns>A new GeoSegment, rotated through the requested angle</returns>
        public static GeoSegment Rotate(this GeoSegment segment, double angle)
        {
            var x = segment[1].Longitude - segment[0].Longitude;
            var y = segment[1].Latitude - segment[0].Latitude;
            var cosTheta = Math.Cos(angle);
            var sinTheta = Math.Sin(angle);
            var rotatedLongitude = x * cosTheta - y * sinTheta;
            var rotatedLatitude = x * sinTheta + y * cosTheta;
            return new GeoSegment(segment[0], new Geo(segment[0].Latitude + rotatedLatitude, segment[0].Longitude + rotatedLongitude));
        }

        /// <summary>
        /// Does the segment come near the region defined by boundingCircle?
        /// </summary>
        /// <param name="segment"></param>
        /// <param name="boundingCircle"></param>
        /// <param name="near">Radius of approach, in radians</param>
        /// <returns></returns>
        public static bool IsNear(this GeoSegment segment, BoundingCircle boundingCircle, double near)
        {
            return segment.IsInside(near + boundingCircle.Radius, boundingCircle.Center);
        }

        /// <summary>
        /// Is a segment within 'near' range of region?
        /// </summary>
        /// <param name="segment"></param>
        /// <param name="region"></param>
        /// <param name="near"></param>
        /// <returns>True if any points are near the region</returns>
        public static bool IsNear(this GeoSegment segment, GeoArray region, double near)
        {
            return region.Segments.Select(curSegment => segment.Intersection(curSegment, near)).FirstOrDefault(ret => ret != null) != null;
        }

        /// <summary>
        /// Where is a segment within 'near' range of region
        /// </summary>
        /// <param name="segment">Segment to test</param>
        /// <param name="region">Region to test</param>
        /// <param name="near">Range, in radians</param>
        /// <returns>List of points that are within 'near' radians of region</returns>
        public static GeoArray Near(this GeoSegment segment, GeoArray region, double near)
        {
            return new GeoArray(region.Segments.Select(curSegment => segment.Intersection(curSegment, near)).Where(ret => ret != null));
        }

        /// <summary>
        /// Is the point, p, within radius of this segment?
        /// </summary>
        /// <param name="segment"> </param>
        /// <param name="radius">Radius of interest, in radians</param>
        /// <param name="p">Point to test</param>
        /// <returns></returns>
        public static bool IsInside(this GeoSegment segment, double radius, Geo p)
        {
            //gc is a unit vector perpendicular to the plane defined by v1 and v2
            var gc = segment[0].Cross(segment[1]).Normalized;

            // |gc . p| is the size of the projection of p onto gc (the normal of
            // v1,v2) cos(pi/2-r) is effectively the size of the projection of a
            // vector along gc of the radius length. If the former is larger than
            // the latter, than p is further than radius from arc, so must not be
            // isInside
            if (Math.Abs(gc.Dot(p)) > Math.Cos((Math.PI / 2.0) - radius))
                return false;

            // If p is within radius of either endpoint, then we know it isInside
            if (segment[0].DistanceRadians(p) <= radius || segment[1].DistanceRadians(p) <= radius)
                return true;

            // d is the vector from the v2 to v1
            var d = segment[1] - segment[0];

            // n is the d normalized to length=1
            var n = d.Normalized;

            // dp is the vector from p to v1
            var dp = p - segment[0];

            // size is the size of the projection of dp onto n
            var size = n.Dot(dp);

            // p is inside iff size>=0 and size <= d.Length
            return (0 <= size && size <= d.Length);
        }

        /// <summary>
        /// Returns a Geo location if the great circle segments come within the range 
        /// (r, radians) of each other. The angles between the segments must be less 
        /// than PI or the results are ambiguous. 
        /// </summary>
        /// <param name="segment1"> </param>
        /// <param name="segment2"> </param>
        /// <param name="r"></param>
        /// <returns>null if the segments don't intersect within the range.</returns>
        public static Geo Intersection(this GeoSegment segment1, GeoSegment segment2, double r)
        {
            if (segment2 == null) throw new ArgumentNullException("segment2");
            if (r < 0) throw new ArgumentException("Must be non-negative", "r");

            // ac and bc are the unit vectors normal to the two great
            // circles defined by the segments
            var ac = segment1[0].Cross(segment1[1]).Normalized;
            var bc = segment2[0].Cross(segment2[1]).Normalized;

            // aL and bL are the lengths (in radians) of the segments
            var aL = segment1[0].DistanceRadians(segment1[1]) + r;
            var bL = segment2[0].DistanceRadians(segment2[1]) + r;

            // i is one of the two points where the two great circles
            // intersect. 
            var i = ac.Cross(bc).Normalized;

            // if i is not on A
            if (!(i.DistanceRadians(segment1[0]) <= aL && i.DistanceRadians(segment1[1]) <= aL))
            {
                i = i.Antipode; // switch to the antipode instead
                // check again
                if (!(i.DistanceRadians(segment1[0]) <= aL && i.DistanceRadians(segment1[1]) <= aL))
                {
                    // nope - neither i nor i' is on A, so we'll bail out
                    return null;
                }
            }
            // i is intersection or anti-intersection point now.

            // Now see if it intersects with b
            if (i.DistanceRadians(segment2[0]) <= bL && i.DistanceRadians(segment2[1]) <= bL) return i;
            return null;
        }

        public static bool Intersects(this GeoSegment segment1, GeoSegment segment2)
        {
            return segment1.GreatCircleIntersection(segment2) != null;
        }

        /// <summary>
        /// Returns true or false depending on whether the segment intersects the bounding circle
        /// </summary>
        /// <param name="segment"></param>
        /// <param name="boundingCircle"></param>
        /// <returns></returns>
        public static bool Intersects(this GeoSegment segment, BoundingCircle boundingCircle)
        {
            // check if either of the end points of the seg are inside the
            // circle
            var d1 = segment[0].DistanceRadians(boundingCircle.Center);
            if (d1 < boundingCircle.Radius)
                return true;

            var d2 = segment[1].DistanceRadians(boundingCircle.Center);
            if (d2 < boundingCircle.Radius)
                return true;

            if (boundingCircle.Center.DistanceToGreatCircle(segment) > boundingCircle.Radius)
                return false;

            // calculate point of intersection of great circle containing
            // (lat, lon) and perpendicular to great circle containing
            // (lat1, lon1) and (lat2, lon2)

            var g = segment[0].Cross(segment[1]);
            var f = boundingCircle.Center.Cross(g);
            // Reusing g object for i
            var i = f.Cross(g).Normalized;

            // check if point of intersection lies on the segment
            // length of seg
            var d = segment.LengthRadians;

            // Make sure the intersection point is inside the exclusion
            // zone
            if (boundingCircle.Center.DistanceRadians(i) < boundingCircle.Radius)
            {
                // between seg endpoints and first point of intersection
                var d11 = segment[0].DistanceRadians(i);
                var d12 = segment[1].DistanceRadians(i);
                // Check the distance of the intersection point and either
                // endpoint to see if it falls between them. Add a second
                // test to make sure that we are on the shorter of the two
                // segments between the endpoints.
                return (d11 <= d && d12 <= d && Math.Abs(d11 + d12 - d) < 0.01f);
            }

            // Make sure the intersection point is inside the exclusion
            // zone, reusing i object for i2
            var i2 = i.Antipode;
            if (boundingCircle.Center.DistanceRadians(i2) < boundingCircle.Radius)
            {
                // between seg1 endpoints and second point of intersection
                var d21 = segment[0].DistanceRadians(i2);
                var d22 = segment[1].DistanceRadians(i2);
                // Check the distance of the intersection point and either
                // endpoint to see if it falls between them. Add a second
                // test to make sure that we are on the shorter of the two
                // segments between the endpoints.
                return (d21 <= d && d22 <= d && Math.Abs(d21 + d22 - d) < 0.01f);
            }

            return false;
        }
    }
}