using System;
using System.IO;
using System.Linq;

namespace HRC.Navigation
{
    public static class GeoExtensions
    {
        /// <summary>
        /// Is Geo p inside the time bubble along the great circle segment from 
        /// this to v2 looking forward forwardRadius and backward backwardRadius.
        /// </summary>
        /// <param name="segment"> </param>
        /// <param name="forwardRadius"></param>
        /// <param name="backRadius"></param>
        /// <param name="p"></param>
        /// <returns></returns>
        public static bool IsInBubble(this Geo p, GeoSegment segment, double forwardRadius, double backRadius)
        {
            return segment[0].DistanceRadians(p) <= (((segment[1] - segment[0]).Normalized.Dot(p - segment[0]) > 0.0) ? forwardRadius : backRadius);
        }

        /// <summary>
        /// Is geo inside region?
        /// Region must be closed.
        /// It is recommended that a bounds check is run before this method is called. 
        /// This method will return true if either geo or it's antipode are inside the region
        /// </summary>
        /// <param name="geo"></param>
        /// <param name="region"></param>
        /// <returns></returns>
        public static bool IsInsideBAD(this Geo geo, GeoArray region)
        {
            if (region == null) throw new ArgumentNullException("region");
            if (!region.IsClosed) throw new ArgumentException("Region must be closed", "region");

            var c = region.Center;

            // bail out if the point is more than 90 degrees off the centroid
            if (geo.DistanceRadians(c) >= MoreMath.PiOverTwo) throw new ArgumentException("Point cannot be more than 90 degrees from center of region to test", "geo");
            
            // ray is normal to the great circle from c to x.
            var ray = c.CrossNormalize(geo);

            // side is a point on the great circle between c and x. It is used to choose a direction.
            var side = geo.CrossNormalize(ray);
            var isIn = false;
            foreach (var segment in region.Segments)
            {
                if ((segment[0].Dot(ray) < 0.0) != (segment[1].Dot(ray) < 0.0) && segment.GreatCircleIntersection(ray).Dot(side) > 0.0)
                    isIn = !isIn;
            }
            return isIn;
        }

        /// <summary>
        /// Adapted from http://msdn.microsoft.com/en-us/library/cc451895.aspx
        /// </summary>
        /// <param name="geo"></param>
        /// <param name="region"></param>
        /// <returns></returns>
        public static bool IsInside(this Geo geo, GeoArray region)
        {
            if (region == null) throw new ArgumentNullException("region");
            if (!region.IsClosed) throw new ArgumentException("Region must be closed", "region");
            var points = region.ToArray();

            var j = points.Length - 1;
            var inPoly = false;

            for (var i = 0; i < points.Length; i++)
            {
                if (points[i].Longitude < geo.Longitude && points[j].Longitude >= geo.Longitude || 
                    points[j].Longitude < geo.Longitude && points[i].Longitude >= geo.Longitude) 
                    if (points[i].Latitude + (geo.Longitude - points[i].Longitude) / (points[j].Longitude - points[i].Longitude) * 
                        (points[j].Latitude - points[i].Latitude) < geo.Latitude) 
                        inPoly = !inPoly;
                j = i;
            }
            return inPoly;
        }

        public static bool IsInsideOld(this Geo geo, GeoArray region)
        {
            if (region == null) throw new ArgumentNullException("region");
            if (!region.BoundingBox.Contains(geo)) return false;

            if (!region.IsClosed) throw new ArgumentException("Region must be closed", "region");

            var c = region.Center;
            // bail out if the point is more than 90 degrees off the centroid
            if (geo.DistanceRadians(c) >= MoreMath.PiOverTwo) throw new ArgumentException("Point cannot be more than 90 degrees from center of region to test", "geo");

            var n = region.Length - 1;
            var v = region.ToArray();

            var wn = 0;    // the winding number counter

            // loop through all edges of the polygon
            for (var i = 0; i < n; i++)
            {   // edge from V[i] to V[i+1]
                if (v[i].Latitude <= geo.Latitude)
                {
                    // start y <= P.y
                    if ((v[i + 1].Latitude > geo.Latitude) &&   // an upward crossing and
                        (IsLeft(v[i], v[i + 1], geo) > 0))      // P left of edge
                        ++wn;                                   // have a valid up intersect
                }
                else
                {
                    // start y > P.y (no test needed)
                    if ((v[i + 1].Latitude <= geo.Latitude) &&  // a downward crossing
                        (IsLeft(v[i], v[i + 1], geo) < 0))      // P right of edge
                        --wn;                                   // have a valid down intersect
                }
            }
            return wn != 0;
        }

        private static int IsLeft(Geo p0, Geo p1, Geo p2)
        {
            var calc = ((p1.Longitude - p0.Longitude)*(p2.Latitude - p0.Latitude) -
                        (p2.Longitude - p0.Longitude)*(p1.Latitude - p0.Latitude));
            if (calc > 0) return 1;
            if (calc < 0) return -1;
            return 0;
        }
        /// <summary>
        /// Calculates the great circle distance from geo to the great circle described by segment.
        /// </summary>
        /// <param name="geo"></param>
        /// <param name="segment"></param>
        /// <returns>distance, in radians</returns>
        public static double DistanceToGreatCircle(this Geo geo, GeoSegment segment)
        {
            var cosTheta = segment.Normal.Dot(geo.Normalized);
            var theta = Math.Acos(cosTheta);
            return Math.Abs(Math.PI / 2 - theta);
        }

        /// <summary>
        /// Is geo on the specified segment?
        /// </summary>
        /// <param name="segment"></param>
        /// <param name="geo"></param>
        /// <returns>true if geo is on segment, false otherwise</returns>
        public static bool IsOn(this Geo geo, GeoSegment segment)
        {
            return (MoreMath.IsApproximatelyEqual(Math.Abs(segment[0].Cross(segment[1]).Normalized.Dot(geo)), 0.0) &&
                    (segment[0].DistanceRadians(geo) <= segment[0].DistanceRadians(segment[1])) &&
                    (segment[1].DistanceRadians(geo) <= segment[1].DistanceRadians(segment[0])));
        }

        /// <summary>
        /// Is geo on the specified segment or within a specified radius of it?
        /// </summary>
        /// <param name="segment"></param>
        /// <param name="geo"></param>
        /// <param name="withinRadians">
        /// Non-negative radius (in radians) that the geo must be within in order to return true.
        /// Defaults to zero (geo must be exactly on the segment to return true)
        /// </param>
        /// <returns>true if geo is near segment, false otherwise</returns>
        public static bool IsNear(this Geo geo, GeoSegment segment, double withinRadians)
        {
            if (withinRadians < 0) throw new ArgumentException("Must be non-negative", "withinRadians");
            return ((Math.Abs(segment.Normal.Dot(geo)) <= withinRadians) &&
                    (segment[0].DistanceRadians(geo) <= segment[0].DistanceRadians(segment[1])) &&
                    (segment[1].DistanceRadians(geo) <= segment[1].DistanceRadians(segment[0])));
        }

        /// <summary>
        /// Does the point s come within 'near' radians of the border of the region defined by the polygon in array?
        /// </summary>
        /// <param name="s"></param>
        /// <param name="array"></param>
        /// <param name="near"></param>
        /// <returns></returns>
        public static bool IsNear(this Geo s, GeoArray array, double near)
        {
            return array.Segments.Any(segment => s.IsNear(segment, near));
        }

        /// <summary>
        /// Write a Geo to the stream
        /// </summary>
        /// <param name="writer"></param>
        /// <param name="geo"></param>
        public static void Write(this BinaryWriter writer, Geo geo)
        {
            geo.Write(writer);
        }

        /// <summary>
        /// Read a Geo from the stream
        /// </summary>
        /// <param name="reader"></param>
        /// <returns></returns>
        public static Geo ReadGeo(this BinaryReader reader)
        {
            return Geo.Read(reader);
        }
    }
}