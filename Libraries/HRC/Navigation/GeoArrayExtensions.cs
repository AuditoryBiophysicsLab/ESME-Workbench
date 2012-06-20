using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Serialization;

namespace HRC.Navigation
{
    public static class GeoArrayExtensions
    {
        static readonly Random Random = new Random();

        /// <summary>
        /// Bounce a platform around inside a given perimeter
        /// </summary>
        /// <param name="perimeter">A GeoArray of points in the perimeter, which must satisfy certain conditions (no segments may cross and the polygon 
        /// must be closed)</param>
        /// <param name="startPoint">A Geo containing the starting point.  If this is null, a point inside the perimeter is chosen randomly</param>
        /// <param name="initialCourse">The initial course, in radians from true north.  If startPoint is null, or if initialCourse is NaN, this is 
        /// chosen randomly</param>
        /// <param name="minimumCourseLength">The minimum length of the returned course, in meters</param>
        /// <returns>A GeoArray containing the start location and each point where the course bounces off the perimeter</returns>
        public static GeoArray PerimeterBounce(this GeoArray perimeter, Geo startPoint, double initialCourse, double minimumCourseLength)
        {
            if (!perimeter.IsClosed) throw new InvalidPerimeterException("Perimeter is not closed");
            if (perimeter.HasCrossingSegments) throw new InvalidPerimeterException("Perimeter is not a simple polygon (segments cross each other)");
            var points = new List<Geo>();
            while ((startPoint == null) || (!startPoint.IsInside(perimeter)))
            {
                var distance = Random.NextDouble() * perimeter.BoundingCircle.Radius;
                var azimuth = Random.NextDouble() * MoreMath.TwoPi;
                startPoint = perimeter.Center.Offset(distance, azimuth);
            }
            points.Add(startPoint);
            if (double.IsNaN(initialCourse)) initialCourse = Random.NextDouble()*MoreMath.TwoPi;
            var courseSegment = new GeoSegment(startPoint, startPoint.Offset(Geo.KilometersToRadians(0.001), initialCourse)).Scale(1000 * Geo.RadiansToKilometers(MoreMath.PiOverTwo));
            var bounceCount = 1;
            while (minimumCourseLength > 0)
            {
                var minIntersectionRange = double.MaxValue;
                GeoSegment firstIntersectingSegment = null;
                Geo firstIntersection = null;
                foreach (var segment in perimeter.Segments)
                {
                    var intersection = courseSegment.Intersection(segment);
                    if (intersection == null) continue;
                    var curIntersectionRange = startPoint.DistanceRadians(intersection);
                    if (curIntersectionRange < Geo.KilometersToRadians(0.001) || curIntersectionRange >= minIntersectionRange) continue;
                    minIntersectionRange = curIntersectionRange;
                    firstIntersectingSegment = segment;
                    firstIntersection = intersection;
                }
                if (firstIntersection == null) throw new PerimeterBounceException(string.Format("Course segment failed to intersect the perimeter on bounce {0}", bounceCount));
                var actualCourseSegment = new GeoSegment(points.Last(), firstIntersection);
                minimumCourseLength -= Geo.RadiansToKilometers(actualCourseSegment.LengthRadians) * 1000;
                points.Add(firstIntersection);
                var reflectedSegment = courseSegment.Reflect(firstIntersectingSegment);
                var reflectionAzimuth = reflectedSegment[0].Azimuth(reflectedSegment[1]);
                courseSegment = new GeoSegment(reflectedSegment[1], 1000 * Geo.RadiansToKilometers(MoreMath.PiOverTwo), Geo.RadiansToDegrees(reflectionAzimuth));
                bounceCount++;
            }
            return new GeoArray(points);
        }

        /// <summary>
        /// Returns a random location that is within the specified perimeter
        /// </summary>
        /// <param name="perimeter"></param>
        /// <returns></returns>
        public static Geo RandomLocationWithinPerimeter(this GeoArray perimeter)
        {
            if (!perimeter.IsClosed) throw new InvalidPerimeterException("Perimeter is not closed");
            if (perimeter.HasCrossingSegments) throw new InvalidPerimeterException("Perimeter is not a simple polygon (segments cross each other)");
            Geo location = null;
            while ((location == null) || (!location.IsInside(perimeter)))
            {
                var distance = Random.NextDouble() * perimeter.BoundingCircle.Radius;
                var azimuth = Random.NextDouble() * MoreMath.TwoPi;
                location = perimeter.Center.Offset(distance, azimuth);
            }
            return location;
        }

        public static bool Contains(this GeoArray region, Geo p)
        {
            return p.IsInside(region);
        }

        /// <summary>
        /// return true if at least one point of poly is inside region
        /// </summary>
        /// <param name="poly">Polygon to check</param>
        /// <param name="region">Closed region</param>
        /// <returns></returns>
        public static bool HasPointsInside(this GeoArray poly, GeoArray region)
        {
            return poly.Geos.Any(geo => geo.IsInside(region));
        }

        public static bool IsSelfIntersecting(IGeoArray geoPoly)
        {
            var segments = geoPoly.Segments.ToArray();
            for (var i = 0; i < segments.Length; i++)
            {
                var segment1 = segments[i];
                for (var j = i + 1; j < segments.Length; j++)
                {
                    var segment2 = segments[j];
                    if (segment1.Intersects(segment2)) return true;
                }
            }
            return false;
        }

        public static bool Intersects(this GeoArray geoPoly1, GeoArray geoPoly2)
        {
            return geoPoly1.Segments.Any(segment1 => geoPoly2.Segments.Any(segment1.Intersects));
        }

        /// <summary>
        /// Is one region's boundary within 'near' range of a region? Note: good practice is s describes a smaller area than r.
        /// </summary>
        /// <param name="s"></param>
        /// <param name="r"></param>
        /// <param name="near"></param>
        /// <returns>True if at least one point is within range, false otherwise</returns>
        public static bool IsNear(this IGeoArray s, IGeoArray r, double near)
        {
            return s.Segments.SelectMany(sSegment => r.Segments.Select(rSegment => sSegment.Intersection(rSegment, near)).Where(ret => ret != null)).FirstOrDefault() != null;
        }

        /// <summary>
        /// Is one region's boundary within 'near' range of a region? Note: good practice is s describes a smaller area than r.
        /// </summary>
        /// <param name="s"></param>
        /// <param name="r"></param>
        /// <param name="near"></param>
        /// <returns>List of Geo where the polys intersect within the range, empty list if the condition wasn't met.</returns>
        public static GeoArray Near(this IGeoArray s, IGeoArray r, double near)
        {
            return new GeoArray(s.Segments.SelectMany(sSegment => r.Segments.Select(rSegment => sSegment.Intersection(rSegment, near)).Where(ret => ret != null)));
        }

        /// <summary>
        /// Does the point s come within 'near' radians of the border of the region defined by the polygon in array?
        /// </summary>
        /// <param name="s"></param>
        /// <param name="array"></param>
        /// <param name="near"></param>
        /// <returns></returns>
        public static bool IsNear(this GeoArray array, Geo s, double near)
        {
            return array.Segments.Any(segment => segment.IsInside(near, s));
        }

        /// <summary>
        /// compute a polygonal approximation of an arc centered at pc, beginning at 
        /// p0 and ending at p1, going clockwise and including the two end points.
        /// </summary>
        /// <param name="center">center point</param>
        /// <param name="start">starting point</param>
        /// <param name="end">ending point</param>
        /// <param name="err">
        /// The maximum angle between approximates allowed, in radians.
        /// Smaller values will look better but will result in more returned points.
        /// </param>
        /// <returns></returns>
        public static GeoArray ApproximateArc(this Geo center, Geo start, Geo end, double err)
        {
            var theta = start.Angle(center, end);
            // if the rest of the code is undefined in this situation, just skip it.
            if (Double.IsNaN(theta))
            {
                return new GeoArray(new[]
                                        {
                                            start,
                                            end
                                        });
            }

            var n = (int)(2.0 + Math.Abs(theta / err)); // number of points
            // (counting the end
            // points)
            var result = new Geo[n];
            result[0] = start;
            var dtheta = theta / (n - 1);

            var rho = 0.0; // angle starts at 0 (directly at p0)

            for (var i = 1; i < n - 1; i++)
            {
                rho += dtheta;
                // Rotate p0 around this so it has the right azimuth.
                result[i] = Rotation.Rotate(center, start, 2.0 * Math.PI - rho, false);
            }
            result[n - 1] = end;

            return new GeoArray(result);
        }

        /// <summary>
        /// alias for ComputeCorridor(path, radius, Geo.DegreesToRadians(10.0), true)
        /// </summary>
        /// <param name="path"></param>
        /// <param name="radius"></param>
        /// <returns></returns>
        public static GeoArray ComputeCorridor(this GeoArray path, double radius)
        {
            return ComputeCorridor(path, radius, Geo.DegreesToRadians(10.0), true);
        }

        /// <summary>
        /// Wrap a fixed-distance corridor around an (open) path, as specified by a GeoArray
        /// </summary>
        /// <param name="path">Open path, must not have repeated points or consecutive antipodes</param>
        /// <param name="radius">Distance from path to widen corridor, in angular radians.</param>
        /// <param name="err">maximum angle of rounded edges, in radians. If 0, will directly cut outside bends.</param>
        /// <param name="roundEndCaps">if true, will round end caps</param>
        /// <returns>a closed polygon representing the specified corridor around the path.</returns>
        public static GeoArray ComputeCorridor(this IGeoArray path, double radius, double err, bool roundEndCaps)
        {
            if (radius < 0) throw new ArgumentException("Must be non negative", "radius");
            if (path == null) throw new ArgumentException("Must be non null", "path");
            if (path.IsClosed) throw new ArgumentException("Must not be closed", "path");

            var pl = path.Length;
            if (pl < 2)
                return null;

            // polygon will be right[0],...,right[n],left[m],...,left[0]
            var right = new List<Geo>();
            var left = new List<Geo>();

            Geo g0 = null; // previous point
            Geo n0 = null; // previous normal vector
            Geo l0 = null;
            Geo r0 = null;

            var g1 = path[0]; // current point

            int j;
            for (var i = 1; i < pl; i++)
            {
                var g2 = path[i]; // next point
                var n1 = g1.Cross(g2).Normalized; // n is perpendicular to the vector
                // from g1 to g2
                n1 = n1.Scale(radius); // normalize to radius
                // these are the offsets on the g2 side at g1
                var r1b = g1 + n1;
                var l1b = g1 - n1;

                if (n0 == null)
                {
                    if (roundEndCaps && err > 0)
                    {
                        // start cap
                        var arc = g1.ApproximateArc(l1b, r1b, err);
                        for (j = arc.Length - 1; j >= 0; j--)
                        {
                            right.Add(arc[j]);
                        }
                    }
                    else
                    {
                        // no previous point - we'll just be square
                        right.Add(l1b);
                        left.Add(r1b);
                    }
                    // advance normals
                    l0 = l1b;
                    r0 = r1b;
                }
                else
                {
                    // otherwise, compute a more complex shape

                    // these are the right and left on the g0 side of g1
                    var r1a = g1 + n0;
                    var l1a = g1 - n0;

                    var handed = g0.Cross(g1).Dot(g2); // right or left handed
                    // divergence
                    if (handed > 0)
                    {
                        // left needs two points, right needs 1
                        if (err > 0)
                        {
                            var arc = g1.ApproximateArc(l1b, l1a, err);
                            for (j = arc.Length - 1; j >= 0; j--)
                            {
                                right.Add(arc[j]);
                            }
                        }
                        else
                        {
                            right.Add(l1a);
                            right.Add(l1b);
                        }
                        l0 = l1b;

                        var ip = new GeoSegment(r0, r1a).GreatCircleIntersection(new GeoSegment(r1b, g2 + n1));
                        // if they intersect, take the intersection, else use the
                        // points and punt
                        if (ip != null)
                        {
                            left.Add(ip);
                        }
                        else
                        {
                            left.Add(r1a);
                            left.Add(r1b);
                        }
                        r0 = ip;
                    }
                    else
                    {
                        var ip = new GeoSegment(l0, l1a).GreatCircleIntersection(new GeoSegment(l1b, g2 - n1));
                        // if they intersect, take the intersection, else use the
                        // points and punt
                        if (ip != null)
                        {
                            right.Add(ip);
                        }
                        else
                        {
                            right.Add(l1a);
                            right.Add(l1b);
                        }
                        l0 = ip;
                        if (err > 0)
                        {
                            var arc = g1.ApproximateArc(r1a, r1b, err);
                            for (j = 0; j < arc.Length; j++)
                            {
                                left.Add(arc[j]);
                            }
                        }
                        else
                        {
                            left.Add(r1a);
                            left.Add(r1b);
                        }
                        r0 = r1b;
                    }
                }

                // advance points
                g0 = g1;
                n0 = n1;
                g1 = g2;
            }

            // finish it off
            var rn = g1 - n0;
            var ln = g1 + n0;
            if (roundEndCaps && err > 0)
            {
                // end cap
                var arc = g1.ApproximateArc(ln, rn, err);
                for (j = arc.Length - 1; j >= 0; j--)
                {
                    right.Add(arc[j]);
                }
            }
            else
            {
                right.Add(rn);
                left.Add(ln);
            }

            var ll = right.Count;
            var rl = left.Count;
            var result = new List<Geo>();
            for (var i = 0; i < ll; i++)
            {
                result.Add(right[i]);
            }
            for (var i = rl - 1; i >= 0; i--)
            {
                result.Add(left[i]);
            }
            return new GeoArray(result);
        }
    }

    [Serializable]
    public class PerimeterBounceException : Exception
    {
        //
        // For guidelines regarding the creation of new exception types, see
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/cpgenref/html/cpconerrorraisinghandlingguidelines.asp
        // and
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dncscol/html/csharp07192001.asp
        //

        public PerimeterBounceException() { }
        public PerimeterBounceException(string message) : base(message) { }
        public PerimeterBounceException(string message, Exception inner) : base(message, inner) { }
        protected PerimeterBounceException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    [Serializable]
    public class InvalidPerimeterException : Exception
    {
        //
        // For guidelines regarding the creation of new exception types, see
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/cpgenref/html/cpconerrorraisinghandlingguidelines.asp
        // and
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dncscol/html/csharp07192001.asp
        //

        public InvalidPerimeterException() { }
        public InvalidPerimeterException(string message) : base(message) { }
        public InvalidPerimeterException(string message, Exception inner) : base(message, inner) { }
        protected InvalidPerimeterException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }
}
