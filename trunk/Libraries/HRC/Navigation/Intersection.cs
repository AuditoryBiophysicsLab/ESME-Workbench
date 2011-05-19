using System;
using System.Collections.Generic;
using System.Linq;

namespace HRC.Navigation
{
    public class Intersection
    {
        //
        // Utility methods (The Mathematics)
        //

        /**
         * Returns the two antipodal points of interection of two great circles
         * defined by the arcs (lat1, lon1) to (lat2, lon2) and (lat2, lon2) to
         * (lat4, lon4). All lat-lon values are in degrees.
         * 
         * @return an array of two lat-lon points arranged as lat, lon, lat, lon
         */

        public static float[] GetIntersection(float lat1, float lon1, float lat2, float lon2, float lat3, float lon3, float lat4, float lon4)
        {
            var geoCross1 = (new Geo(lat1, lon1)).crossNormalize(new Geo(lat2, lon2));
            var geoCross2 = (new Geo(lat3, lon3)).crossNormalize(new Geo(lat4, lon4));

            var geo = geoCross1.crossNormalize(geoCross2);
            var anti = geo.antipode();

            return new[]
                   {
                       ((float) geo.getLatitude()), ((float) geo.getLongitude()), ((float) anti.getLatitude()), ((float) anti.getLongitude())
                   };
        }

        /**
         * Returns a Geo representing the interection of two great circles defined
         * by the arcs (lat1, lon1) to (lat2, lon2) and (lat2, lon2) to (lat4,
         * lon4). All lat-lon values are in degrees.
         * 
         * @return Geo containing intersection, might have to check antipode of Geo
         *         for actual intersection.
         */

        public static Geo GetIntersectionGeo(float lat1, float lon1, float lat2, float lon2, float lat3, float lon3, float lat4, float lon4)
        {
            var geoCross1 = (new Geo(lat1, lon1)).crossNormalize(new Geo(lat2, lon2));
            var geoCross2 = (new Geo(lat3, lon3)).crossNormalize(new Geo(lat4, lon4));

            // geoCross memory is reused for answer.
            return geoCross1.crossNormalize(geoCross2, geoCross1);
        }

        /**
         * Returns true if the two segs intersect in at least one point. All lat-lon
         * values are in degrees. lat1,lon1-lat2,lon2 make up one segment,
         * lat3,lon3-lat4,lon4 make up the other segment.
         */

        public static bool Intersects(float lat1, float lon1, float lat2, float lon2, float lat3, float lon3, float lat4, float lon4)
        {
            var llp = GetSegIntersection(lat1, lon1, lat2, lon2, lat3, lon3, lat4, lon4);

            return (llp[0] != float.MaxValue && llp[1] != float.MaxValue) || (llp[2] != float.MaxValue && llp[3] != float.MaxValue);
        }

        /**
         * Checks if the two polygonal areas intersect. The two polygonal regions
         * are represented by two lat-lon arrays in the lat1, lon1, lat2, lon2,...
         * format. For closed polygons the last pair of points in the array should
         * be the same as the first pair. All lat-lon values are in degrees.
         */

        public static bool PolyIntersect(float[] polyPoints1, float[] polyPoints2)
        {
            // go through each side of poly1 and test to see if it
            // intersects with any side of poly2

            for (var i = 0; i < polyPoints1.Length / 2 - 1; i++)
            {
                for (var j = 0; j < polyPoints2.Length / 2 - 1; j++)
                {
                    if (Intersects(polyPoints1[2 * i], polyPoints1[2 * i + 1], polyPoints1[2 * i + 2], polyPoints1[2 * i + 3], polyPoints2[2 * j], polyPoints2[2 * j + 1], polyPoints2[2 * j + 2], polyPoints2[2 * j + 3])) return true;
                }
            }

            return false;
        }

        /**
         * checks if the polygon or polyline represented by the polypoints contains
         * any lines that intersect each other. All lat-lon values are in degrees.
         */

        public static bool IsSelfIntersectingPoly(float[] polyPoints)
        {
            for (var i = 0; i < polyPoints.Length / 2 - 1; i++)
            {
                for (var j = i + 1; j < polyPoints.Length / 2 - 1; j++)
                {
                    var lat1 = polyPoints[2 * i];
                    var lon1 = polyPoints[2 * i + 1];
                    var lat2 = polyPoints[2 * i + 2];
                    var lon2 = polyPoints[2 * i + 3];

                    var lat3 = polyPoints[2 * j];
                    var lon3 = polyPoints[2 * j + 1];
                    var lat4 = polyPoints[2 * j + 2];
                    var lon4 = polyPoints[2 * j + 3];

                    // ignore adjacent segments
                    if ((lat1 == lat4 && lon1 == lon4) || (lat2 == lat3 && lon2 == lon3)) continue;

                    if (Intersects(lat1, lon1, lat2, lon2, lat3, lon3, lat4, lon4)) return true;
                }
            }

            return false;
        }

        /**
         * Calculates the great circle distance from the point (lat, lon) to the
         * great circle containing the points (lat1, lon1) and (lat2, lon2).
         * 
         * @return nautical miles
         */
        public static float PointCircleDistanceNm(Geo p1, Geo p2, Geo center) { return (float) Geo.nm(PointCircleDistance(p1, p2, center)); }

        /**
         * Calculates the great circle distance from the point (lat, lon) to the
         * great circle containing the points (lat1, lon1) and (lat2, lon2).
         * 
         * @return radians
         */

        public static double PointCircleDistance(Geo p1, Geo p2, Geo center)
        {
            var n = Geo.crossNormalize(p1, p2, new Geo());
            var c = center.normalize(new Geo());
            var cosTheta = Geo.dot(n, c);
            var theta = Math.Acos(cosTheta);

            return Math.Abs(Math.PI / 2 - theta);
        }

        /**
         * Point i is on the great circle defined by the points a and b. Returns
         * true if i is between a and b, false otherwise. NOTE: i is assumed to be
         * on the great circle line.
         */

        public static bool IsOnSegment(Geo a, Geo b, Geo i)
        {
            // assert (< (Math.abs (.dot (.crossNormalize a b) i))
            // 1.e-15))

            return ((a.distance(i) < a.distance(b)) && (b.distance(i) < b.distance(a)));
        }

        /**
         * Returns true if i is on the great circle between a and b and between
         * them, false otherwise. In other words, this method does the great circle
         * test for you, too.
         */

        public static bool IsOnSegment(Geo a, Geo b, Geo i, double withinRad)
        {
            // assert (< (Math.abs (.dot (.crossNormalize a b) i))
            // 1.e-15))

            return ((Math.Abs(a.crossNormalize(b).dot(i)) <= withinRad) && (a.distance(i) < a.distance(b)) && (b.distance(i) < b.distance(a)));
        }

        /**
         * @return the Geo point i, which is on the great circle segment between Geo
         *         points a and b and which is closest to Geo point c. Returns null
         *         if there is no such point.
         */

        public static Geo SegIntersection(Geo a, Geo b, Geo c)
        {
            // Normal to great circle between a and b
            var g = a.crossNormalize(b);
            // Normal to the great circle between c and g
            var f = c.crossNormalize(g);
            // The intersection is normal to both, reusing g object for it's memory.
            var i = f.crossNormalize(g, g);
            if (IsOnSegment(a, b, i))
            {
                return i;
            }
            // reuse i object memory
            var ai = i.antipode(i);
            return IsOnSegment(a, b, ai) ? ai : null;
        }

        /**
         * Test if [p1-p2] and [p3-p4] intersect
         */

        public static bool SegIntersects(Geo p1, Geo p2, Geo p3, Geo p4)
        {
            var r = GetSegIntersection(p1, p2, p3, p4);
            return (r[0] != null || r[1] != null);
        }

        /**
         * returns the distance in NM between the point (lat, lon) and the point of
         * intersection of the great circle passing through (lat, lon) and
         * perpendicular to great circle segment (lat1, lon1, lat2, lon2). returns
         * -1 if point of intersection of the two great circle segs is not on the
         * great circle segment (lat1, lon1, lat2, lon2).
         */

        public static float PointSegDistanceNm(float lat1, float lon1, float lat2, float lon2, float lat, float lon)
        {
            var ret = PointSegDistance(new Geo(lat1, lon1), new Geo(lat2, lon2), new Geo(lat, lon));

            return (float) (ret == -1 ? ret : Geo.nm(ret));
        }

        /**
         * Returns the distance in radians between the point c and the point of
         * intersection of the great circle passing through c and perpendicular to
         * great circle segment between a and b. Returns -1 if point of intersection
         * of the two great circle segs is not on the great circle segment a-b.
         */

        public static double PointSegDistance(Geo a, Geo b, Geo c)
        {
            var i = SegIntersection(a, b, c);
            return (i == null) ? -1 : c.distance(i);
        }

        /**
         * Returns true or false depending on whether the great circle seg from
         * point p1 to point p2 intersects the circle of radius (radians) around
         * center.
         */

        public static bool IntersectsCircle(Geo p1, Geo p2, Geo center, double radius)
        {
            // check if either of the end points of the seg are inside the
            // circle
            var d1 = Geo.distance(p1, center);
            if (d1 < radius) return true;

            var d2 = Geo.distance(p2, center);
            if (d2 < radius) return true;

            var dist = PointCircleDistance(p1, p2, center);

            if (dist > radius) return false;

            // calculate point of intersection of great circle containing
            // (lat, lon) and perpendicular to great circle containing
            // (lat1, lon1) and (lat2, lon2)

            var g = p1.cross(p2);
            var f = center.cross(g);
            // Reusing g object for i
            var i = f.crossNormalize(g, g);

            // check if point of intersection lies on the segment
            // length of seg
            var d = Geo.distance(p1, p2);

            // Make sure the intersection point is inside the exclusion
            // zone
            if (center.distance(i) < radius)
            {
                // between seg endpoints and first point of intersection
                var d11 = Geo.distance(p1, i);
                var d12 = Geo.distance(p2, i);
                // Check the distance of the intersection point and either
                // endpoint to see if it falls between them. Add a second
                // test to make sure that we are on the shorter of the two
                // segments between the endpoints.
                return (d11 <= d && d12 <= d && Math.Abs(d11 + d12 - d) < 0.01f);
            }

            // Make sure the intersection point is inside the exclusion
            // zone, reusing i object for i2
            var i2 = i.antipode(i);
            if (center.distance(i2) < radius)
            {
                // between seg1 endpoints and second point of intersection
                var d21 = Geo.distance(p1, i2);
                var d22 = Geo.distance(p2, i2);
                // Check the distance of the intersection point and either
                // endpoint to see if it falls between them. Add a second
                // test to make sure that we are on the shorter of the two
                // segments between the endpoints.
                return (d21 <= d && d22 <= d && Math.Abs(d21 + d22 - d) < 0.01f);
            }

            return false;
        }

        /**
         * returns true if the specified poly path intersects the circle centered at
         * (lat, lon). All lat-lon values are in degrees. radius is in radians.
         */

        public static bool IntersectsCircle(float[] polyPoints, float lat, float lon, double radius)
        {
            var a = new Geo(polyPoints[0], polyPoints[1]);
            var b = new Geo();
            var c = new Geo(lat, lon);

            var numCoords = polyPoints.Length / 2 - 1;
            for (var i = 1; i < numCoords; i++)
            {
                var lat2 = polyPoints[2 * i];
                var lon2 = polyPoints[2 * i + 1];

                b.initialize(lat2, lon2);

                if (IntersectsCircle(a, b, c, radius)) return true;

                a.initialize(b);
            }

            return false;
        }

        /**
         * Returns the center of the polygon poly.
         */
        public static Geo Center(List<Geo> poly) { return Center(poly, new Geo()); }

        /**
         * Returns the center of the polygon poly.
         */

        public static Geo Center(List<Geo> poly, Geo ret)
        {
            var c = new Geo(poly[0]);
            for (var i = 1; i < poly.Count; i++)
            {
                ret.initialize(poly[i]);
                c = c.add(poly[i], c);
            }
            return c.normalize(ret);
        }

        /**
         * Returns the center of the polygon poly.
         * 
         * @param poly the List<GeoPoint> of the polygon
         * @param ret a Geo to use for the return values.
         * @return ret.
         */

        public static Geo Center(List<GeoPoint> poly)
        {
            var c = new Geo(poly[0].Point);
            var size = poly.Count;
            var ret = new Geo();
            for (var i = 1; i < size; i++)
            {
                ret = new Geo(poly[i].Point);
                c = c.add(ret, c);
            }
            return c.normalize(ret);
        }

        /**
         * Determines whether <code>x</code> is inside <code>poly</code>.
         * 
         * <p>
         * <em>N.B.</em><br>
         * <ul>
         * <li><code>poly</code> must be a closed polygon. In other words, the
         * first and last point must be the same.
         * <li>It is recommended that a bounds check is run before this method.
         * This method will return true if either <code>x</code> or the antipode
         * (the point on the opposite side of the planet) of <code>x</code> are
         * inside <code>poly</code>.
         * </ul>
         * 
         * <p>
         * <code>poly<code> is an array of latitude/longitude points where:
         * <br>
         * <pre>
         *                                                        
         *                                                                                               
         *                 poly[0] = latitude 1
         *                 poly[1] = longitude 1
         *                 poly[2] = latitude 2
         *                 poly[3] = longitude 2
         *                 .
         *                 .
         *                 .
         *                 poly[n-1] = latitude 1
         *                 poly[n] = longitude 1
         *                                                                                                
         * </pre>
         *
         * @param x a geographic coordinate
         * @param poly an array of lat/lons describing a closed polygon
         * @return true iff <code>x</code> or <code>antipode(x)</code> is
         * inside <code>poly</code>
         */

        public static bool IsPointInPolygon(Geo x, List<GeoPoint> poly)
        {
            var c = Center(poly);

            // bail out if the point is more than 90 degrees off the
            // centroid
            var d = x.distance(c);
            if (d >= (Math.PI / 2))
            {
                return false;
            }
            // ray is normal to the great circle from c to x. reusing c to hold ray
            // info
            var ray = c.crossNormalize(x, c);
            /*
         * side is a point on the great circle between c and x. It is used to
         * choose a direction.
         */
            var side = x.crossNormalize(ray, new Geo());
            var isIn = false;
            // Why do we need to allocate new Geos?
            // Geo p1 = new Geo(poly[0]);
            // Geo p2 = new Geo(poly[0]);
            var p1 = new Geo(poly[0].Point);
            Geo p2;
            var tmp = new Geo();
            var polySize = poly.Count;
            for (var i = 1; i < polySize; i++)
            {
                // p2.initialize(poly[i]);
                p2 = new Geo(poly[i].Point);
                /*
             * p1 and p2 are on different sides of the ray, and the great
             * acircle between p1 and p2 is on the side that counts;
             */
                if ((p1.dot(ray) < 0.0) != (p2.dot(ray) < 0.0) && p1.intersect(p2, ray, tmp).dot(side) > 0.0) isIn = !isIn;

                p1.initialize(p2);
            }

            // Check for unclosed polygons, if the polygon isn't closed,
            // do the calculation for the last point to the starting
            // point.
            if (!poly[0].Point.Equals(p1))
            {
                p2 = new Geo(poly[0].Point);
                if ((p1.dot(ray) < 0.0) != (p2.dot(ray) < 0.0) && p1.intersect(p2, ray, tmp).dot(side) > 0.0)
                {
                    isIn = !isIn;
                }
            }

            return isIn;
        }

        /**
         * Ask if a Geo point is in a polygon.
         * 
         * @param x
         * @param poly float array where [lat, lon, lat, lon,...]
         * @param polyInDegrees true of poly floats represent decimal degrees.
         * @return true for Geo in poly
         */

        public static bool IsPointInPolygon(Geo x, float[] poly, bool polyInDegrees)
        {
            var polyList = poly.Select((t, i) => polyInDegrees ? new GeoPoint(Geo.makeGeoDegrees(t, poly[i + 1])) : new GeoPoint(t, poly[i + 1])).ToList();
            return IsPointInPolygon(x, polyList);
        }

        /**
         * return true IFF some point of the first argument is inside the region
         * specified by the closed polygon specified by the second argument
         */

        public static bool IsPolylineInsidePolygon(List<GeoPoint> poly, List<GeoPoint> region)
        {
            var polySize = poly.Count;
            for (var i = 0; i < polySize; i++)
            {
                var testPoint = new Geo(poly[i].Point);
                if (IsPointInPolygon(testPoint, region))
                {
                    return true;
                }
            }
            return false;
        }

        /**
         * Returns the point of intersection of two great circle segments defined by
         * the segments. (lat1, lon1) to (lat2, lon2) and (lat2, lon2) to (lat4,
         * lon4). All lat-lon values are in degrees.
         * 
         * @return a float array of length 4 containing upto 2 valid lat-lon points
         *         of intersection that lie on both segments. Positions in the array
         *         not containing a valid lat/lon value are initialized to
         *         float.MaxValue.
         */

        public static float[] GetSegIntersection(float lat1, float lon1, float lat2, float lon2, float lat3, float lon3, float lat4, float lon4)
        {
            // KRA 03SEP03: The original version of this consed 26+ Geo's.
            // This one conses 8+. WAIT! Now it uses 6

            var p1 = new Geo(lat1, lon1);
            var p2 = new Geo(lat2, lon2);
            var p3 = new Geo(lat3, lon3);
            var p4 = new Geo(lat4, lon4);

            var results = GetSegIntersection(p1, p2, p3, p4);
            var i1 = results[0];
            var i2 = results[1];

            var llp = new[]
                      {
                          float.MaxValue, float.MaxValue, float.MaxValue, float.MaxValue
                      };

            // check if first point of intersection lies on both segments
            if (i1 != null)
            {
                llp[0] = ((float) i1.getLatitude());
                llp[1] = ((float) i1.getLongitude());
            }
            // check if second point of intersection lies on both segments
            if (i2 != null)
            {
                llp[2] = ((float) i2.getLatitude());
                llp[3] = ((float) i2.getLongitude());
            }
            return llp;
        }

        /**
         * Find the intersection(s) between [p1-p2] and [p3-p4]
         */

        public static Geo[] GetSegIntersection(Geo p1, Geo p2, Geo p3, Geo p4)
        {
            var geoCross1 = p1.crossNormalize(p2);
            var geoCross2 = p3.crossNormalize(p4);

            // i1 is really geoCross1, i2 is really geoCross2, memory-wise.
            var i1 = geoCross1.crossNormalize(geoCross2, geoCross1);
            var i2 = i1.antipode(geoCross2);

            // check if the point of intersection lies on both segs
            // length of seg1
            var d1 = p1.distance(p2);
            // length of seg2
            var d2 = p3.distance(p4);

            // between seg1 endpoints and first point of intersection
            var d111 = p1.distance(i1);
            var d121 = p2.distance(i1);

            // between seg1 endpoints and second point of intersection
            var d112 = p1.distance(i2);
            var d122 = p2.distance(i2);

            // between seg2 endpoints and first point of intersection
            var d211 = p3.distance(i1);
            var d221 = p4.distance(i1);

            // between seg2 endpoints and second point of intersection
            var d212 = p3.distance(i2);
            var d222 = p4.distance(i2);

            var result = new Geo[]
                         {
                             null, null
                         };

            // check if first point of intersection lies on both segments
            if (d1 >= d111 && d1 >= d121 && d2 >= d211 && d2 >= d221)
            {
                result[0] = i1;
            }
            // check if second point of intersection lies on both segments
            if (d1 >= d112 && d1 >= d122 && d2 >= d212 && d2 >= d222)
            {
                result[1] = i2;
            }
            return result;
        }

        // /**
        // * returns the point of interection of two great circle segments
        // * defined by the segments. (lat1, lon1) to (lat2, lon2) and
        // * (lat2, lon2) to (lat4, lon4). All lat-lon values are in
        // * degrees.
        // *
        // * @return a float array of length 4 containing upto 2 valid
        // * lat-lon points of intersection that lie on both
        // * segments. Positions in the array not containing a valid
        // * lat/lon value are initialized to float.MaxValue.
        // */
        // public static float[] getSegIntersectionOrig(float lat1, float
        // lon1,
        // float lat2, float lon2,
        // float lat3, float lon3,
        // float lat4, float lon4) {
        // // KRA 03SEP03: We can do better than this.
        //
        // float[] ll = getIntersection(lat1,
        // lon1,
        // lat2,
        // lon2,
        // lat3,
        // lon3,
        // lat4,
        // lon4);
        //
        // // check if the point of intersection lies on both segs
        //
        // // length of seg1
        // double d1 = Geo.distance(lat1, lon1, lat2, lon2);
        // // length of seg2
        // double d2 = Geo.distance(lat3, lon3, lat4, lon4);
        //
        // // between seg1 endpoints and first point of intersection
        // double d111 = Geo.distance(lat1, lon1, ll[0], ll[1]);
        // double d121 = Geo.distance(lat2, lon2, ll[0], ll[1]);
        //
        // // between seg1 endpoints and second point of intersection
        // double d112 = Geo.distance(lat1, lon1, ll[2], ll[3]);
        // double d122 = Geo.distance(lat2, lon2, ll[2], ll[3]);
        //
        // // between seg2 endpoints and first point of intersection
        // double d211 = Geo.distance(lat3, lon3, ll[0], ll[1]);
        // double d221 = Geo.distance(lat4, lon4, ll[0], ll[1]);
        //
        // // between seg2 endpoints and second point of intersection
        // double d212 = Geo.distance(lat3, lon3, ll[2], ll[3]);
        // double d222 = Geo.distance(lat4, lon4, ll[2], ll[3]);
        //
        // float[] llp = new float[] { float.MaxValue, float.MaxValue,
        // float.MaxValue, float.MaxValue };
        //
        // // check if first point of intersection lies on both segments
        // if (d1 >= d111 && d1 >= d121 && d2 >= d211 && d2 >= d221) {
        // llp[0] = ll[0];
        // llp[1] = ll[1];
        // }
        //
        // // check if second point of intersection lies on both segments
        // if (d1 >= d112 && d1 >= d122 && d2 >= d212 && d2 >= d222) {
        // llp[2] = ll[2];
        // llp[3] = ll[3];
        // }
        //
        // return llp;
        // }

        /**
         * Does the segment come within near radians of the region defined by
         * rCenter at rRadius?
         */

        public static bool IsSegmentNearRadialRegion(GeoSegment segment, Geo rCenter, double rRadius, double near)
        {
            var s = segment.Segments;
            if (s != null && s.Count == 2)
            {
                return IsSegmentNearRadialRegion(s[0], s[1], rCenter, rRadius, near);
            }
            return false;
        }

        /**
         * Does the segment come within near radians of the region defined by
         * rCenter at rRadius?
         */
        public static bool IsSegmentNearRadialRegion(Geo s1, Geo s2, Geo rCenter, double rRadius, double near) { return s1.isInside(s2, near + rRadius, rCenter); }

        /** Is a segment horizontally within range of a Region region? */

        public static bool IsSegmentNearRegion(GeoSegment segment, double hrange, GeoRegion region)
        {
            // Need to be careful here - calling
            // region.isSegmentNear(segment, hrange) can result in
            // circular code if the region just calls this method, which
            // may seem reasonable, if you look at the API.
            return IsSegmentNearPolyRegion(segment, region.Points, hrange);
        }

        /**
         * Does the segment come within near radians of the region defined by the
         * polygon in r[*]? Catches segments within poly region and returns after
         * first hit, which is why it returns bool.
         */

        public static bool IsSegmentNearPolyRegion(GeoSegment segment, List<GeoPoint> r, double near)
        {
            var s = segment.Segments;
            if (s != null && s.Count == 2)
            {
                return IsSegmentNearPolyRegion(s[0], s[1], r, near);
            }
            return false;
        }

        /**
         * Does the segment s1-s2 come within near radians of the region defined by
         * the polygon in r[*]? Catches segments within poly region and returns
         * after first hit, which is why it returns bool.
         */
        public static bool IsSegmentNearPolyRegion(Geo s1, Geo s2, List<GeoPoint> r, double near) { return IsSegmentNearPoly(s1, s2, r, near) != null || IsPointInPolygon(s1, r); }

        /**
         * Where is a segment within range of a region?
         */

        public static Geo IsSegmentNearPoly(GeoSegment segment, List<GeoPoint> r, double near)
        {
            var s = segment.Segments;
            if (s != null && s.Count == 2) return IsSegmentNearPoly(s[0], s[1], r, near);
            return null;
        }

        /**
         * Is a segment, represented by endpoints 's1' and 's2', withing a range
         * 'near' of region 'r'?
         * 
         * @param s1 Endpoint of segment
         * @param s2 Endpoint of segment
         * @param r Region of interest
         * @param near acceptable range between the segment and region, in radians.
         * @return Geo location where the condition was initially met (yes), null if
         *         conditions weren't met (no).
         */

        public static Geo IsSegmentNearPoly(Geo s1, Geo s2, List<GeoPoint> r, double near)
        {
            var rlen = r.Count;
            var pl0 = r.Last().Point;
            // check will be returned as ret, but we only allocate one per this
            // method call, instead of one per loop, since we are returning if ret
            // is not null.
            var check = new Geo();
            for (var j = 0; j < rlen; j++)
            {
                var pl1 = new Geo(r[j].Point);
                var ret = SegmentsIntersectOrNear(s1, s2, pl0, pl1, near, check);

                if (ret != null)
                {
                    return ret;
                }

                pl0.initialize(pl1);
            }
            return null;
        }

        /**
     * Where is a segment within range of a region?
     */

        public static List<Geo> SegmentNearPoly(GeoSegment segment, List<Geo> r, double near)
        {
            var s = segment.Segments;
            if (s != null && s.Count == 2) return SegmentNearPoly(s[0], s[1], r, near);
            return new List<Geo>();
        }

        /**
         * Where is a segment, represented by endpoints 's1' and 's2', withing a
         * range 'near' of region 'r'?
         * 
         * @param s1 Endpoint of segment
         * @param s2 Endpoint of segment
         * @param r Region of interest
         * @param near acceptable range between the segment and region, in radians.
         * @return Geo location where the condition was met (yes), null if
         *         conditions weren't met (no).
         */

        public static List<Geo> SegmentNearPoly(Geo s1, Geo s2, List<Geo> r, double near)
        {
            var rlen = r.Count;
            var pl0 = new Geo(r.Last());
            var list = new List<Geo>();
            var check = new Geo();
            for (var j = 0; j < rlen; j++)
            {
                var pl1 = new Geo(r[j]);
                var ret = SegmentsIntersectOrNear(s1, s2, pl0, pl1, near, check);

                if (ret != null)
                {
                    list.Add(ret);
                    // ret is actually the last created check. This mechanism limits
                    // the creation of Geos to only hits + 1.
                    check = new Geo();
                }

                pl0.initialize(pl1);
            }
            return list;
        }

        /**
         * Does the point s come within 'near' radians of the boarder of the region
         * defined by the polygon in r[*]?
         */

        public static bool IsPointNearPoly(Geo s, List<Geo> r, double near)
        {
            var rlen = r.Count;
            var pl0 = new Geo(r.Last());
            for (var j = 0; j < rlen; j++)
            {
                var pl1 = new Geo(r[j]);
                if (pl0.isInside(pl1, near, s))
                {
                    return true; // near enough to a region edge
                }
                pl0.initialize(pl1);
            }
            return false;
        }

        /**
         * Is one region's boundary within 'near' range of a region? Note: good
         * practice is s describes a smaller area than r.
         * 
         * @return the Geo location where the condition was first met, null if the
         *         condition wasn't met.
         */

        public static Geo IsPolyNearPoly(List<Geo> s, List<Geo> r, double near)
        {
            var rlen = r.Count;
            var slen = s.Count;
            var pl0 = new Geo(r.Last());
            var sl0 = new Geo(s.Last());
            for (var j = 0; j < rlen; j++)
            {
                var pl1 = new Geo(r[j]);
                for (var i = 0; i < slen; i++)
                {
                    var sl1 = new Geo(s[i]);
                    var ret = SegmentsIntersectOrNear(sl0, sl1, pl0, pl1, near);

                    if (ret != null)
                    {
                        return ret;
                    }
                    sl0 = sl1;
                }
                pl0 = pl1;
            }
            return null;
        }

        /**
         * Is one region's boundary within 'near' range of a region? Note: good
         * practice is s describes a smaller area than r.
         * 
         * @return a List where the polys intersect within the range, null if the
         *         condition wasn't met.
         */

        public static List<Geo> PolyNearPoly(List<Geo> s, List<Geo> r, double near)
        {
            var rlen = r.Count;
            var slen = s.Count;
            var pl0 = new Geo(r.Last());
            var sl0 = new Geo(s.Last());
            var list = new List<Geo>();
            for (var j = 0; j < rlen; j++)
            {
                var pl1 = new Geo(r[j]);
                for (var i = 0; i < slen; i++)
                {
                    var sl1 = new Geo(s[i]);
                    var ret = SegmentsIntersectOrNear(sl0, sl1, pl0, pl1, near);

                    if (ret != null) list.Add(ret);
                    sl0 = sl1;
                }
                pl0 = pl1;
            }

            return list;
        }

        /**
         * @return a Geo location iff the great circle segments defined by a1-a2 and
         *         b1-b2 intersect. the angles between the segments must be < PI or
         *         the results are ambiguous.Returns null if the segments don't
         *         interset within the range.
         */
        public static Geo SegmentsIntersect(Geo a1, Geo a2, Geo b1, Geo b2) { return SegmentsIntersectOrNear(a1, a2, b1, b2, 0); }

        /**
         * @return a Geo location iff the great circle segments defined by a1-a2 and
         *         b1-b2 come within the range (r, radians) of each other. The
         *         angles between the segments must be < PI or the results are
         *         ambiguous. Returns null if the segments don't interset within the
         *         range.
         */

        public static Geo SegmentsIntersectOrNear(Geo a1, Geo a2, Geo b1, Geo b2, double r)
        {
            if (a1 == null || a2 == null || b1 == null || b2 == null)
            {
                return null;
            }

            // ac and bc are the unit vectors normal to the two great
            // circles defined by the segments
            var ac = a1.crossNormalize(a2);
            var bc = b1.crossNormalize(b2);

            // aL and bL are the lengths (in radians) of the segments
            var aL = a1.distance(a2) + r;
            var bL = b1.distance(b2) + r;

            // i is one of the two points where the two great circles
            // intersect. Since we don't use bc anymore, let's reuse it for i, gets
            // passed back from crossNormalize as the second argument.
            var i = ac.crossNormalize(bc, bc);

            // if i is not on A
            if (!(i.distance(a1) <= aL && i.distance(a2) <= aL))
            {
                i = i.antipode(i); // switch to the antipode instead, reuse i
                // object for new values.
                if (!(i.distance(a1) <= aL && i.distance(a2) <= aL))
                {
                    // check
                    // again
                    // nope - neither i nor i' is on A, so we'll bail out
                    return null;
                }
            }
            // i is intersection or anti-intersection point now.

            // Now see if it intersects with b
            if (i.distance(b1) <= bL && i.distance(b2) <= bL) return i;
            return null;
        }

        /**
         * @return a Geo location iff the great circle segments defined by a1-a2 and
         *         b1-b2 come within the range (r, radians) of each other. The
         *         angles between the segments must be < PI or the results are
         *         ambiguous. Returns null if the segments don't interset within the
         *         range.
         */

        public static Geo SegmentsIntersectOrNear(Geo a1, Geo a2, Geo b1, Geo b2, double r, Geo ret)
        {
            if (a1 == null || a2 == null || b1 == null || b2 == null)
            {
                return null;
            }

            // ac and bc are the unit vectors normal to the two great
            // circles defined by the segments. ret is returned from crossNormalize
            // as bc.
            var ac = a1.crossNormalize(a2);
            var bc = b1.crossNormalize(b2, ret);

            // aL and bL are the lengths (in radians) of the segments
            var aL = a1.distance(a2) + r;
            var bL = b1.distance(b2) + r;

            // i is one of the two points where the two great circles
            // intersect. Since we don't use bc anymore, let's reuse it for i, gets
            // passed back from crossNormalize as the second argument.
            var i = ac.crossNormalize(bc, bc);

            // if i is not on A
            if (!(i.distance(a1) <= aL && i.distance(a2) <= aL))
            {
                i = i.antipode(ret); // switch to the antipode instead, reuse ret
                // yet again.
                if (!(i.distance(a1) <= aL && i.distance(a2) <= aL))
                {
                    // check
                    // again
                    // nope - neither i nor i' is on A, so we'll bail out
                    return null;
                }
            }
            // i is intersection or anti-intersection point now.

            // Now see if it intersects with b
            if (i.distance(b1) <= bL && i.distance(b2) <= bL) // remember ret -> bc -> i, so i == ret
                return i;
            return null;
        }
    }
}