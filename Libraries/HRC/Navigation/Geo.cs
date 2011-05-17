using System;

namespace HRC.Navigation
{
    public class Geo
    {
        public static double flattening = 1.0 / 298.257223563;
        public static double FLATTENING_C = (1.0 - flattening) * (1.0 - flattening);

        public static double METERS_PER_NM = 1852;
        static readonly double NPD_LTERM1 = 111412.84 / METERS_PER_NM;
        static readonly double NPD_LTERM2 = -93.5 / METERS_PER_NM;
        static readonly double NPD_LTERM3 = 0.118 / METERS_PER_NM;

        double _x;
        double _y;
        double _z;

        public static double npdAtLat(double latdeg)
        {
            var lat = (latdeg * Math.PI) / 180.0;
            return (NPD_LTERM1 * Math.Cos(lat) + NPD_LTERM2 * Math.Cos(3 * lat) + NPD_LTERM3 * Math.Cos(5 * lat));
        }

        /** Convert from geographic to geocentric latitude (radians) */
        public static double geocentricLatitude(double geographicLatitude) { return Math.Atan((Math.Tan(geographicLatitude) * FLATTENING_C)); }

        /** Convert from geocentric to geographic latitude (radians) */
        public static double geographicLatitude(double geocentricLatitude) { return Math.Atan(Math.Tan(geocentricLatitude) / FLATTENING_C); }

        /** Convert from degrees to radians. */
        public static double radians(double degrees) { return Length.DECIMAL_DEGREE.toRadians(degrees); }

        /** Convert from radians to degrees. */
        public static double degrees(double radians) { return Length.DECIMAL_DEGREE.fromRadians(radians); }

        /** Convert radians to kilometers. * */
        public static double km(double radians) { return Length.KM.fromRadians(radians); }

        /** Convert kilometers to radians. * */
        public static double kmToAngle(double km) { return Length.KM.toRadians(km); }

        /** Convert radians to nauticalMiles. * */
        public static double nm(double radians) { return Length.NM.fromRadians(radians); }

        /** Convert nautical miles to radians. * */
        public static double nmToAngle(double nm) { return Length.NM.toRadians(nm); }

        public Geo() { }

        /**
     * Construct a Geo from its latitude and longitude.
     * 
     * @param lat latitude in decimal degrees.
     * @param lon longitude in decimal degrees.
     */
        public Geo(double lat, double lon) { initialize(lat, lon); }

        /**
     * Construct a Geo from its latitude and longitude.
     * 
     * @param lat latitude.
     * @param lon longitude.
     * @param isDegrees should be true if the lat/lon are specified in decimal
     *        degrees, false if they are radians.
     */

        public Geo(double lat, double lon, bool isDegrees)
        {
            if (isDegrees)
            {
                initialize(lat, lon);
            }
            else
            {
                initializeRadians(lat, lon);
            }
        }

        /** Construct a Geo from its parts. */

        public Geo(double x, double y, double z)
        {
            x = x;
            y = y;
            z = z;
        }

        /** Construct a Geo from another Geo. */
        public Geo(Geo geo) : this(geo._x, geo._y, geo._z) { }

        public static Geo makeGeoRadians(double latr, double lonr)
        {
            var rlat = geocentricLatitude(latr);
            var c = Math.Cos(rlat);
            return new Geo(c * Math.Cos(lonr), c * Math.Sin(lonr), Math.Sin(rlat));
        }

        public static Geo makeGeoDegrees(double latd, double lond) { return makeGeoRadians(radians(latd), radians(lond)); }

        public static Geo makeGeo(double x, double y, double z) { return new Geo(x, y, z); }

        public static Geo makeGeo(Geo p) { return new Geo(p._x, p._y, p._z); }

        /**
     * Initialize this Geo to match another.
     * 
     * @param g
     */

        public void initialize(Geo g)
        {
            _x = g._x;
            _y = g._y;
            _z = g._z;
        }

        /**
     * Initialize this Geo with new parameters.
     * 
     * @param x
     * @param y
     * @param z
     */

        public void initialize(double x, double y, double z)
        {
            _x = x;
            _y = y;
            _z = z;
        }

        /**
     * Initialize this Geo with to represent coordinates.
     * 
     * @param lat latitude in decimal degrees.
     * @param lon longitude in decimal degrees.
     */
        public void initialize(double lat, double lon) { initializeRadians(radians(lat), radians(lon)); }

        /**
     * Initialize this Geo with to represent coordinates.
     * 
     * @param lat latitude in radians.
     * @param lon longitude in radians.
     */

        public void initializeRadians(double lat, double lon)
        {
            var rlat = geocentricLatitude(lat);
            var c = Math.Cos(rlat);
            _x = c * Math.Cos(lon);
            _y = c * Math.Sin(lon);
            _z = Math.Sin(rlat);
        }

        /**
     * Find the midpoint Geo between this one and another on a Great Circle line
     * between the two. The result is undefined of the two points are antipodes.
     * 
     * @param g2
     * @return midpoint Geo.
     */
        public Geo midPoint(Geo g2) { return add(g2).normalize(); }

        /**
     * Find the midpoint Geo between this one and another on a Great Circle line
     * between the two. The result is undefined of the two points are antipodes.
     * 
     * @param g2
     * @param ret a Geo value to set returned values in. Do not pass in a null
     *        value.
     * @return midpoint Geo.
     */
        public Geo midPoint(Geo g2, Geo ret) { return add(g2).normalize(ret); }

        public Geo interpolate(Geo g2, double x) { return scale(x).add(g2.scale(1 - x)).normalize(); }

        /**
     * 
     * @param g2
     * @param x
     * @param ret Do not pass in a null value.
     * @return
     */
        public Geo interpolate(Geo g2, double x, Geo ret) { return scale(x).add(g2.scale(1 - x, ret), ret).normalize(ret); }

        public String toString() { return "Geo[" + getLatitude() + "," + getLongitude() + "]"; }

        public double getLatitude() { return degrees(geographicLatitude(Math.Atan2(_z, Math.Sqrt(_x * _x + _y * _y)))); }

        public double getLongitude() { return degrees(Math.Atan2(_y, _x)); }

        public double getLatitudeRadians() { return geographicLatitude(Math.Atan2(_z, Math.Sqrt(_x * _x + _y * _y))); }

        public double getLongitudeRadians() { return Math.Atan2(_y, _x); }

        /**
     * Reader for x, in internal axis representation (positive to the right side
     * of screen).
     * 
     * @return
     */

        public double x
        {
            get { return _x; }
        }

        /**
     * Reader for y in internal axis reprensentation (positive into screen).
     * 
     * @return
     */

        public double y
        {
            get { return _y; }
        }

        /**
     * Reader for z in internal axis representation (positive going to top of
     * screen).
     * 
     * @return
     */

        public double z
        {
            get { return _z; }
        }

        public void setLength(double r)
        {
            // It's tempting to call getLatitudeRadians() here, but it changes the
            // angle. I think we want to keep the angles the same, and just extend
            // x, y, z, and then let the latitudes get refigured out for the
            // ellipsoid when they are asked for.
            var rlat = Math.Atan2(_z, Math.Sqrt(_x * _x + _y * _y));
            var rlon = getLongitudeRadians();

            var c = r * Math.Cos(rlat);
            _x = c * Math.Cos(rlon);
            _y = c * Math.Sin(rlon);
            _z = r * Math.Sin(rlat);
        }

        /** North pole. */
        public static Geo north = new Geo(0.0, 0.0, 1.0);

        /** Dot product. */
        public double dot(Geo b) { return ((_x * b._x) + (_y * b._y) + (_z * b._z)); }

        /** Dot product. */
        public static double dot(Geo a, Geo b) { return ((a._x * b._x) + (a._y * b._y) + (a._z * b._z)); }

        /** Euclidian length. */
        public double length() { return Math.Sqrt(dot(this)); }

        /** Multiply this by s. * */
        public Geo scale(double s) { return scale(s, new Geo()); }

        /**
     * Multiply this by s.
     * 
     * @return ret that was passed in, filled in with scaled values. Do not pass
     *         in a null value.
     */

        public Geo scale(double s, Geo ret)
        {
            ret.initialize(_x * s, _y * s, _z * s);
            return ret;
        }

        /** Returns a unit length vector parallel to this. */
        public Geo normalize() { return scale(1.0 / length()); }

        /**
     * Returns a unit length vector parallel to this.
     * 
     * @return ret with normalized values. Do not pass in a null value.
     */
        public Geo normalize(Geo ret) { return scale(1.0 / length(), ret); }

        /** Vector cross product. */
        public Geo cross(Geo b) { return cross(b, new Geo()); }

        /**
     * Vector cross product.
     * 
     * @return ret Do not pass in a null value.
     */

        public Geo cross(Geo b, Geo ret)
        {
            ret.initialize(_y * b._z - _z * b._y, _z * b._x - _x * b._z, _x * b._y - _y * b._x);
            return ret;
        }

        /** Eqvivalent to this.cross(b).length(). */

        public double crossLength(Geo b)
        {
            var x = _y * b._z - _z * b._y;
            var y = _z * b._x - _x * b._z;
            var z = _x * b._y - _y * b._x;
            return Math.Sqrt(x * x + y * y + z * z);
        }

        /** Eqvivalent to <code>this.cross(b).normalize()</code>. */
        public Geo crossNormalize(Geo b) { return crossNormalize(b, new Geo()); }

        /**
     * Eqvivalent to <code>this.cross(b).normalize()</code>.
     * 
     * @return ret Do not pass in a null value.
     */

        public Geo crossNormalize(Geo b, Geo ret)
        {
            var x = _y * b._z - _z * b._y;
            var y = _z * b._x - _x * b._z;
            var z = _x * b._y - _y * b._x;
            var L = Math.Sqrt(x * x + y * y + z * z);

            ret.initialize(x / L, y / L, z / L);
            return ret;
        }

        /**
     * Eqvivalent to <code>this.cross(b).normalize()</code>.
     * 
     * @return ret Do not pass in a null value.
     */
        public static Geo crossNormalize(Geo a, Geo b, Geo ret) { return a.crossNormalize(b, ret); }

        /** Returns this + b. */
        public Geo add(Geo b) { return add(b, new Geo()); }

        /*
     * @return ret Do not pass in a null value.
     */

        public Geo add(Geo b, Geo ret)
        {
            ret.initialize(_x + b._x, _y + b._y, _z + b._z);
            return ret;
        }

        /** Returns this - b. */
        public Geo subtract(Geo b) { return subtract(b, new Geo()); }

        /**
     * Returns this - b. *
     * 
     * @return ret Do not pass in a null value.
     */

        public Geo subtract(Geo b, Geo ret)
        {
            ret.initialize(_x - b._x, _y - b._y, _z - b._z);
            return ret;
        }

        public bool equals(Geo v2) { return _x == v2._x && _y == v2._y && _z == v2._z; }

        /** Angular distance, in radians between this and v2. */
        public double distance(Geo v2) { return Math.Atan2(v2.crossLength(this), v2.dot(this)); }

        /** Angular distance, in radians between v1 and v2. */
        public static double distance(Geo v1, Geo v2) { return v1.distance(v2); }

        /** Angular distance, in radians between the two lat lon points. */
        public static double distance(double lat1, double lon1, double lat2, double lon2) { return distance(new Geo(lat1, lon1), new Geo(lat2, lon2)); }

        /** Distance in kilometers. * */
        public double distanceKM(Geo v2) { return km(distance(v2)); }

        /** Distance in kilometers. * */
        public static double distanceKM(Geo v1, Geo v2) { return v1.distanceKM(v2); }

        /** Distance in kilometers. * */
        public static double distanceKM(double lat1, double lon1, double lat2, double lon2) { return distanceKM(new Geo(lat1, lon1), new Geo(lat2, lon2)); }

        /** Distance in nautical miles. * */
        public double distanceNM(Geo v2) { return nm(distance(v2)); }

        /** Distance in nautical miles. * */
        public static double distanceNM(Geo v1, Geo v2) { return v1.distanceNM(v2); }

        /** Distance in nautical miles. * */
        public static double distanceNM(double lat1, double lon1, double lat2, double lon2) { return distanceNM(new Geo(lat1, lon1), new Geo(lat2, lon2)); }

        /** Azimuth in radians from this to v2. */

        public double azimuth(Geo v2)
        {
            /*
         * n1 is the great circle representing the meridian of this. n2 is the
         * great circle between this and v2. The azimuth is the angle between
         * them but we specialized the cross product.
         */
            // Geo n1 = north.cross(this);
            // Geo n2 = v2.cross(this);
            // crossNormalization is needed to geos of different length.
            var n1 = north.crossNormalize(this);
            var n2 = v2.crossNormalize(this);
            var az = Math.Atan2(-north.dot(n2), n1.dot(n2));
            return (az > 0.0) ? az : 2.0 * Math.PI + az;
        }

        /**
     * Given 3 points on a sphere, p0, p1, p2, return the angle between them in
     * radians.
     */
        public static double angle(Geo p0, Geo p1, Geo p2) { return Math.PI - p0.cross(p1).distance(p1.cross(p2)); }

#if false
        /**
     * Computes the area of a polygon on the surface of a unit sphere given an
     * enumeration of its point.. For a non unit sphere, multiply this by the
     * radius of sphere squared.
     */

        public static double area(Enumeration vs)
        {
            var count = 0;
            double area = 0;
            var v0 = (Geo) vs.nextElement();
            var v1 = (Geo) vs.nextElement();
            var p0 = v0;
            var p1 = v1;
            Geo p2 = null;
            while (vs.hasMoreElements())
            {
                count = count + 1;
                p2 = (Geo) vs.nextElement();
                area = area + angle(p0, p1, p2);
                p0 = p1;
                p1 = p2;
            }

            count = count + 1;
            p2 = v0;
            area = area + angle(p0, p1, p2);
            p0 = p1;
            p1 = p2;

            count = count + 1;
            p2 = v1;
            area = area + angle(p0, p1, p2);

            return area - (count - 2) * Math.PI;
        }
#endif

        /**
     * Is the point, p, within radius radians of the great circle segment
     * between this and v2?
     */

        public bool isInside(Geo v2, double radius, Geo p)
        {
            // Allocate a Geo to be reused for all of these calculations, instead of
            // creating 3 of them that are just thrown away. There's one more we
            // still need to allocate, for dp below.
            var tmp = new Geo();

            /*
         * gc is a unit vector perpendicular to the plane defined by v1 and v2
         */
            var gc = crossNormalize(v2, tmp);

            /*
         * |gc . p| is the size of the projection of p onto gc (the normal of
         * v1,v2) cos(pi/2-r) is effectively the size of the projection of a
         * vector along gc of the radius length. If the former is larger than
         * the latter, than p is further than radius from arc, so must not be
         * isInside
         */
            if (Math.Abs(gc.dot(p)) > Math.Cos((Math.PI / 2.0) - radius)) return false;

            /*
         * If p is within radius of either endpoint, then we know it isInside
         */
            if (distance(p) <= radius || v2.distance(p) <= radius) return true;

            /* d is the vector from the v2 to v1 */
            var d = v2.subtract(this, tmp);

            /* L is the length of the vector d */
            var L = d.length();

            /* n is the d normalized to length=1 */
            var n = d.normalize(tmp);

            /* dp is the vector from p to v1 */
            var dp = p.subtract(this, new Geo());

            /* size is the size of the projection of dp onto n */
            var size = n.dot(dp);

            /* p is inside iff size>=0 and size <= L */
            return (0 <= size && size <= L);
        }

        /**
     * do the segments v1-v2 and p1-p2 come within radius (radians) of each
     * other?
     */
        public static bool isInside(Geo v1, Geo v2, double radius, Geo p1, Geo p2) { return v1.isInside(v2, radius, p1) || v1.isInside(v2, radius, p2) || p1.isInside(p2, radius, v1) || p1.isInside(p2, radius, v2); }

        /**
     * Static version of isInside uses conventional (decimal degree)
     * coordinates.
     */
        public static bool isInside(double lat1, double lon1, double lat2, double lon2, double radius, double lat3, double lon3) { return (new Geo(lat1, lon1)).isInside(new Geo(lat2, lon2), radius, new Geo(lat3, lon3)); }

        /**
     * Is Geo p inside the time bubble along the great circle segment from this
     * to v2 looking forward forwardRadius and backward backwardRadius.
     */
        public bool inBubble(Geo v2, double forwardRadius, double backRadius, Geo p) { return distance(p) <= ((v2.subtract(this).normalize().dot(p.subtract(this)) > 0.0) ? forwardRadius : backRadius); }

        /** Returns the point opposite this point on the earth. */
        public Geo antipode() { return scale(-1.0, new Geo()); }

        /**
     * Returns the point opposite this point on the earth. *
     * 
     * @return ret Do not pass in a null value.
     */
        public Geo antipode(Geo ret) { return scale(-1.0, ret); }

        /**
     * Find the intersection of the great circle between this and q and the
     * great circle normal to r.
     * <p>
     * 
     * That is, find the point, y, lying between this and q such that
     * 
     * <pre>
     *                              
     *  y = [x*this + (1-x)*q]*c
     *  where c = 1/y.dot(y) is a factor for normalizing y.
     *  y.dot(r) = 0
     *  substituting:
     *  [x*this + (1-x)*q]*c.dot(r) = 0 or
     *  [x*this + (1-x)*q].dot(r) = 0
     *  x*this.dot(r) + (1-x)*q.dot(r) = 0
     *  x*a + (1-x)*b = 0
     *  x = -b/(a - b)
     *                                        
     * </pre>
     * 
     * We assume that this and q are less than 180 degrees appart. When this and
     * q are 180 degrees appart, the point -y is also a valid intersection.
     * <p>
     * Alternatively the intersection point, y, satisfies y.dot(r) = 0
     * y.dot(this.crossNormalize(q)) = 0 which is satisfied by y =
     * r.crossNormalize(this.crossNormalize(q));
     * 
     */
        public Geo intersect(Geo q, Geo r) { return intersect(q, r, new Geo()); }

        /**
     * Find the intersection of the great circle between this and q and the
     * great circle normal to r.
     * <p>
     * 
     * That is, find the point, y, lying between this and q such that
     * 
     * <pre>
     *                              
     *  y = [x*this + (1-x)*q]*c
     *  where c = 1/y.dot(y) is a factor for normalizing y.
     *  y.dot(r) = 0
     *  substituting:
     *  [x*this + (1-x)*q]*c.dot(r) = 0 or
     *  [x*this + (1-x)*q].dot(r) = 0
     *  x*this.dot(r) + (1-x)*q.dot(r) = 0
     *  x*a + (1-x)*b = 0
     *  x = -b/(a - b)
     *                                        
     * </pre>
     * 
     * We assume that this and q are less than 180 degrees appart. When this and
     * q are 180 degrees appart, the point -y is also a valid intersection.
     * <p>
     * Alternatively the intersection point, y, satisfies y.dot(r) = 0
     * y.dot(this.crossNormalize(q)) = 0 which is satisfied by y =
     * r.crossNormalize(this.crossNormalize(q));
     * 
     * @return ret Do not pass in a null value.
     */

        public Geo intersect(Geo q, Geo r, Geo ret)
        {
            var a = dot(r);
            var b = q.dot(r);
            var x = -b / (a - b);
            // This still results in one Geo being allocated and lost, in the
            // q.scale call.
            return scale(x, ret).add(q.scale(1.0 - x), ret).normalize(ret);
        }

#if false
        /** alias for computeCorridor(path, radius, radians(10), true) * */
        public static Geo[] computeCorridor(Geo[] path, double radius) { return computeCorridor(path, radius, radians(10.0), true); }

        /**
     * Wrap a fixed-distance corridor around an (open) path, as specified by an
     * array of Geo.
     * 
     * @param path Open path, must not have repeated points or consecutive
     *        antipodes.
     * @param radius Distance from path to widen corridor, in angular radians.
     * @param err maximum angle of rounded edges, in radians. If 0, will
     *        directly cut outside bends.
     * @param capp iff true, will round end caps
     * @return a closed polygon representing the specified corridor around the
     *         path.
     * 
     */

        public static Geo[] computeCorridor(Geo[] path, double radius, double err, bool capp)
        {
            if (path == null || radius <= 0.0)
            {
                return new Geo[]
                       {};
            }
            // assert path!=null;
            // assert radius > 0.0;

            int pl = path.length;
            if (pl < 2) return null;

            // polygon will be right[0],...,right[n],left[m],...,left[0]
            var right = new ArrayList((int) (pl * 1.5));
            var left = new ArrayList((int) (pl * 1.5));

            Geo g0 = null; // previous point
            Geo n0 = null; // previous normal vector
            Geo l0 = null;
            Geo r0 = null;

            var g1 = path[0]; // current point

            for (var i = 1; i < pl; i++)
            {
                var g2 = path[i]; // next point
                var n1 = g1.crossNormalize(g2); // n is perpendicular to the vector
                // from g1 to g2
                n1 = n1.scale(radius); // normalize to radius
                // these are the offsets on the g2 side at g1
                var r1b = g1.add(n1);
                var l1b = g1.subtract(n1);

                if (n0 == null)
                {
                    if (capp && err > 0)
                    {
                        // start cap
                        var arc = approximateArc(g1, l1b, r1b, err);
                        for (var j = arc.length - 1; j >= 0; j--)
                        {
                            right.add(arc[j]);
                        }
                    }
                    else
                    {
                        // no previous point - we'll just be square
                        right.add(l1b);
                        left.add(r1b);
                    }
                    // advance normals
                    l0 = l1b;
                    r0 = r1b;
                }
                else
                {
                    // otherwise, compute a more complex shape

                    // these are the right and left on the g0 side of g1
                    var r1a = g1.add(n0);
                    var l1a = g1.subtract(n0);

                    var handed = g0.cross(g1).dot(g2); // right or left handed
                    // divergence
                    if (handed > 0)
                    {
                        // left needs two points, right needs 1
                        if (err > 0)
                        {
                            var arc = approximateArc(g1, l1b, l1a, err);
                            for (var j = arc.length - 1; j >= 0; j--)
                            {
                                right.add(arc[j]);
                            }
                        }
                        else
                        {
                            right.add(l1a);
                            right.add(l1b);
                        }
                        l0 = l1b;

                        Geo ip = Intersection.segmentsIntersect(r0, r1a, r1b, g2.add(n1));
                        // if they intersect, take the intersection, else use the
                        // points and punt
                        if (ip != null)
                        {
                            left.add(ip);
                        }
                        else
                        {
                            left.add(r1a);
                            left.add(r1b);
                        }
                        r0 = ip;
                    }
                    else
                    {
                        Geo ip = Intersection.segmentsIntersect(l0, l1a, l1b, g2.subtract(n1));
                        // if they intersect, take the intersection, else use the
                        // points and punt
                        if (ip != null)
                        {
                            right.add(ip);
                        }
                        else
                        {
                            right.add(l1a);
                            right.add(l1b);
                        }
                        l0 = ip;
                        if (err > 0)
                        {
                            var arc = approximateArc(g1, r1a, r1b, err);
                            for (var j = 0; j < arc.length; j++)
                            {
                                left.add(arc[j]);
                            }
                        }
                        else
                        {
                            left.add(r1a);
                            left.add(r1b);
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
            var rn = g1.subtract(n0);
            var ln = g1.add(n0);
            if (capp && err > 0)
            {
                // end cap
                var arc = approximateArc(g1, ln, rn, err);
                for (var j = arc.length - 1; j >= 0; j--)
                {
                    right.add(arc[j]);
                }
            }
            else
            {
                right.add(rn);
                left.add(ln);
            }

            int ll = right.size();
            int rl = left.size();
            var result = new Geo[ll + rl];
            for (var i = 0; i < ll; i++)
            {
                result[i] = (Geo) right.get(i);
            }
            var j = ll;
            for (var i = rl - 1; i >= 0; i--)
            {
                result[j++] = (Geo) left.get(i);
            }
            return result;
        }
#endif

        /** simple vector angle (not geocentric!) */
        static double simpleAngle(Geo p1, Geo p2) { return Math.Acos(p1.dot(p2) / (p1.length() * p2.length())); }

        /**
     * compute a polygonal approximation of an arc centered at pc, beginning at
     * p0 and ending at p1, going clockwise and including the two end points.
     * 
     * @param pc center point
     * @param p0 starting point
     * @param p1 ending point
     * @param err The maximum angle between approximates allowed, in radians.
     *        Smaller values will look better but will result in more returned
     *        points.
     * @return
     */

        public static Geo[] approximateArc(Geo pc, Geo p0, Geo p1, double err)
        {
            var theta = angle(p0, pc, p1);
            // if the rest of the code is undefined in this situation, just skip it.
            if (Double.IsNaN(theta))
            {
                return new[]
                       {
                           p0, p1
                       };
            }

            var n = (int) (2.0 + Math.Abs(theta / err)); // number of points
            // (counting the end
            // points)
            var result = new Geo[n];
            result[0] = p0;
            var dtheta = theta / (n - 1);

            var rho = 0.0; // angle starts at 0 (directly at p0)

            for (var i = 1; i < n - 1; i++)
            {
                rho += dtheta;
                // Rotate p0 around this so it has the right azimuth.
                result[i] = Rotation.rotate(pc, 2.0 * Math.PI - rho, p0, new Geo());
            }
            result[n - 1] = p1;

            return result;
        }

        public Geo[] approximateArc(Geo p0, Geo p1, double err) { return approximateArc(this, p0, p1, err); }

        /** @deprecated use </b>#offset(double, double) */
        public Geo geoAt(double distance, double azimuth) { return offset(distance, azimuth); }

        /**
     * Returns a Geo that is distance (radians), and azimuth (radians) away from
     * this.
     * 
     * @param distance distance of this to the target point in radians.
     * @param azimuth Direction of target point from this, in radians, clockwise
     *        from north.
     * @note this is undefined at the north pole, at which point "azimuth" is
     *       undefined.
     */
        public Geo offset(double distance, double azimuth) { return offset(distance, azimuth, new Geo()); }

        /**
     * Returns a Geo that is distance (radians), and azimuth (radians) away from
     * this.
     * 
     * @param distance distance of this to the target point in radians.
     * @param azimuth Direction of target point from this, in radians, clockwise
     *        from north.
     * @note this is undefined at the north pole, at which point "azimuth" is
     *       undefined.
     * @return ret Do not pass in a null value.
     */

        public Geo offset(double distance, double azimuth, Geo ret)
        {
            // m is normal the the meridian through this.
            var m = crossNormalize(north, ret);
            // p is a point on the meridian distance <tt>distance</tt> from this.
            // Geo p = (new Rotation(m, distance)).rotate(this);
            var p = Rotation.rotate(m, distance, this, ret);
            // Rotate p around this so it has the right azimuth.
            return Rotation.rotate(this, 2.0 * Math.PI - azimuth, p, ret);
        }

        public static Geo offset(Geo origin, double distance, double azimuth) { return origin.offset(distance, azimuth); }

        /**
     * 
     * @param origin
     * @param distance
     * @param azimuth
     * @param ret
     * @return ret Do not pass in a null value.
     */
        public static Geo offset(Geo origin, double distance, double azimuth, Geo ret) { return origin.offset(distance, azimuth, ret); }

        /*
     * //same as offset, except using trig instead of vector mathematics public
     * Geo trig_offset(double distance, double azimuth) { double latr =
     * getLatitudeRadians(); double lonr = getLongitudeRadians();
     * 
     * double coslat = Math.Cos(latr); double sinlat = Math.Sin(latr); double
     * cosaz = Math.Cos(azimuth); double sinaz = Math.Sin(azimuth); double sind =
     * Math.Sin(distance); double cosd = Math.Cos(distance);
     * 
     * return makeGeoRadians(Math.asin(sinlat * cosd + coslat * sind * cosaz),
     * Math.Atan2(sind * sinaz, coslat * cosd - sinlat * sind * cosaz) + lonr); }
     */

        //
        // Follows are a series of Geo array operations as useful utilities
        //
        /**
     * convert a String containing space-separated pairs of comma-separated
     * decimal lat-lon pairs into a Geo array.
     */
        public static Geo[] posToGa(String coords) { return posToGa(coords.Split(' ')); }

        /**
     * Convert an array of strings with comma-separated decimal lat,lon pairs
     * into a Geo array
     */

        public static Geo[] posToGa(String[] coords)
        {
            // convert to floating lat/lon degrees
            var ga = new Geo[coords.Length];
            for (var i = 0; i < coords.Length; i++)
            {
                var ll = coords[i].Split(',');
                ga[i] = makeGeoDegrees(Double.Parse(ll[0]), Double.Parse(ll[1]));
            }
            return ga;
        }

        /**
     * Convert a Geo array into a floating point lat lon array (alternating lat
     * and lon values).
     * 
     * @return the ll array provided, or a new array of lla is null.
     */

        public static double[] GaToLLa(Geo[] ga, double[] lla)
        {
            if (lla == null)
            {
                lla = new double[2 * ga.Length];
            }

            for (var i = 0; i < ga.Length; i++)
            {
                var g = ga[i];
                lla[i * 2] = g.getLatitude();
                lla[i * 2 + 1] = g.getLongitude();
            }
            return lla;
        }

        /**
     * Convert a Geo array into a floating point lat lon array (alternating lat
     * and lon values).
     * 
     * @return the ll array provided, or a new array of lla is null.
     */

        public static float[] GaToLLa(Geo[] ga, float[] lla)
        {
            if (lla == null)
            {
                lla = new float[2 * ga.Length];
            }

            for (var i = 0; i < ga.Length; i++)
            {
                var g = ga[i];
                lla[i * 2] = (float) g.getLatitude();
                lla[i * 2 + 1] = (float) g.getLongitude();
            }
            return lla;
        }

        /**
     * Convert a Geo array into a floating point lat lon array (alternating lat
     * and lon values)
     */
        public static float[] GaToLLa(Geo[] ga) { return GaToLLa(ga, new float[2 * ga.Length]); }

        /**
     * Return a Geo array with the duplicates removed. May arbitrarily mutate
     * the input array.
     */

        public static Geo[] removeDups(Geo[] ga)
        {
            var r = new Geo[ga.Length];
            var p = 0;
            for (var i = 0; i < ga.Length; i++)
            {
                if (p == 0 || !(r[p - 1].equals(ga[i])))
                {
                    r[p] = ga[i];
                    p++;
                }
            }
            if (p != ga.Length)
            {
                var x = new Geo[p];
                Array.Copy(r, 0, x, 0, p);
                return x;
            }
            else
            {
                return ga;
            }
        }

        /**
     * Convert a float array of alternating lat and lon pairs into a Geo array.
     */
        public static Geo[] LLaToGa(float[] lla) { return LLaToGa(lla, true); }

        /**
     * Convert a float array of alternating lat and lon pairs into a Geo array.
     */

        public static Geo[] LLaToGa(float[] lla, bool isDegrees)
        {
            var r = new Geo[lla.Length / 2];
            for (var i = 0; i < lla.Length / 2; i++)
            {
                if (isDegrees)
                {
                    r[i] = makeGeoDegrees(lla[i * 2], lla[i * 2 + 1]);
                }
                else
                {
                    r[i] = makeGeoRadians(lla[i * 2], lla[i * 2 + 1]);
                }
            }
            return r;
        }

        /**
     * Convert a double array of alternating lat and lon pairs into a Geo array.
     */
        public static Geo[] LLaToGa(double[] lla) { return LLaToGa(lla, true); }

        /**
     * Convert a double array of alternating lat and lon pairs into a Geo array.
     */

        public static Geo[] LLaToGa(double[] lla, bool isDegrees)
        {
            var r = new Geo[lla.Length / 2];
            for (var i = 0; i < lla.Length / 2; i++)
            {
                if (isDegrees)
                {
                    r[i] = makeGeoDegrees(lla[i * 2], lla[i * 2 + 1]);
                }
                else
                {
                    r[i] = makeGeoRadians(lla[i * 2], lla[i * 2 + 1]);
                }
            }
            return r;
        }

        /// <summary>
        ///   return a float array of alternating lat lon pairs where the first and
        ///   last pair are the same, thus closing the path, by adding a point if
        ///   needed. Does not mutate the input.
        /// </summary>
        public static float[] closeLLa(float[] lla)
        {
            var l = lla.Length;
            var s = (l / 2) - 1;
            if (lla[0] == lla[s * 2] && lla[1] == lla[s * 2 + 1])
            {
                return lla;
            }
            else
            {
                var llx = new float[l + 2];
                for (var i = 0; i < l; i++)
                {
                    llx[i] = lla[i];
                }
                llx[l] = lla[0];
                llx[l + 1] = lla[1];
                return llx;
            }
        }

        /// <summary>
        ///   return a Geo array where the first and last elements are the same, thus
        ///   closing the path, by adding a point if needed. Does not mutate the input.
        /// </summary>
        /// <param name = "ga"></param>
        /// <returns></returns>
        public static Geo[] closeGa(Geo[] ga)
        {
            var l = ga.Length;
            if (ga[0].equals(ga[l - 1]))
            {
                return ga;
            }
            else
            {
                var x = new Geo[l + 1];
                Array.Copy(ga, 0, x, 0, l);
                x[l] = ga[0];
                return x;
            }
        }
    }
}