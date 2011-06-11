using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace HRC.Navigation
{
    public class Limits
    {
        // used as a zero reference
        static readonly TrackPoint ZeroPosit = new TrackPoint();

        // used as a default Limit
        public static Limits Root = new Limits();

        public static Limits EmptyLimit = new Limits
                                          {
                                              Name = "empty.ovr",
                                          };

        // used to help generate random values inside the region
        double _minLat = Double.MaxValue;
        double _maxLat = Double.MinValue;
        double _minLon = Double.MaxValue;
        double _maxLon = Double.MinValue;

        // the points converted to Geo
        List<Geo> _geoPointList = new List<Geo>();

        // this is used to create a dynamic shape - mostly beam patterns. it
        // maintains the original 0,0 north facing pattern that allow the
        // geoPointList to be rotated and translated
        readonly List<Geo> _shapeList = new List<Geo>();

        // used to test inside polygon (as in beam pattern)
        GeoRegion _region;
        Geo _centerOfRegion;
        bool _isClockWise;
        BoundingCircle _boundingCircle;

        Limits() { Initialize(); }

        public static explicit operator Limits(GeoRect geoRect)
        {
            var result = new Limits();
            result._geoPointList.Add(geoRect.NorthWest);
            result._geoPointList.Add(geoRect.NorthEast);
            result._geoPointList.Add(geoRect.SouthEast);
            result._geoPointList.Add(geoRect.SouthWest);
            result._geoPointList.Add(geoRect.NorthWest);
            result.Initialize();
            return result;
        }

        public Limits(IEnumerable<Geo> geos)
        {
            _geoPointList.AddRange(geos);
            Initialize();
        }

        /**
    * this can be used to set up the "rest" of the Limit if created from
    * serialization. just make it public
    */

        void Initialize()
        {
            foreach (var geo in _geoPointList)
            {
                // this reason we extract a rectangular extents is
                // for placing random points within the area.
                if (geo.Latitude < _minLat)
                {
                    _minLat = geo.Latitude;
                }
                if (geo.Latitude > _maxLat)
                {
                    _maxLat = geo.Latitude;
                }
                if (geo.Longitude < _minLon)
                {
                    _minLon = geo.Longitude;
                }
                if (geo.Longitude > _maxLon)
                {
                    _maxLon = geo.Longitude;
                }
            }
            // need this for a few things

            // create a region for testing inside / outside
            _region = new GeoRegion(_geoPointList);

            // BoundingCircle attempt to get the center of the polygon. when the
            // shape is not a poly, it hacks a fur ball
            _boundingCircle = _geoPointList.Count < 3 ? new BoundingCircle(Geo.FromDegrees(0.0, 0.0), 0.0) : new BoundingCircle(new GeoPath(_geoPointList));

            _centerOfRegion = _boundingCircle.Center;

            SetIsClockWise();
        }

        public static Limits CreateBoundingBoxLimit(List<Limits> areas, double rangeOutKm)
        {
            var result = new Limits
                         {
                             Name = "bounds-cw.ovr"
                         };

            foreach (var l in areas)
            {
                foreach (var p in l._geoPointList) result._geoPointList.Add(p);
            }

            foreach (var geo in result._geoPointList)
            {
                if (geo.Latitude < result._minLat)
                {
                    result._minLat = geo.Latitude;
                }
                if (geo.Latitude > result._maxLat)
                {
                    result._maxLat = geo.Latitude;
                }
                if (geo.Longitude < result._minLon)
                {
                    result._minLon = geo.Longitude;
                }
                if (geo.Longitude > result._maxLon)
                {
                    result._maxLon = geo.Longitude;
                }
            }

            result._geoPointList.Clear();

            result._geoPointList.Add(Geo.FromDegrees(result._minLat, result._minLon).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(225)));
            result._geoPointList.Add(Geo.FromDegrees(result._maxLat, result._minLon).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(315)));
            result._geoPointList.Add(Geo.FromDegrees(result._maxLat, result._maxLon).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(45)));
            result._geoPointList.Add(Geo.FromDegrees(result._minLat, result._maxLon).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(135)));
            result._geoPointList.Add(Geo.FromDegrees(result._minLat, result._minLon).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(225)));

            result.Initialize();

            return result;
        }

        public Limits CreateBoundingBoxLimit(double rangeOutKm)
        {
            var result = new Limits();

            if (_geoPointList == null || _geoPointList.Count == 0)
            {
                result.Name = Name + "-Undefined";
            }
            else
            {
                result.Name = Name + "-Bounding";

                foreach (var geo in _geoPointList)
                {
                    var dot = _centerOfRegion.Azimuth(geo);

                    var point = geo.Offset(Geo.KilometersToRadians(rangeOutKm), dot);

                    result._geoPointList.Add(point);
                }

                result.Initialize();
            }

            return result;
        }

        public Limits CreateExpandedLimit(double rangeOutKm)
        {
            var result = new Limits();

            if (_geoPointList == null || _geoPointList.Count == 0)
            {
                result.Name = Name + "-Undefined";
            }
            else
            {
                var newName = Name;
                if (newName != null)
                {
                    var suffix = "";
                    if (newName.EndsWith(".ovr"))
                    {
                        newName = newName.Substring(0, newName.IndexOf(".ovr"));
                        suffix = ".ovr";
                    }
                    result.Name = String.Format("{0}_{1}K{2}", newName, rangeOutKm, suffix);
                }

                Geo lastEndPt = null;
                var segIt = _region.Segments;
                GeoSegment seg;
                List<Geo> segPts = null;
                while (segIt.MoveNext())
                {
                    seg = segIt.Current;
                    segPts = seg.Segments;

                    var lineCourse = segPts[0].Azimuth(segPts[1]);

                    // gives specular reflection with angle from center of region
                    // double ang = Geo.angle(centerOfRegion, segPts[1], segPts[0]);
                    // double azimuth = Math.toDegrees((lineCourse + (isClockWise ? 1 :
                    // -1) * ang)) % (360.0);
                    var azimuth = (lineCourse + (_isClockWise ? -1 : 1) * (Math.PI / 2)) % ((Math.PI * 2.0));

                    var newPt0 = Geo.Offset(segPts[0], Geo.KilometersToRadians(rangeOutKm), azimuth);
                    var newPt1 = Geo.Offset(segPts[1], Geo.KilometersToRadians(rangeOutKm), azimuth);

                    if (lastEndPt != null)
                    {
                        var left = (_isClockWise ? lastEndPt : newPt0);
                        var right = (_isClockWise ? newPt0 : lastEndPt);

                        var arc = Geo.ApproximateArc(segPts[0], left, right, Geo.DegreesToRadians(5.0));
                        var arcList = new List<Geo>(arc.Length);
                        foreach (var a in arc)
                        {
                            if (_isClockWise)
                            {
                                arcList.Add(a);
                            }
                            else
                            {
                                arcList.Insert(0, a);
                            }
                        }

                        foreach (var a in arcList)
                        {
                            result._geoPointList.Add(a);
                        }
                    }
                    result._geoPointList.Add(newPt0);
                    result._geoPointList.Add(newPt1);

                    lastEndPt = newPt1;
                }

                if (lastEndPt != null)
                {
                    var left = (_isClockWise ? lastEndPt : result._geoPointList[0]);
                    var right = (_isClockWise ? result._geoPointList[0] : lastEndPt);
                    var arc = Geo.ApproximateArc(segPts[1], left, right, Geo.DegreesToRadians(5.0));

                    var arcList = new List<Geo>(arc.Length);
                    foreach (var a in arc)
                    {
                        if (_isClockWise)
                        {
                            arcList.Add(a);
                        }
                        else
                        {
                            arcList.Insert(0, a);
                        }
                    }

                    foreach (var a in arcList)
                    {
                        result._geoPointList.Add(a);
                    }
                }

                result.Initialize();
            }

            return result;
        }

        /**
    * create a Limits from parameters range and arc. the beam pattern in created
    * at geographical location 0, 0 lat, long, facing north. to use it, it must
    * be relocated to the current position and course
    * 
    * @param double aRadius - radius in meters
    * @param double aBeamWidth - beam width in degrees
    * @param double relAngle - relative angular offset -180/+180
    * @return - an Limits
    */
        public static Limits CreateBeamPattern(double aRadius, double aBeamWidth, double relAngle) { return CreateBeamPatternAt(ZeroPosit, aRadius, aBeamWidth, relAngle); }

        /**
    * @see createBeamPattern
    * @param posit
    *           - position to create beam pattern at.
    * @return
    */

        public static Limits CreateBeamPatternAt(TrackPoint posit, double aRadius, double aBeamWidth, double relAngle)
        {
            var flag360 = false;
            var beamWidth = aBeamWidth;

            var radius = Geo.KilometersToRadians(aRadius / 1000.0);

            if (Math.Abs(beamWidth - 360.0) < 0.001)
            {
                flag360 = true;
                beamWidth -= 5.0;
            }

            var origin = posit.GetGeoLlh();

            var top = beamWidth;
            var bot = 0.0;

            if (top >= 180.0)
            {
                top = 180.0;
                bot = beamWidth - 180.0;
            }

            var arc = new List<Geo>();

            var left = Geo.Offset(origin, radius, -Geo.DegreesToRadians(top / 2.0));
            var right = Geo.Offset(origin, radius, Geo.DegreesToRadians(top / 2.0));

            var arc1 = Geo.ApproximateArc(origin, left, right, Geo.DegreesToRadians(5.0));

            arc.AddRange(arc1);

            if (bot > 0.0)
            {
                var half = bot / 2.0;

                left = Geo.Offset(origin, radius, Geo.DegreesToRadians(270.0 - half));
                right = Geo.Offset(origin, radius, Geo.DegreesToRadians(270.0));

                var arc2 = Geo.ApproximateArc(origin, left, right, Geo.DegreesToRadians(5.0));

                var index = 0;
                foreach (var g in arc2)
                {
                    arc.Insert(index, g);
                    ++index;
                }

                left = Geo.Offset(origin, radius, Geo.DegreesToRadians(90.0));
                right = Geo.Offset(origin, radius, Geo.DegreesToRadians(90.0 + half));

                arc2 = Geo.ApproximateArc(origin, left, right, Geo.DegreesToRadians(5.0));

                arc.AddRange(arc2);
            }

            if (flag360)
            {
                arc.Add(arc[0]);
                arc.Insert(0, origin);
            }
            else
            {
                arc.Insert(0, origin);
                arc.Add(origin);
            }

            var limit = new Limits
                        {
                            Name = String.Format("BEAM {0} {1}", aRadius, beamWidth),
                            _geoPointList = arc
                        };

            // I believe shapelist is intended to hold the unrotated version of
            // the beam pattern
            limit._shapeList.AddRange(limit._geoPointList);

            // this call affects geoPointlist
            limit.Rotate(posit.GetCourse() + relAngle);

            limit.Initialize();

            return limit;
        }

        public String Name { get; set; }

        /**
    * return the list of points composing this limit
    * 
    * @return the Geo point list. there are encapsulation issues here
    */
        public List<Geo> GetGeoList() { return _geoPointList; }

        public GeoRegion GetGeoRegion() { return _region; }

        public Geo GetNorthWestPoint() { return Geo.FromDegrees(_maxLat, _minLon); }

        public Geo GetSouthEastPoint() { return Geo.FromDegrees(_minLat, _maxLon); }

        /**
    * create a random Geo point inside this pattern. tries 100 times to get a
    * point inside the boundary. if it fails, it will return the center point of
    * the min and max of the enclosing rectangle.
    * 
    * This fallback may be of little use ...
    * 
    * @return a Geo point
    */
        static readonly Random Random = new Random();

        public Geo GetRandomPointInside()
        {
            // this has happened
            if (_region == null) throw new ApplicationException("Limits.isInside: region is null");

            double lat;
            double lon;

            var geo = new Geo();

            var count = 100;

            while (count > 0)
            {
                var latDelta = _maxLat - _minLat;
                var lonDelta = _maxLon - _minLon;
                lat = _minLat + (Random.NextDouble() * latDelta);
                lon = _minLon + (Random.NextDouble() * lonDelta);

                geo.Initialize(lat, lon);

                if (IsPointInPolygon(geo, _centerOfRegion, _geoPointList))
                {
                    return geo;
                }

                --count;
            }

            // if the closure didn't work, default to center. really need a diag
            // here
            lat = _minLat + ((_maxLat - _minLat) / 2.0);
            lon = _minLon + ((_maxLon - _minLon) / 2.0);

            geo.Initialize(lat, lon);

            return geo;
        }

        //
        // if the closure didn't work, then try to make a bounding circle and get the
        // point from that.
        // GeoRegion region = limit.getGeoRegion();
        // if(region != null)
        // {
        // BoundingCircle circle = region.getBoundingCircle();
        // Geo center = circle.getCenter();
        // Geo northW = center.offset(circle.getRadius(),
        // 0).offset(circle.getRadius(), 1.5*Math.PI);
        // Geo southE = center.offset(circle.getRadius(),
        // 1.0*Math.PI).offset(circle.getRadius(), 0.5*Math.PI);
        // }
        /**
    * checks to see if this object is inside the passed in limit and point
    * outside the list causes the test to fail
    * 
    * @param limit
    * @return
    */

        public bool IsInside(Limits limit)
        {
            // this has happened
            if (_region == null) throw new ApplicationException("Limits.isInside: region is null");

            foreach (var point in limit._geoPointList)
            {
                if (_region.IsPointInside(point) == false)
                {
                    return false;
                }
            }

            return true;
        }

        public bool Contains(double aLat, double aLong)
        {
            // this has happened
            if (_region == null)
            {
                //Console.WriteLine("contains called with null region. name: " + Name);
                return false;
            }

            var geo = Geo.FromDegrees(aLat, aLong);

            // return getGeoRegion().isPointInside(Geo.FromDegrees(aLat, aLong));
            if (_boundingCircle == null || _boundingCircle.Intersects(geo, 0.0))
            {
                return IsPointInPolygon(geo, _centerOfRegion, _geoPointList);
            }

            return false;
        }

        bool IsPointInside(TrackPoint loc) { return IsPointInPolygon(loc.GetGeoLlh(), _centerOfRegion, _geoPointList); }

        static bool IsPointInPolygon(Geo x, Geo center, IList<Geo> poly)
        {
            // do this only once and pass it in for the math.
            // Geo c = Limits.center(poly);

            // bail out if the point is more than 90 degrees off the
            // centroid
            var d = x.DistanceRadians(center);
            if (d >= (Math.PI / 2))
            {
                return false;
            }
            // ray is normal to the great circle from c to x. reusing c to hold ray
            // info
            var ray = center.CrossNormalize(x);
            /*
       * side is a point on the great circle between c and x. It is used to
       * choose a direction.
       */
            var side = x.CrossNormalize(ray);
            var isIn = false;
            // Why do we need to allocate new Geos?
            // Geo p1 = new Geo(poly[0]);
            // Geo p2 = new Geo(poly[0]);
            var p1 = new Geo(poly[0]);
            var p2 = new Geo(poly[0]);
            var polySize = poly.Count;
            for (var i = 1; i < polySize; i++)
            {
                p2.Initialize(poly[i]);
                /*
          * p1 and p2 are on different sides of the ray, and the great acircle
          * between p1 and p2 is on the side that counts;
          */
                if ((p1.Dot(ray) < 0.0) != (p2.Dot(ray) < 0.0) && p1.Intersect(p2, ray).Dot(side) > 0.0)
                {
                    isIn = !isIn;
                }

                p1.Initialize(p2);
            }

            // Check for unclosed polygons, if the polygon isn't closed,
            // do the calculation for the last point to the starting
            // point.
            if (!poly[0].Equals(p1))
            {
                p2.Initialize(poly[0]);
                if ((p1.Dot(ray) < 0.0) != (p2.Dot(ray) < 0.0) && p1.Intersect(p2, ray).Dot(side) > 0.0)
                {
                    isIn = !isIn;
                }
            }

            return isIn;
        }


        public bool Contains(TrackPoint aLocation) { return IsPointInside(aLocation); }

        // public double getAreaKm2()
        // {
        // return (0.5 * (aRadius * aRadius) * Math.toRadians(aBeamWidth)) /
        // 1000000.0;
        // }

        public double GetAreaNm2()
        {
            if (_region == null)
            {
                //Console.WriteLine("getAreaNM2 called with null region");
                return 0.0;
            }

            var array = _region.Points;

            var se = Area(array);

            // square NM
            const double r2 = 3443.9182 * 3443.9182;

            var km = r2 * se;

            return km;
        }

        public double GetAreaKm2()
        {
            if (_region == null)
            {
                //Console.WriteLine("getAreaNM2 called with null region");
                return 0.0;
            }

            var array = _region.Points;

            var se = Area(array);

            // square clicks
            const double r2 = 6371.0 * 6371.0;

            var km = r2 * se;

            return km;
        }

        static double Area(IList<GeoPoint> array)
        {
            var count = 0;
            double area = 0;
            var v0 = new Geo(array[0].Point);
            var v1 = new Geo(array[1].Point);
            var p0 = new Geo(v0);
            var p1 = new Geo(v1);
            var p2 = new Geo();
            var size = array.Count - 1;
            double angle;

            for (var i = 2; i < size; i++)
            {
                count += 1;
                p2 = new Geo(array[i].Point);
                angle = Geo.AngleRadians(p0, p1, p2);
                area += angle;
                p0.Initialize(p1);
                p1.Initialize(p2);
            }

            count += 1;
            p2.Initialize(v0);
            angle = Geo.AngleRadians(p0, p1, p2);
            area += angle;
            p0.Initialize(p1);
            p1.Initialize(p2);

            count += 1;
            p2.Initialize(v1);
            angle = Geo.AngleRadians(p0, p1, p2);
            area += angle;

            return area - ((count - 2) * Math.PI);
        }

        void Rotate(double course)
        {
            if (_shapeList.Count == 0)
            {
                return;
            }

            // it might be possible to load the new posits in this array without
            // reallocating
            _geoPointList.Clear();

            // the math routine in Geo rotates counter-clockwise
            var rads = Geo.DegreesToRadians(360.0 - course);

            foreach (var g in _shapeList)
            {
                var geo = Rotation.Rotate(_shapeList[0], rads, g);

                _geoPointList.Add(geo);
            }
        }

        public new String ToString()
        {
            var sb = new StringBuilder();
            sb.AppendLine(Name);
            foreach (var p in _region.Points) sb.AppendLine(p.Point.ToString());
            //return Name;
            return sb.ToString();
        }

        public void ExportOvr(string directory)
        {
            var name = Name;
            if (!name.EndsWith(".ovr")) name += ".ovr";
            using (var writer = new StreamWriter(Path.Combine(directory, name)))
            {
                writer.WriteLine("#NEMO limits overlay:");
                writer.WriteLine("#{0}", name);
                writer.WriteLine();
                writer.WriteLine("navigation");
                writer.WriteLine();
                writer.WriteLine("orange");
                writer.WriteLine("solid");
                writer.WriteLine("move");
                writer.WriteLine("{0:0.#####} {1:0.#####}", _geoPointList[0].Latitude, _geoPointList[0].Longitude);
                writer.WriteLine("lines");
                var first = true;
                foreach (var p in _geoPointList)
                {
                    if (!first) writer.WriteLine("{0:0.#####} {1:0.#####}", p.Latitude, p.Longitude);
                    first = false;
                }
                writer.WriteLine();
            }
        }


        public bool IsLimitClockWise() { return _isClockWise; }

        void SetIsClockWise()
        {
            //Console.WriteLine(Name + " isClockWise check");
            var result = false;
            if (_region.Length > 0)
            {
                var segIt = _region.Segments;

                if (segIt.MoveNext())
                {
                    var seg1 = new Geo[2];
                    var temp = segIt.Current;
                    seg1[0] = Geo.FromGeo(temp.Segments[0]);
                    seg1[1] = Geo.FromGeo(temp.Segments[1]);

                    var seg2 = new Geo[2];
                    if (!segIt.MoveNext()) throw new ApplicationException("Not enough segments!");
                    temp = segIt.Current;
                    seg2[0] = Geo.FromGeo(temp.Segments[0]);
                    seg2[1] = Geo.FromGeo(temp.Segments[1]);

                    var pointInside = seg1[0].MidPoint(seg2[1]);
                    var s1 = seg1[0];
                    var s2 = seg1[1];
                    var p1 = s1.Cross(s2);
                    var p2 = s1.Cross(pointInside);
                    var p3 = p1.Cross(p2);

                    //Console.WriteLine("line seg: " + PrintGeo(s1) + " - to - " + PrintGeo(s2));
                    //Console.WriteLine("  --        inside: " + PrintGeo(pointInside));
                    //Console.WriteLine("  -- normal normal: " + PrintGeo(p3));

                    var sigPiLat = KillZeroSignum(pointInside.LatitudeRadians);
                    var sigPiLon = KillZeroSignum(pointInside.LongitudeRadians);
                    var sigP3Lat = KillZeroSignum(p3.LatitudeRadians);
                    var sigP3Lon = KillZeroSignum(p3.LongitudeRadians);

                    var pilatpos = (sigPiLat > 0);
                    var pilonpos = (sigPiLon > 0);
                    var p3Latpos = (sigP3Lat > 0);
                    var p3Lonpos = (sigP3Lon > 0);

                    // demorgan
                    // bool flag4 = !(pilatpos ^ p3latpos ) && !(p3lonpos ^ pilonpos);
                    // result = !flag4;

                    var flag4 = (pilatpos ^ p3Latpos) || (p3Lonpos ^ pilonpos);
                    result = flag4;
                }
            }
            //Console.WriteLine(" is clockwise? " + result);

            _isClockWise = result;
        }

        static double KillZeroSignum(double num)
        {
            double answer = Math.Sign(num);
            if (answer == -0.0)
            {
                answer = -1.0;
            }
            else if (answer == 0.0)
            {
                answer = 1.0;
            }
            return answer;
        }

        public static String PrintAngle(double d) { return String.Format("{0}", Geo.RadiansToDegrees(d)); }

        public static String PrintGeo(Geo p) { return String.Format("{0} {1}", p.Latitude, p.Longitude); }
    }
}