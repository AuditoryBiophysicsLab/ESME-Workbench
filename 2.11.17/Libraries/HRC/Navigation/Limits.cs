using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;

namespace HRC.Navigation
{
    public class Limits
    {
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
        public List<Geo> Geos { get; private set; }

        // this is used to create a dynamic shape - mostly beam patterns. it
        // maintains the original 0,0 north facing pattern that allow the
        // geoPointList to be rotated and translated
        readonly List<Geo> _shapeList = new List<Geo>();

        // used to test inside polygon (as in beam pattern)
        GeoArray _region;
        Geo _centerOfRegion;
        bool _isClockWise;
        BoundingCircle _boundingCircle;

        Limits()
        {
            Geos = new List<Geo>();
            Initialize();
        }

        public static explicit operator Limits(GeoRect geoRect)
        {
            var result = new Limits();
            result.Geos.Add(geoRect.NorthWest);
            result.Geos.Add(geoRect.NorthEast);
            result.Geos.Add(geoRect.SouthEast);
            result.Geos.Add(geoRect.SouthWest);
            result.Geos.Add(geoRect.NorthWest);
            result.Initialize();
            return result;
        }

        public Limits(IEnumerable<Geo> geos) : this()
        {
            Geos.AddRange(geos);
            Initialize();
        }

        /**
    * this can be used to set up the "rest" of the Limit if created from
    * serialization. just make it public
    */

        void Initialize()
        {
            foreach (var geo in Geos)
            {
                // this reason we extract a rectangular extents is
                // for placing random points within the area.
                if (geo.Latitude < _minLat) _minLat = geo.Latitude;
                if (geo.Latitude > _maxLat) _maxLat = geo.Latitude;
                if (geo.Longitude < _minLon) _minLon = geo.Longitude;
                if (geo.Longitude > _maxLon) _maxLon = geo.Longitude;
            }
            // need this for a few things

            // create a region for testing inside / outside
            _region = new GeoArray(Geos);

            // BoundingCircle attempt to get the center of the polygon. when the
            // shape is not a poly, it hacks a fur ball
            _boundingCircle = Geos.Count < 3 ? new BoundingCircle(new Geo(0.0, 0.0), 0.0) : new BoundingCircle(_region);

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
                foreach (var p in l.Geos) result.Geos.Add(p);
            }

            foreach (var geo in result.Geos)
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

            result.Geos.Clear();

            result.Geos.Add(new Geo(result._minLat, result._minLon).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(225)));
            result.Geos.Add(new Geo(result._maxLat, result._minLon).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(315)));
            result.Geos.Add(new Geo(result._maxLat, result._maxLon).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(45)));
            result.Geos.Add(new Geo(result._minLat, result._maxLon).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(135)));
            result.Geos.Add(new Geo(result._minLat, result._minLon).Offset(Geo.KilometersToRadians(Math.Sqrt(2) * rangeOutKm), Geo.DegreesToRadians(225)));

            result.Initialize();

            return result;
        }

        public Limits CreateBoundingBoxLimit(double rangeOutKm)
        {
            var result = new Limits();

            if (Geos == null || Geos.Count == 0)
            {
                result.Name = Name + "-Undefined";
            }
            else
            {
                result.Name = Name + "-Bounding";

                foreach (var geo in Geos)
                {
                    var dot = _centerOfRegion.Azimuth(geo);

                    var point = geo.Offset(Geo.KilometersToRadians(rangeOutKm), dot);

                    result.Geos.Add(point);
                }

                result.Initialize();
            }

            return result;
        }

        public Limits CreateExpandedLimit(double rangeOutKm)
        {
            var result = new Limits();
            var geoList = new List<Geo>();

            if (Geos == null || Geos.Count == 0)
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
                GeoSegment segment = null;
                foreach (var curSegment in _region.Segments)
                {
                    var lineCourse = curSegment[0].Azimuth(curSegment[1]);
                    segment = curSegment;

                    // gives specular reflection with angle from center of region
                    // double ang = Geo.angle(centerOfRegion, segPts[1], segPts[0]);
                    // double azimuth = Math.toDegrees((lineCourse + (isClockWise ? 1 :
                    // -1) * ang)) % (360.0);
                    var azimuth = (lineCourse + (_isClockWise ? -1 : 1) * (Math.PI / 2)) % ((Math.PI * 2.0));

                    var newPt0 = curSegment[0].Offset(Geo.KilometersToRadians(rangeOutKm), azimuth);
                    var newPt1 = curSegment[1].Offset(Geo.KilometersToRadians(rangeOutKm), azimuth);

                    if (lastEndPt != null)
                    {
                        var left = (_isClockWise ? lastEndPt : newPt0);
                        var right = (_isClockWise ? newPt0 : lastEndPt);

                        var arc = curSegment[0].ApproximateArc(left, right, Geo.DegreesToRadians(5.0));
                        var arcList = new List<Geo>(arc.Length);
                        for (var i = 1; i < arc.Length - 1; i++)
                        {
                            var a = arc[i];
                            if (_isClockWise) arcList.Add(a);
                            else arcList.Insert(0, a);
                        }
                        geoList.AddRange(arcList);
                    }
                    geoList.Add(newPt0);
                    geoList.Add(newPt1);
                    for (var i = 0; i < geoList.Count - 1; i++)
                        if ((Math.Abs(geoList[i].Latitude - geoList[i + 1].Latitude) < .000001) && (Math.Abs(geoList[i].Longitude - geoList[i + 1].Longitude) < .000001)) Debugger.Break();

                    lastEndPt = newPt1;
                }

                if (lastEndPt != null)
                {
                    var left = (_isClockWise ? lastEndPt : geoList[0]);
                    var right = (_isClockWise ? geoList[0] : lastEndPt);
                    var arc = segment[1].ApproximateArc(left, right, Geo.DegreesToRadians(5.0));

                    var arcList = new List<Geo>(arc.Length);
                    for (var i = 1; i < arc.Length; i++)
                    {
                        var a = arc[i];
                        if (_isClockWise) arcList.Add(a);
                        else arcList.Insert(0, a);
                    }

                    geoList.AddRange(arcList);
                }
                for (var i = 0; i < geoList.Count - 1; i++)
                    if ((Math.Abs(geoList[i].Latitude - geoList[i + 1].Latitude) < .000001) && (Math.Abs(geoList[i].Longitude - geoList[i + 1].Longitude) < .000001)) Debugger.Break();
                result.Geos = geoList;
                result.Initialize();
            }

            return result;
        }

        public String Name { get; set; }

        /**
    * return the list of points composing this limit
    * 
    * @return the Geo point list. there are encapsulation issues here
    */
        public List<Geo> GetGeoList() { return Geos; }

        public GeoArray GetGeoRegion() { return _region; }

        public Geo GetNorthWestPoint() { return new Geo(_maxLat, _minLon); }

        public Geo GetSouthEastPoint() { return new Geo(_minLat, _maxLon); }

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

            Geo geo;

            var count = 100;

            while (count > 0)
            {
                var latDelta = _maxLat - _minLat;
                var lonDelta = _maxLon - _minLon;
                lat = _minLat + (Random.NextDouble() * latDelta);
                lon = _minLon + (Random.NextDouble() * lonDelta);

                geo = new Geo(lat, lon);

                if (IsPointInPolygon(geo, _centerOfRegion, Geos))
                {
                    return geo;
                }

                --count;
            }

            // if the closure didn't work, default to center. really need a diag
            // here
            lat = _minLat + ((_maxLat - _minLat) / 2.0);
            lon = _minLon + ((_maxLon - _minLon) / 2.0);

            geo = new Geo(lat, lon);

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

            return limit.Geos.All(point => _region.Contains(point));
        }

        public bool Contains(double aLat, double aLong)
        {
            // this has happened
            if (_region == null)
            {
                //Console.WriteLine("contains called with null region. name: " + Name);
                return false;
            }

            var geo = new Geo(aLat, aLong);

            // return getGeoRegion().isPointInside(Geo.FromDegrees(aLat, aLong));
            if (_boundingCircle == null || _boundingCircle.Intersects(geo, 0.0))
            {
                return IsPointInPolygon(geo, _centerOfRegion, Geos);
            }

            return false;
        }

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
            Geo p2;
            var polySize = poly.Count;
            for (var i = 1; i < polySize; i++)
            {
                p2 = new Geo(poly[i]);
                /*
          * p1 and p2 are on different sides of the ray, and the great acircle
          * between p1 and p2 is on the side that counts;
          */
                if ((p1.Dot(ray) < 0.0) != (p2.Dot(ray) < 0.0) && new GeoSegment(p1, p2).GreatCircleIntersection(ray).Dot(side) > 0.0)
                {
                    isIn = !isIn;
                }

                p1 = new Geo(p2);
            }

            // Check for unclosed polygons, if the polygon isn't closed,
            // do the calculation for the last point to the starting
            // point.
            if (!poly[0].Equals(p1))
            {
                p2 = new Geo(poly[0]);
                if ((p1.Dot(ray) < 0.0) != (p2.Dot(ray) < 0.0) && new GeoSegment(p1, p2).GreatCircleIntersection(ray).Dot(side) > 0.0)
                {
                    isIn = !isIn;
                }
            }

            return isIn;
        }


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

            var se = Area(_region.Geos.ToList());

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

            var se = Area(_region.Geos.ToList());

            // square clicks
            const double r2 = 6371.0 * 6371.0;

            var km = r2 * se;

            return km;
        }

        static double Area(IList<Geo> array)
        {
            var count = 0;
            double area = 0;
            var v0 = new Geo(array[0]);
            var v1 = new Geo(array[1]);
            var p0 = new Geo(v0);
            var p1 = new Geo(v1);
            var p2 = new Geo();
            var size = array.Count - 1;
            double angle;

            for (var i = 2; i < size; i++)
            {
                count += 1;
                p2 = new Geo(array[i]);
                angle = p0.Angle(p1, p2);
                area += angle;
                p0 = p1;
                p1 = p2;
            }

            count += 1;
            p2 = v0;
            angle = p0.Angle(p1, p2);
            area += angle;
            p0 = p1;
            p1 = p2;

            count += 1;
            p2 = v1;
            angle = p0.Angle(p1, p2);
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
            Geos.Clear();

            // the math routine in Geo rotates counter-clockwise
            var rads = Geo.DegreesToRadians(360.0 - course);

            foreach (var g in _shapeList)
            {
                var geo = Rotation.Rotate(_shapeList[0], g, rads, false);

                Geos.Add(geo);
            }
        }

        public new String ToString()
        {
            var sb = new StringBuilder();
            sb.AppendLine(Name);
            foreach (var p in _region.Geos) sb.AppendLine(p.ToString());
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
                writer.WriteLine("{0:0.#####} {1:0.#####}", Geos[0].Latitude, Geos[0].Longitude);
                writer.WriteLine("lines");
                var first = true;
                foreach (var p in Geos)
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
                var segments = _region.Segments.ToList();
                for (int segmentIndex = 0; segmentIndex < segments.Count; segmentIndex++)
                {
                    var segment = segments[segmentIndex];
                    var seg1 = new Geo[2];
                    seg1[0] = new Geo(segment[0]);
                    seg1[1] = new Geo(segment[1]);

                    var seg2 = new Geo[2];
                    segmentIndex++;
                    if (segmentIndex >= segments.Count) throw new ApplicationException("Not enough segments!");
                    segment = segments[segmentIndex];
                    seg2[0] = new Geo(segment[0]);
                    seg2[1] = new Geo(segment[1]);

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