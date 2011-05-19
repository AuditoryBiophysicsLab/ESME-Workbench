using System.Collections.Generic;

namespace HRC.Navigation
{
    public class GeoPath : IGeoExtent
    {
        /**
         * Create a path of LatLon pairs.
         * 
         * @param lls alternating lat/lon in decimal degrees.
         */
        public GeoPath(IList<float> lls) : this(lls, true){}

        /**
         * Create a path of LatLon pairs.
         * 
         * @param lls alternating lat/lon values.
         * @param isDegrees true if lat/lon are in degrees, false if in radians.
         */
        public GeoPath(IList<float> lls, bool isDegrees) 
        {
            Points = new List<GeoPoint>();
            for (var i = 0; i < lls.Count; i += 2)
            {
                Points.Add(new GeoPoint(lls[i], lls[i + 1], isDegrees));
            }
        }

        public int Length { get { return Points.Count; } }

        /**
         * Create a path from Geos.
         * 
         * @param geos
         */
        public GeoPath(IEnumerable<Geo> geos) 
        {
            Points = new List<GeoPoint>();
            foreach (var geo in geos)
                Points.Add(new GeoPoint(geo));
        }

        public List<GeoPoint> Points { get; set; }

        public IEnumerator<GeoSegment> Segments
        {
            get
            {
                for (var i = 0; i < Points.Count; i += 2) yield return new GeoSegment(Points[i].Point, Points[i + 1].Point);
                yield break;
            }
        }

        public bool IsSegmentNear(GeoSegment s, double epsilon) {
            return Intersection.IsSegmentNearPoly(s, Points, epsilon) != null;
        }

        protected BoundingCircle Bc;

        public BoundingCircle BoundingCircle 
        {
            get { return Bc ?? (Bc = new BoundingCircle(this)); }
        }         
    }

    public class GeoPoint : IGeoExtent
    {
        readonly Geo _point;

        public GeoPoint(Geo p) { _point = p; }

        public GeoPoint(double lat, double lon) { _point = new Geo(lat, lon); }

        public GeoPoint(double lat, double lon, bool isDegrees) { _point = new Geo(lat, lon, isDegrees); }

        public Geo Point
        {
            get { return _point; }
        }

        public BoundingCircle BoundingCircle 
        {
            get { return new BoundingCircle(_point, 0.0); }
        }
    }

    public interface IGeoSegment : IGeoExtent
    {
        float[] SegmentArray { get; }
    }

    public class GeoSegment : IGeoSegment
    {
        public GeoSegment(Geo g1, Geo g2)
        {
            Segments = new List<Geo>
                       {
                           g1,
                           g2
                       };
        }

        public GeoSegment(IEnumerable<Geo> segments)
        {
            Segments = new List<Geo>(segments);
        }

        public List<Geo> Segments { get; private set; }

        public float[] SegmentArray 
        {
            get
            {
                return new[]
                       {
                           (float) Segments[0].getLatitude(), (float) Segments[0].getLongitude(), (float) Segments[1].getLatitude(), (float) Segments[1].getLongitude()
                       };
            }
        }

        public BoundingCircle BoundingCircle 
        {
            get { return new BoundingCircle(new GeoPath(Segments)); }
        }
    }
}
