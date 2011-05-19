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
                for (var i = 0; i < Points.Count - 1; i++) yield return new GeoSegment(Points[i].Point, Points[i + 1].Point);
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
}
