namespace HRC.Navigation
{
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
}