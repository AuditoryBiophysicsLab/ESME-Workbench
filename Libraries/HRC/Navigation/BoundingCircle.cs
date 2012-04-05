using System.Linq;

namespace HRC.Navigation
{
    public class BoundingCircle
    {
        /// <summary>
        /// Construct a BoundingCircle
        /// </summary>
        /// <param name="center">Center of the BoundingCircle</param>
        /// <param name="radius">Radius of the BoundingCircle, in radians</param>
        public BoundingCircle(Geo center, double radius)
        {
            Center = center;
            Radius = radius;
        }

        public BoundingCircle(IGeoArray geoArray)
        {
            Center = geoArray.Center;
            Radius = geoArray.Geos.Max(g => Center.DistanceRadians(g));
        }

        public Geo Center { get; private set; }

        /// <summary>
        /// Radius of the bounding circle, in radians.
        /// </summary>
        public double Radius { get; private set; }
        
        public bool Intersects(BoundingCircle bc) { return Intersects(bc.Center, bc.Radius); }
        public bool Intersects(Geo g, double r) { return Center.DistanceRadians(g) <= (Radius + r); }
        public bool Contains(Geo g) { return Intersects(g.BoundingCircle); }

        public override string ToString() { return string.Format("BoundingCircle: center: {0}, radius: {1:0.#####} degrees", Center, Geo.RadiansToDegrees(Radius)); }
    }
}