using System;
using System.Collections.Generic;

namespace HRC.Navigation
{
    public class BoundingCircle
    {
        public BoundingCircle(Geo center, double radius) 
        {
            Init(center, radius);
        }

        public BoundingCircle(GeoPath path)
        {
            Init(path.Points);
        }

        void Init(Geo center, double radius) 
        {
            Center = center;
            Radius = radius;
        }

        void Init(List<GeoPoint> region)
        {
            var c = Intersection.Center(region); // centroid
            var storage = new Geo();
            var r = 0.0;
            var length = region.Count;
            for (var i = 0; i < length; i++)
            {
                storage.initialize(region[i].Point);
                var pr = c.distance(storage);
                if (pr > r)
                {
                    r = pr;
                }
            }

            Init(c, r);
        }

        public Geo Center { get; private set; }

        public double Radius { get; private set; }

        public bool Intersects(BoundingCircle bc) {
            return Intersects(bc.Center, bc.Radius);
        }
        
        public bool Intersects(Geo g, double r) {
            return Center.distance(g) <= (Radius + r);
        }
        
        public new String ToString() {
            return "BoundingCircle: center(" + Center + ") with radius (" + Radius + ")";
        }
        
    }
}
