using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HRC.Navigation
{
public class GeoRegion : GeoPath 
{

    /**
     * Is the Geo inside the region?
     * 
     * @param point
     * @return true if point is inside region.
     */
        public GeoRegion(IEnumerable<Geo> coords) : base(coords) { }

    /**
         * Create a region of LatLon pairs.
         * 
         * @param lls alternating lat/lon in decimal degrees.
         */
        public GeoRegion(IList<float> lls) : this(lls, true) { }

    /**
         * Create a region of LatLon pairs.
         * 
         * @param lls alternating lat/lon values.
         * @param isDegrees true if lat/lon are in degrees, false if in radians.
         */
        public GeoRegion(IList<float> lls, bool isDegrees) : base(lls, isDegrees)
        {
        }

        public new bool IsSegmentNear(GeoSegment s, double epsilon) 
        {
            return Intersection.IsSegmentNearPolyRegion(s, Points, epsilon);
        }

        /// <summary>
        /// Is the Geo inside this region?
        /// </summary>
        /// <param name="p"></param>
        /// <returns>true if the point is inside this region</returns>
        public bool IsPointInside(Geo p)
        {
            return Intersection.IsPointInPolygon(p, Points);
        }

        public new BoundingCircle BoundingCircle
        {
            get
            {
                return Bc ?? new BoundingCircle(this);
            }
        }
    }
}
