using System;
using System.Collections;
using System.Collections.Generic;

namespace HRC.Navigation
{
    public class Intersection
    {
        protected IMatchFilter filter;
        protected IMatchCollector collector;

        /**
     * Create an Intersection class that will use the provided MatchFilter and
     * MatchCollector.
     * 
     * @param filter
     * @param collector
     */

        public Intersection(IMatchFilter filter, IMatchCollector collector)
        {
            this.filter = filter;
            this.collector = collector;
        }

        /**
     * Create an Intersection class that will use the
     * MatchFilter.MatchParameters class with STRICT settings, and a
     * MatchCollector.SetMatchCollector.
     */

        public static Intersection StrictHashSetIntersector<T>()
        {
            return new Intersection(new MatchParametersMF(MatchParameters.Exact),
                                    new MatchCollector<HashSet<T>, T>());
        }

        /**
     * Create an Intersection class that will use the
     * MatchFilter.MatchParameters class with provided settings, and a
     * MatchCollector.SetMatchCollector.
     */

        public static Intersection HashSetIntersector<T>(IMatchParameters matchParams)
        {
            return new Intersection(new MatchParametersMF(matchParams), new MatchCollector<HashSet<T>, T>());
        }

        /**
     * Create an Intersection class that will use the
     * MatchFilter.MatchParameters class with provided settings, and a
     * MatchCollector.CollectionMatchCollector with the provided collector.
     */

        public static Intersection CollectionIntersector<T>(MatchParameters matchParams, List<T> c)
        {
            return new Intersection(new MatchParametersMF(matchParams),
                                    new MatchCollector<List<T>, T>());
        }

     /**
     * Retrieve the MatchCollector that contains the results from the
     * Intersection query.
     * 
     * @return MatchCollector that can be used to retrieve intersection results.
     */

        public IMatchCollector Collector { get { return collector; } }

        /**
     * Retrieve the MatchFilter that can be used to control which GeoExtents are
     * considered for Intersection queries.
     * 
     * @return The MatchFilter used to process intersection results.
     */

        public IMatchFilter Filter { get { return filter; } }

        /**
     * Asks the Intersection class to calculate the relationships between object
     * a and b. Calls the other consider methods, depending on what a and b are.
     * Consult the MatchCollector for the results.
     * 
     * @param a A GeoExtent object, generally.
     * @param b A ExtentImpl object or GeoExtent object, generally.
     */

        public void consider(Object a, Object b)
        {
            if (b is ICollection<GeoArray>)
            {
                if (a is GeoArray)
                {
                    considerRegionXRegions((GeoArray)a, (ICollection)b);
                }
                else if (a is IGeoPath)
                {
                    considerPathXRegions((IGeoPath) a, (ICollection) b);
                }
                else if (a is Geo)
                {
                    considerPointXRegions((Geo) a, (ICollection) b);
                }
            }
            else if (b is GeoArray)
            {
                if (a is GeoArray)
                {
                    considerRegionXRegion((GeoArray)a, (GeoArray)b);
                }
                else if (a is IGeoPath)
                {
                    considerPathXRegion((IGeoPath) a, (GeoArray) b);
                }
                else if (a is Geo)
                {
                    considerPointXRegion((Geo) a, (GeoArray) b);
                }
            }
        }

        /**
     * Loads the collector with regions from the Collection that intersect with
     * r.
     * 
     * @param r the region in question
     * @param regions a Collection of GeoRegions.
     */

        public void considerRegionXRegions(GeoArray geoRegion, ICollection regions)
        {
            /*
             * since the path is closed we'll check the region index for the whole
             * thing instead of segment-by-segment
             */
            foreach (var possible in regions)
            {
                var extent = (IGeoExtent) possible;
                if (possible is GeoArray)
                {
                    considerRegionXRegion(geoRegion, (GeoArray)possible);
                }
                else if (extent is IGeoPath)
                {
                    // This body used to be the following:
                    // considerPathXRegion((GeoPath) extent, r);
                    // but this reverses the match order and leads to "r" getting
                    // collected
                    // instead of extent. I've inlined the essential body and left
                    // it here
                    foreach (var segment in ((IGeoPath)extent).Segments)
                    {
                        if (filter.preConsider(segment, geoRegion) && considerSegmentXRegion(segment, geoRegion))
                        {
                            collector.collect(segment, extent);
                        }
                    }
                }
                else
                {
                    // usually, getting here means poly region vs radial region
                    var bc = extent.BoundingCircle;
                    var rbc = geoRegion.BoundingCircle;
                    // first pass check - the bounding circles intersect
                    if (rbc.Intersects(bc.Center, bc.Radius + filter.getHRange()))
                    {
                        var geos = new GeoArray(geoRegion.Geos);
                        if (bc.Center.IsInside(geos))
                        {
                            // the center of extent is inside r
                            collector.collect(geoRegion, extent);
                        }
                        else if (bc.Center.IsNear(geos, bc.Radius + filter.getHRange()))
                        {
                            // Center+radius of extent is within range an edge of
                            // the r
                            collector.collect(geoRegion, extent);
                        } // else no intersection
                    }
                }
            }
        }

        /**
     * Puts the region in the Collector if r intersects with it.
     * 
     * @param r
     * @param region
     */

        public void considerRegionXRegion(GeoArray region1, GeoArray region2)
        {
            var region1Boundary = new GeoArray(region1.Geos);
            var region1Point = region1Boundary[0];
            var region2Boundary = new GeoArray(region2.Geos);
            var region2Point = region2Boundary[0];

            // check for total containment
            if (region1Point.IsInside(region2Boundary) || region2Point.IsInside(region1Boundary))
                collector.collect(region1, region2);
            else
            {
                // gotta try harder, so we fall back to segment-by-segment
                // intersections
                foreach (var segment in region1.Segments)
                {
                    if (!filter.preConsider(segment, region2) || !considerSegmentXRegion(segment, region2)) continue;
                    collector.collect(segment, region2);
                    return;
                }
            }
        }

        /**
     * Puts regions in the Collector if they intersect the path.
     * 
     * @param path
     * @param regions
     */

        public void considerPathXRegions(IGeoPath path, ICollection regions)
        {
            /*
         * Since the path is open, then our best bet is to check each segment
         * separately
         */
            foreach (var seg in path.Segments)
            {
                foreach (var region in regions)
                {
                    var extent = (IGeoExtent) region;
                    if (!filter.preConsider(path, extent)) continue;
                    if (extent is GeoArray)
                    {
                        var curRegion = (GeoArray)extent;
                        if (considerSegmentXRegion(seg, curRegion))
                        {
                            collector.collect(seg, curRegion);
                        }
                    }
                    else
                        if (extent is IGeoPath)
                        {
                            var p = (IGeoPath) extent;
                            if (seg.IsNear(new GeoArray(p.Geos), filter.getHRange()))
                            {
                                collector.collect(seg, p);
                            }
                        }
                        else
                        {
                            var bc = extent.BoundingCircle;
                            if (seg.IsNear(bc, filter.getHRange()))
                            {
                                collector.collect(seg, extent);
                            }
                        }
                }
            }
        }

        /**
     * Puts the region in the Collector if it intersects with the path.
     * 
     * @param path
     * @param region
     */

        public void considerPathXRegion(IGeoPath path, GeoArray region)
        {
            foreach (var segment in path.Segments)
            {
                if (!filter.preConsider(segment, region) || !considerSegmentXRegion(segment, region)) continue;
                collector.collect(segment, region);
                return;
            }
        }

        /**
     * Returns true if the segment intersects with the region. The range of the
     * query is determined by the filter set on this Intersection object.
     * 
     * @param seg
     * @param region
     * @return true if segment intersects region
     */

        public bool considerSegmentXRegion(GeoSegment seg, GeoArray region)
        {
            return region.IsNearSegment(seg, filter.getHRange());
        }

        /**
     * Adds regions to the Collector if the GeoPoint p is on or in the regions
     * of the Collection.
     * 
     * @param p
     * @param regions
     */

        public void considerPointXRegions(Geo p, ICollection regions)
        {
            foreach (var region in regions)
            {
                var extent = (IGeoExtent) region;
                if (!filter.preConsider(p, extent)) continue;
                if (extent is GeoArray)
                {
                    var curRegion = (GeoArray)extent;
                    if (considerPointXRegion(p, curRegion))
                    {
                        collector.collect(p, curRegion);
                    }
                }
                else
                    if (extent is IGeoPath)
                    {
                        var path = (IGeoPath) extent;
                        if (p.IsNear(new GeoArray(path.Geos), filter.getHRange()))
                        {
                            collector.collect(p, path);
                        }
                    }
                    else
                    {
                        var bc = extent.BoundingCircle;
                        if (p.DistanceRadians(bc.Center) <= bc.Radius + filter.getHRange())
                        {
                            collector.collect(p, extent);
                        }
                    }
            }
        }

        /**
     * @param p
     * @param region
     * @return true if p is in region.
     */

        public bool considerPointXRegion(Geo p, GeoArray region)
        {
            return p.IsInside(region);
        }

        //
        // Static versions of intersection methods
        //

        /**
     * Simplified version of #intersect(Path, Collection, Algorithm) for old
     * code, using the default match algorithm, and returning the identifiers of
     * the regions that intersect with the path.
     * 
     * @param path
     * @param regions
     * @return an iterator over list of the intersecting regions.
     */

        public static IEnumerator intersect(Object path, Object regions)
        {
            var c = new SetMatchCollector<Geo>();
            var ix = new Intersection(new MatchParametersMF(MatchParameters.Exact), c);
            ix.consider(path, regions);
            return c.GetEnumerator();
        }

        /**
     * @return the Geo point i, which is on the segment which is closest to Geo point c. Returns null
     *         if there is no such point.
     */

        public static Geo SegmentIntersection(GeoSegment segment, Geo c)
        {
            // Normal to the great circle between c and the segment's normal
            var f = c.CrossNormalize(segment.Normal);
            // The intersection is normal to both
            var i = f.CrossNormalize(segment.Normal);
            if (i.IsOn(segment)) return i;
            var ai = i.Antipode;
            return ai.IsOn(segment) ? ai : null;
        }

        /**
     * Returns the distance in radians between the point c and the point of
     * intersection of the great circle passing through c and perpendicular to
     * great circle segment between a and b. Returns NaN if point of intersection
     * of the two great circle segs is not on the great circle segment a-b.
     */

        public static double PointSegmentDistance(GeoSegment segment, Geo c)
        {
            var i = SegmentIntersection(segment, c);
            return (i == null) ? double.NaN : c.DistanceRadians(i);
        }
    }
}