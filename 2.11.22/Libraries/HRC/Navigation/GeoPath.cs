using System.Collections.Generic;

namespace HRC.Navigation
{
/**
 * An abstraction of an arbitrary geographic path. A path is assumed to mean a
 * chain of points that although it may share a common starting and end point,
 * it will not not represent an area in that case.
 * 
 * @author mthome@bbn.com
 */


    public interface IGeoPath : IGeoExtent
    {
        IEnumerable<GeoSegment> Segments { get; }
        IEnumerable<Geo> Geos { get; }

        /** Does the segment s come within epsilon (in radians) of us? */
        bool IsNearSegment(GeoSegment s, double epsilon);
    }
}
