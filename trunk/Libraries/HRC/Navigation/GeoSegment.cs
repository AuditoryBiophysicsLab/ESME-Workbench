﻿using System.Collections.Generic;

namespace HRC.Navigation
{
    public class GeoSegment : IGeoExtent
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