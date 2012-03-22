using System;
using ESME.Database;
using ESME.Locations;
using HRC.Aspects;

namespace ESME.Scenarios
{
    public class TrackDefinition : IHaveGuid
    {
        [Initialize]
        public Guid Guid { get; set; }

        public DbTrackType TrackType { get; set; }
        public DbTimeSpan StartTime { get; set; }
        public DbTimeSpan Duration { get; set; }
        public bool Random { get; set; }
        public bool OpsBounds { get; set; }
        public bool OpsTimes { get; set; }
        public float InitialLatitude { get; set; }
        public float InitialLongitude { get; set; }
        public float InitialDepth { get; set; }
        public float InitialCourse { get; set; }
        public float InitialSpeed { get; set; }

        public virtual Platform Platform { get; set; }
        public virtual Perimeter Perimeter { get; set; }
    }
}