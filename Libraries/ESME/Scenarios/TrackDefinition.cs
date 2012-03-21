using System;
using System.ComponentModel.DataAnnotations;
using ESME.Database;
using ESME.Locations;
using HRC.Aspects;

namespace ESME.Scenarios
{
    public class TrackDefinition : IHaveGuid
    {
        [Key, Initialize]
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

        //[Association("Platform_TrackDefinition", "Guid", "Guid")]
        public Platform Platform { get; set; }
        public virtual Perimeter Perimeter { get; set; }
    }
}