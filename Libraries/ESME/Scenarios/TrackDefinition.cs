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
        public bool Random { get; set; }
        public bool OpsBounds { get; set; }
        public bool OpsTimes { get; set; }
        public float InitialLatitude { get; set; }
        public float InitialLongitude { get; set; }
        public float InitialDepth { get; set; }
        public float InitialCourse { get; set; }

        /// <summary>
        /// Speed in knots (nautical miles per hour)
        /// </summary>
        public float InitialSpeed { get; set; }

        public virtual Platform Platform { get; set; }
        public virtual Perimeter Perimeter { get; set; }
    }
}