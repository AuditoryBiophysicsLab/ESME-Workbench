using System;
using System.ComponentModel.DataAnnotations;
using ESME.Database;
using ESME.Locations;
using HRC.Aspects;

namespace ESME.Scenarios
{
#if false
    public class TrackDefinition : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }

        public DbTrackType TrackType { get; set; }
        public DbGeo Geo { get; set; }
        public bool IsRandom { get; set; }
        public float Depth { get; set; }
        public float Course { get; set; }

        /// <summary>
        /// Speed in knots (nautical miles per hour)
        /// </summary>
        public float Speed { get; set; }

        public virtual Platform Platform { get; set; }
        public virtual Perimeter Perimeter { get; set; }
    }
#endif
}