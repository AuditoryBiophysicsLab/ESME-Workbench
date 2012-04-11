using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using ESME.Database;
using ESME.Locations;
using HRC.Aspects;

namespace ESME.Scenarios
{
    public class Platform : IHaveGuid, IHaveLayerSettings
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string Description { get; set; }
        public bool Launches { get; set; }
        public bool Tows { get; set; }
        public int RepeatCount { get; set; }

        // Copied from the PSM Platform
        public string PSMPlatformGuid { get; set; }
        public string PlatformName { get; set; }
        public string PlatformType { get; set; }

        public DbTrackType TrackType { get; set; }
        public DbGeo Geo { get; set; }
        public bool IsRandom { get; set; }
        public float Depth { get; set; }
        public float Course { get; set; }

        /// <summary>
        /// Speed in knots (nautical miles per hour)
        /// </summary>
        public float Speed { get; set; }

        public virtual Scenario Scenario { get; set; }
        public virtual Perimeter Perimeter { get; set; }
        public virtual LayerSettings LayerSettings { get; set; }
        public virtual ICollection<Source> Sources { get; set; }
        public virtual ICollection<LogEntry> Logs { get; set; }
    }
}