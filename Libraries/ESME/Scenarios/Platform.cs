using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using ESME.Locations;
using HRC.Aspects;

namespace ESME.Scenarios
{
    public class Platform : IHaveGuid
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

        public virtual Scenario Scenario { get; set; }
        //[Association("Platform_TrackDefinition", "Guid", "Guid")]
        public TrackDefinition TrackDefinition { get; set; }
        public virtual ICollection<Source> Sources { get; set; }
        public virtual ICollection<LogEntry> Logs { get; set; }
    }
}