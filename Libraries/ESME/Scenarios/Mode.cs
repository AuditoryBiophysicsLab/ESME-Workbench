using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using ESME.Database;
using ESME.Locations;
using HRC.Aspects;

namespace ESME.Scenarios
{
    public class Mode : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string PSMModeGuid { get; set; }
        public string ModeName { get; set; }
        public string ModeType { get; set; }
        public float? ActiveTime { get; set; }
        public float? Depth { get; set; }
        public float SourceLevel { get; set; }
        public float LowFrequency { get; set; }
        public float HighFrequency { get; set; }
        public DbTimeSpan PulseInterval { get; set; }
        public DbTimeSpan PulseLength { get; set; }
        public float HorizontalBeamWidth { get; set; }
        public float VerticalBeamWidth { get; set; }
        public float DepressionElevationAngle { get; set; }
        public float RelativeBeamAngle { get; set; }
        public float MaxPropagationRadius { get; set; }

        public virtual Source Source { get; set; }
        public virtual ICollection<LogEntry> Logs { get; set; }
    }
}