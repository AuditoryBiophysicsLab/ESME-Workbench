using System;
using System.Collections.Generic;
using ESME.Locations;
using HRC.Aspects;

namespace ESME.Scenarios
{
    public class Mode : IHaveGuid
    {
        [Initialize]
        public Guid Guid { get; set; }
        public string PSMModeGuid { get; set; }
        public string ModeName { get; set; }
        public string ModeType { get; set; }
        public float? ActiveTime { get; set; }
        public float? Depth { get; set; }
        public float SourceLevel { get; set; }
        public float LowFrequency { get; set; }
        public float HighFrequency { get; set; }
        /// <summary>
        /// In seconds
        /// </summary>
        public float PulseInterval { get; set; }
        /// <summary>
        /// In milliseconds
        /// </summary>
        public float PulseLength { get; set; }
        public float HorizontalBeamWidth { get; set; }
        public float VerticalBeamWidth { get; set; }
        public float DepressionElevationAngle { get; set; }
        public float RelativeBeamAngle { get; set; }
        public float MaxPropagationRadius { get; set; }

        public virtual Source Source { get; set; }
        public virtual ICollection<LogEntry> Logs { get; set; }
    }
}