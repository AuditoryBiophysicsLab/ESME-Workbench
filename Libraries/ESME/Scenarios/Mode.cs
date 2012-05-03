using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using ESME.Database;
using ESME.Locations;
using HRC.Aspects;

namespace ESME.Scenarios
{
    public class Mode : IHaveGuid, IHaveLayerSettings, IEquatable<Mode>
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
        public virtual LayerSettings LayerSettings { get; set; }
        public virtual ICollection<LogEntry> Logs { get; set; }
        public virtual ICollection<TransmissionLoss> TransmissionLosses { get; set; }

        [NotMapped] public string PSMName { get { return string.Format("{0}:{1}:{2}", Source.Platform.PlatformName, Source.SourceName, ModeName); } }

        /// <summary>
        /// Indicates whether the current object is equal to another object of the same type.
        /// </summary>
        /// <returns>
        /// true if the current object is equal to the <paramref name="other"/> parameter; otherwise, false.
        /// </returns>
        /// <param name="other">An object to compare with this object.</param>
        public bool Equals(Mode other)
        {
            if (Guid != other.Guid) return false;
            var mydepth=0f;
            if (Depth.HasValue)
                mydepth = Depth.Value;
            if ((Source == null || Source.Platform == null) && (other.Source == null || other.Source.Platform == null)) return true;
            mydepth += Source.Platform.Depth;
            var otherdepth = 0f;
            if (other.Depth.HasValue)
                otherdepth = other.Depth.Value;
            otherdepth += other.Source.Platform.Depth;
            return (Math.Abs(mydepth - otherdepth) < .001);
        }

        public void CreateMapLayers() { throw new NotImplementedException(); }
    }
}