﻿using System;
using ESME.NEMO;
using ESME.TransmissionLoss;

namespace ESME.Model
{
    public class AcousticProperties : IEquatable<AcousticProperties>, IEquatable<TransmissionLossField>
    {
        public AcousticProperties() { }

        public AcousticProperties(AcousticProperties that)
        {
            SourceDepth = that.SourceDepth;
            VerticalBeamWidth = that.VerticalBeamWidth;
            DepressionElevationAngle = that.DepressionElevationAngle;
            LowFrequency = that.LowFrequency;
            HighFrequency = that.HighFrequency;
        }

        public AcousticProperties(NemoMode nemoMode)
        {
            SourceDepth = Math.Max(1, nemoMode.SourceDepth);
            VerticalBeamWidth = nemoMode.VerticalBeamWidth;
            DepressionElevationAngle = nemoMode.DepressionElevationAngle;
            LowFrequency = nemoMode.LowFrequency;
            HighFrequency = nemoMode.HighFrequency;
        }

        /// <summary>
        /// Depth of the source, in meters
        /// </summary>
        public float SourceDepth { get; set; }
        /// <summary>
        /// Vertical beam width of this source, in degrees
        /// </summary>
        public float VerticalBeamWidth { get; set; }
        /// <summary>
        /// Depression/elevation angle, in degrees.
        /// Depression is the look angle towards the seafloor, and is expressed in positive degrees.
        /// Elevation is the look angle towards the sea surface, and is expressed in negative degrees
        /// </summary>
        public float DepressionElevationAngle { get; set; }
        /// <summary>
        /// Lowest frequency produced by this source, in Hertz
        /// Eventually, wideband sources will be modeled as a group of 1/3 octave narrowband sources.
        /// For narrowband sources, please specify LowFrequency and HighFrequency as the same value
        /// </summary>
        public float LowFrequency { get; set; }
        /// <summary>
        /// Highest frequency produced by this source, in Hertz.
        /// Eventually, wideband sources will be modeled as a group of 1/3 octave narrowband sources.
        /// For narrowband sources, please specify LowFrequency and HighFrequency as the same value
        /// </summary>
        public float HighFrequency { get; set; }

        #region IEquatable<AcousticProperties> Members

        bool IEquatable<AcousticProperties>.Equals(AcousticProperties other)
        {
            if (SourceDepth != other.SourceDepth) return false;
            if (VerticalBeamWidth != other.VerticalBeamWidth) return false;
            if (DepressionElevationAngle != other.DepressionElevationAngle) return false;
            if (LowFrequency != other.LowFrequency) return false;
            if (HighFrequency != other.HighFrequency) return false;
            return true;
        }

        #endregion

        public bool Equals(TransmissionLossField other) 
        {
            if (SourceDepth != other.SourceDepth) return false;
            if (VerticalBeamWidth != other.VerticalBeamWidth) return false;
            if (DepressionElevationAngle != other.DepressionElevationAngle) return false;
            if (LowFrequency != other.LowFrequency) return false;
            if (HighFrequency != other.HighFrequency) return false;
            return true;
        }
    }
}