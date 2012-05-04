using System;
using System.Xml.Serialization;
using ESME.NEMO;

namespace ESME.TransmissionLoss
{
    [Serializable]
    public class AcousticProperties : IEquatable<AcousticProperties>, IEquatable<NemoMode>
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

        /// <summary>
        /// If HighFrequency and LowFrequency are both non-zero, 
        ///   the geometric mean of HighFrequency and LowFrequency is returned
        /// Otherwise:
        ///   If HighFrequency is zero, LowFrequency is returned
        ///   If LowFrequency is zero, HighFrequency is returned
        ///   If both are zero, zero is returned
        /// </summary>
        [XmlIgnore]
        public float Frequency
        {
            get
            {
                if ((LowFrequency == 0) && (HighFrequency == 0)) return 0;
                if (LowFrequency == 0) return HighFrequency;
                if (HighFrequency == 0) return LowFrequency;
                return (float)Math.Sqrt(HighFrequency * LowFrequency);
            }
        }

        #region IEquatable<AcousticProperties> Members

        public bool Equals(AcousticProperties other)
        {
            if (SourceDepth != other.SourceDepth) return false;
            if (VerticalBeamWidth != other.VerticalBeamWidth) return false;
            if (DepressionElevationAngle != other.DepressionElevationAngle) return false;
            return Math.Abs(Frequency - other.Frequency) < 0.001;
        }

        #endregion

        public bool Equals(NemoMode other) { return Equals(other.AcousticProperties); }

        public override string ToString()
        {
            return string.Format("{0}|{1}|{2}|{3:0.###}", SourceDepth, VerticalBeamWidth, DepressionElevationAngle, Frequency);
        }
    }
}