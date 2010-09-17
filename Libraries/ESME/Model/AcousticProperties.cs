using System;

namespace ESME.Model
{
    public class AcousticProperties : IEquatable<AcousticProperties>
    {
        public AcousticProperties() { }

        public AcousticProperties(AcousticProperties that)
        {
            DepressionElevationAngle = that.DepressionElevationAngle;
            SourceDepth = that.SourceDepth;
            HighFrequency = that.HighFrequency;
            LowFrequency = that.LowFrequency;
            VerticalBeamWidth = that.VerticalBeamWidth;
        }

        public float SourceDepth { get; set; }
        public float VerticalBeamWidth { get; set; }
        public float DepressionElevationAngle { get; set; }
        public float LowFrequency { get; set; }
        public float HighFrequency { get; set; }

        #region IEquatable<AcousticProperties> Members

        bool IEquatable<AcousticProperties>.Equals(AcousticProperties that)
        {
            if (SourceDepth != that.SourceDepth) return false;
            if (VerticalBeamWidth != that.VerticalBeamWidth) return false;
            if (DepressionElevationAngle != that.DepressionElevationAngle) return false;
            if (LowFrequency != that.LowFrequency) return false;
            if (HighFrequency != that.HighFrequency) return false;
            return true;
        }

        #endregion
    }
}