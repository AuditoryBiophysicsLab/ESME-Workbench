using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.Model
{
    public class AcousticProperties : IEquatable<AcousticProperties>
    {
        public float SourceDepth_meters { get; set; }
        public float VerticalBeamWidth_degrees { get; set; }
        public float DepressionElevationAngle_degrees { get; set; }
        public float LowFrequency_Hz { get; set; }
        public float HighFrequency_Hz { get; set; }

        bool IEquatable<AcousticProperties>.Equals(AcousticProperties that)
        {
            if (this.SourceDepth_meters != that.SourceDepth_meters)
                return false;
            if (this.VerticalBeamWidth_degrees != that.VerticalBeamWidth_degrees)
                return false;
            if (this.DepressionElevationAngle_degrees != that.DepressionElevationAngle_degrees)
                return false;
            if (this.LowFrequency_Hz != that.LowFrequency_Hz)
                return false;
            if (this.HighFrequency_Hz != that.HighFrequency_Hz)
                return false;
            return true;
        }

        public AcousticProperties() { }

        public AcousticProperties(AcousticProperties that)
        {
            this.DepressionElevationAngle_degrees = that.DepressionElevationAngle_degrees;
            this.SourceDepth_meters = that.SourceDepth_meters;
            this.HighFrequency_Hz = that.HighFrequency_Hz;
            this.LowFrequency_Hz = that.LowFrequency_Hz;
            this.VerticalBeamWidth_degrees = that.VerticalBeamWidth_degrees;
        }
    }
}
