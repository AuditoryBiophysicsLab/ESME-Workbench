using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossSlice
    {

        public class TransmissionLossRadialSlice
        {
            public float[] Values { get; internal set; }
            public float Bearing { get; internal set; }
        }

        public class TransmissionLossFieldSlice
        {
            public List<TransmissionLossRadialSlice> RadialSlices { get; internal set; }
            public float Radius { get; internal set; }
            public int RangeCellCount { get; internal set; }
        }
    }
}
