using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossRadialSlice
    {
        public float[] Values { get; internal set; }
        public float Bearing { get; internal set; }
    }

    public class TransmissionLossFieldSlice
    {
        public enum SliceType
        {
            Minimum,
            Maximum,
            Mean,
        }

        public TransmissionLossFieldSlice(TransmissionLossField transmissionLossField, int maxDisplaySize, SliceType sliceType) : this(transmissionLossField, maxDisplaySize)
        { }
        public TransmissionLossFieldSlice(TransmissionLossField transmissionLossField, int maxDisplaySize, int depthIndex) : this(transmissionLossField, maxDisplaySize)
        { }

        protected TransmissionLossFieldSlice(TransmissionLossField transmissionLossField, int maxDisplaySize)
        {
            RangeCellCount = maxDisplaySize / 2;
        }

        public List<TransmissionLossRadialSlice> RadialSlices { get; private set; }
        public float Radius { get; private set; }
        public int RangeCellCount { get; private set; }
        public float[,] SliceData { get; private set; }
    }

    public class HorizontalWeightInfo
    {
        public uint RangeIndex { get; internal set; }
        public uint SourceRadialIndex { get; internal set; }
    }
}
