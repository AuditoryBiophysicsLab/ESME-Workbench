using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Media;
using ESME.TransmissionLoss.Bellhop;

namespace ESME.TransmissionLoss
{
#if false
    public class TransmissionLossRadial
    {
        public TransmissionLossRadial(float bearingFromSource, ShadeFile bellhopOutput)
        {
            BearingFromSource = bearingFromSource;
            DataMax = bellhopOutput.DataMax;
            DataMin = bellhopOutput.DataMin;
            StatMax = bellhopOutput.StatMax;
            StatMin = bellhopOutput.StatMin;
            BellhopMean = bellhopOutput.Mean;
            Median = bellhopOutput.Median;
            Variance = bellhopOutput.Variance;
            StandardDeviation = bellhopOutput.StandardDeviation;

            TransmissionLoss = bellhopOutput.TransmissionLoss;

            Depths = new List<float>(bellhopOutput.ReceiverDepths);
            Ranges = new List<float>(bellhopOutput.ReceiverRanges);

            IsSaved = false;
        }
        
        public List<float> this[int rangeIndex]
        {
            get
            {
                if (rangeIndex > Ranges.Count) throw new IndexOutOfRangeException("");

                var retval = new List<float>();
                for (var i = 0; i < Depths.Count; i++)
                    retval.Add(TransmissionLoss[i, rangeIndex]);

                return retval;
            }
        }
        public float BearingFromSource { get; private set; }
        public float DataMax { get; private set; }
        public float DataMin { get; private set; }
        public float StatMax { get; private set; }
        public float StatMin { get; private set; }
        public float BellhopMean { get; private set; }
        public float Median { get; private set; }
        public float Variance { get; private set; }
        public float StandardDeviation { get; private set; }
        public float[,] TransmissionLoss { get; private set; }
        public bool IsSaved { get; private set; }
        public List<float> Depths { get; internal set; }
        public List<float> Ranges { get; internal set; }
        public List<float> Minimum { get; internal set; }
        public List<float> Maximum { get; internal set; }
        public List<float> Mean { get; internal set; }


        public float this[int depthCell, int rangeCell]
        {
            get
            {
                if (TransmissionLoss == null) throw new ApplicationException("TransmissionLossRadialData: Indexing is invalid unless all data is in memory.  Try ReadSingleValue(...) instead.");
                if ((depthCell >= Depths.Count) || (rangeCell >= Ranges.Count)) throw new IndexOutOfRangeException("TransmissionLossRadialData: Requested DepthCell or RangeCell out of valid range");
                return TransmissionLoss[depthCell, rangeCell];
            }
        }

        public float this[double range, double depth]
        {
            get
            {
                if (range > Ranges.Last()) throw new IndexOutOfRangeException("TransmissionLossRadialData: Requested range is past the end of the radial");
                var rangeIndex = (from r in Ranges
                                  orderby Math.Abs(range - r)
                                  select Ranges.IndexOf(r)).First();
                var depthIndex = Depths.Count - 1;
                if (depth < Depths.Last())
                    depthIndex = (from d in Depths
                                  orderby Math.Abs(depth - d)
                                  select Depths.IndexOf(d)).First();
                return TransmissionLoss[depthIndex, rangeIndex];
            }
        }

        #region IComparable<TransmissionLossRadial> Members

        public int CompareTo(TransmissionLossRadial other) { return BearingFromSource.CompareTo(other.BearingFromSource); }

        #endregion

    }

    public static class TransmissionLossRadialExtensions
    {
        public static int[] RenderToPixelBuffer(this TransmissionLossRadial transmissionLossRadial, Func<float, Color> valueToColorFunc, int width = 0, int height = 0)
        {
            if (width < 0) throw new ArgumentOutOfRangeException("width", "width must be non-negative");
            if (height < 0) throw new ArgumentOutOfRangeException("height", "height must be non-negative");

            if (width == 0) width = transmissionLossRadial.Ranges.Count;
            if (height == 0) height = transmissionLossRadial.Depths.Count;

            var buffer = new int[width * height];
            var rangeStep = transmissionLossRadial.Ranges.Count / (float)width;
            var depthStep = transmissionLossRadial.Depths.Count / (float)height;
#if false
            var yValues = Enumerable.Range(0, height).AsParallel();
            yValues.ForAll(y =>
            {
                var curOffset = y * width;
                for (var x = 0; x < width; x++)
                {
                    var curColor = valueToColorFunc(transmissionLossRadial[(int)(y * depthStep), (int)(x * rangeStep)]);
                    buffer[curOffset++] = ((curColor.A << 24) | (curColor.R << 16) | (curColor.G << 8) | (curColor.B));
                }
            });
#else
            for (var y = 0; y < height; y++)
            {
                var curOffset = y * width;
                for (var x = 0; x < width; x++)
                {
                    var curColor = valueToColorFunc(transmissionLossRadial[(int)(y * depthStep), (int)(x * rangeStep)]);
                    buffer[curOffset++] = ((curColor.A << 24) | (curColor.R << 16) | (curColor.G << 8) | (curColor.B));
                }
            }
#endif
            return buffer;
        }
    }
#endif
}