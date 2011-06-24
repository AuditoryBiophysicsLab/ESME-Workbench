using System;
using System.Collections.Generic;
using System.Linq;

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
            Depth,
            Minimum,
            Maximum,
            Mean,
        }

        delegate float ValueSelector(List<float> values, int index);

        public TransmissionLossFieldSlice(TransmissionLossField transmissionLossField, RadialLookupInfo[,] lookupInfo, SliceType sliceType, int depthIndex = 0)
        {
            _rangeCellCount = transmissionLossField.Radials[0].Ranges.Count;
            _radialSlices = new List<TransmissionLossRadialSlice>();
            foreach (var radial in transmissionLossField.Radials)
                _radialSlices.Add(new TransmissionLossRadialSlice { Bearing = radial.BearingFromSource, Values = new float[_rangeCellCount] });

            _radialLookupInfo = lookupInfo;
            SliceData = new float[lookupInfo.GetLength(0), lookupInfo.GetLength(1)];

            ValueSelector selector;
            switch (sliceType)
            {
                case SliceType.Minimum:
                    selector = (values, index) => { return values.Min(); };
                    break;
                case SliceType.Maximum:
                    selector = (values, index) => { return values.Max(); };
                    break;
                case SliceType.Mean:
                    selector = (values, index) => { return values.Average(); };
                    break;
                case SliceType.Depth:
                    selector = (values, index) => { return values[index]; };
                    break;
                default:
                    throw new ApplicationException("unknown slice type");
            }
            for (var i = 0; i < transmissionLossField.Radials.Count; i++)
            {
                var radial = transmissionLossField.Radials[i];
                var radialSlice = _radialSlices[i];
                for (var j = 0; j < radial.Ranges.Count; j++)
                    radialSlice.Values[j] = selector(radial[j], depthIndex);
            }

            var radialCount = _radialSlices.Count;
            for (var x = 0; x < SliceData.GetLength(0); x++)
                for (var y = 0; y < SliceData.GetLength(1); y++)
                {
                    var sourceRadial = _radialLookupInfo[x, y].SourceRadialIndex;
                    var rangeIndex = _radialLookupInfo[x, y].RangeIndex;
                    SliceData[x, y] = 0;
                    if ((sourceRadial < radialCount) && (rangeIndex < _rangeCellCount))
                        SliceData[x, y] = _radialSlices[(int)sourceRadial].Values[rangeIndex];
                }
        }

        public float[,] SliceData { get; private set; }

        readonly List<TransmissionLossRadialSlice> _radialSlices;
        readonly int _rangeCellCount;
        readonly RadialLookupInfo[,] _radialLookupInfo;
    }

    public class RadialLookupInfo
    {
        public uint RangeIndex { get; internal set; }
        public uint SourceRadialIndex { get; internal set; }

        #region Static helpers
        public static RadialLookupInfo[,] Create(TransmissionLossField transmissionLossField, int maxDisplaySize)
        {
            if (maxDisplaySize < 10) throw new ApplicationException("maxDisplaySize too small");

            var result = new RadialLookupInfo[maxDisplaySize, maxDisplaySize];
            var rangeCellCount = transmissionLossField.Radials[0].Ranges.Count;
            var radius = transmissionLossField.Radius;
            var halfDisplaySize = maxDisplaySize / 2;

            for (var i = 0; i < maxDisplaySize; i++)
                for (var j = 0; j < maxDisplaySize; j++)
                {
                    result[i, j] = new RadialLookupInfo();
                    const double radiansToDegrees = 180.0 / Math.PI;

                    var xCoord = i - halfDisplaySize;
                    var yCoord = j - halfDisplaySize;
                    var bearingDegrees = Math.Atan2(xCoord, yCoord) * radiansToDegrees;
                    if (bearingDegrees < 0) bearingDegrees += 360;
                    bearingDegrees %= 360;
                    var rangeCellStepSize = (float)rangeCellCount / halfDisplaySize;

                    var range = (uint)Math.Round(Math.Sqrt((xCoord * xCoord) + (yCoord * yCoord)));

                    // Set default values for the weight array
                    result[i, j].RangeIndex = (uint)Math.Round(range * rangeCellStepSize);
                    result[i, j].SourceRadialIndex = uint.MaxValue;
                    var radialCount = transmissionLossField.Radials.Count();
                    // The current point is not out of the sonar beam, unless it's beyond the max range we're computing
                    for (uint radialIndex = 0; radialIndex < radialCount; radialIndex++)
                    {
                        var startRadialIndex = radialIndex;
                        uint endRadialIndex = 0;
                        if (radialIndex < (radialCount - 1)) endRadialIndex = radialIndex + 1;
                        var startBearing = transmissionLossField.Radials[(int)startRadialIndex].BearingFromSource;
                        var endBearing = transmissionLossField.Radials[(int)endRadialIndex].BearingFromSource;
                        if (!IsWithinArcEndpoints(bearingDegrees, startBearing, endBearing)) continue;
                        if (range < radius)
                        {
                            double transectAngleDegrees = AngularDistance(startBearing, endBearing);
                            var curBearingPercentAngularDistance = AngularDistance(startBearing, bearingDegrees) / transectAngleDegrees;
                            result[i, j].SourceRadialIndex = curBearingPercentAngularDistance <= 0.5 ? startRadialIndex : endRadialIndex;
                            result[i, j].RangeIndex = range;
                            break;
                        } // if (Range < MaxRange)
                        // If it's out of the sonar beam because the range to source is greater than max range
                        result[i, j].RangeIndex = range;
                        result[i, j].SourceRadialIndex = uint.MaxValue;
                        break;
                    } // for (int TransectNumber = 0; TransectNumber < (soundSource.TransectNumber - 1); TransectNumber++)
                }

            return result;
        }

        // This function ONLY works for arcs less than 180 degrees.  Arcs are assumed to run from ArcStart_Degrees to ArcEnd_Degrees
        // by the shortest route around the circle.
        static bool IsWithinArcEndpoints(double bearing, double arcStart, double arcEnd)
        {
            if (arcStart > arcEnd)
            {
                // The arc contains the discontinuity at 360/0 degrees
                var angleDelta = 360 - arcStart;
                arcStart = 0;
                arcEnd += angleDelta;
                bearing += angleDelta;
                bearing %= 360;
            }

            return (arcStart <= bearing) && (bearing <= arcEnd);
        }

        static float AngularDistance(double angle1, double angle2)
        {
            var result = (float)(angle2 - angle1);

            if (result < -180.0)
                result += 360;
            if (result > 180.0)
                result -= 360;
            return result;
        }
        #endregion
    }
}
