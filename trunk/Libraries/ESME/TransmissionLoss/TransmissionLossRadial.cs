﻿using System;
using System.Collections.Generic;
using System.IO;
using ESME.TransmissionLoss.Bellhop;
using HRC.Navigation;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossRadial : IComparable<TransmissionLossRadial>
    {
        const UInt32 Magic = 0xe6763c72;
        readonly long _rangeStride;
        long _seekOffset = -1;

        public TransmissionLossRadial(float bearingFromSource, BellhopOutput bellhopOutput)
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

            _rangeStride = sizeof(float) * TransmissionLoss.GetLength(1);

            Depths = new List<float>(bellhopOutput.ReceiverDepths);
            Ranges = new List<float>(bellhopOutput.ReceiverRanges);

            IsSaved = false;
        }
        
        public TransmissionLossRadial(float bearingFromSource, float[,] pressureField,float sourceLevel)
        {
            BearingFromSource = bearingFromSource;

            var tl = new float[pressureField.GetLength(0),pressureField.GetLength(1)];
            var max = float.MinValue;
            var min = float.MaxValue;
            for (var i = 0; i < pressureField.GetLength(0); i++)
            {
                for (var j = 0; j < pressureField.GetLength(1); j++)
                {
                    tl[i, j] = sourceLevel - pressureField[i, j];
                    max = Math.Max(tl[i, j], max);
                    min = Math.Min(tl[i, j], min);
                }
            }
            TransmissionLoss = tl;
            DataMax = sourceLevel;
            StatMax = sourceLevel;
            DataMin = min;
            StatMin = min;
        }
        public TransmissionLossRadial(float bearingFromSource, float[,] pressureField, IEnumerable<float> depths, IEnumerable<float> ranges, float sourceLevel)
            : this(bearingFromSource, pressureField, sourceLevel)
        {
            Depths = new List<float>(depths);
            Ranges = new List<float>(ranges);
        }

        public TransmissionLossRadial() {  }
        
        public TransmissionLossRadial(BinaryReader stream)
        {
            if (_seekOffset == -1) _seekOffset = stream.BaseStream.Seek(0, SeekOrigin.Current);
            else stream.BaseStream.Seek(_seekOffset, SeekOrigin.Begin);
            if (stream.ReadUInt32() != Magic) throw new FileFormatException("Attempted to read invalid data into a TransmissionLossRadialData object");
            BearingFromSource = stream.ReadSingle();
            DataMax = stream.ReadSingle();
            DataMin = stream.ReadSingle();
            StatMax = stream.ReadSingle();
            StatMin = stream.ReadSingle();
            BellhopMean = stream.ReadSingle();
            Median = stream.ReadSingle();
            Variance = stream.ReadSingle();
            StandardDeviation = stream.ReadSingle();
            var depthCount = stream.ReadInt32();
            var rangeCount = stream.ReadInt32();
            _rangeStride = sizeof (float)*rangeCount;
            TransmissionLoss = null;
            IsSaved = true;
            TransmissionLoss = new float[depthCount,rangeCount];
            Minimum = new List<float>();
            Maximum = new List<float>();
            Mean = new List<float>();
            for (var i = 0; i < rangeCount; i++)
            {
                Minimum.Add(float.MaxValue);
                Maximum.Add(float.MinValue);
                Mean.Add(0);
            }
            ClearAxisData();
            for (var i = 0; i < depthCount; i++) // Depths
                for (var j = 0; j < rangeCount; j++) // Ranges
                {
                    TransmissionLoss[i, j] = stream.ReadSingle();
                    Minimum[j] = Math.Min(Minimum[j], TransmissionLoss[i, j]);
                    Maximum[j] = Math.Max(Maximum[j], TransmissionLoss[i, j]);
                    Mean[j] += TransmissionLoss[i, j];
                }
            for (var i = 0; i < rangeCount; i++)
            {
                Mean[i] /= depthCount;
            }
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

        #region IComparable<TransmissionLossRadial> Members

        public int CompareTo(TransmissionLossRadial other) { return BearingFromSource.CompareTo(other.BearingFromSource); }

        #endregion

        public void ClearAxisData() { Depths = Ranges = null; }

        public float ReadSingleValue(BinaryReader stream, int depthCell, int rangeCell)
        {
            if ((stream == null) || (!stream.BaseStream.CanSeek)) throw new ArgumentException("TransmissionLossRadialData: Attempted to seek on an invalid stream");
            if ((depthCell >= Depths.Count) || (rangeCell >= Ranges.Count)) throw new ArgumentException("TransmissionLossRadialData: Attempted to seek to data indices out of their valid ranges");

            // Skip past the header information (9 floats [statistics] and 2 ints [depth count and range count])
            long seekOffset = _seekOffset + (sizeof (float)*9) + (sizeof (int)*2);
            // Compute the offset to the desired depth and range cell 
            // offset = (Depth Cell * Stride of one Range) + RangeCell) * sizeof(float)
            seekOffset += (depthCell*_rangeStride) + (rangeCell*sizeof (float));
            stream.BaseStream.Seek(seekOffset, SeekOrigin.Begin);
            return stream.ReadSingle();
        }

        public void Save(BinaryWriter stream)
        {
            if (IsSaved) return;

            if (TransmissionLoss == null) throw new ApplicationException("TransmissionLossRadialData: Save() cannot be called unless all data is in memory.");

            // Always write a new radial on the end of the current file
            _seekOffset = stream.BaseStream.Seek(0, SeekOrigin.End);
            stream.Write(Magic);
            stream.Write(BearingFromSource);
            stream.Write(DataMax);
            stream.Write(DataMin);
            stream.Write(StatMax);
            stream.Write(StatMin);
            stream.Write(BellhopMean);
            stream.Write(Median);
            stream.Write(Variance);
            stream.Write(StandardDeviation);
            stream.Write(Depths.Count);
            stream.Write(Ranges.Count);
            for (var i = 0; i < Depths.Count; i++) // Depths
                for (var j = 0; j < Ranges.Count; j++) // Ranges
                    stream.Write(TransmissionLoss[i, j]);
            TransmissionLoss = null;
            IsSaved = true;
        }

        public void SaveAsCSV(string fileName, TransmissionLossField transmissionLossField)
        {
            using (var sw = new StreamWriter(fileName))
            {
                // Write the X axis values out first
                sw.WriteLine("Vertical Transmission Loss (dB)");
                sw.Write(",Range (m),");

                foreach (var t in Ranges) sw.Write(t + ","); //write out the X axis values.
                sw.WriteLine(); // Terminate the line
                sw.WriteLine("Depth (m)");
                // Write the slice data
                for (var i = 0; i < Depths.Count; i++)
                {
                    // Write out the Y axis value
                    sw.Write(Depths[i] + ",,");
                    for (var j = 0; j < Ranges.Count; j++)
                        sw.Write(TransmissionLoss[i, j] + ","); 
                    sw.WriteLine(); // Terminate the line
                } // for i
                sw.WriteLine();
                //sw.Write(",Bottom depth:,");
                //for (var i = 0; i < Ranges.Length; i++)
                //    sw.Write(bottomProfile.Profile[i].ToString() + ",");
                sw.WriteLine();
                sw.WriteLine();
                sw.WriteLine("Sound Source information");
                sw.WriteLine("Source Latitude," + transmissionLossField.Latitude);
                sw.WriteLine("Source Longitude," + transmissionLossField.Longitude);
                sw.WriteLine("Depth (m)," + transmissionLossField.SourceDepth);
                sw.WriteLine("High Frequency (Hz)," + transmissionLossField.HighFrequency);
                sw.WriteLine("Low Frequency (Hz)," + transmissionLossField.LowFrequency);
                sw.WriteLine("Depression/elevation angle (deg)" + transmissionLossField.DepressionElevationAngle);
                sw.WriteLine("Vertical beam width (deg)" + transmissionLossField.VerticalBeamWidth);
                var radialEnd = new EarthCoordinate(transmissionLossField.Latitude, transmissionLossField.Longitude);
                radialEnd.Move(BearingFromSource, transmissionLossField.Radius);
                sw.WriteLine("Receiver Latitude," + radialEnd.Latitude);
                sw.WriteLine("Receiver Longitude," + radialEnd.Longitude);
                sw.WriteLine();
            } // using sw
        }
    }
}