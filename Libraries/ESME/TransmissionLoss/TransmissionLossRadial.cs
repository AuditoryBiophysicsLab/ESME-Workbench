using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossRadial : IComparable<TransmissionLossRadial>
    {
        public float BearingFromSource_degrees { get; private set; }
        public float DataMax { get; private set; }
        public float DataMin { get; private set; }
        public float StatMax { get; private set; }
        public float StatMin { get; private set; }
        public float Mean { get; private set; }
        public float Median { get; private set; }
        public float Variance { get; private set; }
        public float StandardDeviation { get; private set; }
        public float[,] TransmissionLoss_dBSPL { get; private set; }
        public bool IsSaved { get; private set; }
        public float[] Depths_meters { get; private set; }
        public float[] Ranges_meters { get; private set; }

        private const UInt32 _Magic = 0xe6763c72;
        private long mSeekOffset = -1;
        private int mDepths, mRanges;
        private long mRangeStride;

        public TransmissionLossRadial(float BearingFromSource_degrees, Bellhop.BellhopOutput BellhopOutput)
        {
            this.BearingFromSource_degrees = BearingFromSource_degrees;
            this.DataMax = BellhopOutput.DataMax;
            this.DataMin = BellhopOutput.DataMin;
            this.StatMax = BellhopOutput.StatMax;
            this.StatMin = BellhopOutput.StatMin;
            this.Mean = BellhopOutput.Mean;
            this.Median = BellhopOutput.Median;
            this.Variance = BellhopOutput.Variance;
            this.StandardDeviation = BellhopOutput.StandardDeviation;

            this.TransmissionLoss_dBSPL = BellhopOutput.TransmissionLoss_dBSPL;

            mDepths = TransmissionLoss_dBSPL.GetLength(0);
            mRanges = TransmissionLoss_dBSPL.GetLength(1);
            mRangeStride = sizeof(float) * mRanges;

            Depths_meters = new float[BellhopOutput.ReceiverDepths_meters.Length];
            Array.Copy(BellhopOutput.ReceiverDepths_meters, Depths_meters, BellhopOutput.ReceiverDepths_meters.Length);
            
            Ranges_meters = new float[BellhopOutput.ReceiverRanges_meters.Length];
            Array.Copy(BellhopOutput.ReceiverRanges_meters, Ranges_meters, BellhopOutput.ReceiverRanges_meters.Length);
            
            IsSaved = false;
        }

        public void ClearAxisData()
        {
            Depths_meters = Ranges_meters = null;
        }

        public TransmissionLossRadial(BinaryReader stream, bool HeaderOnly)
        {
            if (mSeekOffset == -1)
                mSeekOffset = stream.BaseStream.Seek(0, SeekOrigin.Current);
            else
                stream.BaseStream.Seek(mSeekOffset, SeekOrigin.Begin);
            if (stream.ReadUInt32() != _Magic)
                throw new FileFormatException("Attempted to read invalid data into a TransmissionLossRadialData object");
            BearingFromSource_degrees = stream.ReadSingle();
            DataMax = stream.ReadSingle();
            DataMin = stream.ReadSingle();
            StatMax = stream.ReadSingle();
            StatMin = stream.ReadSingle();
            Mean = stream.ReadSingle();
            Median = stream.ReadSingle();
            Variance = stream.ReadSingle();
            StandardDeviation = stream.ReadSingle();
            mDepths = stream.ReadInt32();
            mRanges = stream.ReadInt32();
            mRangeStride = sizeof(float) * mRanges;
            TransmissionLoss_dBSPL = null;
            IsSaved = true;
            TransmissionLoss_dBSPL = new float[mDepths, mRanges];
            ClearAxisData();
            for (int i = 0; i < mDepths; i++)       // Depths
                for (int j = 0; j < mRanges; j++)   // Ranges
                    TransmissionLoss_dBSPL[i, j] = stream.ReadSingle();
        }

        public float this[int DepthCell, int RangeCell]
        {
            get
            {
                if (TransmissionLoss_dBSPL == null)
                    throw new ApplicationException("TransmissionLossRadialData: Indexing is invalid unless all data is in memory.  Try ReadSingleValue(...) instead.");
                if ((DepthCell >= mDepths) || (RangeCell >= mRanges))
                    throw new IndexOutOfRangeException("TransmissionLossRadialData: Requested DepthCell or RangeCell out of valid range");
                return TransmissionLoss_dBSPL[DepthCell, RangeCell];
            }
        }

        public float ReadSingleValue(BinaryReader stream, int DepthCell, int RangeCell)
        {
            if ((stream == null) || (!stream.BaseStream.CanSeek))
                throw new ArgumentException("TransmissionLossRadialData: Attempted to seek on an invalid stream");
            if ((DepthCell >= mDepths) || (RangeCell >= mRanges))
                throw new ArgumentException("TransmissionLossRadialData: Attempted to seek to data indices out of their valid ranges");

            long SeekOffset;

            // Skip past the header information (9 floats [statistics] and 2 ints [depth count and range count])
            SeekOffset = mSeekOffset + (sizeof(float) * 9) + (sizeof(int) * 2);
            // Compute the offset to the desired depth and range cell 
            // offset = (Depth Cell * Stride of one Range) + RangeCell) * sizeof(float)
            SeekOffset += (DepthCell * mRangeStride) + (RangeCell * sizeof(float));
            stream.BaseStream.Seek(SeekOffset, SeekOrigin.Begin);
            return stream.ReadSingle();
        }

        public void Save(BinaryWriter stream, bool DiscardDataArrayAfterSave)
        {
            if (IsSaved)
                return;
            
            if (TransmissionLoss_dBSPL == null)
                throw new ApplicationException("TransmissionLossRadialData: Save() cannot be called unless all data is in memory.");

            // Always write a new radial on the end of the current file
            mSeekOffset = stream.BaseStream.Seek(0, SeekOrigin.End);  
            stream.Write(_Magic);
            stream.Write(BearingFromSource_degrees);
            stream.Write(DataMax);
            stream.Write(DataMin);
            stream.Write(StatMax);
            stream.Write(StatMin);
            stream.Write(Mean);
            stream.Write(Median);
            stream.Write(Variance);
            stream.Write(StandardDeviation);
            stream.Write(mDepths);
            stream.Write(mRanges);
            for (int i = 0; i < mDepths; i++)       // Depths
                for (int j = 0; j < mRanges; j++)   // Ranges
                    stream.Write(TransmissionLoss_dBSPL[i, j]);
            if (DiscardDataArrayAfterSave)
                TransmissionLoss_dBSPL = null;
            IsSaved = true;
        }

        public int CompareTo(TransmissionLossRadial Other)
        {
            return BearingFromSource_degrees.CompareTo(Other.BearingFromSource_degrees);
        }
    }
}
