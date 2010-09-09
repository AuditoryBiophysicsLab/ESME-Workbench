using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace ESME.TransmissionLoss.Bellhop
{
    public class BellhopOutput
    {
        public string Title { get; private set; }
        public string PlotType { get; private set; }
        public float Xs { get; private set; }
        public float Ys { get; private set; }
        public float Frequency_Hz { get; private set; }
        //public float[] Theta { get; private set; }
        public float[] SourceDepths_meters { get; private set; }
        public float[] ReceiverDepths_meters { get; private set; }
        public float[] ReceiverRanges_meters { get; private set; }
        public float[,] TransmissionLoss_dBSPL { get; private set; }
        public float DataMax { get; private set; }
        public float DataMin { get; private set; }
        public float StatMax { get; private set; }
        public float StatMin { get; private set; }
        public float Mean { get; private set; }
        public float Median { get; private set; }
        public float Variance { get; private set; }
        public float StandardDeviation { get; private set; }

        public BellhopOutput(string BellhopFilename)
        {
            int RecordNumber, RecordLength, CurTheta, Source, Range, Depth;
            int ThetaCount;
            double Real, Imag;

            using (BinaryReader reader = new BinaryReader(new FileStream(BellhopFilename, FileMode.Open, FileAccess.Read)))
            {
                RecordLength = reader.ReadInt32() * 4;
                Title = new string(reader.ReadChars(80)).Trim();
                reader.BaseStream.Seek(RecordLength, SeekOrigin.Begin);
                PlotType = new string(reader.ReadChars(10));
                Xs = reader.ReadSingle();
                Ys = reader.ReadSingle();
                reader.BaseStream.Seek(2 * RecordLength, SeekOrigin.Begin);
                Frequency_Hz = reader.ReadSingle();
                //Theta = new float[reader.ReadInt32()];
                ThetaCount = reader.ReadInt32();
                SourceDepths_meters = new float[reader.ReadInt32()];
                ReceiverDepths_meters = new float[reader.ReadInt32()];
                ReceiverRanges_meters = new float[reader.ReadInt32()];
                TransmissionLoss_dBSPL = new float[ReceiverDepths_meters.Length, ReceiverRanges_meters.Length];

                reader.BaseStream.Seek(3 * RecordLength, SeekOrigin.Begin);
                for (CurTheta = 0; CurTheta < ThetaCount; CurTheta++)
                    /*Theta[CurTheta] = */
                    reader.ReadSingle();

                reader.BaseStream.Seek(4 * RecordLength, SeekOrigin.Begin);
                for (Source = 0; Source < SourceDepths_meters.Length; Source++)
                    SourceDepths_meters[Source] = reader.ReadSingle();

                reader.BaseStream.Seek(5 * RecordLength, SeekOrigin.Begin);
                for (Depth = 0; Depth < ReceiverDepths_meters.Length; Depth++)
                    ReceiverDepths_meters[Depth] = reader.ReadSingle();

                reader.BaseStream.Seek(6 * RecordLength, SeekOrigin.Begin);
                for (Range = 0; Range < ReceiverRanges_meters.Length; Range++)
                    ReceiverRanges_meters[Range] = reader.ReadSingle();

                for (CurTheta = 0; CurTheta < ThetaCount; CurTheta++)
                {
                    for (Source = 0; Source < SourceDepths_meters.Length; Source++)
                    {
                        for (Depth = 0; Depth < ReceiverDepths_meters.Length; Depth++)
                        {
                            RecordNumber = 7 + (CurTheta * SourceDepths_meters.Length * ReceiverDepths_meters.Length) +
                                (Source * ReceiverDepths_meters.Length) + Depth;
                            reader.BaseStream.Seek(RecordNumber * RecordLength, SeekOrigin.Begin);
                            for (Range = 0; Range < ReceiverRanges_meters.Length; Range++)
                            {
                                Real = reader.ReadSingle();
                                Imag = reader.ReadSingle();
                                if (double.IsNaN(Real)) 
                                    Real = 0;
                                if (double.IsNaN(Imag)) 
                                    Imag = 0;
                                if (Source == 0)    // Currently we only support a single source with this code
                                    TransmissionLoss_dBSPL[Depth, Range] = (float)Math.Abs(Math.Sqrt((Real * Real) + (Imag * Imag)));
                            } // for Range
                        } // for Depth
                    } // for Source
                } // for CurTheta
            } // using
            DataMin = StatMin = float.MaxValue;
            DataMax = StatMax = float.MinValue;
            ProcessRawData();
        }

        private void ProcessRawData()
        {
            int Range, Depth;
            float CurData;
            float total = 0;
            List<float> StatValues = new List<float>();

            for (Depth = 0; Depth < ReceiverDepths_meters.Length; Depth++)
            {
                for (Range = 0; Range < ReceiverRanges_meters.Length; Range++)
                {
                    //if (Range == (ReceiverRanges_meters.Length - 1))
                    //    System.Diagnostics.Debugger.Break();

                    CurData = TransmissionLoss_dBSPL[ReceiverDepths_meters.Length - Depth - 1, Range];
                    if (double.IsNaN(CurData))
                        System.Diagnostics.Debug.WriteLine("NaN!");

                    CurData = (float)(-20 * Math.Log10(Math.Max(CurData, 1e-10)));

                    DataMin = Math.Min(CurData, DataMin);
                    DataMax = Math.Max(CurData, DataMax);
                    if (CurData <= 120)
                    {
                        total += CurData;
                        StatValues.Add(CurData);
                    }
                    TransmissionLoss_dBSPL[ReceiverDepths_meters.Length - Depth - 1, Range] = CurData;

                } // for (Range)
            } // for (Receiver)
            if (StatValues.Count > 0)
            {
                StatValues.Sort();
                Median = StatValues[StatValues.Count / 2];
                Mean = StatValues.Average();
                total = 0;
                for (int i = 0; i < StatValues.Count; i++)
                {
                    CurData = StatValues[i] - Mean;
                    CurData *= CurData;
                    total += CurData;
                }
                Variance = total / StatValues.Count;
                StandardDeviation = (float)Math.Sqrt(Variance);
                StatMax = (float)Math.Round(Median + (0.75 * StandardDeviation));
                StatMin = StatMax - 50;
            }
            else
            {
                DataMin -= 1;
                DataMax += 1;
                StatMin = DataMin;
                StatMax = DataMax;
            }
        }
    }
}
