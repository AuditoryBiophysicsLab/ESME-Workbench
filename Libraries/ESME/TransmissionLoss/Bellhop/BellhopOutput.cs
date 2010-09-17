using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;

namespace ESME.TransmissionLoss.Bellhop
{
    public class BellhopOutput
    {
        public BellhopOutput(string bellhopFilename)
        {
            using (var reader = new BinaryReader(new FileStream(bellhopFilename, FileMode.Open, FileAccess.Read)))
            {
                var recordLength = reader.ReadInt32()*4;
                Title = new string(reader.ReadChars(80)).Trim();
                reader.BaseStream.Seek(recordLength, SeekOrigin.Begin);
                PlotType = new string(reader.ReadChars(10));
                Xs = reader.ReadSingle();
                Ys = reader.ReadSingle();
                reader.BaseStream.Seek(2*recordLength, SeekOrigin.Begin);
                Frequency = reader.ReadSingle();
                //Theta = new float[reader.ReadInt32()];
                var thetaCount = reader.ReadInt32();
                SourceDepths = new float[reader.ReadInt32()];
                ReceiverDepths = new float[reader.ReadInt32()];
                ReceiverRanges = new float[reader.ReadInt32()];
                TransmissionLoss = new float[ReceiverDepths.Length,ReceiverRanges.Length];

                reader.BaseStream.Seek(3*recordLength, SeekOrigin.Begin);
                for (var curTheta = 0; curTheta < thetaCount; curTheta++) /*Theta[CurTheta] = */ reader.ReadSingle();

                reader.BaseStream.Seek(4*recordLength, SeekOrigin.Begin);
                for (var source = 0; source < SourceDepths.Length; source++) SourceDepths[source] = reader.ReadSingle();

                reader.BaseStream.Seek(5*recordLength, SeekOrigin.Begin);
                for (var depth = 0; depth < ReceiverDepths.Length; depth++) ReceiverDepths[depth] = reader.ReadSingle();

                reader.BaseStream.Seek(6*recordLength, SeekOrigin.Begin);
                int range;
                for (range = 0; range < ReceiverRanges.Length; range++) ReceiverRanges[range] = reader.ReadSingle();

                for (var curTheta = 0; curTheta < thetaCount; curTheta++)
                {
                    for (var source = 0; source < SourceDepths.Length; source++)
                    {
                        for (var depth = 0; depth < ReceiverDepths.Length; depth++)
                        {
                            var recordNumber = 7 + (curTheta*SourceDepths.Length*ReceiverDepths.Length) + (source*ReceiverDepths.Length) + depth;
                            reader.BaseStream.Seek(recordNumber*recordLength, SeekOrigin.Begin);
                            for (range = 0; range < ReceiverRanges.Length; range++)
                            {
                                var real = reader.ReadSingle();
                                var imag = reader.ReadSingle();
                                if (double.IsNaN(real)) real = 0;
                                if (double.IsNaN(imag)) imag = 0;
                                if (source == 0) // Currently we only support a single source with this code
                                    TransmissionLoss[depth, range] = (float) Math.Abs(Math.Sqrt((real*real) + (imag*imag)));
                            } // for Range
                        } // for Depth
                    } // for Source
                } // for CurTheta
            } // using
            DataMin = StatMin = float.MaxValue;
            DataMax = StatMax = float.MinValue;
            ProcessRawData();
        }

        public string Title { get; private set; }
        public string PlotType { get; private set; }
        public float Xs { get; private set; }
        public float Ys { get; private set; }
        public float Frequency { get; private set; }
        public float[] SourceDepths { get; private set; }
        public float[] ReceiverDepths { get; private set; }
        public float[] ReceiverRanges { get; private set; }
        public float[,] TransmissionLoss { get; private set; }
        public float DataMax { get; private set; }
        public float DataMin { get; private set; }
        public float StatMax { get; private set; }
        public float StatMin { get; private set; }
        public float Mean { get; private set; }
        public float Median { get; private set; }
        public float Variance { get; private set; }
        public float StandardDeviation { get; private set; }

        void ProcessRawData()
        {
            float curData;
            float total = 0;
            var statValues = new List<float>();

            for (var depth = 0; depth < ReceiverDepths.Length; depth++)
            {
                for (var range = 0; range < ReceiverRanges.Length; range++)
                {
                    curData = TransmissionLoss[ReceiverDepths.Length - depth - 1, range];
                    if (double.IsNaN(curData)) 
                        Debug.WriteLine("NaN!");

                    curData = (float) (-20*Math.Log10(Math.Max(curData, 1e-10)));

                    DataMin = Math.Min(curData, DataMin);
                    DataMax = Math.Max(curData, DataMax);
                    if (curData <= 120)
                    {
                        total += curData;
                        statValues.Add(curData);
                    }
                    TransmissionLoss[ReceiverDepths.Length - depth - 1, range] = curData;
                } // for (Range)
            } // for (Receiver)
            if (statValues.Count > 0)
            {
                statValues.Sort();
                Median = statValues[statValues.Count/2];
                Mean = statValues.Average();
                total = 0;
                foreach (var t in statValues) 
                {
                    curData = t - Mean;
                    curData *= curData;
                    total += curData;
                }
                Variance = total/statValues.Count;
                StandardDeviation = (float) Math.Sqrt(Variance);
                StatMax = (float) Math.Round(Median + (0.75*StandardDeviation));
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