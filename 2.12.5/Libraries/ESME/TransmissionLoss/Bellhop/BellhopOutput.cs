using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Threading;

namespace ESME.TransmissionLoss.Bellhop
{
    public class BellhopOutput
    {
        public BellhopOutput(string bellhopFilename)
        {
            var retry = 20;
            while (!File.Exists(bellhopFilename) && retry > 0)
            {
                Thread.Sleep(50);
                retry--;
            }
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
                for (var curTheta = 0; curTheta < thetaCount; curTheta++) reader.ReadSingle();

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
                                    TransmissionLoss[depth, range] = (float)Math.Abs(Math.Sqrt((real * real) + (imag * imag)));
                                else 
                                    TransmissionLoss[depth, range] = float.NaN;
                                if (TransmissionLoss[depth, range] < 0) Debugger.Break();
                            } // for Range
                        } // for Depth
                    } // for Source
                } // for CurTheta
            } // using
            DataMin = StatMin = float.MaxValue;
            DataMax = StatMax = float.MinValue;
            ProcessRawData();
        }

        public static void WriteShadeFile(string fileName, double sourceDepth, double frequency, double[] receiverDepths, double[] receiverRanges, List<Complex[]> pressures)
        {
            var rangeCount = pressures.Count;
            var depthCount = pressures[0].Length;
            if (receiverRanges.Length != rangeCount) throw new ArgumentOutOfRangeException("receiverRanges", "receiverRanges.Length must equal pressures.Count");
            if (receiverDepths.Length != depthCount) throw new ArgumentOutOfRangeException("receiverDepths", "receiverDepths.Length must equal pressures[0].Length");
            var recordLength = rangeCount * 8;
            using (var writer = new BinaryWriter(new FileStream(fileName, FileMode.Create, FileAccess.Write)))
            {
                writer.Write(recordLength / 4);
                writer.Write(new string(' ', 80).ToCharArray());
                writer.BaseStream.Seek(recordLength, SeekOrigin.Begin);
                writer.Write(new string(' ', 10).ToCharArray());
                writer.Write(0.0f); // Xs
                writer.Write(0.0f); // Ys
                writer.BaseStream.Seek(2 * recordLength, SeekOrigin.Begin);
                writer.Write((float)frequency);
                writer.Write(1); // thetaCount
                writer.Write(1); // sourceDepths
                writer.Write(depthCount);
                writer.Write(rangeCount);
                // Skipping over the theta record, we don't use it in ESME
                // writer.BaseStream.Seek(3 * recordLength, SeekOrigin.Begin);
                writer.BaseStream.Seek(4 * recordLength, SeekOrigin.Begin);
                writer.Write((float)sourceDepth); // depth of the single source
                writer.BaseStream.Seek(5 * recordLength, SeekOrigin.Begin);
                foreach (var depth in receiverDepths) writer.Write((float)depth);
                writer.BaseStream.Seek(6 * recordLength, SeekOrigin.Begin);
                foreach (var depth in receiverRanges) writer.Write((float)depth);
                foreach (var range in receiverRanges) writer.Write((float)range);
                for (var depthIndex = 0; depthIndex < depthCount; depthIndex++)
                {
                    writer.BaseStream.Seek((7 + depthIndex) * recordLength, SeekOrigin.Begin);
                    for (var rangeIndex = 0; rangeIndex < rangeCount; rangeIndex++)
                    {
                        var curPressure = pressures[rangeIndex][depthIndex];
                        writer.Write((float)curPressure.Real);
                        writer.Write((float)curPressure.Imaginary);
                    }
                }
            }
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
                    var originalData = TransmissionLoss[depth, range];
                    if (double.IsNaN(originalData)) Debugger.Break();
                    //curData = (float)(-20 * Math.Log10(Math.Max(originalData, 1e-10)));
                    curData = (float)(-20 * Math.Log10(originalData));
                    // This is where we're ending up with negative TL values.  Don't think that should happen...
                    //if (curData < 0) Debugger.Break();
                    if (!double.IsInfinity(curData))
                    {
                        DataMin = Math.Min(curData, DataMin);
                        DataMax = Math.Max(curData, DataMax);
                        if (curData <= 120)
                        {
                            total += curData;
                            statValues.Add(curData);
                        }
                    }
                    TransmissionLoss[depth, range] = curData;
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