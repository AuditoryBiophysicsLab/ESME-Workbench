using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Threading;
using System.Windows.Media;

namespace ESME.TransmissionLoss.Bellhop
{
#if false
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
                TransmissionLoss = new float[ReceiverDepths.Length, ReceiverRanges.Length];

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
                                double real = reader.ReadSingle();
                                double imag = reader.ReadSingle();
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
            var shadeFile = ShadeFile.Read(bellhopFilename);
            if (shadeFile.Equals(this)) Debug.WriteLine(string.Format("ShadeFile != BellhopOutput for {0}", bellhopFilename));
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
                        TransmissionLoss[depth, range] = curData;
                    }
                    else TransmissionLoss[depth, range] = float.NaN;
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
#endif

    public class ShadeFile // : IEquatable<BellhopOutput>
    {
        private ShadeFile() {}
        public ShadeFile(double sourceDepth, double frequency, IList<double> receiverDepths, IList<double> receiverRanges, IList<Complex[]> pressures)
        {
            if (receiverRanges.Count != pressures.Count) throw new ArgumentOutOfRangeException("receiverRanges", "receiverRanges.Length must equal pressures.Count");
            if (receiverDepths.Count != pressures[0].Length) throw new ArgumentOutOfRangeException("receiverDepths", "receiverDepths.Length must equal pressures[0].Length");
            SourceDepths = new float[1];
            SourceDepths[0] = (float)sourceDepth;
            Frequency = (float)frequency;
            ReceiverDepths = new float[receiverDepths.Count];
            for (var i = 0; i < receiverDepths.Count; i++) ReceiverDepths[i] = (float)receiverDepths[i];
            ReceiverRanges = new float[receiverRanges.Count];
            for (var i = 0; i < receiverRanges.Count; i++) ReceiverRanges[i] = (float)receiverRanges[i];
            _pressure = new Complex[ReceiverDepths.Length, ReceiverRanges.Length];
            for (var depthIndex = 0; depthIndex < ReceiverDepths.Length; depthIndex++)
            {
                for (var rangeIndex = 0; rangeIndex < ReceiverRanges.Length; rangeIndex++)
                {
                    var curPressure = pressures[rangeIndex][depthIndex];
                    _pressure[depthIndex, rangeIndex] = new Complex(curPressure.Real, curPressure.Imaginary);
                }
            }
        }

        public static ShadeFile Read(string bellhopFilename, float bearingFromSource)
        {
            var shadeFile = new ShadeFile { BearingFromSource = bearingFromSource };
            var retry = 20;
            while (!File.Exists(bellhopFilename) && retry > 0)
            {
                Thread.Sleep(50);
                retry--;
            }
            using (var reader = new BinaryReader(new FileStream(bellhopFilename, FileMode.Open, FileAccess.Read)))
            {
                var recordLength = reader.ReadInt32() * 4;
                shadeFile.Title = new string(reader.ReadChars(80)).Trim();
                reader.BaseStream.Seek(recordLength, SeekOrigin.Begin);
                shadeFile.PlotType = new string(reader.ReadChars(10));
                shadeFile.Xs = reader.ReadSingle();
                shadeFile.Ys = reader.ReadSingle();
                reader.BaseStream.Seek(2 * recordLength, SeekOrigin.Begin);
                shadeFile.Frequency = reader.ReadSingle();
                //Theta = new float[reader.ReadInt32()];
                var thetaCount = reader.ReadInt32();
                shadeFile.SourceDepths = new float[reader.ReadInt32()];
                shadeFile.ReceiverDepths = new float[reader.ReadInt32()];
                shadeFile.ReceiverRanges = new float[reader.ReadInt32()];

                reader.BaseStream.Seek(3 * recordLength, SeekOrigin.Begin);
                for (var curTheta = 0; curTheta < thetaCount; curTheta++) reader.ReadSingle();

                reader.BaseStream.Seek(4 * recordLength, SeekOrigin.Begin);
                for (var source = 0; source < shadeFile.SourceDepths.Length; source++) shadeFile.SourceDepths[source] = reader.ReadSingle();

                reader.BaseStream.Seek(5 * recordLength, SeekOrigin.Begin);
                for (var depth = 0; depth < shadeFile.ReceiverDepths.Length; depth++) shadeFile.ReceiverDepths[depth] = reader.ReadSingle();

                reader.BaseStream.Seek(6 * recordLength, SeekOrigin.Begin);
                int range;
                for (range = 0; range < shadeFile.ReceiverRanges.Length; range++) shadeFile.ReceiverRanges[range] = reader.ReadSingle();

                shadeFile._pressure = new Complex[shadeFile.ReceiverDepths.Length, shadeFile.ReceiverRanges.Length];
                for (var curTheta = 0; curTheta < thetaCount; curTheta++)
                {
                    for (var source = 0; source < shadeFile.SourceDepths.Length; source++)
                    {
                        for (var depth = 0; depth < shadeFile.ReceiverDepths.Length; depth++)
                        {
                            var recordNumber = 7 + (curTheta * shadeFile.SourceDepths.Length * shadeFile.ReceiverDepths.Length) + (source * shadeFile.ReceiverDepths.Length) + depth;
                            reader.BaseStream.Seek(recordNumber * recordLength, SeekOrigin.Begin);
                            for (range = 0; range < shadeFile.ReceiverRanges.Length; range++)
                                shadeFile._pressure[depth, range] = new Complex(reader.ReadSingle(), reader.ReadSingle());
                        } // for Depth
                    } // for Source
                } // for CurTheta
            } // using
            shadeFile.DataMin = shadeFile.StatMin = float.MaxValue;
            shadeFile.DataMax = shadeFile.StatMax = float.MinValue;
            shadeFile.ExtractStatisticalData();
            return shadeFile;
        }

        public void Write(string fileName)
        {
            var recordLength = ReceiverRanges.Length * 8;
            using (var writer = new BinaryWriter(new FileStream(fileName, FileMode.Create, FileAccess.Write)))
            {
                writer.Write(recordLength / 4);
                writer.Write(new string(' ', 80).ToCharArray());
                writer.BaseStream.Seek(recordLength, SeekOrigin.Begin);
                writer.Write(new string(' ', 10).ToCharArray());
                writer.Write(0.0f); // Xs
                writer.Write(0.0f); // Ys
                writer.BaseStream.Seek(2 * recordLength, SeekOrigin.Begin);
                writer.Write(Frequency);
                writer.Write(1); // thetaCount
                writer.Write(1); // sourceDepths
                writer.Write(ReceiverDepths.Length);
                writer.Write(ReceiverRanges.Length);
                // Skipping over the theta record, we don't use it in ESME
                // writer.BaseStream.Seek(3 * recordLength, SeekOrigin.Begin);
                writer.BaseStream.Seek(4 * recordLength, SeekOrigin.Begin);
                foreach (var sourceDepth in SourceDepths) writer.Write(sourceDepth);
                writer.BaseStream.Seek(5 * recordLength, SeekOrigin.Begin);
                foreach (var depth in ReceiverDepths) writer.Write(depth);
                writer.BaseStream.Seek(6 * recordLength, SeekOrigin.Begin);
                foreach (var range in ReceiverRanges) writer.Write(range);
                for (var depthIndex = 0; depthIndex < ReceiverDepths.Length; depthIndex++)
                {
                    writer.BaseStream.Seek((7 + depthIndex) * recordLength, SeekOrigin.Begin);
                    for (var rangeIndex = 0; rangeIndex < ReceiverRanges.Length; rangeIndex++)
                    {
                        writer.Write((float)_pressure[depthIndex, rangeIndex].Real);
                        writer.Write((float)_pressure[depthIndex, rangeIndex].Imaginary);
                    }
                }
            }
        }

        public List<float> this[int rangeIndex] { get { return ReceiverDepths.Select((t, i) => TransmissionLoss[i, rangeIndex]).ToList(); } }
        public float this[int depthCell, int rangeCell] { get { return TransmissionLoss[depthCell, rangeCell]; } }
        public float this[double range, double depth]
        {
            get
            {
                if (range > ReceiverRanges.Last()) throw new IndexOutOfRangeException("TransmissionLossRadialData: Requested range is past the end of the radial");
                var rangeIndex = (from r in ReceiverRanges
                                  orderby Math.Abs(range - r)
                                  select Array.IndexOf(ReceiverRanges, r)).First();
                var depthIndex = ReceiverDepths.Length - 1;
                if (depth < ReceiverDepths.Last())
                    depthIndex = (from d in ReceiverDepths
                                  orderby Math.Abs(depth - d)
                                  select Array.IndexOf(ReceiverDepths, d)).First();
                return TransmissionLoss[depthIndex, rangeIndex];
            }
        }

        Complex[,] _pressure;
        public float BearingFromSource { get; private set; }
        public string Title { get; private set; }
        public string PlotType { get; private set; }
        public float Xs { get; private set; }
        public float Ys { get; private set; }
        public float Frequency { get; private set; }
        public float[] SourceDepths { get; private set; }
        public float[] ReceiverDepths { get; private set; }
        public float[] ReceiverRanges { get; private set; }
        public float DataMax { get; private set; }
        public float DataMin { get; private set; }
        public float StatMax { get; private set; }
        public float StatMin { get; private set; }
        public float Mean { get; private set; }
        public float Median { get; private set; }
        public float Variance { get; private set; }
        public float StandardDeviation { get; private set; }
        public float[,] TransmissionLoss { get; private set; }
        float[] _bottomDepths;
        public float[] BottomDepths
        {
            get { return _bottomDepths; } 
            set
            {
                _bottomDepths = value;
                ExtractStatisticalData(_bottomDepths);
            }
        }

        void ExtractStatisticalData(IList<float> bottomDepths = null)
        {
            float curData;
            var statValues = new List<float>();
            float? bottomDepth = null;
            if (bottomDepths != null && bottomDepths.Count != ReceiverRanges.Length) throw new ArgumentException(string.Format("bottomDepths (length: {0}) must be the same length as ReceiverRanges (length: {1})", bottomDepths.Count, ReceiverRanges.Length), "bottomDepths");

            DataMax = float.MinValue;
            DataMin = float.MaxValue;
            TransmissionLoss = new float[ReceiverDepths.Length, ReceiverRanges.Length];
            for (var range = 0; range < ReceiverRanges.Length; range++)
            {
                if (bottomDepths != null) bottomDepth = bottomDepths[range];
                for (var depth = 0; depth < ReceiverDepths.Length; depth++)
                {
                    curData = (float)(-20 * Math.Log10(_pressure[depth, range].Magnitude));
                    // If the current value is not infinity, AND the current value is not NaN AND we either don't have a bottom depth value
                    // OR if we do have a bottom depth value, the bottom depth at the current range is DEEPER THAN the current receiver depth
                    // process this depth cell
                    if (!float.IsInfinity(curData) && !float.IsNaN(curData) && (!bottomDepth.HasValue || bottomDepth.Value > ReceiverDepths[depth]))
                    {
                        DataMin = Math.Min(curData, DataMin);
                        DataMax = Math.Max(curData, DataMax);
                        if (curData <= 120) statValues.Add(curData);
                        TransmissionLoss[depth, range] = curData;
                    }
                    else TransmissionLoss[depth, range] = float.NaN;
                } // for (Depth)
            } // for (Range)
            if (statValues.Count > 0)
            {
                statValues.Sort();
                Median = statValues[statValues.Count / 2];
                Mean = statValues.Average();
                var total = 0f;
                foreach (var curValue in statValues)
                {
                    curData = curValue - Mean;
                    curData *= curData;
                    total += curData;
                }
                Variance = total / statValues.Count;
                StandardDeviation = (float)Math.Sqrt(Variance);
                StatMax = (float)Math.Round(Median + (0.75 * StandardDeviation));
                StatMin = StatMax - 50;
            }
            else
            {
                DataMin = 0;
                DataMax = 100;
                StatMin = DataMin;
                StatMax = DataMax;
            }
        }
#if false
        public bool Equals(BellhopOutput bellhopOutput)
        {
            var isEqual = true;
            if (Math.Abs(Xs - bellhopOutput.Xs) > 0.0001)
            {
                Debug.WriteLine("ShadeFile.Xs: {0} differs from BellhopOutput.Xs: {1}", Xs, bellhopOutput.Xs);
                isEqual = false;
            }
            if (Math.Abs(Ys - bellhopOutput.Ys) > 0.0001)
            {
                Debug.WriteLine("ShadeFile.Ys: {0} differs from BellhopOutput.Ys: {1}", Ys, bellhopOutput.Ys);
                isEqual = false;
            }
            if (Math.Abs(Frequency - bellhopOutput.Frequency) > 0.0001)
            {
                Debug.WriteLine("ShadeFile.Frequency: {0} differs from BellhopOutput.Frequency: {1}", Frequency, bellhopOutput.Frequency);
                isEqual = false;
            }
            if (SourceDepths.Length != bellhopOutput.SourceDepths.Length)
            {
                Debug.WriteLine("ShadeFile.SourceDepths.Length: {0} differs from BellhopOutput.SourceDepths.Length: {1}", SourceDepths.Length, bellhopOutput.SourceDepths.Length);
                isEqual = false;
            }
            if (SourceDepths.Where((t, i) => Math.Abs(t - bellhopOutput.SourceDepths[i]) > 0.0001).Any())
            {
                Debug.WriteLine("ShadeFile.SourceDepths differs from BellhopOutput.SourceDepths");
                isEqual = false;
            }
            if (ReceiverDepths.Length != bellhopOutput.ReceiverDepths.Length)
            {
                Debug.WriteLine("ShadeFile.ReceiverDepths.Length: {0} differs from BellhopOutput.ReceiverDepths.Length: {1}", ReceiverDepths.Length, bellhopOutput.ReceiverDepths.Length);
                isEqual = false;
            }
            if (ReceiverDepths.Where((t, i) => Math.Abs(t - bellhopOutput.ReceiverDepths[i]) > 0.0001).Any())
            {
                Debug.WriteLine("ShadeFile.ReceiverDepths differs from BellhopOutput.ReceiverDepths");
                isEqual = false;
            }
            if (ReceiverRanges.Length != bellhopOutput.ReceiverRanges.Length)
            {
                Debug.WriteLine("ShadeFile.ReceiverRanges.Length: {0} differs from BellhopOutput.ReceiverRanges.Length: {1}", ReceiverRanges.Length, bellhopOutput.ReceiverRanges.Length);
                isEqual = false;
            }
            if (ReceiverRanges.Where((t, i) => Math.Abs(t - bellhopOutput.ReceiverRanges[i]) > 0.0001).Any())
            {
                Debug.WriteLine("ShadeFile.ReceiverRanges differs from BellhopOutput.ReceiverRanges");
                isEqual = false;
            }
            if (Math.Abs(DataMin - bellhopOutput.DataMin) > 0.0001)
            {
                Debug.WriteLine("ShadeFile.DataMin: {0} differs from BellhopOutput.DataMin: {1}", DataMin, bellhopOutput.DataMin);
                isEqual = false;
            }
            if (Math.Abs(DataMax - bellhopOutput.DataMax) > 0.0001)
            {
                Debug.WriteLine("ShadeFile.DataMax: {0} differs from BellhopOutput.DataMax: {1}", DataMax, bellhopOutput.DataMax);
                isEqual = false;
            }
            if (Math.Abs(StatMin - bellhopOutput.StatMin) > 0.0001)
            {
                Debug.WriteLine("ShadeFile.StatMin: {0} differs from BellhopOutput.StatMin: {1}", StatMin, bellhopOutput.StatMin);
                isEqual = false;
            }
            if (Math.Abs(StatMax - bellhopOutput.StatMax) > 0.0001)
            {
                Debug.WriteLine("ShadeFile.StatMax: {0} differs from BellhopOutput.StatMax: {1}", StatMax, bellhopOutput.StatMax);
                isEqual = false;
            }
            if (Math.Abs(Median - bellhopOutput.Median) > 0.0001)
            {
                Debug.WriteLine("ShadeFile.Median: {0} differs from BellhopOutput.Median: {1}", Median, bellhopOutput.Median);
                isEqual = false;
            }
            if (Math.Abs(Mean - bellhopOutput.Mean) > 0.0001)
            {
                Debug.WriteLine("ShadeFile.Mean: {0} differs from BellhopOutput.Mean: {1}", Mean, bellhopOutput.Mean);
                isEqual = false;
            }
            if (Math.Abs(Variance - bellhopOutput.Variance) > 0.0001)
            {
                Debug.WriteLine("ShadeFile.Variance: {0} differs from BellhopOutput.Variance: {1}", Variance, bellhopOutput.Variance);
                isEqual = false;
            }
            if (Math.Abs(StandardDeviation - bellhopOutput.StandardDeviation) > 0.0001)
            {
                Debug.WriteLine("ShadeFile.StandardDeviation: {0} differs from BellhopOutput.StandardDeviation: {1}", StandardDeviation, bellhopOutput.StandardDeviation);
                isEqual = false;
            }
            if (TransmissionLoss.GetLength(0) != bellhopOutput.TransmissionLoss.GetLength(0))
            {
                Debug.WriteLine("ShadeFile.ReceiverRanges.GetLength(0): {0} differs from BellhopOutput.ReceiverRanges.GetLength(0): {1}", ReceiverRanges.GetLength(0), bellhopOutput.ReceiverRanges.GetLength(0));
                isEqual = false;
            }
            if (TransmissionLoss.GetLength(1) != bellhopOutput.TransmissionLoss.GetLength(1))
            {
                Debug.WriteLine("ShadeFile.ReceiverRanges.GetLength(1): {0} differs from BellhopOutput.ReceiverRanges.GetLength(1): {1}", ReceiverRanges.GetLength(1), bellhopOutput.ReceiverRanges.GetLength(1));
                isEqual = false;
            }
            for (var i = 0; i < TransmissionLoss.GetLength(0); i++)
                for (var j = 0; j < TransmissionLoss.GetLength(1); j++)
                {
                    if (!float.IsNaN(TransmissionLoss[i, j]) && (Math.Abs(TransmissionLoss[i, j] - bellhopOutput.TransmissionLoss[i, j]) > 0.0001))
                    {
                        Debug.WriteLine("ShadeFile.TransmissionLoss[{0}, {1}]: {2} differs from BellhopOutput.TransmissionLoss[{0}, {1}]: {3}", i, j, TransmissionLoss[i, j], bellhopOutput.TransmissionLoss[i, j]);
                        isEqual = false;
                        break;
                    }
                }
            if (!isEqual) Debugger.Break();
            return isEqual;
        }
#endif
    }

    public static class ShadeFileExtensions
    {
        public static int[] RenderToPixelBuffer(this ShadeFile shadeFile, Func<float, Color> valueToColorFunc, int width = 0, int height = 0)
        {
            if (width < 0) throw new ArgumentOutOfRangeException("width", "width must be non-negative");
            if (height < 0) throw new ArgumentOutOfRangeException("height", "height must be non-negative");

            if (width == 0) width = shadeFile.ReceiverRanges.Length;
            if (height == 0) height = shadeFile.ReceiverDepths.Length;

            var buffer = new int[width * height];
            var rangeStep = shadeFile.ReceiverRanges.Length / (float)width;
            var depthStep = shadeFile.ReceiverDepths.Length / (float)height;
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
                    var curColor = valueToColorFunc(shadeFile[(int)(y * depthStep), (int)(x * rangeStep)]);
                    buffer[curOffset++] = ((curColor.A << 24) | (curColor.R << 16) | (curColor.G << 8) | (curColor.B));
                }
            }
#endif
            return buffer;
        }
    }
}