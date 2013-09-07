using System;
using System.Collections.Generic;
using System.IO;
using System.Numerics;

namespace DavesConsoleTester
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length != 1)
            {
                Console.WriteLine("Usage: ReadArrivalsFile <path-to-arrivals-file>");
                return;
            }
            var result = ArrivalsFile.Read(args[0]);
        }
    }

    public class ArrivalsFile
    {
        public static ArrivalsFile Read(string fileName)
        {
            var result = new ArrivalsFile();
            using (var reader = new BinaryReader(File.OpenRead(fileName)))
            {
                reader.BaseStream.Seek(4, SeekOrigin.Begin);
                result.Frequency = reader.ReadSingle();
                result.SourceDepths = new float[reader.ReadInt32()];
                result.ReceiverDepths = new float[reader.ReadInt32()];
                result.ReceiverRanges = new float[reader.ReadInt32()];
                reader.BaseStream.Seek(8, SeekOrigin.Current);
                for (var i = 0; i < result.SourceDepths.Length; i++) result.SourceDepths[i] = reader.ReadSingle();
                reader.BaseStream.Seek(8, SeekOrigin.Current);
                for (var i = 0; i < result.ReceiverDepths.Length; i++) result.ReceiverDepths[i] = reader.ReadSingle();
                reader.BaseStream.Seek(8, SeekOrigin.Current);
                for (var i = 0; i < result.ReceiverRanges.Length; i++) result.ReceiverRanges[i] = reader.ReadSingle();
                reader.BaseStream.Seek(8, SeekOrigin.Current);

                result.Arrivals = new List<List<Arrival>[,]>();
                for (var sourceIndex = 0; sourceIndex < result.SourceDepths.Length; sourceIndex++)
                {
                    reader.ReadInt32();   // max number of arrivals for current source index
                    var curArrivals = new List<Arrival>[result.ReceiverDepths.Length, result.ReceiverRanges.Length];
                    result.Arrivals.Add(curArrivals);
                    reader.BaseStream.Seek(8, SeekOrigin.Current);
                    for (var depthIndex = 0; depthIndex < result.ReceiverDepths.Length; depthIndex++)
                    {
                        for (var rangeIndex = 0; rangeIndex < result.ReceiverRanges.Length; rangeIndex++)
                        {
                            var arrivalCount = reader.ReadInt32();
                            reader.BaseStream.Seek(8, SeekOrigin.Current);
                            curArrivals[rangeIndex, depthIndex] = new List<Arrival>(arrivalCount);
                            for (var arrival = 0; arrival < arrivalCount; arrival++) curArrivals[depthIndex, rangeIndex].Add(reader.ReadArrival());
                        }
                    }
                }
            }
            return result;
        }

        /// <summary>
        /// Return a list of Arrival objects describing the rays from a particular sourceIndex that entered a given cell at depthIndex, rangeIndex
        /// </summary>
        /// <param name="sourceIndex"></param>
        /// <param name="depthIndex"></param>
        /// <param name="rangeIndex"></param>
        /// <returns></returns>
        public List<Arrival> this[int sourceIndex, int depthIndex, int rangeIndex]
        {
            get
            {
                if (sourceIndex < 0 || sourceIndex >= SourceDepths.Length) throw new IndexOutOfRangeException(string.Format("sourceIndex is out of range 0:{0}", SourceDepths.Length));
                if (depthIndex < 0 || depthIndex >= ReceiverDepths.Length) throw new IndexOutOfRangeException(string.Format("depthIndex is out of range 0:{0}", ReceiverDepths.Length));
                if (rangeIndex < 0 || rangeIndex >= ReceiverRanges.Length) throw new IndexOutOfRangeException(string.Format("rangeIndex is out of range 0:{0}", ReceiverRanges.Length));
                return Arrivals[sourceIndex][depthIndex, rangeIndex];
            }
        }

        public float Frequency { get; private set; }
        public float[] SourceDepths { get; private set; }
        public float[] ReceiverDepths { get; private set; }
        public float[] ReceiverRanges { get; private set; }
        /// <summary>
        /// A List of two dimensional arrays of lists of Arrival classes
        /// The outermost list is for each source in SourceDepths
        /// Each separate source has a 2-D array[depthIndex, rangeIndex] of lists of Arrival objects
        /// Each array element is a list of Arrival objects, one for each ray that has entered that Depth/Range cell
        /// If no rays have entered a particular cell, the List of Arrival objects is zero-length
        /// </summary>
        public List<List<Arrival>[,]> Arrivals { get; private set; }
    }

    public class Arrival
    {
        public Arrival(Complex amplitude, float delay) 
        {
            Amplitude = amplitude;
            Delay = delay;
        }
        public Complex Amplitude { get; private set; }
        public float Delay { get; private set; }
    }

    public static class BinaryReaderExtensions
    {
        public static Arrival ReadArrival(this BinaryReader reader)
        {
            var amplitude = reader.ReadSingle() * Complex.Exp(new Complex(0, reader.ReadSingle() * (Math.PI / 180.0)));
            var result = new Arrival(amplitude, reader.ReadSingle());
            // Skip SourceAngle, ReceiverAngle, NumTopBounces, NumBottomBounces
            reader.BaseStream.Seek(24, SeekOrigin.Current);
            return result;
        }
    }
}
