using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using HRC.Utility;

namespace ESME.TransmissionLoss.Bellhop
{
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
// ReSharper disable ForCanBeConvertedToForeach
                for (var sourceIndex = 0; sourceIndex < result.SourceDepths.Length; sourceIndex++)
// ReSharper restore ForCanBeConvertedToForeach
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
                            curArrivals[depthIndex, rangeIndex] = new List<Arrival>(arrivalCount);
                            for (var arrival = 0; arrival < arrivalCount; arrival++) curArrivals[depthIndex, rangeIndex].Add(reader.ReadArrival());
                        }
                    }
                }
            }
            return result;
        }

        async public Task<Tuple<double[], double, double>> DelayAndSumAsync(double chargeDepth, double chargeWeight, double sampleRate, double outputWaveformDuration, double outputRange, double outputDepth, Func<double, double, double, double, double, double[]> timeSeriesGenerator)
        {
            var nSamples = (int)Math.Round(sampleRate * outputWaveformDuration);
            const double tShift = 0.002;
            var arrivals = this[outputDepth, outputRange];
            if (arrivals.Count == 0) return null;    // If there are no arrivals in the selected range and depth cell, return null

            // determine the min, max of all arrival times at this receiver
            var minDelay = arrivals.Min(a => a.Delay);

            // compute a reasonable start time based on the arrival times
            var startTime = minDelay - tShift;

            var result = new double[nSamples];
            var createDelayTimeSeriesTransform = new TransformBlock<Arrival, double[]>(arrival =>
            {
                // arrival time relative to start of rcv timeseries
                var arrivalTime = arrival.Delay - startTime;

                // compute (guess-timate) of the ray path length to this receiver
                var pathLength = 1500 * arrival.Delay;

                // complex amplitude of this arrival, with spherical spreading removed
                var amplitude = arrival.Amplitude * pathLength;

                var uShock = timeSeriesGenerator(chargeDepth, chargeWeight, pathLength, sampleRate, outputWaveformDuration);
                //Numerics.WriteVector(uShock, "ushock.esme");
                var hShock = Numerics.MatlabHilbert(uShock);
                var timeIndex = (int)Math.Round(sampleRate * arrivalTime);
                var timeSeries = new double[nSamples];
                for (var i = timeIndex; i < result.Length; i++) timeSeries[i] = (amplitude * hShock[i - timeIndex]).Real;
                return timeSeries;
            }, new ExecutionDataflowBlockOptions{BoundedCapacity = -1, MaxDegreeOfParallelism = arrivals.Count});
            var sumTimeSeriesTransform = new ActionBlock<double[]>(timeSeries =>
            {
                for (var i = 0; i < result.Length; i++) result[i] += timeSeries[i];
            }, new ExecutionDataflowBlockOptions{BoundedCapacity = 1, MaxDegreeOfParallelism = 1});
            createDelayTimeSeriesTransform.LinkTo(sumTimeSeriesTransform);
            createDelayTimeSeriesTransform.Completion.ContinueWith(t => sumTimeSeriesTransform.Complete());
            foreach (var arrival in arrivals) createDelayTimeSeriesTransform.Post(arrival);
            createDelayTimeSeriesTransform.Complete();
            var pressure = double.MinValue;
            var energy = 0.0;
            await sumTimeSeriesTransform.Completion.ContinueWith(t =>
            {
                pressure = double.MinValue;
                energy = 0.0;
                foreach (var sample in result)
                {
                    pressure = Math.Max(pressure, Math.Abs(sample));
                    energy += sample * sample;
                }
                energy /= sampleRate;
            });
            return Tuple.Create(result, 20 * Math.Log10(pressure), 10 * Math.Log10(energy));
        }

        public double[] DelayAndSum(double chargeDepth, double chargeWeight, double sampleRate, double outputWaveformDuration, double outputRange, double outputDepth, Func<double, double, double, double, double, double[]> timeSeriesGenerator, out double peakPressure, out double totalEnergy)
        {
            var nSamples = (int)Math.Round(sampleRate * outputWaveformDuration);
            const double tShift = 0.002;
            var arrivals = this[outputDepth, outputRange];
            peakPressure = 0.0;
            totalEnergy = 0.0;
            if (arrivals.Count == 0) return null;    // If there are no arrivals in the selected range and depth cell, return null

            // determine the min, max of all arrival times at this receiver
            var minDelay = arrivals.Min(a => a.Delay);

            // compute a reasonable start time based on the arrival times
            var startTime = minDelay - tShift;

            var result = new double[nSamples];
            foreach (var arrival in arrivals)
            {
                // arrival time relative to start of rcv timeseries
                var arrivalTime = arrival.Delay - startTime;

                // compute (guess-timate) of the ray path length to this receiver
                var pathLength = 1500 * arrival.Delay;

                // complex amplitude of this arrival, with spherical spreading removed
                var amplitude = arrival.Amplitude * pathLength;

                var uShock = timeSeriesGenerator(chargeDepth, chargeWeight, pathLength, sampleRate, outputWaveformDuration);
                //Numerics.WriteVector(uShock, "ushock.esme");
                var hShock = Numerics.MatlabHilbert(uShock);
                var timeIndex = (int)Math.Round(sampleRate * arrivalTime);
                for (var i = timeIndex; i < result.Length; i++) result[i] = (amplitude * hShock[i - timeIndex]).Real;
            }
            peakPressure = double.MinValue;
            totalEnergy = 0.0;
            foreach (var sample in result)
            {
                peakPressure = Math.Max(peakPressure, Math.Abs(sample));
                totalEnergy += sample * sample;
            }
            totalEnergy /= sampleRate;
            peakPressure = 20 * Math.Log10(peakPressure);
            totalEnergy = 10 * Math.Log10(totalEnergy);
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

        /// <summary>
        /// Return a list of Arrival objects describing the rays from a particular sourceIndex that entered a given cell at depth and range
        /// </summary>
        /// <param name="depth"></param>
        /// <param name="range"></param>
        /// <returns></returns>
        public List<Arrival> this[double depth, double range]
        {
            get
            {
                var depthIndex = Array.IndexOf(ReceiverDepths, ReceiverDepths.Last(d => d <= depth));
                var rangeIndex = Array.IndexOf(ReceiverRanges, ReceiverRanges.Last(r => r <= range));
                return this[0, depthIndex, rangeIndex];
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
        public Arrival(Complex amplitude, double delay)
        {
            Amplitude = amplitude;
            Delay = delay;
        }
        public Complex Amplitude { get; private set; }
        public double Delay { get; private set; }
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
