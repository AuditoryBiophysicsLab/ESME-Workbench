using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using ESME.TransmissionLoss.Bellhop;
using HRC.Utility;
using Nito.AsyncEx;

namespace DavesConsoleTester
{
    public class Program
    {
        static int Main(string[] args)
        {
            int result;
            try
            {
                MainSync(args);
                AsyncContext.Run(() => MainAsync(args));
                return 0;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
                return -1;
            }
            return result;
        }

        static int MainSync(string[] args)
        {
            if (args.Length != 1)
            {
                Console.WriteLine("Usage: ReadArrivalsFile <path-to-arrivals-file>");
                return -1;
            }
#if false
            var real10Hz = CreateSinusoid(44100, 10).Take(17640).ToArray();
            Numerics.WriteVector(real10Hz, "real10Hz.esme");
            var complex10Hz = (from sample in real10Hz select new Complex(sample, 0)).ToArray();
            Numerics.WriteVector(complex10Hz, "complex10Hz.esme");
            Transform.FourierForward(complex10Hz, FourierOptions.Matlab);
            Numerics.WriteVector(complex10Hz, "fft10Hz.esme");
            var hilbert10Hz = Numerics.MatlabHilbert(real10Hz);
            Numerics.WriteVector(hilbert10Hz, "hilbert10Hz.esme");
            var real100Hz = CreateSinusoid(44100, 100).Take(17640).ToArray();
            Numerics.WriteVector(real100Hz, "real100Hz.esme");
            var complex100Hz = (from sample in real100Hz select new Complex(sample, 0)).ToArray();
            Numerics.WriteVector(complex100Hz, "complex100Hz.esme");
            Transform.FourierForward(complex100Hz, FourierOptions.Matlab);
            Numerics.WriteVector(complex100Hz, "fft100Hz.esme");
            var hilbert100Hz = Numerics.MatlabHilbert(real100Hz);
            Numerics.WriteVector(hilbert100Hz, "hilbert100Hz.esme");
            var real1KHz = CreateSinusoid(44100, 1000).Take(17640).ToArray();
            Numerics.WriteVector(real1KHz, "real1KHz.esme");
            var complex1KHz = (from sample in real1KHz select new Complex(sample, 0)).ToArray();
            Numerics.WriteVector(complex1KHz, "complex1KHz.esme");
            Transform.FourierForward(complex1KHz, FourierOptions.Matlab);
            Numerics.WriteVector(complex1KHz, "fft1KHz.esme");
            var hilbert1KHz = Numerics.MatlabHilbert(real1KHz);
            Numerics.WriteVector(hilbert1KHz, "hilbert1KHz.esme");
#endif
            var stopwatch = new Stopwatch();
            stopwatch.Start();
            const double chargeDepth = 2500.0 / 3.2808;
            const double chargeWeight = 300 * 0.45359237;
            const double sampleRate = 88200;
            const double waveformDuration = 0.25;
            //var arons = ExplosiveWaveformGenerators.Arons(chargeDepth, chargeWeight, 1, 88200, .25);
            //var chapman = ExplosiveWaveformGenerators.Chapman(10, 1, 1, 88200, .25);
            var arrivalsFile = ArrivalsFile.Read(args[0]);
            var waveforms = new double[arrivalsFile.ReceiverRanges.Length, arrivalsFile.ReceiverDepths.Length][];
            for (var depthIndex = 0; depthIndex < arrivalsFile.ReceiverDepths.Length; depthIndex++)
                for (var rangeIndex = 0; rangeIndex < arrivalsFile.ReceiverRanges.Length; rangeIndex++)
                {
                    double peakPressure, totalEnergy;
                    var range = arrivalsFile.ReceiverRanges[rangeIndex];
                    var depth = arrivalsFile.ReceiverDepths[depthIndex];
                    waveforms[rangeIndex, depthIndex] = arrivalsFile.DelayAndSum(chargeDepth,
                                                                                 chargeWeight,
                                                                                 sampleRate,
                                                                                 waveformDuration,
                                                                                 range,
                                                                                 depth,
                                                                                 ExplosiveWaveformGenerators.Chapman,
                                                                                 out peakPressure,
                                                                                 out totalEnergy);
                    //Numerics.WriteVector(waveforms[rangeIndex, depthIndex], String.Format("r{0:0}d{1:0}.esme", range, depth));
                    //Console.WriteLine("range {0} depth {1}: Peak pressure: {2} dB  Total energy: {3} dB", range, depth, peakPressure, totalEnergy);
                }
            //var impulseResponse = arrivalsFile.DelayAndSum(chapman, 100000, 1, 1000, 50);
            Debug.WriteLine("Elapsed time as sync: " + stopwatch.Elapsed);
            return 0;
        }

        async static void MainAsync(string[] args)
        {
            if (args.Length != 1)
            {
                Console.WriteLine("Usage: ReadArrivalsFile <path-to-arrivals-file>");
                return;
            }
            var stopwatch = new Stopwatch();
            stopwatch.Start();
            const double chargeDepth = 2500.0 / 3.2808;
            const double chargeWeight = 300 * 0.45359237;
            const double sampleRate = 88200;
            const double waveformDuration = 0.25;
            //var arons = ExplosiveWaveformGenerators.Arons(chargeDepth, chargeWeight, 1, 88200, .25);
            //var chapman = ExplosiveWaveformGenerators.Chapman(10, 1, 1, 88200, .25);
            var arrivalsFile = ArrivalsFile.Read(args[0]);
            //var waveforms = new double[arrivalsFile.ReceiverRanges.Length, arrivalsFile.ReceiverDepths.Length][];
            //await arrivalsFile.DelayAndSumAsync(chargeDepth, chargeWeight, sampleRate, waveformDuration, 1000, 100, ExplosiveWaveformGenerators.Arons).ContinueWith(t =>
            //{
            //    Numerics.WriteVector(t.Result.Item1, String.Format("r{0:0}d{1:0}.esme", 1000, 100));
            //    Debug.WriteLine("range {0} depth {1}: Peak pressure: {2} dB  Total energy: {3} dB", 1000, 100, t.Result.Item2, t.Result.Item3);
            //});
            //return 0;
            var tasks = (from depth in arrivalsFile.ReceiverDepths
                         from range in arrivalsFile.ReceiverRanges
                         let curDepth = depth
                         let curRange = range
                         select arrivalsFile.DelayAndSumAsync(chargeDepth, chargeWeight, sampleRate, waveformDuration, curRange, curDepth, ExplosiveWaveformGenerators.Chapman)
                         .ContinueWith(t => Numerics.WriteVector(t.Result.Item1, String.Format("r{0:0}d{1:0}.esme", curRange, curDepth)))
                         //.ContinueWith(t => Console.WriteLine("{4:HH:mm:ss.fff} range {0} depth {1}: Peak pressure: {2} dB  Total energy: {3} dB", range, depth, t.Result.Item2, t.Result.Item3, DateTime.Now))
                         ).ToList();
            await Task.WhenAll(tasks);
            //var impulseResponse = arrivalsFile.DelayAndSum(chapman, 100000, 1, 1000, 50);
            Debug.WriteLine("Elapsed time as async: " + stopwatch.Elapsed);
        }


        public static IEnumerable<double> CreateSinusoid(double sampleRate, double frequency)
        {
            var phaseStepPerSample = (2.0 * Math.PI) / (sampleRate / frequency);
            var currentPhase = 0.0;
            while (true)
            {
                yield return Math.Sin(currentPhase);
                currentPhase += phaseStepPerSample;
            }
// ReSharper disable FunctionNeverReturns
        }
// ReSharper restore FunctionNeverReturns
    }
}
