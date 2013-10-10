using System;
using System.IO;
using System.Linq;
using System.Numerics;
using MathNet.Numerics.IntegralTransforms;

namespace HRC.Utility
{
    public static class Numerics
    {
        public static Complex[] MatlabHilbert(double[] xr)
        {
            //WriteVector(xr, "xr.esme");
            //var fft = new MathNet.Numerics.IntegralTransforms.Algorithms.DiscreteFourierTransform();
            var x = (from sample in xr select new Complex(sample, 0)).ToArray();
            Transform.FourierForward(x, FourierOptions.Matlab);
            //fft.BluesteinForward(x, FourierOptions.Matlab);
            //WriteVector(x, "x_fwd.esme");
            var h = new double[x.Length];
            var fftLengthIsOdd = (x.Length | 1) == 1;
            if (fftLengthIsOdd)
            {
                h[0] = 1;
                for (var i = 1; i < xr.Length / 2; i++) h[i] = 2;
            }
            else
            {
                h[0] = 1;
                h[(xr.Length / 2)] = 1;
                for (var i = 1; i < xr.Length / 2; i++) h[i] = 2;
            }
            for (var i = 0; i < x.Length; i++) x[i] *= h[i];
            Transform.FourierInverse(x, FourierOptions.Matlab);
            //fft.BluesteinInverse(x, FourierOptions.Matlab);
            //WriteVector(x, "x_inv.esme");
            return x;
        }

        public static void WriteVector(double[] realvec, string fileName)
        {
            using (var writer = new StreamWriter(fileName, false))
            {
                foreach (var v in realvec) writer.WriteLine(v < 0 ? " {0:e16}" : "  {0:e16}", v);
            }
        }

        public static void WriteVector(Complex[] compvec, string fileName)
        {
            using (var writer = new StreamWriter(fileName, false))
            {
                foreach (var c in compvec)
                {
                    writer.Write(c.Real < 0 ? " {0:e16}" : "  {0:e16}", c.Real);
                    writer.Write(c.Imaginary < 0 ? " {0:e16}" : "  {0:e16}", c.Imaginary);
                    writer.Write("  {0:e16}", c.Magnitude);
                    writer.WriteLine(c.Phase < 0 ? " {0:e16}" : "  {0:e16}", c.Phase);
                }
            }
        }
    }
}
