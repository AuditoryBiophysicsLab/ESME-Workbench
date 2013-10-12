using System;
using System.IO;
using System.Linq;
using System.Numerics;
using ILNumerics;

namespace HRC.Utility
{
    public static class Numerics
    {
        static Numerics() { ILMath.FFTImplementation = new ILNumerics.Native.ILFFTW3FFT(); }

        public static Complex[] MatlabHilbert(double[] xr)
        {
            //WriteVector(xr, "xr.esme");
            //var fft = new MathNet.Numerics.IntegralTransforms.Algorithms.DiscreteFourierTransform();
            //var x = (from sample in xr select new Complex(sample, 0)).ToArray();
            //Transform.FourierForward(x, FourierOptions.Matlab);
            var xc = ILMath.fft((ILArray<double>)xr).ToArray();
            //fft.BluesteinForward(x, FourierOptions.Matlab);
            //WriteVector(x, "x_fwd.esme");
            var h = new double[xc.Length];
            var fftLengthIsOdd = (xc.Length | 1) == 1;
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
            for (var i = 0; i < xc.Length; i++) xc[i] *= h[i];
            //Transform.FourierInverse(x, FourierOptions.Matlab);
            //fft.BluesteinInverse(x, FourierOptions.Matlab);
            //WriteVector(x, "x_inv.esme");
            var result = (from value in ILMath.ifft((ILArray<complex>)xc)
                          select new Complex(value.real, value.imag)).ToArray();
            return result;
        }

        public static double[] LeftDivide(double[,] matrix, double[] vector)
        {
            if (matrix.GetLength(0) != matrix.GetLength(1)) throw new InvalidOperationException("Cannot calculate the inverse of a non-square matrix");
            if (matrix.GetLength(0) != vector.GetLength(0)) throw new InvalidOperationException(string.Format("Cannot divide a matrix of size [{0}, {1}] by a vector of size [{2}]", matrix.GetLength(0), matrix.GetLength(1), vector.Length));
            var input = (ILInArray<double>)matrix;
            var inverse = ILMath.linsolve(input.T, ILMath.eye(matrix.GetLength(0), matrix.GetLength(1)));
            return ILMath.multiply(inverse, vector).ToArray();
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
