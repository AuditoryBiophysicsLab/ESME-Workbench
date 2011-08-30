using System;
using System.Collections.Generic;

namespace ESME.TransmissionLoss.REFMS
{
    public class PiecewiseCubicHermitePolynomialInterpolator : IUnivariateRealInterpolator
    {
        /**
        * Generates a UnivariateRealFunction which is a piecewise cubic Hermite interpolating polynomial.
        * <p>
        * Used the Octave pchip.m code as a source of which this is mostly a port. <br>
        * Original Author:  Kai Habel<br>
        * {@link http://hg.savannah.gnu.org/hgweb/octave/file/d81b6144c4ba/scripts/polynomial/pchip.m}
        * 
        * @param x independent variable array - must always be increasing
        * @param y dependent variable array - must be the same length as x.
        * 
        * @return the pchi polynomial
        * @throws MathException as a pass-through from the PolynomialFunction constructors.
        */

        #region IUnivariateRealInterpolator Members
        public IUnivariateRealFunction Interpolate(double[] x, double[] y)
        {
            if (x.Length != y.Length) throw new ArgumentException(string.Format("dimension mismatch {0} != {1}", x.Length, y.Length), "x");

            if (x.Length < 3) throw new ApplicationException(string.Format("{0} points are required, got only {1}", 3, x.Length));

            // Number of intervals. The number of data points is n + 1.
            var n = x.Length - 1;

            // monotonic x[] check
            for (var i = 0; i < n; i++)
                if (x[i] >= x[i + 1])
                    throw new ApplicationException(
                        string.Format("points {0} and {1} are not strictly increasing ({2} >= {3})", i, i + 1, x[i],
                                      x[i + 1]));

            // Differences between knot points
            var h = new double[n];
            for (var i = 0; i < n; i++) h[i] = x[i + 1] - x[i];

            // compute derivatives
            var d = CalcPchipDeriv(x, y);
            var d1 = new double[n];
            Array.Copy(d, 0, d1, 0, n);
            var d2 = new double[n];
            Array.Copy(d, 1, d2, 0, n);

            var c2 = new double[n];
            var c3 = new double[n];

            for (var i = 0; i < n; i++)
            {
                var delta = (y[i + 1] - y[i]) / h[i];
                var del1 = (d1[i] - delta) / h[i];
                var del2 = (d2[i] - delta) / h[i];
                c3[i] = del1 + del2;
                c2[i] = (-1 * c3[i]) - del1;
                c3[i] = c3[i] / h[i];
            }

            var polynomials = new PolynomialFunction[n];
            var coefficients = new double[4];
            for (var i = 0; i < n; i++)
            {
                coefficients[0] = y[i];
                coefficients[1] = d1[i];
                coefficients[2] = c2[i];
                coefficients[3] = c3[i];
                polynomials[i] = new PolynomialFunction(coefficients);
            }

            return new PolynomialSplineFunction(x, polynomials);
        }
        #endregion

        /**
        * Calculated the derivatives needed to determine a monotone piecewise cubic
        * Hermite interpolant to given data.  The interpolant will have an extremum at 
        * each point where monotonicity switches direction.
        * Used as part of a port of the FORTRAN code which calculates the 
        * piecewise cubic hermite interpolation polynomials.  
        * Error conditions are not handled properly yet.
        * Original code from:
        * {@link http://www.netlib.org/slatec/pchip/pchim.f}
        * pchim.f<br>
        * Original author:
        * <pre>
        *   Fritsch, F. N. (LLNL)
        *   Lawerence Livermore National Laboratory
        *   P.O. Box 808 (L-316)
        *   Livermore, CA  94550
        *   FTS 532-4275, (510) 422-4275
        * </pre>
        * 
        * @param x array of independent values
        * @param y array of dependent values
        * 
        * @return 
        */

        static double[] CalcPchipDeriv(IList<double> x, IList<double> y)
        {
            var d = new double[y.Count];
            var n = y.Count - 1;
            var h1 = (x[1] - x[0]);
            var del1 = (y[1] - y[0]) / h1;
            var dsave = del1;
            var ierr = 0;

            if (y.Count == 2)
            {
                d[0] = del1;
                d[1] = del1;
            }
            else
            {
                var h2 = (x[2] - x[1]);
                var del2 = (y[2] - y[1]) / h2;
                var hsum = h1 + h2;
                var w1 = (h1 + hsum) / hsum;
                var w2 = (-1 * h1) / hsum;
                d[0] = w1 * del1 + w2 * del2;
                double dmax;

                if (PchipSignTester(d[0], del1) <= 0) d[0] = 0;
                else if (PchipSignTester(del1, del2) < 0)
                {
                    dmax = 3 * del1;
                    if (Math.Abs(d[0]) > Math.Abs(dmax)) d[0] = dmax;
                }

                for (var i = 1; i < n; i++)
                {
                    if (i > 1)
                    {
                        h1 = h2;
                        h2 = x[i + 1] - x[i];
                        hsum = h1 + h2;
                        del1 = del2;
                        del2 = (y[i + 1] - y[i]) / h2;
                    }

                    d[i] = 0;

                    var pchst = (int)PchipSignTester(del1, del2);
                    if (pchst < 0)
                    {
                        dsave = del2;
                        ierr++;
                    }
                    else if (pchst == 0)
                    {
                        if (del2 > 0.0 || del2 < 0.0)
                        {
                            if (PchipSignTester(dsave, del2) < 0) ierr++;
                            dsave = del2;
                        }
                    }
                    else
                    {
                        var hsum3 = hsum + hsum + hsum;
                        w1 = (hsum + h1) / hsum3;
                        w2 = (hsum + h2) / hsum3;
                        dmax = Math.Max(Math.Abs(del1), Math.Abs(del2));
                        var dmin = Math.Min(Math.Abs(del1), Math.Abs(del2));
                        var drat1 = del1 / dmax;
                        var drat2 = del2 / dmax;
                        d[i] = dmin / ((w1 * drat1) + (w2 * drat2));
                    }
                }

                w1 = (-1 * h2) / hsum;
                w2 = (h2 + hsum) / hsum;
                d[n] = w1 * del1 + w2 * del2;

                if (PchipSignTester(d[n], del2) <= 0) d[n] = 0;
                else if (PchipSignTester(del1, del2) > 0)
                {
                    dmax = 3 * del2;
                    if (Math.Abs(d[n]) > Math.Abs(dmax)) d[n] = dmax;
                }
            }
            return d;
        }

        /**
        * Sign testing routine.
        * 
        * Used as part of a port of the FORTRAN code which calculates the 
        * piecewise cubic hermite interpolation polynomial.
        * 
        * Idea from:
        * {@link http://www.netlib.org/slatec/pchip/pchst.f}
        * pchst.f<br>
        * Original author: Fritsch, F. N. (LLNL)
        * 
        * @param a
        * @param b
        * @return -1 if a and b have opposite signs<br> 
        *          0 if either a or b equals 0<br>
        *          1 if a and b have the same sign.
        */

        static double PchipSignTester(double a, double b) { return Math.Sign(a) * Math.Sign(b); }
    }
}