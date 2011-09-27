using System;

namespace ESME.TransmissionLoss.REFMS
{
    /**
 * Computes a natural (also known as "free", "unclamped") cubic spline interpolation for the data set.
 * <p>
 * The {@link #interpolate(double[], double[])} method returns a {@link PolynomialSplineFunction}
 * consisting of n cubic polynomials, defined over the subintervals determined by the x values,
 * x[0] < x[i] ... < x[n].  The x values are referred to as "knot points."</p>
 * <p>
 * The value of the PolynomialSplineFunction at a point x that is greater than or equal to the smallest
 * knot point and strictly less than the largest knot point is computed by finding the subinterval to which
 * x belongs and computing the value of the corresponding polynomial at <code>x - x[i] </code> where
 * <code>i</code> is the index of the subinterval.  See {@link PolynomialSplineFunction} for more details.
 * </p>
 * <p>
 * The interpolating polynomials satisfy: <ol>
 * <li>The value of the PolynomialSplineFunction at each of the input x values equals the
 *  corresponding y value.</li>
 * <li>Adjacent polynomials are equal through two derivatives at the knot points (i.e., adjacent polynomials
 *  "match up" at the knot points, as do their first and second derivatives).</li>
 * </ol></p>
 * <p>
 * The cubic spline interpolation algorithm implemented is as described in R.L. Burden, J.D. Faires,
 * <u>Numerical Analysis</u>, 4th Ed., 1989, PWS-Kent, ISBN 0-53491-585-X, pp 126-131.
 * </p>
 *
 * @version $Revision: 983921 $ $Date: 2010-08-10 12:46:06 +0200 (mar. 10 août 2010) $
 *
 */

    public class SplineInterpolator : IUnivariateRealInterpolator
    {
        /**
         * Computes an interpolating function for the data set.
         * @param x the arguments for the interpolation points
         * @param y the values for the interpolation points
         * @return a function which interpolates the data set
         * @throws DimensionMismatchException if {@code x} and {@code y}
         * have different sizes.
         * @throws org.apache.commons.math.exception.NonMonotonousSequenceException
         * if {@code x} is not sorted in strict increasing order.
         * @throws NumberIsTooSmallException if the size of {@code x} is smaller
         * than 3.
         */

        #region OrderDirection enum
        public enum OrderDirection
        {
            /** Constant for increasing direction. */
            Increasing,
            /** Constant for decreasing direction. */
            Decreasing
        }
        #endregion

        #region IUnivariateRealInterpolator Members
        public IUnivariateRealFunction Interpolate(double[] x, double[] y)
        {
            if (x.Length != y.Length) throw new ApplicationException("mismatch length");

            if (x.Length < 3) throw new ApplicationException("wrong length");

            // Number of intervals.  The number of data points is n + 1.
            var n = x.Length - 1;

            CheckOrder(x);

            // Differences between knot points
            var h = new double[n];
            for (var i = 0; i < n; i++) h[i] = x[i + 1] - x[i];

            var mu = new double[n];
            var z = new double[n + 1];
            mu[0] = 0d;
            z[0] = 0d;
            for (var i = 1; i < n; i++)
            {
                var g = 2d * (x[i + 1] - x[i - 1]) - h[i - 1] * mu[i - 1];
                mu[i] = h[i] / g;
                z[i] = (3d * (y[i + 1] * h[i - 1] - y[i] * (x[i + 1] - x[i - 1]) + y[i - 1] * h[i]) /
                        (h[i - 1] * h[i]) - h[i - 1] * z[i - 1]) / g;
            }

            // cubic spline coefficients --  b is linear, c quadratic, d is cubic (original y's are constants)
            var b = new double[n];
            var c = new double[n + 1];
            var d = new double[n];

            z[n] = 0d;
            c[n] = 0d;

            for (var j = n - 1; j >= 0; j--)
            {
                c[j] = z[j] - mu[j] * c[j + 1];
                b[j] = (y[j + 1] - y[j]) / h[j] - h[j] * (c[j + 1] + 2d * c[j]) / 3d;
                d[j] = (c[j + 1] - c[j]) / (3d * h[j]);
            }

            var polynomials = new PolynomialFunction[n];
            var coefficients = new double[4];
            for (var i = 0; i < n; i++)
            {
                coefficients[0] = y[i];
                coefficients[1] = b[i];
                coefficients[2] = c[i];
                coefficients[3] = d[i];
                polynomials[i] = new PolynomialFunction(coefficients);
            }

            return new PolynomialSplineFunction(x, polynomials);
        }
        #endregion

        /// <summary>
        ///   originally in Math/util/MathUtils.java
        /// </summary>
        /// <param name = "val"></param>
        /// <param name = "dir"></param>
        /// <param name = "strict"></param>
        public static void CheckOrder(double[] val, OrderDirection dir, bool strict)
        {
            var previous = val[0];
            var ok = true;

            var max = val.Length;
            for (var i = 1; i < max; i++)
            {
                switch (dir)
                {
                    case OrderDirection.Increasing:
                        if (strict)
                        {
                            if (val[i] <= previous) ok = false;
                        }
                        else if (val[i] < previous) ok = false;
                        break;
                    case OrderDirection.Decreasing:
                        if (strict)
                        {
                            if (val[i] >= previous) ok = false;
                        }
                        else if (val[i] > previous) ok = false;
                        break;
                    default:
                        // Should never happen.
                        throw new ApplicationException("");
                }

                if (!ok) throw new ApplicationException("");
                previous = val[i];
            }
        }

        /**
         * Checks that the given array is sorted in strictly increasing order.
         *
         * @param val Values.
         * @throws NonMonotonousSequenceException if the array is not sorted.
         * @since 2.2
         */
        public static void CheckOrder(double[] val) { CheckOrder(val, OrderDirection.Increasing, true); }
    }
}