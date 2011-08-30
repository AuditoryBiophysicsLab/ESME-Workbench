using System;

namespace ESME.TransmissionLoss.REFMS
{
    public class PolynomialFunction : IDifferentiableUnivariateRealFunction
    {
        readonly double[] _coefficients;
        public double Value(double x) { return Evaluate(_coefficients, x); }

        protected static double Evaluate(double[] coefficients, double argument)
        {
            var n = coefficients.Length;
            if (n == 0) throw new ApplicationException("");
            var result = coefficients[n - 1];
            for (var j = n - 2; j >= 0; j--) result = argument * result + coefficients[j];
            return result;
        }

        public PolynomialFunction(double[] coefficients)
        {
            var n = coefficients.Length;
            if (n == 0) throw new ApplicationException("");
            while ((n > 1) && (Math.Abs(coefficients[n - 1]) < .000001)) --n;
            _coefficients = new double[n];
            Array.Copy(coefficients, 0, _coefficients, 0, n);
        }

        #region IDifferentiableUnivariateRealFunction Members
        public IUnivariateRealFunction Derivative() { return PolynomialDerivative(); }
        #endregion

        public PolynomialFunction PolynomialDerivative() { return new PolynomialFunction(Differentiate(_coefficients)); }

        protected static double[] Differentiate(double[] coefficients)
        {
            var n = coefficients.Length;
            if (n == 0) throw new ApplicationException("");
            if (n == 1) return new double[] {0};
            var result = new double[n - 1];
            for (var i = n - 1; i > 0; i--) result[i - 1] = i * coefficients[i];
            return result;
        }
    }
}