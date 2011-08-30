using System;
using System.Collections.Generic;

namespace ESME.TransmissionLoss.REFMS
{
    public class PolynomialSplineFunction : IDifferentiableUnivariateRealFunction
    {
        readonly double[] _knots;
        readonly int _n;
        readonly PolynomialFunction[] _polynomials;

        public PolynomialSplineFunction(double[] knots, PolynomialFunction[] polynomials)
        {
            if (knots.Length < 2) throw new ApplicationException("");
            if (knots.Length - 1 != polynomials.Length) throw new ApplicationException("");
            if (!IsStrictlyIncreasing(knots)) throw new ApplicationException("");

            _n = knots.Length - 1;
            _knots = new double[_n + 1];
            Array.Copy(knots, 0, _knots, 0, _n + 1);
            _polynomials = new PolynomialFunction[_n];
            Array.Copy(polynomials, 0, _polynomials, 0, _n);
        }

        #region IDifferentiableUnivariateRealFunction Members
        public IUnivariateRealFunction Derivative() { return PolynomialSplineDerivative(); }
        #endregion

        PolynomialSplineFunction PolynomialSplineDerivative()
        {
            var derivativePolynomials = new PolynomialFunction[_n];
            for (var i = 0; i < _n; i++) derivativePolynomials[i] = _polynomials[i].PolynomialDerivative();
            return new PolynomialSplineFunction(_knots, derivativePolynomials);
        }

        static bool IsStrictlyIncreasing(IList<double> x)
        {
            for (var i = 1; i < x.Count; ++i) if (x[i - 1] >= x[i]) return false;
            return true;
        }

        public double Value(double v)
        {
            if (v < _knots[0] || v > _knots[_n]) throw new ApplicationException("");
            var i = Array.BinarySearch(_knots, v);
            if (i < 0) i = -i - 2;
            //This will handle the case where v is the last knot value
            //There are only n-1 polynomials, so if v is the last knot
            //then we will use the last polynomial to calculate the value.
            if (i >= _polynomials.Length) i--;
            return _polynomials[i].Value(v - _knots[i]);
        }
    }
}