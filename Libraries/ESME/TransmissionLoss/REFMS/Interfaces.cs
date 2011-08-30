namespace ESME.TransmissionLoss.REFMS
{
    /**
 * Provides piecewise cubic Hermite Interpolating polynomial (pchip) spline interpolator.
 * <p>
 * PCHIP has no overshoots and less oscillation than natural splines if the data are not smooth.
 * The slopes at the x[i] are chosen in such a way that P(x) preserves the shape of the data and respects
 * monotonicity. This means that, on intervals where the data are monotonic, so is P(x) and at points
 * where the data has a local extremum, so does P(x).
 * 
 * See {@link http://en.wikipedia.org/wiki/Cubic_Hermite_spline} for more information.
 * 
 * @author Kai Habel, F. N. Fritsch, Jay Repko
 */

    public interface IUnivariateRealInterpolator
    {
        IUnivariateRealFunction Interpolate(double[] xval, double[] yval);
    }

    public interface IUnivariateRealFunction
    {
        double Value(double x);
    }

    public interface IDifferentiableUnivariateRealFunction : IUnivariateRealFunction
    {
        IUnivariateRealFunction Derivative();
    }
}