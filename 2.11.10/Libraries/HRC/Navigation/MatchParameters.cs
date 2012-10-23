namespace HRC.Navigation
{
    public interface IMatchParameters
    {
        /**
         * return the horizontal deviation (range) to consider matching.
         * The value is in radians. 0.0 implies strict intersection. Note
         * that if a RegionIndex is being used, then this value probably
         * must be no larger than the index's margin to avoid missing
         * regions that are near index boundaries.
         */
        double horizontalRange();
    }

    public class MatchParameters : IMatchParameters
    {
        /**
         * A set of parameters that matches radius of 10 kilometers.
         */
        public static MatchParameters RouteDefault = new MatchParameters(Geo.KilometersToRadians(10));

        /**
         * A set of parameters for intersections within 10 meters.
         */
        public static MatchParameters TenMeters = new MatchParameters(Geo.KilometersToRadians(0.01));

        /**
         * A set of parameters for exact intersections.
         */
        public static MatchParameters Exact = new MatchParameters(Geo.KilometersToRadians(0));
        
        readonly double _hr;

        public MatchParameters(double hr)
        {
            _hr = hr;
        }

        public double horizontalRange()
        {
            return _hr;
        }
    }
}
