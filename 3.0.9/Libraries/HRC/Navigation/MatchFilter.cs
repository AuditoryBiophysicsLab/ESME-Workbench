namespace HRC.Navigation
{
    public interface IMatchFilter
    {
        /**
         * do inexpensive comparisons to determine if the two elements might
         * intersect.
         * 
         * @return true iff further checks might yield intersection.
         */
        bool preConsider(IGeoExtent seg, IGeoExtent region);

        /**
         * the distance (in radians) to consider two objects to be interacting, that
         * is, intersecting for our purposes.
         * 
         * @return the maximum distance to consider touching. Must be non-negative.
         */
        double getHRange();
    }

    public class MatchParametersMF : IMatchFilter
    {
        protected double hrange;

        public MatchParametersMF(IMatchParameters matchParams)
        {
            // initialize search parameters from method calls
            hrange = matchParams.horizontalRange();
        }

        public MatchParametersMF(double hrange)
        {
            this.hrange = hrange;
        }

        public double getHRange()
        {
            return hrange;
        }

        public bool preConsider(IGeoExtent seg, IGeoExtent region)
        {
            return true;
        }
    }

    public class ExactMF
    {
        public double getHRange()
        {
            return 0.0;
        }

        public bool preConsider(IGeoExtent seg, IGeoExtent region)
        {
            return true;
        }
    }
}
