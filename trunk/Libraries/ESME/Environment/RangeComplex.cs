using HRC.Navigation;

namespace ESME.Environment
{
    public class RangeComplex : EarthCoordinate
    {
        public RangeComplex() { }

        internal RangeComplex(double latitude, double longitude) : base(latitude, longitude) { }

        /// <summary>
        /// Name of the sim area
        /// </summary>
        public string Name { get; internal set; }

        /// <summary>
        /// Height of the sim area, in meters
        /// </summary>
        public double Height { get; internal set; }

        /// <summary>
        /// Geoid Separation, in meters
        /// </summary>
        public double GeoidSeparation { get; internal set; }

        /// <summary>
        /// OpsLimit filename
        /// </summary>
        public string OpsLimitFile { get; internal set; }

        /// <summary>
        /// SimLimit filename
        /// </summary>
        public string SimLimitFile { get; internal set; }
    }
}