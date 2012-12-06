using System.Collections.Generic;
using HRC.Navigation;

namespace ESME.Behaviors
{
    public class PlatformLocation
    {
        /// <summary>
        /// The location of the platform at the specified time
        /// </summary>
        public Geo Location { get; internal set; }

        /// <summary>
        /// Current course, in degrees from true north (clockwise = positive)
        /// </summary>
        public float Course { get; internal set; }

        /// <summary>
        /// Current platform speed in meters per second
        /// </summary>
        public float Speed { get; internal set; }

        /// <summary>
        /// Depth, in meters
        /// </summary>
        public float Depth { get; internal set; }
    }

    public class PlatformStates : List<PlatformLocation>
    {
    }
}
