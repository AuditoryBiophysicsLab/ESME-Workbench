using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using HRC.Navigation;

namespace ESME.Platform
{
    public class PlatformState
    {
        /// <summary>
        /// The location of the platform at the specified time
        /// </summary>
        public EarthCoordinate3D Location { get; internal set; }

        /// <summary>
        /// Current course, in degrees from true north (clockwise = positive)
        /// </summary>
        public float Course_degrees { get; internal set; }

        /// <summary>
        /// Current platform speed in meters per second
        /// </summary>
        public float Speed_meters_per_second { get; internal set; }

        /// <summary>
        /// Simulated date and time for which this state is valid
        /// </summary>
        public DateTime SimulationTime { get; internal set; }

        /// <summary>
        /// A list of active sources at the current time
        /// </summary>
        public List<ActiveSourceState> ActiveSourceStates { get; internal set; }

        public PlatformState()
        {
            ActiveSourceStates = null;
            Speed_meters_per_second = float.NaN;
            Course_degrees = float.NaN;
        }
    }
}
