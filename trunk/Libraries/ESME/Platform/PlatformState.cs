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
        public float Course { get; internal set; }

        /// <summary>
        /// Current platform speed in meters per second
        /// </summary>
        public float Speed { get; internal set; }

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
            Speed = float.NaN;
            Course = float.NaN;
        }
    }

    public class PlatformStates : List<PlatformState>
    {
        public PlatformState this[DateTime simulationTime]
        {
            get
            {
                foreach (var platformState in this.Where(platformState => platformState.SimulationTime >= simulationTime)) 
                    return platformState;
                throw new IndexOutOfRangeException(string.Format("PlatformStates: Requested time index {0} not found", simulationTime));
            }
        }
    }
}
