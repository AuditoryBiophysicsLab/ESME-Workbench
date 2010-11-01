using System;
using ESME.NEMO;

namespace ESME.Platform
{
    public class ActiveSourceState
    {
        /// <summary>
        ///   Index into the active modes table
        /// </summary>
        public int ActiveModeID { get; internal set; }

        /// <summary>
        /// A reference to the active mode
        /// </summary>
        public NemoMode NemoMode { get; internal set; }

        /// <summary>
        /// A TimeSpan value indicating how long the mode is active during the current time step.
        /// If the mode is active multiple times during the current step, this value will reflect the
        /// total amount of active time.
        /// </summary>
        public TimeSpan ActiveTime { get; internal set; }

        /// <summary>
        ///   Source level in dB SPL, re: 1 uPa
        /// </summary>
        public float SourceLevel { get; internal set; }

        /// <summary>
        ///   Reference to the psmID of the mode we're referring to.  Intended to be used in joins.
        /// </summary>
        public string PsmId { get; internal set; }

        /// <summary>
        ///   Simulation Time at the beginning of the current timestep.
        /// </summary>
        public DateTime SimulationTime { get; internal set; }

        /// <summary>
        ///   1.0 means the source is active for the entire time step, or
        ///   less than 1.0 means it's active for less than the whole time step
        /// </summary>
        public float ActiveFraction { get; internal set; }

        /// <summary>
        ///   Look direction RELATIVE TO PLATFORM'S COURSE for this source
        /// </summary>
        public float RelativeHeading { get; internal set; }
    }
}