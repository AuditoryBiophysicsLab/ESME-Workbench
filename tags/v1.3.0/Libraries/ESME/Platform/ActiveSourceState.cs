using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ESME.NEMO;

namespace ESME.Platform
{
    public class ActiveSourceState
    {
        /// <summary>
        /// Index into the active modes table
        /// </summary>
        public int ActiveModeID { get; internal set; }

        /// <summary>
        /// Source level in dB SPL, re: 1 uPa
        /// </summary>
        public float SourceLevel_dBSPL { get; internal set; }

        /// <summary>
        /// Reference to the psmID of the mode we're referring to.  Intended to be used in joins.
        /// </summary>
        public string psmID { get; internal set; }

        /// <summary>
        /// Simulation Time at the beginning of the current timestep.
        /// </summary>
        public DateTime SimulationTime { get; internal set; }

        /// <summary>
        /// 1.0 means the source is active for the entire time step, or
        /// less than 1.0 means it's active for less than the whole time step
        /// </summary>
        public float ActiveFraction { get; internal set; }

        /// <summary>
        /// Look direction RELATIVE TO PLATFORM'S COURSE for this source
        /// </summary>
        public float Heading_degrees { get; internal set; }
    }
}
