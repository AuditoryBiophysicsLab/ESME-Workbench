using System;
using System.Collections.Generic;
using System.Linq;

namespace ESME.NEMO
{
    public class ActiveTimeStep
    {
        /// <summary>
        /// A TimeSpan value indicating how long the mode is active during the current time step.
        /// If the mode is active multiple times during the current step, this value will reflect the
        /// total amount of active time.
        /// </summary>
        public TimeSpan ActiveTime { get; internal set; }

        /// <summary>
        ///   Simulation Time at the beginning of the current timestep.
        /// </summary>
        public DateTime SimulationTime { get; internal set; }
    }

    public class ActiveTimeSteps : List<ActiveTimeStep>
    {
        public ActiveTimeStep this[DateTime simulationTime]
        {
            get { return this.Where(activeSourceState => activeSourceState.SimulationTime >= simulationTime).FirstOrDefault(); }
        }
    }
}