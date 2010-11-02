using System;
using System.Collections.Generic;
using ESME.NEMO;
using HRC.Utility;

namespace ESME.Platform
{
#if false
    public class ActiveMode
    {
        public ActiveMode()
        {
            NemoMode = null;
            ActiveModeID = -1;
        }

        public NemoMode NemoMode { get; internal set; }
        public int ActiveModeID { get; internal set; }

        internal bool Contains(DateTime simulationTime)
        {
            if (NemoMode == null)
                throw new NullReferenceException("ActiveMode.Contains: NemoMode is null");

            return NemoMode.Contains(simulationTime);
        }

        internal IEnumerable<ActiveSourceState> TimeSeries(TimeSpan timeStep)
        {
            var timestepValue = timeStep;
            var repRate = NemoMode.PulseInterval;
            var pulseLength = NemoMode.PulseLength;
            var sourceLevel = NemoMode.SourceLevel;
            var psmid = NemoMode.PSMId;
            var beamAngle = NemoMode.RelativeBeamAngle;

            var remainingPulseLength = pulseLength;
            var remainingRepRate = repRate;
            var remainingTimeStep = timestepValue;
            if (NemoMode == null)
                throw new NullReferenceException("ActiveMode.Contains: NemoMode is null");
            for (var curSimTime = NemoMode.StartTime; curSimTime < NemoMode.EndTime; curSimTime += timestepValue)
            {
                float currentSourceLevel = 0;
                double activeFrac = 0;
                if (remainingPulseLength.Milliseconds > 0)
                {
                    activeFrac = remainingPulseLength > remainingTimeStep ? 1 : remainingPulseLength.DivideBy(remainingTimeStep);
                    remainingPulseLength -= remainingTimeStep;
                    currentSourceLevel = sourceLevel;
                }
                yield return new ActiveSourceState
                                 {
                                     ActiveModeID = ActiveModeID,
                                     SourceLevel = currentSourceLevel,
                                     PsmId = psmid,
                                     SimulationTime = curSimTime,
                                     RelativeHeading = beamAngle,
                                     ActiveFraction = (float) activeFrac
                                     // Add beam width and radius
                                 };
                remainingRepRate -= remainingTimeStep;
                remainingTimeStep = timestepValue;

                // For now, assume that if there's less than one timestep remaining in the repetition rate, 
                // the sonar is not active
                if (remainingRepRate < timeStep)
                {
                    remainingTimeStep -= remainingRepRate;
                    remainingRepRate = repRate;
                    remainingPulseLength = pulseLength;
                }
            }
            yield break;
        }
    }
#endif
}