using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ESME.NEMO;
using ESME.Platform;

namespace ESME.Model
{
    public partial class BehaviorModel
    {
        //todo: think about this. 
        static List<ActiveSourceState> TestCalculateActiveTimeSteps(NemoScenario nemoScenario, NemoMode nemoMode)
        {
            var results = new List<ActiveSourceState>();
            var scenarioEndTime = nemoScenario.StartTime + nemoScenario.Duration;
            var pulsesPerStep = NemoBase.SimulationStepTime.TotalSeconds / nemoMode.PulseInterval.TotalSeconds;
            //var durationPerStep = pulsesPerStep*nemoMode.PulseLength.TotalSeconds;
            var fractionalPulseCount = 0.0;
            var realPulseCount = 0;
            for (var curTime = nemoScenario.StartTime; curTime <= scenarioEndTime; curTime += NemoBase.SimulationStepTime)
            {
                fractionalPulseCount += pulsesPerStep;
                if ((int)fractionalPulseCount > realPulseCount)
                {
                    var actualPulses = (int)fractionalPulseCount - realPulseCount;
                    results.Add(new ActiveSourceState
                    {
                        NemoMode = nemoMode,
                        SimulationTime = curTime,
                        ActiveTime = new TimeSpan(0, 0, 0, 0, (int)(nemoMode.PulseLength.TotalMilliseconds * actualPulses)),
                    });
                    realPulseCount += actualPulses;
                }
            }
            return results;
        }
    }
}
