using System;
using System.Collections.Generic;
using ESME.Locations;
using FileHelpers;

namespace ESME.Simulator
{
    public interface ITimeStepProcessor
    {
        void Process(SimulationTimeStepRecord record);
        void Initialize(Simulation simulation);
    }
}
