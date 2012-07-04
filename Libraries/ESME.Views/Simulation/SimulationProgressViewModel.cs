using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using HRC.ViewModels;

namespace ESME.Views.Simulation
{
    public class SimulationProgressViewModel : ViewModelBase
    {
        public Simulator.Simulation Simulation { get; set; }
    }
}
