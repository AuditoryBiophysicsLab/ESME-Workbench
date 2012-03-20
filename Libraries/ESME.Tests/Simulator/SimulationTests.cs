using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ESME.Simulator;
using NUnit.Framework;

namespace ESME.Tests.Simulator
{
    public class SimulationTests
    {
        [Test]
        public void CreateNewSimulation()
        {
            var foo = new Simulation(@"S:\SimulationTests", null);
        }

    }
}
