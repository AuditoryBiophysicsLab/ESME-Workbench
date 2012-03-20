using System.IO;
using ESME.Locations;

namespace ESME.Simulator
{
    public class Simulation
    {
        public static Simulation Run(Scenario scenario, string simulationDirectory)
        {
            Directory.CreateDirectory(simulationDirectory);
            var result = new Simulation(scenario, simulationDirectory);
            return result;
        }

        Simulation(Scenario scenario, string simulationDirectory)
        {

        }
    }
}
