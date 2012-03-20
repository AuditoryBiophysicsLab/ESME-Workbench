using System.IO;
using ESME.Locations;

namespace ESME.Simulator
{
    public class Simulation
    {
        public static Simulation Create(Scenario scenario, string simulationDirectory)
        {
            Directory.CreateDirectory(simulationDirectory);
            var result = new Simulation(scenario, simulationDirectory);
            return result;
        }

        Simulation(Scenario scenario, string simulationDirectory)
        {
            _scenario = scenario;
            _simulationDirectory = simulationDirectory;
            _database = SimulationContext.OpenOrCreate(Path.Combine(_simulationDirectory, "simulation.db"));
            _database.ImportScenario(_scenario);
        }

        readonly Scenario _scenario;
        readonly string _simulationDirectory;
        readonly SimulationContext _database;
    }
}
