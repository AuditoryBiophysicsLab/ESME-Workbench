using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESME.Scenarios;

namespace ESME.Simulator
{
    public class Simulation
    {
        public List<Actor> GetActors() { return _database.Actors.ToList(); }

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
        SimulationLog _log;

        public void Start(int timeStepCount, TimeSpan timeStepSize)
        {
            foreach (var platform in _scenario.Platforms) _database.Actors.Add(new Actor { Platform = platform });
            foreach (var species in _scenario.Species) 
                foreach (var animat in species.AnimatLocations)
                    _database.Actors.Add(new Actor { Animat = animat });
            _database.SaveChanges();
            _log = SimulationLog.Create(Path.Combine(_simulationDirectory, "simulation.log"), timeStepCount, timeStepSize);
        }
    }
}
