using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using ESME.Animats;
using ESME.Scenarios;

namespace ESME.Simulator
{
    public class Simulation
    {
        public List<Actor> GetActors() { return _database.Actors.ToList(); }
        public Scenario Scenario { get { return _database.Scenarios.First(); } }

        public static Simulation Create(Scenario scenario, string simulationDirectory)
        {
            Directory.CreateDirectory(simulationDirectory);
            var result = new Simulation(scenario, simulationDirectory);
            return result;
        }

        Simulation(Scenario scenario, string simulationDirectory)
        {
            _simulationDirectory = simulationDirectory;
            _database = SimulationContext.OpenOrCreate(Path.Combine(_simulationDirectory, "simulation.db"));
            _scenario = _database.ImportScenario(scenario);
        }

        readonly Scenario _scenario;
        readonly string _simulationDirectory;
        readonly SimulationContext _database;
        SimulationLog _log;

        public void Start(int timeStepCount, TimeSpan timeStepSize)
        {
            foreach (var platform in _scenario.Platforms) _database.Actors.Add(new Actor { Platform = platform });
            foreach (var species in _scenario.ScenarioSpecies)
            {
                var animatDataTask = new Task<AnimatFile>(() => AnimatFile.Load(species.SpeciesFile));
                animatDataTask.Start();
                animatDataTask.Wait();
                foreach (var animat in animatDataTask.Result.AnimatStartPoints)
                    _database.Actors.Add(new Actor { AnimatLocation = new AnimatLocation { Geo = animat, Depth = animat.Data } });
            }
            _database.SaveChanges();
            _log = SimulationLog.Create(Path.Combine(_simulationDirectory, "simulation.log"), timeStepCount, timeStepSize);
        }
    }
}
