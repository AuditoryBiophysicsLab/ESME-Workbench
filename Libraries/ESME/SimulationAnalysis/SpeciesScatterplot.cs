using ESME.Scenarios;
using ESME.Simulator;
using HRC.Aspects;

namespace ESME.SimulationAnalysis
{
    public class SpeciesScatterplot : ITimeStepProcessor
    {
        public Simulation Simulation { get; set; }
        [Initialize]
        public ScatterExposureDictionary<ScenarioSpecies> Scatterplot { get; set; }

        public SpeciesScatterplot()
        {
            Scatterplot.Filter1 = (actor, record) => actor.AnimatLocation != null ? actor.AnimatLocation.ScenarioSpecies : null;
        }


        public void Process(SimulationTimeStepRecord record)
        {
            var actors = Simulation.GetActors();
            for (var i = 0; i < record.ActorPositionRecords.Count; i++)
            {
                foreach (var t in record.ActorPositionRecords[actors[i].ID].Exposures) Scatterplot.Expose(actors[i], t);
            }
        }

        public void Initialize(Simulation simulation)
        {
            Simulation = simulation;
        }
    }

    public class AnimatScatterplot : ITimeStepProcessor
    {
        public Simulation Simulation { get; set; }
        [Initialize]
        public ScatterExposureDictionary<Actor> Scatterplot { get; set; }

        public AnimatScatterplot()
        {
            Scatterplot.Filter1 = (actor, record) => actor.AnimatLocation != null ? actor : null;
        }

        public void Process(SimulationTimeStepRecord record)
        {
            var actors = Simulation.GetActors();
            for (var i = 0; i < record.ActorPositionRecords.Count; i++)
            {
                foreach (var t in record.ActorPositionRecords[actors[i].ID].Exposures) Scatterplot.Expose(actors[i], t);
            }
        }

        public void Initialize(Simulation simulation)
        {
            Simulation = simulation;
        }
    }
}
