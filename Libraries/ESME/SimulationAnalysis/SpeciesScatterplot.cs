using ESME.Scenarios;
using ESME.Simulator;
using HRC.Aspects;

namespace ESME.SimulationAnalysis
{
#if false
    public class SpeciesScatterplot : ITimeStepProcessor
    {
        public Simulation Simulation { get; set; }
        [Initialize] public ScatterExposureDictionary<ScenarioSpecies> Scatterplot { get; set; }

        public SpeciesScatterplot(Simulation simulation)
        {
            Scatterplot.Filter1 = (actor, record) => actor.Species;
            Simulation = simulation;
        }

        public void Process(SimulationTimeStepRecord record)
        {
            var actors = Simulation.Actors;
            for (var i = 0; i < record.ActorPositionRecords.Count; i++)
            {
                foreach (var t in record.ActorPositionRecords[i].Exposures) Scatterplot.Expose(actors[i], t);
            }
        }
    }

    public class AnimatScatterplot : ITimeStepProcessor
    {
        public Simulation Simulation { get; set; }
        [Initialize] public ScatterExposureDictionary<Actor> Scatterplot { get; set; }

        public AnimatScatterplot(Simulation simulation)
        {
            Scatterplot.Filter1 = (actor, record) => actor.Species != null ? actor : null;
            Simulation = simulation;
        }

        public void Process(SimulationTimeStepRecord record)
        {
            var actors = Simulation.Actors;
            for (var i = 0; i < record.ActorPositionRecords.Count; i++)
            {
                foreach (var t in record.ActorPositionRecords[i].Exposures) Scatterplot.Expose(actors[i], t);
            }
        }
    }
#endif

}
