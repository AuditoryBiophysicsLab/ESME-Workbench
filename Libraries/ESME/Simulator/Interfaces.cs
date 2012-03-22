using ESME.Locations;
using System.Linq;

namespace ESME.Simulator
{
    public interface ITimeStepProcessor
    {
        void Process(SimulationTimeStepRecord record);
        void Initialize(Simulation simulation);
    }

    public class SourceModeThreshholdHistogramExporter:ITimeStepProcessor
    {
        private Simulation _simulation;

        
        public void Process(SimulationTimeStepRecord record)
        {
            //the actors in the simulation at this time step.
            var actors = _simulation.GetActors();
            // for all actors in one species, accumulate their hits.
            foreach (var species in _simulation.Scenario.ScenarioSpecies)
            {
                var speciesanimats = (from a in actors
                              where a.AnimatLocation.ScenarioSpecies == species
                              select a);
                foreach (var actor in speciesanimats)
                {
                    foreach (var exposure in record.ActorPositionRecords[actor.ID].Exposures)
                    {
                        //exposure.SourceActorModeID;
                    }
                }
            }



        }

        public void Write(string fileName)
        {
            
        }

        public void Initialize(Simulation simulation)
        {
            _simulation = simulation;
        }
    }
}
