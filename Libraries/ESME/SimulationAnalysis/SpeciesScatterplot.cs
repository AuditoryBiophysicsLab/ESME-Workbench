using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using ESME.Scenarios;
using ESME.Simulator;
using FileHelpers;
using HRC.Aspects;

namespace ESME.SimulationAnalysis
{
    public class ScatterDictionary<TKey1>
        where TKey1 : class
        
    {
        public ConcurrentDictionary<TKey1, int> Counts { get; private set; }
        public Func<Actor, ActorExposureRecord, TKey1> Filter1 { get; set; }
        
        public ScatterDictionary()
        {
            Counts = new ConcurrentDictionary<TKey1, int>();
        }

        public void Expose(Actor actor, ActorExposureRecord exposureRecord)
        {
            var key1 = Filter1(actor, exposureRecord);
            if (key1 == null) return;
            if (exposureRecord.PeakSPL > 120)
            {
                var counts = 1;
                if (!Counts.TryAdd(key1, counts)) if (!Counts.TryGetValue(key1, out counts)) throw new ApplicationException("Could not add count.");
            }
        }

    }

    public class SpeciesScatterplot : ITimeStepProcessor
    {
        Simulation Simulation { get; set; }
        [Initialize] public ScatterDictionary<ScenarioSpecies> Scatterplot { get; set; }

        public SpeciesScatterplot() {
            Scatterplot.Filter1 = (actor, record) => actor.AnimatLocation != null ? actor.AnimatLocation.ScenarioSpecies : null;
        }

        public void Process(SimulationTimeStepRecord record) { 
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
