using System;
using System.Collections.Concurrent;
using ESME.Simulator;

namespace ESME.SimulationAnalysis
{
    public class ScatterExposureDictionary<TKey1>
        where TKey1 : class
    {
        public ConcurrentDictionary<TKey1, int> Counts { get; private set; }
        public Func<Actor, ActorExposureRecord, TKey1> Filter1 { get; set; }

        public ScatterExposureDictionary()
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
}