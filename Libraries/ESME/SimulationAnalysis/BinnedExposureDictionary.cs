using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics;
using ESME.Simulator;

namespace ESME.SimulationAnalysis
{
    public class BinnedExposureDictionary<TKey1, TKey2>
        where TKey2 : class
        where TKey1 : class
    {
        public ConcurrentDictionary<TKey1, ConcurrentDictionary<TKey2, List<HistogramBins>>> Exposures { get; private set; }
        public Func<Actor, ActorExposureRecord, TKey1> Filter1 { get; set; }
        public Func<Actor, ActorExposureRecord, TKey2> Filter2 { get; set; }

        public BinnedExposureDictionary()
        {
            Exposures = new ConcurrentDictionary<TKey1, ConcurrentDictionary<TKey2, List<HistogramBins>>>();
        }

        public void Expose(Actor actor, ActorExposureRecord exposureRecord)
        {
            var key1 = Filter1(actor, exposureRecord);
            if (key1 == null) return;
            var key2 = Filter2(actor, exposureRecord);
            if (key2 == null) return;
            ConcurrentDictionary<TKey2, List<HistogramBins>> level2;
            if (!Exposures.TryGetValue(key1, out level2))
            {
                level2 = new ConcurrentDictionary<TKey2, List<HistogramBins>>();
                if (!Exposures.TryAdd(key1, level2)) if (!Exposures.TryGetValue(key1, out level2)) throw new ApplicationException("Could not add level two dictionary.");
            }
            List<HistogramBins> bins;
            if (!level2.TryGetValue(key2, out bins))
            {
                bins = new List<HistogramBins> { new HistogramBins(), new HistogramBins() };  //peakSPL and Energy
                if (!level2.TryAdd(key2, bins)) if (!level2.TryGetValue(key2, out bins)) throw new ApplicationException("Could not add bins.");
            }
            bins[0].Add(exposureRecord.PeakSPL);
            bins[1].Add(exposureRecord.Energy);
        }

        public void Display(Func<TKey1, string> key1NameFunc, Func<TKey2, string> key2NameFunc)
        {
            foreach (var key1 in Exposures.Keys)
                foreach (var key2 in Exposures[key1].Keys)
                {
                    Debug.WriteLine(string.Format("{0} {1}", key1NameFunc(key1), key2NameFunc(key2)));
                    Debug.WriteLine("PeakSPL bins:");
                    Exposures[key1][key2][0].Display();
                    Debug.WriteLine("");
                }
        }
    }
}