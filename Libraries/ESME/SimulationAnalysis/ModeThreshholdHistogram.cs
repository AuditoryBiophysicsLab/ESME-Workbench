using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using ESME.Scenarios;
using ESME.Simulator;
using HRC.Aspects;

namespace ESME.SimulationAnalysis
{
    public class ExposureDictionary<TKey1, TKey2>
        where TKey2 : class
        where TKey1 : class
    {
        public ConcurrentDictionary<TKey1, ConcurrentDictionary<TKey2, List<HistogramBins>>> Exposures { get; private set; }
        public Func<Actor, ActorExposureRecord, TKey1> Filter1 { get; set; }
        public Func<Actor, ActorExposureRecord, TKey2> Filter2 { get; set; }

        public ExposureDictionary()
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
    }

    /// <summary>
    /// Accumulates binned data.  x-axis: species.  legend: mode.
    /// </summary>
    public class ModeThreshholdHistogram : ITimeStepProcessor
    {
        Simulation Simulation { get; set; }
        [Initialize] public ExposureDictionary<ScenarioSpecies, Mode> ModeExposureDictionary { get; set; }

        public ModeThreshholdHistogram()
        {
            ModeExposureDictionary.Filter1 = (actor, exposureRecord) => actor.AnimatLocation != null ? actor.AnimatLocation.ScenarioSpecies : null;
            ModeExposureDictionary.Filter2 = (actor, exposureRecord) => exposureRecord.Mode;
        }

        public void Process(SimulationTimeStepRecord record)
        {
            var actors = Simulation.GetActors();
            for (var i = 0; i < record.ActorPositionRecords.Count; i++)
            {
                foreach (var t in record.ActorPositionRecords[actors[i].ID].Exposures) ModeExposureDictionary.Expose(actors[i], t);
            }
        }

        public void Initialize(Simulation simulation) { Simulation = simulation; }

#if falsek
        public void Serialize(string outFile)
        {
            var engine = new FileHelperEngine<SourceModeThreshholdHistogramFileRecord>();
            var records = new List<SourceModeThreshholdHistogramFileRecord>();
            for (int i = 0; i < _simulation.Scenario.ScenarioSpecies.Count; i++)
            {
                var record = new SourceModeThreshholdHistogramFileRecord
                                 {
                                     SpeciesName = _simulation.Scenario.ScenarioSpecies.ToList()[i].LatinName,
                                     ScenarioName = _simulation.Scenario.Name,
                                     LocationName = _simulation.Scenario.Location.Name,
                                     NumberOfEmitters = ModeDictionary[_simulation.Scenario.ScenarioSpecies.ToList()[i].Guid].Count,
                                     SoundEmitterData = new List<Tuple<string, int[]>>(),
                                 };
                for (int j = 0; j < record.NumberOfEmitters; j++)
                {
                    var curMode = ModeDictionary[_simulation.Scenario.ScenarioSpecies.ToList()[i].Guid].ToList()[j];
                    record.SoundEmitterData.Add(new Tuple<string, int[]>(curMode.Key.ModeName, curMode.Value.Bins));
                }
                records.Add(record);
            }
            engine.WriteFile(outFile, records);
        } 
#endif
    }

    /// <summary>
    /// Accumulates binned data.  x-axis: modes. legend: species.
    /// </summary>
    public class SpeciesThreshholdHistogram : ITimeStepProcessor
    {
        public Simulation Simulation { get; set; }
        [Initialize] public ExposureDictionary<Mode, ScenarioSpecies> SpeciesExposureDictionary { get; set; }

        public SpeciesThreshholdHistogram()
        {
            SpeciesExposureDictionary.Filter1 = (actor, expsureRecord) => expsureRecord.Mode;
            SpeciesExposureDictionary.Filter2 = (actor, exposureRecord) => actor.AnimatLocation != null ? actor.AnimatLocation.ScenarioSpecies : null;
        }
        public void Process(SimulationTimeStepRecord record)
        {
            var actors = Simulation.GetActors();
            for (var i = 0; i < record.ActorPositionRecords.Count; i++)
            {
                foreach (var t in record.ActorPositionRecords[actors[i].ID].Exposures) SpeciesExposureDictionary.Expose(actors[i], t);
            }
        }
        public void Initialize(Simulation simulation) { Simulation = simulation; }
    }
}
#if false

[DelimitedRecord(",")]
public class SourceModeThreshholdHistogramFileRecord
{
    public string SpeciesName; // the species these sources were accumulated for
    public string ScenarioName; //the scenario name
    public string LocationName;
    public int NumberOfEmitters;
    public List<Tuple<string, int[]>> SoundEmitterData;
}
#endif