using System;
using System.Collections.Generic;
using System.IO;
using ESME.Scenarios;
using ESME.Simulator;
using FileHelpers;
using HRC.Aspects;

namespace ESME.SimulationAnalysis
{
    /// <summary>
    /// Accumulates binned data.  x-axis: species.  legend: mode.
    /// </summary>
    public class ModeThreshholdHistogram : ITimeStepProcessor
    {
        public Simulation Simulation { get; set; }
        [Initialize] public BinnedExposureDictionary<ScenarioSpecies, Mode> ModeBinnedExposureDictionary { get; set; }

        public ModeThreshholdHistogram()
        {
            ModeBinnedExposureDictionary.Filter1 = (actor, exposureRecord) => actor.Species;
            ModeBinnedExposureDictionary.Filter2 = (actor, exposureRecord) => exposureRecord.Mode;
        }

        public void Process(SimulationTimeStepRecord record)
        {
            var actors = Simulation.Actors;
            for (var i = 0; i < record.ActorPositionRecords.Count; i++)
            {
                foreach (var t in record.ActorPositionRecords[actors[i].ID].Exposures) ModeBinnedExposureDictionary.Expose(actors[i], t);
            }
        }

        public void Initialize(Simulation simulation) { Simulation = simulation; }

        public void Serialize(string fileName)
        {
            var engine = new FileHelperEngine(typeof (ModeThreshholdHistogramRecord));
            
            foreach (var species in ModeBinnedExposureDictionary.Exposures.Keys)
            {
                var records = new List<ModeThreshholdHistogramRecord>();
                foreach (var mode in ModeBinnedExposureDictionary.Exposures[species].Keys)
                {
                    var data = ModeBinnedExposureDictionary.Exposures[species].Values;
                    
                    var record = new ModeThreshholdHistogramRecord {ModeName = mode.ModeName};
                    
                    records.Add(record);
                }
                var outfile = Path.Combine(Path.GetFileNameWithoutExtension(fileName),"_",species.LatinName, Path.GetExtension(fileName));
                engine.WriteFile(outfile,records);
            }
            
        }

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
        [Initialize] public BinnedExposureDictionary<Mode, ScenarioSpecies> SpeciesBinnedExposureDictionary { get; set; }

        public SpeciesThreshholdHistogram()
        {
            SpeciesBinnedExposureDictionary.Filter1 = (actor, expsureRecord) => expsureRecord.Mode;
            SpeciesBinnedExposureDictionary.Filter2 = (actor, exposureRecord) => actor.Species;
        }
        public void Process(SimulationTimeStepRecord record)
        {
            var actors = Simulation.Actors;
            for (var i = 0; i < record.ActorPositionRecords.Count; i++)
            {
                foreach (var t in record.ActorPositionRecords[actors[i].ID].Exposures) SpeciesBinnedExposureDictionary.Expose(actors[i], t);
            }
        }
        public void Initialize(Simulation simulation) { Simulation = simulation; }
    }
}


[DelimitedRecord(",")]
public class ModeThreshholdHistogramRecord
{
    public string ModeName { get; set; }
    public List<Tuple<int, int>> Bins { get; set; }
}
