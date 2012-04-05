using System;
using System.Collections.Generic;
using System.Linq;
using ESME.Scenarios;
using ESME.Simulator;
using FileHelpers;

namespace ESME.SimulationAnalysis
{
    public class SourceModeThreshholdHistogram:ITimeStepProcessor
    {
        private Simulation _simulation;
        public Dictionary<Guid,Dictionary<Mode, HistogramBins>> ModeDictionary { get; private set; }
        public Dictionary<Guid,Dictionary<Source, HistogramBins>> SourceDictionary { get; private set; }
        public Dictionary<Guid,Dictionary<Platform, HistogramBins>> PlatformDictionary { get; private set; }
        public Dictionary<Guid,Dictionary<Scenario, HistogramBins>> ScenarioDictionary { get; private set; } 
        
        public void Process(SimulationTimeStepRecord record)
        {
            //the actors in the simulation at this time step.
            var actors = _simulation.GetActors();
            foreach (var species in _simulation.Scenario.ScenarioSpecies)
            {
                //get all the animats for this species
                var species1 = species;
                var speciesanimats = (from a in actors
                                      where a.AnimatLocation.ScenarioSpecies == species1
                                      select a);
                //add each species to the dictionaries
                if(!ModeDictionary.ContainsKey(species.Guid))ModeDictionary.Add(species.Guid, new Dictionary<Mode, HistogramBins>());
                if (!SourceDictionary.ContainsKey(species.Guid)) SourceDictionary.Add(species.Guid, new Dictionary<Source, HistogramBins>());
                if (!PlatformDictionary.ContainsKey(species.Guid)) PlatformDictionary.Add(species.Guid, new Dictionary<Platform, HistogramBins>());
                if (!ScenarioDictionary.ContainsKey(species.Guid)) ScenarioDictionary.Add(species.Guid, new Dictionary<Scenario, HistogramBins>());
                
                foreach (var exposure in speciesanimats.SelectMany(actor => record.ActorPositionRecords[actor.ID].Exposures))
                {
                    //add this exposure's peak SPL to the mode, source, platform, and scenario it corresponds to.
                    if (!ModeDictionary[species.Guid].ContainsKey(exposure.Mode)) { ModeDictionary[species.Guid].Add(exposure.Mode, new HistogramBins(120, 3, 10)); }
                    ModeDictionary[species.Guid][exposure.Mode].Add(exposure.PeakSPL);

                    if (!SourceDictionary[species.Guid].ContainsKey(exposure.Mode.Source)) SourceDictionary[species.Guid].Add(exposure.Mode.Source, new HistogramBins(120, 3, 10));
                    SourceDictionary[species.Guid][exposure.Mode.Source].Add(exposure.PeakSPL);

                    if (!PlatformDictionary[species.Guid].ContainsKey(exposure.Mode.Source.Platform)) PlatformDictionary[species.Guid].Add(exposure.Mode.Source.Platform, new HistogramBins(120, 3, 10));
                    PlatformDictionary[species.Guid][exposure.Mode.Source.Platform].Add(exposure.PeakSPL);

                    if (!ScenarioDictionary[species.Guid].ContainsKey(exposure.Mode.Source.Platform.Scenario)) ScenarioDictionary[species.Guid].Add(exposure.Mode.Source.Platform.Scenario, new HistogramBins(120, 3, 10));
                    ScenarioDictionary[species.Guid][exposure.Mode.Source.Platform.Scenario].Add(exposure.PeakSPL);
                }
            }
        }

        public void Initialize(Simulation simulation)
        {
            _simulation = simulation;
            ModeDictionary = new Dictionary<Guid, Dictionary<Mode, HistogramBins>>();
            SourceDictionary = new Dictionary<Guid, Dictionary<Source, HistogramBins>>();
            PlatformDictionary = new Dictionary<Guid, Dictionary<Platform, HistogramBins>>();
            ScenarioDictionary = new Dictionary<Guid, Dictionary<Scenario, HistogramBins>>();
            
        }

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
                    record.SoundEmitterData.Add(new Tuple<string, int[]>(curMode.Key.ModeName,curMode.Value.Bins));
                }
                records.Add(record);
            }
            engine.WriteFile(outFile,records);
        }
    }
}

[DelimitedRecord(",")]
public class SourceModeThreshholdHistogramFileRecord
{
    public string SpeciesName; // the species these sources were accumulated for
    public string ScenarioName; //the scenario name
    public string LocationName;
    public int NumberOfEmitters;
    public List<Tuple<string, int[]>> SoundEmitterData;
}