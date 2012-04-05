using System;
using System.Collections.Generic;
using ESME.Locations;
using System.Linq;
using ESME.Scenarios;
using FileHelpers;

namespace ESME.Simulator
{
    public interface ITimeStepProcessor
    {
        void Process(SimulationTimeStepRecord record);
        void Initialize(Simulation simulation);
    }

    public class AnimatScatterplot:ITimeStepProcessor
    {
        private Simulation _simulation;
        public Dictionary<int,int> Scatterplot ;
        
        public void Process(SimulationTimeStepRecord record)
        {
            var animats = from a in _simulation.GetActors()
                          where a.AnimatLocation != null && record.ActorPositionRecords[a.ID] !=null
                          select a;

            // for each animat in the simulation, tally their indidual exposure from each unique source.
            //foreach (var animat in from animat in animats from exposure in record.ActorPositionRecords[animat.ID].Exposures where exposure.PeakSPL > 120 select animat) //wat.
            foreach (var animat in animats)
            {
                foreach (var exposure in record.ActorPositionRecords[animat.ID].Exposures)
                {
                    if(exposure.PeakSPL > 120)
                    {
                        if(Scatterplot.ContainsKey(animat.ID)) Scatterplot[animat.ID] ++;
                        else Scatterplot.Add(animat.ID,1);
                    }   
                }
            }
        }

        public void Initialize(Simulation simulation)
        {
            _simulation = simulation;
            Scatterplot = new Dictionary<int, int>();
        }

        public void Serialize (string outFile)
        {
            var engine = new FileHelperEngine<AnimatScatterplotRecord>();
            var records = new List<AnimatScatterplotRecord>();
            var scatterplot = Scatterplot.ToList();
            for (int i = 0; i < Scatterplot.Count; i++)
            {
                records.Add(new AnimatScatterplotRecord
                                {
                                    AnimatID = scatterplot[i].Key,
                                    ExposureCount = scatterplot[i].Value,
                                });

            }
            engine.WriteFile(outFile,records);
        }
    }
    [DelimitedRecord(",")]
    public class AnimatScatterplotRecord
    {
        public int AnimatID;
        public int ExposureCount;
    }

    public class SourceModeThreshholdHistogram:ITimeStepProcessor
    {
        private Simulation _simulation;
        public Dictionary<Guid,Dictionary<Mode, SourceRecieverLevelBins>> ModeDictionary { get; private set; }
        public Dictionary<Guid,Dictionary<Source, SourceRecieverLevelBins>> SourceDictionary { get; private set; }
        public Dictionary<Guid,Dictionary<Platform, SourceRecieverLevelBins>> PlatformDictionary { get; private set; }
        public Dictionary<Guid,Dictionary<Scenario, SourceRecieverLevelBins>> ScenarioDictionary { get; private set; } 
        
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
                if(!ModeDictionary.ContainsKey(species.Guid))ModeDictionary.Add(species.Guid, new Dictionary<Mode, SourceRecieverLevelBins>());
                if (!SourceDictionary.ContainsKey(species.Guid)) SourceDictionary.Add(species.Guid, new Dictionary<Source, SourceRecieverLevelBins>());
                if (!PlatformDictionary.ContainsKey(species.Guid)) PlatformDictionary.Add(species.Guid, new Dictionary<Platform, SourceRecieverLevelBins>());
                if (!ScenarioDictionary.ContainsKey(species.Guid)) ScenarioDictionary.Add(species.Guid, new Dictionary<Scenario, SourceRecieverLevelBins>());
                
                foreach (var exposure in speciesanimats.SelectMany(actor => record.ActorPositionRecords[actor.ID].Exposures))
                {
                    //add this exposure's peak SPL to the mode, source, platform, and scenario it corresponds to.
                    if (!ModeDictionary[species.Guid].ContainsKey(exposure.Mode)) { ModeDictionary[species.Guid].Add(exposure.Mode, new SourceRecieverLevelBins(120, 3, 10)); }
                    ModeDictionary[species.Guid][exposure.Mode].AddExposure(exposure.PeakSPL);

                    if (!SourceDictionary[species.Guid].ContainsKey(exposure.Mode.Source)) SourceDictionary[species.Guid].Add(exposure.Mode.Source, new SourceRecieverLevelBins(120, 3, 10));
                    SourceDictionary[species.Guid][exposure.Mode.Source].AddExposure(exposure.PeakSPL);

                    if (!PlatformDictionary[species.Guid].ContainsKey(exposure.Mode.Source.Platform)) PlatformDictionary[species.Guid].Add(exposure.Mode.Source.Platform, new SourceRecieverLevelBins(120, 3, 10));
                    PlatformDictionary[species.Guid][exposure.Mode.Source.Platform].AddExposure(exposure.PeakSPL);

                    if (!ScenarioDictionary[species.Guid].ContainsKey(exposure.Mode.Source.Platform.Scenario)) ScenarioDictionary[species.Guid].Add(exposure.Mode.Source.Platform.Scenario, new SourceRecieverLevelBins(120, 3, 10));
                    ScenarioDictionary[species.Guid][exposure.Mode.Source.Platform.Scenario].AddExposure(exposure.PeakSPL);
                }
            }
        }

        public void Initialize(Simulation simulation)
        {
            _simulation = simulation;
            ModeDictionary = new Dictionary<Guid, Dictionary<Mode, SourceRecieverLevelBins>>();
            SourceDictionary = new Dictionary<Guid, Dictionary<Source, SourceRecieverLevelBins>>();
            PlatformDictionary = new Dictionary<Guid, Dictionary<Platform, SourceRecieverLevelBins>>();
            ScenarioDictionary = new Dictionary<Guid, Dictionary<Scenario, SourceRecieverLevelBins>>();
            
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
    [DelimitedRecord(",")]
    public class SourceModeThreshholdHistogramFileRecord
    {
        public string SpeciesName; // the species these sources were accumulated for
        public string ScenarioName; //the scenario name
        public string LocationName;
        public int NumberOfEmitters;
        public List<Tuple<string, int[]>> SoundEmitterData;
    }
}
