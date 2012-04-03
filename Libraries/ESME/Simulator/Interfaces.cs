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

        public void Process(SimulationTimeStepRecord record)
        {
            var actors = _simulation.GetActors();
            var animats = from a in actors
                          where a.AnimatLocation != null
                          select a;
            // for each animat in the simulation, tally their exposure 
            
        }

        public void Initialize(Simulation simulation)
        {
            _simulation = simulation;
        }
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
            
        }
    }

    public class SourceModeThreshholdHistogramFileRecord
    {
        public string Name;
    }
}
