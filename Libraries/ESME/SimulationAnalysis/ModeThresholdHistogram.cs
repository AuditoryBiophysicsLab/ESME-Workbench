using System.Diagnostics;
using System.Linq;
using ESME.Simulator;
using HRC;
using HRC.Aspects;

namespace ESME.SimulationAnalysis
{
    /// <summary>
    /// Accumulates binned data.  x-axis: species.  legend: mode.
    /// </summary>
    public class ModeThresholdHistogram : ITimeStepProcessor
    {
        public ModeThresholdHistogram(SimulationLog simulationLog)
        {
            ModeBinnedExposureDictionary.Filter1 = SpeciesIndexFromActorExposureRecord;
            ModeBinnedExposureDictionary.Filter2 = record => record.ModeID;
            SimulationLog = simulationLog;
        }
        public SimulationLog SimulationLog { get; private set; }
        [Initialize, UsedImplicitly] public BinnedExposureDictionary ModeBinnedExposureDictionary { get; private set; }

        public void Process(SimulationTimeStepRecord record)
        {
            foreach (var exposure in record.ActorPositionRecords.SelectMany(actorPositionRecord => actorPositionRecord.Exposures)) 
                ModeBinnedExposureDictionary.Expose(exposure);
        }

        public void Display()
        {
            Debug.WriteLine("Species by Mode");
            ModeBinnedExposureDictionary.Display(speciesIndex => "Species: " + SimulationLog.SpeciesRecords[speciesIndex].Name + ",",
                                                 modeID => "Mode: " + SimulationLog.NameFromModeID(modeID));
        }

        int? SpeciesIndexFromActorExposureRecord(ActorExposureRecord record)
        {
            var result = SimulationLog.RecordFromActorID(record.ActorID) as SpeciesNameGuid;
            if (result != null) return SimulationLog.SpeciesRecords.IndexOf(result);
            return null;
        }

#if false
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

#if false
    /// <summary>
    /// Accumulates binned data.  x-axis: modes. legend: species.
    /// </summary>
    public class SpeciesThresholdHistogram : ITimeStepProcessor
    {
        public Simulation Simulation { get; set; }
        [Initialize] public BinnedExposureDictionary<Mode, ScenarioSpecies> SpeciesBinnedExposureDictionary { get; set; }

        public SpeciesThresholdHistogram(Simulation simulation)
        {
            SpeciesBinnedExposureDictionary.Filter1 = (actor, exposureRecord) => exposureRecord.Mode;
            SpeciesBinnedExposureDictionary.Filter2 = (actor, exposureRecord) => actor.Species;
            Simulation = simulation;
        }
        public void Process(SimulationTimeStepRecord record)
        {
            var actors = Simulation.Actors;
            for (var i = 0; i < record.ActorPositionRecords.Count; i++)
            {
                foreach (var t in record.ActorPositionRecords[i].Exposures) SpeciesBinnedExposureDictionary.Expose(actors[i], t);
            }
        }

        public void Display()
        {
            Debug.WriteLine("Mode by Species");
            SpeciesBinnedExposureDictionary.Display(mode => "Mode: " + mode.ModeName + ",", species => "Species: " + species.LatinName);
        }
    }
#endif
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