using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Xml;
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

        string ModeNameFromActorExposureRecord(ActorExposureRecord record) { return SimulationLog.NameFromModeID(record.ModeID); }
        string SpeciesNameFromActorExposureRecord(ActorExposureRecord record)
        {
            var result = SimulationLog.RecordFromActorID(record.ActorID) as SpeciesNameGuid;
            return result == null ? null : result.Name;
        }

        public void Write(string outFile, string scenarioName,string locationName)
        {
            var s = new XmlWriterSettings
            {
                Indent = true,
                IndentChars = "\t",
            };
            using(var x = XmlWriter.Create(outFile,s))
            {
                x.WriteStartDocument();
                x.WriteStartElement("HistogramBins");
           
                    x.WriteStartElement("Scenario");
                            x.WriteElementString("Name", scenarioName);
                    x.WriteEndElement();
                    
                    x.WriteStartElement("Location");
                        x.WriteElementString("Name", locationName);
                    x.WriteEndElement();

                    ModeBinnedExposureDictionary.WriteXML(x, speciesIndex => SimulationLog.SpeciesRecords[speciesIndex].Name, modeID => SimulationLog.NameFromModeID(modeID));
                x.WriteEndElement();
                x.WriteEndDocument();
            }

#if f
            using (var writer = new StringWriter())
            {
                writer.WriteLine(scenarioName);
                writer.WriteLine(locationName);
                writer.WriteLine("species " + SimulationLog.SpeciesRecords.Count);
                writer.WriteLine(ModeBinnedExposureDictionary.Write(speciesIndex => SimulationLog.SpeciesRecords[speciesIndex].Name + ",",
                                                                 modeID => SimulationLog.NameFromModeID(modeID)));

                File.WriteAllText(outFile, writer.ToString());
            } 
#endif

        }
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
