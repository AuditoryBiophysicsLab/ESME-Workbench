using System;
using System.Diagnostics;
using System.Linq;
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

#if false
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
    /// <summary>
    /// Accumulates binned data.  x-axis: species.  legend: mode.
    /// </summary>
    public class NewModeThresholdHistogram : ITimeStepProcessor
    {
        public NewModeThresholdHistogram(SimulationLog simulationLog)
        {
            SimulationLog = simulationLog;
            Func<ActorExposureRecord, bool> recordFilter = record => (SimulationLog.RecordFromActorID(record.ActorID) as SpeciesNameGuid) != null;
            GroupedExposures = new GroupedExposures(100, 10, 10);
            GroupedExposures.GroupDescriptions.Add(new ExposureGroupDescription
            {
                GroupName = record => SimulationLog.RecordFromModeID(record.ModeID).Name,
                RecordFilter = recordFilter,
                RecordToKey = record => SimulationLog.ModeRecords.IndexOf(SimulationLog.RecordFromModeID(record.ModeID)),
            });
            GroupedExposures.GroupDescriptions.Add(new ExposureGroupDescription
            {
                GroupName = record => SimulationLog.RecordFromActorID(record.ActorID).Name,
                RecordFilter = recordFilter,
                RecordToKey = record => SimulationLog.SpeciesRecords.IndexOf(((SpeciesNameGuid)SimulationLog.RecordFromActorID(record.ActorID))),
            });
        }
        public SimulationLog SimulationLog { get; private set; }
        public GroupedExposures GroupedExposures { get; private set; }

        public void Process(SimulationTimeStepRecord record)
        {
            foreach (var exposure in record.ActorPositionRecords.SelectMany(actorPositionRecord => actorPositionRecord.Exposures))
            {
                var speciesGuid = SimulationLog.RecordFromActorID(exposure.ActorID).Guid;
                GroupedExposures.Expose(exposure);
            }
        }

        public void DebugDisplay()
        {
            Debug.WriteLine("Histogram dump");
            GroupedExposures.DebugDisplay();
        }
    }
}
