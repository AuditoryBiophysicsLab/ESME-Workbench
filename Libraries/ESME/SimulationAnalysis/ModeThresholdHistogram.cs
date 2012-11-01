using System;
using System.Diagnostics;
using System.Linq;
using System.Xml;
using ESME.Simulator;

namespace ESME.SimulationAnalysis
{
    /// <summary>
    /// Accumulates binned data.  x-axis: species.  legend: mode.
    /// </summary>
    public class ModeThresholdHistogram : ITimeStepProcessor
    {
        public ModeThresholdHistogram(IHistogramSource histogramSource, SimulationLog simulationLog)
        {
            SimulationLog = simulationLog;
            Func<ActorExposureRecord, bool> recordFilter = record => (SimulationLog.RecordFromActorID(record.ActorID) as SpeciesNameGuid) != null;
            GroupedExposures = new GroupedExposures(histogramSource, 100, 10, 10);
            GroupedExposures.GroupDescriptions.Add(new ExposureGroupDescription
            {
                GroupName = record => SimulationLog.RecordFromModeID(record.ModeID).Name,
                RecordFilter = recordFilter,
                RecordToKey = record => SimulationLog.ModeRecords.IndexOf(SimulationLog.RecordFromModeID(record.ModeID)),
                RecordToGuid = record => SimulationLog.RecordFromModeID(record.ModeID).Guid,
            });
            GroupedExposures.GroupDescriptions.Add(new ExposureGroupDescription
            {
                GroupName = record => SimulationLog.RecordFromActorID(record.ActorID).Name,
                RecordFilter = recordFilter,
                RecordToKey = record => SimulationLog.SpeciesRecords.IndexOf(((SpeciesNameGuid)SimulationLog.RecordFromActorID(record.ActorID))),
                RecordToGuid = record => SimulationLog.RecordFromActorID(record.ActorID).Guid,
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
