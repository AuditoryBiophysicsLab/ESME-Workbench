using System;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using System.Windows.Threading;
using ESME.Simulator;
using HRC.WPF;

namespace ESME.SimulationAnalysis
{
    /// <summary>
    /// Accumulates binned data.  x-axis: species.  legend: mode.
    /// </summary>
    public class ModeThresholdHistogram : ITimeStepProcessor
    {
        public ModeThresholdHistogram(IHistogramSource histogramSource, SimulationLog simulationLog, double lowBinValue, double binWidth, int binCount)
        {
            SimulationLog = simulationLog;
            Func<ActorExposureRecord, bool> recordFilter = record => (SimulationLog.RecordFromActorID(record.ActorID) as SpeciesNameGuid) != null;
            GroupedExposures = new GroupedExposures(histogramSource, lowBinValue, binWidth, binCount);
            GroupedExposures.GroupDescriptions.Add(new ExposureGroupDescription
            {
                GroupName = record =>
                {
                    var modeRecord = SimulationLog.RecordFromModeID(record.ModeID);
                    return string.Format("{0}:{1}", modeRecord.PlatformRecord.Name, modeRecord.Name);
                },
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
            foreach (var exposure in record.ActorPositionRecords.SelectMany(actorPositionRecord => actorPositionRecord.Exposures)) GroupedExposures.Expose(exposure);
        }

        public Task<bool> Process(SimulationTimeStepRecord record, Dispatcher dispatcher)
        {
            var completionSource = new TaskCompletionSource<bool>();
            dispatcher.InvokeIfRequired(() =>
            {
                foreach (var exposure in record.ActorPositionRecords.SelectMany(actorPositionRecord => actorPositionRecord.Exposures)) GroupedExposures.Expose(exposure);
            });
            completionSource.SetResult(true);
            return completionSource.Task;
        }

        public void DebugDisplay()
        {
            Debug.WriteLine("Histogram dump");
            GroupedExposures.DebugDisplay();
        }
    }
}
