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
        public ModeThresholdHistogram(IHistogramSource histogramSource, SimulationLog simulationLog, double lowBinValue, double binWidth, int binCount, Func<ActorExposureRecord, bool> modeFilter = null, Func<ActorExposureRecord, bool> speciesFilter = null)
        {
            SimulationLog = simulationLog;
            if (modeFilter == null) modeFilter = record => (SimulationLog.RecordFromActorID(record.ActorID) as SpeciesNameGuid) != null;
            if (speciesFilter == null) speciesFilter = modeFilter;
            GroupedExposures = new GroupedExposures(histogramSource, lowBinValue, binWidth, binCount);
            GroupedExposures.GroupDescriptions.Add(new ExposureGroupDescription
            {
                GroupName = record =>
                {
                    var modeRecord = SimulationLog.RecordFromModeID(record.ModeID);
                    return string.Format("{0}:{1}", modeRecord.PlatformRecord.Name, modeRecord.Name);
                },
                RecordFilter = modeFilter,
                RecordToKey = record => SimulationLog.ModeRecords.IndexOf(SimulationLog.RecordFromModeID(record.ModeID)),
                RecordToGuid = record => SimulationLog.RecordFromModeID(record.ModeID).Guid,
            });
            GroupedExposures.GroupDescriptions.Add(new ExposureGroupDescription
            {
                GroupName = record => SimulationLog.RecordFromActorID(record.ActorID).Name,
                RecordFilter = speciesFilter,
                RecordToKey = record => SimulationLog.SpeciesRecords.IndexOf(((SpeciesNameGuid)SimulationLog.RecordFromActorID(record.ActorID))),
                RecordToGuid = record => SimulationLog.RecordFromActorID(record.ActorID).Guid,
            });
        }
        public SimulationLog SimulationLog { get; private set; }
        public GroupedExposures GroupedExposures { get; private set; }

        public void Process(SimulationTimeStepRecord record)
        {
            var exposures = from r in record.ActorPositionRecords
                            where r.Exposures != null
                            from e in r.Exposures
                            select e;

            foreach (var exposure in exposures) GroupedExposures.Expose(exposure);
        }

        public Task<bool> Process(SimulationTimeStepRecord record, Dispatcher dispatcher)
        {
            var completionSource = new TaskCompletionSource<bool>();
            dispatcher.InvokeIfRequired(() =>
            {
                var exposures = from r in record.ActorPositionRecords
                                where r.Exposures != null
                                from e in r.Exposures
                                select e;
                foreach (var exposure in exposures) GroupedExposures.Expose(exposure);
                //foreach (var exposure in record.ActorPositionRecords.SelectMany(actorPositionRecord => actorPositionRecord.Exposures)) GroupedExposures.Expose(exposure);
            });
            completionSource.SetResult(true);
            return completionSource.Task;
        }

        public void Process(ActorExposureRecord exposure)
        {
            GroupedExposures.Expose(exposure);
        }

        public void DebugDisplay()
        {
            Debug.WriteLine("Histogram dump");
            GroupedExposures.DebugDisplay();
        }
    }
}
