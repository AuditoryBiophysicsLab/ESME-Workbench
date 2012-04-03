using System;
using System.ComponentModel.DataAnnotations;
using ESME.Database;
using ESME.Scenarios;
using HRC.Aspects;

namespace ESME.Locations
{
    public class LogEntry : IHaveGuid
    {
        public LogEntry() {}
        public LogEntry(IHaveGuid haveGuid) { SourceGuid = haveGuid.Guid; }

        [Key, Initialize]
        public Guid Guid { get; set; }
        public DbWhoWhenWhere MessageSource { get; set; }
        public string Message { get; set; }
        public Guid SourceGuid { get; set; }
#if true
        public virtual Location Location { get; set; }
        public virtual EnvironmentalDataSet EnvironmentalDataSet { get; set; }
        public virtual Scenario Scenario { get; set; }
        public virtual Platform Platform { get; set; }
        public virtual Source Source { get; set; }
        public virtual Mode Mode { get; set; }
        public virtual Perimeter Perimeter { get; set; }
        public virtual ScenarioSpecies ScenarioSpecies { get; set; }
        public virtual AnalysisPoint AnalysisPoint { get; set; }
        public virtual Scenarios.TransmissionLoss TransmissionLoss { get; set; }
        public virtual Radial Radial { get; set; }
        public virtual LevelRadius LevelRadius { get; set; }
#endif
    }
}