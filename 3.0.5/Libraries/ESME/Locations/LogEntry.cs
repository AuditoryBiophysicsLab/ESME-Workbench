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
        public LogEntry(IHaveGuid guid) { Initialize(guid); }
        void Initialize(IHaveGuid guid)
        {
            if (guid is Location) Location = (Location)guid;
            else if (guid is EnvironmentalDataSet) EnvironmentalDataSet = (EnvironmentalDataSet)guid;
            else if (guid is Scenario) Scenario = (Scenario)guid;
            else if (guid is Platform) Platform = (Platform)guid;
            else if (guid is Source) Source = (Source)guid;
            else if (guid is Mode) Mode = (Mode)guid;
            else if (guid is Perimeter) Perimeter = (Perimeter)guid;
            else if (guid is ScenarioSpecies) ScenarioSpecies = (ScenarioSpecies)guid;
            else if (guid is AnalysisPoint) AnalysisPoint = (AnalysisPoint)guid;
            else if (guid is Scenarios.TransmissionLoss) TransmissionLoss = (Scenarios.TransmissionLoss)guid;
            else if (guid is Radial) Radial = (Radial)guid;
        }

        [Key, Initialize]
        public Guid Guid { get; set; }
        public DbWhoWhenWhere MessageSource { get; set; }
        public string Message { get; set; }
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
#endif
    }
}