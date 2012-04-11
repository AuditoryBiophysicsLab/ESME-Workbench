using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using ESME.Database;
using ESME.Scenarios;
using HRC.Aspects;

namespace ESME.Locations
{
    public class EnvironmentalDataSet : IHaveGuid, IHaveLayerSettings
    {
        public EnvironmentalDataSet() { TimePeriod = Environment.TimePeriod.Invalid; }
        [Key, Initialize]
        public Guid Guid { get; set; }
        public float Resolution { get; set; }
        public int SampleCount { get; set; }
        public long FileSize { get; set; }

        public DbTimePeriod TimePeriod { get; set; }
        
        public string FileName { get; set; }
        public DbPluginIdentifier SourcePlugin { get; set; }

        public virtual Location Location { get; set; }
        public virtual LayerSettings LayerSettings { get; set; }

        public virtual ICollection<LogEntry> Logs { get; set; }
    }
}