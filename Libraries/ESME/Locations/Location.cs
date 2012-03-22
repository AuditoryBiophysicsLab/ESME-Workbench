using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using ESME.Database;
using HRC.Aspects;

namespace ESME.Locations
{
    public interface IHaveGuid
    {
        Guid Guid { get; }
    }

    public class Location : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string Name { get; set; }
        public string Comments { get; set; }
        public DbGeoRect GeoRect { get; set; }
        public string StorageDirectory { get; set; }

        public virtual ICollection<EnvironmentalDataSet> EnvironmentalDataSets { get; set; }
        //public virtual ICollection<Scenario> Scenarios { get; set; }
        public virtual ICollection<LogEntry> Logs { get; set; }
    }
}
