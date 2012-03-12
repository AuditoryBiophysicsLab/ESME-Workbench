using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Data.Common;
using System.Data.Entity;

namespace ESME.Database
{
    public class LocationContext : DbContext
    {
        public LocationContext(DbConnection connection, bool contextOwnsConnection, IDatabaseInitializer<LocationContext> initializer)
            : base(connection, contextOwnsConnection)
        {
            Configuration.AutoDetectChangesEnabled = false;
            Configuration.ProxyCreationEnabled = false;
            Configuration.LazyLoadingEnabled = true;
            Configuration.ValidateOnSaveEnabled = true;
            System.Data.Entity.Database.SetInitializer(initializer);
        }

        public DbSet<Location> Locations { get; set; }
        public DbSet<LocationLogEntry> LocationLogEntries { get; set; }
        public DbSet<EnvironmentalDataSetCollection> EnvironmentalDataSetCollections { get; set; }
        public DbSet<EnvironmentalDataSetCollectionLogEntry> EnvironmentalDataSetCollectionLogEntries { get; set; }
        public DbSet<EnvironmentalDataSet> EnvironmentalDataSets { get; set; }

        protected override void OnModelCreating(DbModelBuilder modelBuilder) { }
    }

    public class Location
    {
        public int LocationID { get; set; }
        public string Name { get; set; }
        public string Comments { get; set; }
        public DbGeoRect GeoRect { get; set; }
        public DbWhoWhenWhere CreationInfo { get; set; }
        public string StorageDirectory { get; set; }

        public virtual ICollection<EnvironmentalDataSetCollection> EnvironmentalDataSetCollections { get; set; }
        public virtual ICollection<LocationLogEntry> LocationLogEntries { get; set; }
    }

    [ComplexType]
    public class LogEntry
    {
        public DbWhoWhenWhere MessageSource { get; set; }
        public string Message { get; set; }
        public int? OldSourceID { get; set; }
    }

    public class LocationLogEntry
    {
        public int LocationLogEntryID { get; set; }
        public LogEntry LogEntry { get; set; }
        public virtual Location Location { get; set; }
    }

    public class EnvironmentalDataSetCollection
    {
        public int EnvironmentalDataSetCollectionID { get; set; }
        public DbPluginIdentifier SourcePlugin { get; set; }
        public DbWhoWhenWhere CreationInfo { get; set; }
        public virtual Location Location { get; set; }
        public virtual ICollection<EnvironmentalDataSet> EnvironmentalDataSets { get; set; }
        public virtual ICollection<EnvironmentalDataSetCollectionLogEntry> LogEntries { get; set; }
    }

    public class EnvironmentalDataSetCollectionLogEntry
    {
        public string EnvironmentalDataSetCollectionLogEntryID { get; set; }
        public LogEntry LogEntry { get; set; }
        public virtual EnvironmentalDataSetCollection EnvironmentalDataSetCollection { get; set; }
    }

    public class EnvironmentalDataSet
    {
        public int EnvironmentalDataSetID { get; set; }
        public float Resolution { get; set; }
        public int SampleCount { get; set; }
        public long FileSize { get; set; }
        public DbTimePeriod TimePeriod { get; set; }
        public string FileName { get; set; }
        public DbWhoWhenWhere CreationInfo { get; set; }
        public virtual EnvironmentalDataSetCollection EnvironmentalDataSetCollection { get; set; }
    }
}
