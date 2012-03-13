using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Data.Common;
using System.Data.Entity;
using ESME.Database;

namespace ESME.Locations
{
    public class LocationContext : DbContext
    {
        public LocationContext(DbConnection connection, bool contextOwnsConnection)
            : base(connection, contextOwnsConnection)
        {
            Configuration.AutoDetectChangesEnabled = false;
            Configuration.ProxyCreationEnabled = false;
            Configuration.LazyLoadingEnabled = true;
            Configuration.ValidateOnSaveEnabled = true;
            System.Data.Entity.Database.SetInitializer(new LocationDatabaseInitializer());
        }

        public DbSet<Location> Locations { get; set; }
        public DbSet<LocationLogEntry> LocationLogEntries { get; set; }
        public DbSet<EnvironmentalDataSetCollection> EnvironmentalDataSetCollections { get; set; }
        public DbSet<EnvironmentalDataSetCollectionLogEntry> EnvironmentalDataSetCollectionLogEntries { get; set; }
        public DbSet<EnvironmentalDataSet> EnvironmentalDataSets { get; set; }

        public DbSet<Scenario> Scenarios { get; set; }
        public DbSet<Platform> Platforms { get; set; }
        public DbSet<Source> Sources { get; set; }
        public DbSet<Mode> Modes { get; set; }
        public DbSet<Perimeter> Perimeters { get; set; }
        public DbSet<PerimeterCoordinate> PerimeterCoordinates { get; set; }
        public DbSet<TrackDefinition> TrackDefinitions { get; set; }
        public DbSet<ScenarioSpecies> ScenarioSpecies { get; set; }
        public DbSet<AnimatLocation> AnimatLocations { get; set; }

        protected override void OnModelCreating(DbModelBuilder modelBuilder) { }

        public class LocationDatabaseInitializer : CreateDatabaseIfNotExists<LocationContext>
        {
            protected override void Seed(LocationContext context)
            {
                //context.Database.ExecuteSqlCommand("");
            }
        }
    }

    public class Location
    {
        public int LocationID { get; set; }
        public string Name { get; set; }
        public string Comments { get; set; }
        public DbGeoRect GeoRect { get; set; }
        public DbWhoWhenWhere CreationInfo { get; set; }
        public string StorageDirectory { get; set; }

        public virtual ICollection<Scenario> Scenarios { get; set; }
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
        public int EnvironmentalDataSetCollectionLogEntryID { get; set; }
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
        public int PercentCached { get; set; }
        public virtual EnvironmentalDataSetCollection EnvironmentalDataSetCollection { get; set; }
    }
}
