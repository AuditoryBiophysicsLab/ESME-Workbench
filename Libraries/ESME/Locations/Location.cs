using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Data.Common;
using System.Data.Entity;
using ESME.Database;
using ESME.NEMO.Overlay;
using ESME.Scenarios;
using HRC.Aspects;

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
        public DbSet<EnvironmentalDataSet> EnvironmentalDataSets { get; set; }
        public DbSet<LogEntry> Log { get; set; }

        public DbSet<Scenario> Scenarios { get; set; }
        public DbSet<Platform> Platforms { get; set; }
        public DbSet<Source> Sources { get; set; }
        public DbSet<Mode> Modes { get; set; }
        public DbSet<Perimeter> Perimeters { get; set; }
        public DbSet<PerimeterCoordinate> PerimeterCoordinates { get; set; }
        public DbSet<TrackDefinition> TrackDefinitions { get; set; }
        public DbSet<ScenarioSpecies> ScenarioSpecies { get; set; }
        public DbSet<AnimatLocation> AnimatLocations { get; set; }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            modelBuilder.Entity<TrackDefinition>().HasRequired(t => t.Platform)
                   .WithOptional();
        }

        public class LocationDatabaseInitializer : CreateDatabaseIfNotExists<LocationContext>
        {
            protected override void Seed(LocationContext context)
            {
                //context.Database.ExecuteSqlCommand("");
            }
        }
    }

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

    public class LogEntry : IHaveGuid
    {
        public LogEntry() {}
        public LogEntry(IHaveGuid haveGuid) { SourceGuid = haveGuid.Guid; }

        [Key, Initialize]
        public Guid Guid { get; set; }
        public DbWhoWhenWhere MessageSource { get; set; }
        public string Message { get; set; }
        public Guid SourceGuid { get; set; }

        public virtual Location Location { get; set; }
        public virtual EnvironmentalDataSet EnvironmentalDataSet { get; set; }
        public virtual Scenario Scenario { get; set; }
        public virtual Platform Platform { get; set; }
        public virtual Source Source { get; set; }
        public virtual Mode Mode { get; set; }
        public virtual TrackDefinition TrackDefinition { get; set; }
        public virtual Perimeter Perimeter { get; set; }
    }

    public class EnvironmentalDataSet : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public float Resolution { get; set; }
        public int SampleCount { get; set; }
        public long FileSize { get; set; }

        public DbTimePeriod TimePeriod { get; set; }
        
        public string FileName { get; set; }
        public DbPluginIdentifier SourcePlugin { get; set; }

        public virtual Location Location { get; set; }
        
        public virtual ICollection<LogEntry> Logs { get; set; }
    }
}
