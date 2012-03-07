using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Data.Entity;
using System.Linq;
using System.Text;
using ESME.Plugins;

namespace ESME.Database
{
    public class LocationContext : DbContext
    {
        public LocationContext(DbConnection connection, bool contextOwnsConnection, IDatabaseInitializer<ScenarioContext> initializer)
            : base(connection, contextOwnsConnection)
        {
            Configuration.AutoDetectChangesEnabled = false;
            Configuration.ProxyCreationEnabled = false;
            Configuration.LazyLoadingEnabled = true;
            Configuration.ValidateOnSaveEnabled = true;
            System.Data.Entity.Database.SetInitializer(initializer);
        }

        public DbSet<Scenario> Scenarios { get; set; }
        public DbSet<ScenarioPlatform> ScenarioPlatforms { get; set; }
        public DbSet<ScenarioSource> ScenarioSources { get; set; }
        public DbSet<ScenarioMode> ScenarioModes { get; set; }
        public DbSet<Perimeter> Perimeters { get; set; }
        public DbSet<PerimeterCoordinate> PerimeterCoordinates { get; set; }
        public DbSet<TrackDefinition> TrackDefinitions { get; set; }
        public DbSet<ScenarioSpecies> ScenarioSpecies { get; set; }
        public DbSet<AnimatLocation> AnimatLocations { get; set; }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            DbDateTime.ModelInitialization(modelBuilder);
            DbTimeSpan.ModelInitialization(modelBuilder);
            DbGeo.ModelInitialization(modelBuilder);
        }
    }

    public class Location
    {
        public int LocationID { get; set; }
        public string Name { get; set; }
        public string Comments { get; set; }
        public DbGeoRect GeoRect { get; set; }
        public DbDateTime CreationDate { get; set; }
        public string CreatorName { get; set; }
        public string HostName { get; set; }

        public virtual ICollection<LogEntry<Location>> LogEntries { get; set; }
    }

    public class LogEntry<T>
    {
        public string ActivityLogID { get; set; }
        public DbDateTime Timestamp { get; set; }
        public string Message { get; set; }
        public int? OldSourceID { get; set; }

        public virtual T Source { get; set; }
    }

    public class EnvironmentDataSet
    {
        public int EnvironmentDataSetID { get; set; }
        public virtual Location Location { get; set; }
    }
}
