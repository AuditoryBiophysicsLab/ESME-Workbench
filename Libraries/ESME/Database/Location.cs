using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Data.Entity;
using System.Linq;
using System.Text;

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
}
