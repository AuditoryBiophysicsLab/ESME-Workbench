using System.Data.Common;
using System.Data.Entity;
using ESME.Database;
using ESME.Scenarios;

namespace ESME.Locations
{
    public class LocationContext : DbContext
    {
        public LocationContext(DbConnection connection, bool contextOwnsConnection)
            : base(connection, contextOwnsConnection)
        {
            Configuration.AutoDetectChangesEnabled = false;
            Configuration.ProxyCreationEnabled = true;
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
            modelBuilder.ComplexType<DbDateTime>();
            modelBuilder.ComplexType<DbGeo>();
            modelBuilder.ComplexType<DbGeoRect>();
            modelBuilder.ComplexType<DbPluginIdentifier>();
            modelBuilder.ComplexType<DbDateTime>();
            modelBuilder.ComplexType<DbTimeSpan>();
            modelBuilder.ComplexType<DbTrackType>();
            modelBuilder.ComplexType<DbWhoWhenWhere>();

            // Explicitly configuring the keys and relationships of each table
            modelBuilder.Entity<Location>().HasKey(l => l.Guid);
            modelBuilder.Entity<Location>().HasMany(l => l.EnvironmentalDataSets);
            modelBuilder.Entity<Location>().HasMany(l => l.Logs).WithOptional();

            modelBuilder.Entity<EnvironmentalDataSet>().HasKey(e => e.Guid);
            modelBuilder.Entity<EnvironmentalDataSet>().HasRequired(e => e.Location);
            modelBuilder.Entity<EnvironmentalDataSet>().HasMany(e => e.Logs).WithOptional();

            modelBuilder.Entity<LogEntry>().HasKey(l => l.Guid);
#if false
            modelBuilder.Entity<LogEntry>().HasOptional(l => l.Location);
            modelBuilder.Entity<LogEntry>().HasOptional(l => l.EnvironmentalDataSet);
            modelBuilder.Entity<LogEntry>().HasOptional(l => l.Scenario);
            modelBuilder.Entity<LogEntry>().HasOptional(l => l.Source);
            modelBuilder.Entity<LogEntry>().HasOptional(l => l.Mode);
            modelBuilder.Entity<LogEntry>().HasOptional(l => l.Perimeter);
            modelBuilder.Entity<LogEntry>().HasOptional(l => l.ScenarioSpecies);
#endif

            modelBuilder.Entity<Scenario>().HasKey(s => s.Guid);
            modelBuilder.Entity<Scenario>().HasRequired(s => s.Location);
            modelBuilder.Entity<Scenario>().HasMany(s => s.Platforms).WithRequired(p => p.Scenario);
            modelBuilder.Entity<Scenario>().HasMany(s => s.ScenarioSpecies).WithRequired(s => s.Scenario);
            modelBuilder.Entity<Scenario>().HasMany(s => s.Logs).WithOptional();
            modelBuilder.Entity<Scenario>().HasOptional(s => s.Wind);
            modelBuilder.Entity<Scenario>().HasOptional(s => s.SoundSpeed);
            modelBuilder.Entity<Scenario>().HasOptional(s => s.Sediment);
            modelBuilder.Entity<Scenario>().HasOptional(s => s.Bathymetry);

            modelBuilder.Entity<Platform>().HasKey(p => p.Guid);
            modelBuilder.Entity<Platform>().HasRequired(p => p.Scenario);
            modelBuilder.Entity<Platform>().HasRequired(p => p.TrackDefinition).WithRequiredPrincipal(t => t.Platform);
            modelBuilder.Entity<Platform>().HasMany(p => p.Sources);
            modelBuilder.Entity<Platform>().HasMany(p => p.Logs).WithOptional();

            modelBuilder.Entity<Source>().HasKey(s => s.Guid);
            modelBuilder.Entity<Source>().HasRequired(s => s.Platform);
            modelBuilder.Entity<Source>().HasMany(s => s.Modes);
            modelBuilder.Entity<Source>().HasMany(s => s.Logs).WithOptional();

            modelBuilder.Entity<Mode>().HasKey(m => m.Guid);
            modelBuilder.Entity<Mode>().HasRequired(m => m.Source);
            modelBuilder.Entity<Mode>().HasMany(m => m.Logs).WithOptional();

            modelBuilder.Entity<Perimeter>().HasKey(p => p.Guid);
            modelBuilder.Entity<Perimeter>().HasRequired(p => p.Scenario);
            modelBuilder.Entity<Perimeter>().HasMany(p => p.PerimeterCoordinates);
            modelBuilder.Entity<Perimeter>().HasMany(s => s.Logs).WithOptional();

            modelBuilder.Entity<PerimeterCoordinate>().HasKey(p => p.Guid);
            modelBuilder.Entity<PerimeterCoordinate>().HasRequired(p => p.Perimeter);

            modelBuilder.Entity<TrackDefinition>().HasKey(t => t.Guid);
            modelBuilder.Entity<TrackDefinition>().HasRequired(t => t.Platform).WithRequiredDependent(p => p.TrackDefinition);
            modelBuilder.Entity<TrackDefinition>().HasRequired(t => t.Perimeter);

            modelBuilder.Entity<ScenarioSpecies>().HasKey(s => s.Guid);
            modelBuilder.Entity<ScenarioSpecies>().HasRequired(s => s.Scenario);
            modelBuilder.Entity<ScenarioSpecies>().HasMany(s => s.AnimatLocations);
            modelBuilder.Entity<ScenarioSpecies>().HasMany(s => s.Logs).WithOptional();

            modelBuilder.Entity<AnimatLocation>().HasKey(a => a.ID);
            modelBuilder.Entity<AnimatLocation>().HasRequired(a => a.ScenarioSpecies);
        }

        public class LocationDatabaseInitializer : CreateDatabaseIfNotExists<LocationContext>
        {
            protected override void Seed(LocationContext context)
            {
                //context.Database.ExecuteSqlCommand("");
            }
        }
    }
}