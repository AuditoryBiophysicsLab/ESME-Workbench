using System;
using System.Data.Common;
using System.Data.Entity;
using System.Data.Entity.Infrastructure;
using System.Data.Entity.SqlServerCompact;
using System.Data.Entity.Validation;
using System.Diagnostics;
using System.IO;
using System.Linq;
using ESME.Migrations;
using ESME.Scenarios;

namespace ESME.Locations
{
    [DbConfigurationType(typeof(MyDbConfiguration))]
    public class LocationContext : DbContext, IDbConnectionFactory
    {
        public LocationContext(DbConnection connection, bool contextOwnsConnection)
            : base(connection, contextOwnsConnection)
        {
            Configuration.AutoDetectChangesEnabled = true;
            Configuration.ProxyCreationEnabled = true;
            Configuration.LazyLoadingEnabled = true;
            Configuration.ValidateOnSaveEnabled = true;
            System.Data.Entity.Database.SetInitializer(new LocationDatabaseInitializer());
        }

        public bool IsModified
        {
            get
            {
                try
                {
                    //ChangeTracker.DetectChanges();
                    return ChangeTracker.Entries().Any(e => e.State != EntityState.Unchanged);
                }
                catch (Exception e)
                {
                    Debug.WriteLine("{0}: Caught (and discarded) exception in LocationContext.IsModified: {1}", DateTime.Now, e.Message);
                    return true;
                }
            }
        }

        static LocationContext Open(string filename)
        {
            var connection = new SqlCeConnectionFactory("System.Data.SqlServerCe.4.0").CreateConnection(filename);
            return new LocationContext(connection, true);
        }

        static string _databaseDirectory;
        public static string DatabaseDirectory
        {
            get
            {
                if (!string.IsNullOrEmpty(_databaseDirectory)) return _databaseDirectory;
                if (!Directory.Exists(Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database"))) Directory.CreateDirectory(Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database"));
                _databaseDirectory = Globals.AppSettings.DatabaseDirectory ?? Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database");
                return _databaseDirectory;
            }
        }

        static string _databaseFile;
        public static string DatabaseFile
        {
            get
            {
                if (!string.IsNullOrEmpty(_databaseFile)) return _databaseFile;
                if (!Directory.Exists(DatabaseDirectory)) Directory.CreateDirectory(DatabaseDirectory);
                _databaseFile = Path.Combine(DatabaseDirectory, "esme.db.sdf");
                return _databaseFile;
            }
        }

        /// <summary>
        /// This is the main application interface to the database backing code for changing the state of the database
        ///  
        /// All changes to the database context are validated and saved on successful execution.
        /// </summary>
        /// <param name="action">a lambda expression, usually of the form c => {}, where c is an instance of the LocationContext.</param>
        public static void Modify(Action<LocationContext> action)
        {
            try
            {
                using (var c = Open(DatabaseFile))
                {
                    action(c);
                    c.SaveChanges();
                }
            }
            catch (DbEntityValidationException e)
            {
                foreach (var eve in e.EntityValidationErrors)
                {
                    Debug.WriteLine("Entity of type \"{0}\" in state \"{1}\" has the following validation errors:",
                        eve.Entry.Entity.GetType().Name, eve.Entry.State);
                    foreach (var ve in eve.ValidationErrors)
                    {
                        Debug.WriteLine("- Property: \"{0}\", Error: \"{1}\"",
                            ve.PropertyName, ve.ErrorMessage);
                    }
                }
                throw;
            }
            catch (DbUpdateException dbUpdateException)
            {
                Console.WriteLine("SaveChanges caught DbUpdateException");
                Console.WriteLine("  {0}", dbUpdateException.InnerException.Message);
                if (dbUpdateException.InnerException.InnerException != null) Console.WriteLine("    {0}", dbUpdateException.InnerException.InnerException.Message);
                throw;
            }
            catch (Exception e)
            {
                Debug.WriteLine(e.Message);
                Debug.WriteLine(e.InnerException.Message);
            }
        }

        /// <summary>
        /// This is the main application interface to the database backing code for querying the current state of the database. 
        /// The state of the database will not change through the use of this method.
        /// </summary>
        /// <typeparam name="TResult">The desired return type</typeparam>
        /// <param name="action">a lambda expression usually of the form c => {}, where c is an instance of the LocationContext.</param>
        /// <returns></returns>
        public static TResult Query<TResult>(Func<LocationContext, TResult> action)
        {
            using (var c = Open(DatabaseFile))
            {
                return action(c);
            }
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
        public DbSet<ScenarioSpecies> ScenarioSpecies { get; set; }
        public DbSet<AnalysisPoint> AnalysisPoints { get; set; }
        public DbSet<Scenarios.TransmissionLoss> TransmissionLosses { get; set; }
        public DbSet<Radial> Radials { get; set; }
        public DbSet<LayerSettings> LayerSettings { get; set; }
        public DbSet<ShipTrack> ShipTracks { get; set; }
        public DbSet<Waypoint> Waypoints { get; set; }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
#if false
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
            modelBuilder.Entity<LogEntry>().HasOptional(l => l.Location);
            modelBuilder.Entity<LogEntry>().HasOptional(l => l.EnvironmentalDataSet);
            modelBuilder.Entity<LogEntry>().HasOptional(l => l.Scenario);
            modelBuilder.Entity<LogEntry>().HasOptional(l => l.Source);
            modelBuilder.Entity<LogEntry>().HasOptional(l => l.Mode);
            modelBuilder.Entity<LogEntry>().HasOptional(l => l.Perimeter);
            modelBuilder.Entity<LogEntry>().HasOptional(l => l.ScenarioSpecies);

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
#endif
            modelBuilder.Entity<Platform>()
                .HasOptional(p => p.Perimeter)  // Platform has an optional Perimeter
                .WithMany();                    // A single perimeter can be attached to many platforms
            modelBuilder.Entity<Platform>()
                .HasOptional(p => p.ShipTrack)  // Platform has an optional ShipTrack
                .WithRequired(p => p.Platform); // The ShipTrack MUST refer back to the platform
            modelBuilder.Entity<ShipTrack>()
                .HasMany(p => p.Waypoints)      // A ShipTrack can have many Waypoints
                .WithRequired()                 // Each Waypoint must refer back to the ShipTrack
                .WillCascadeOnDelete(true);
        }

        public DbConnection CreateConnection(string nameOrConnectionString) { throw new NotImplementedException(); }
    }

    internal class LocationDatabaseInitializer : MigrateDatabaseToLatestVersion<LocationContext, Configuration>
    {
    }

    public class MyDbConfiguration : DbConfiguration
    {
        public MyDbConfiguration()
        {
            SetProviderServices(SqlCeProviderServices.ProviderInvariantName, SqlCeProviderServices.Instance);
            SetDefaultConnectionFactory(new SqlCeConnectionFactory("System.Data.SqlServerCe.4.0"));
        }
    }

    public class LocationContextFactory : IDbContextFactory<LocationContext>
    {
        public LocationContext Create()
        {
            string databaseDirectory;
            if (Globals.AppSettings == null || Globals.AppSettings.DatabaseDirectory == null) databaseDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), "ESME Workbench", "Database");
            else databaseDirectory = Globals.AppSettings.DatabaseDirectory;
            var connection = new SqlCeConnectionFactory("System.Data.SqlServerCe.4.0").CreateConnection(Path.Combine(databaseDirectory, "esme.db"));
            return new LocationContext(connection, true);
        }
    }
}