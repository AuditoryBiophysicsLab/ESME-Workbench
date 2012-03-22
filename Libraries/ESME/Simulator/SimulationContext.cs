using System;
using System.Data.Common;
using System.Data.Entity;
using System.Linq;
using System.Transactions;
using Devart.Data.SQLite;
using ESME.Database;
using ESME.Locations;
using ESME.Scenarios;
using HRC.Navigation;

namespace ESME.Simulator
{
    public class SimulationContext : DbContext
    {
        public static SimulationContext OpenOrCreate(string filename)
        {
            var connectionStringBuilder = new SQLiteConnectionStringBuilder
            {
                FailIfMissing = false,
                DataSource = filename,
                BinaryGUID = true,
            };
            DbConnection connection = new SQLiteConnection(connectionStringBuilder.ToString());
            return new SimulationContext(connection, true);
        }

        public SimulationContext(DbConnection connection, bool contextOwnsConnection)
            : base(connection, contextOwnsConnection)
        {
            Configuration.AutoDetectChangesEnabled = false;
            Configuration.ProxyCreationEnabled = true;
            Configuration.LazyLoadingEnabled = true;
            Configuration.ValidateOnSaveEnabled = true;
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.Workarounds.IgnoreSchemaName = true;
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.DmlOptions.BatchUpdates.Enabled = true;
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.DmlOptions.BatchUpdates.BatchSize = 30;
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.DmlOptions.BatchUpdates.AsynchronousBatch = true;
            System.Data.Entity.Database.SetInitializer(new LocationContext.LocationDatabaseInitializer());
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

        public DbSet<Actor> Actors { get; set; }

        public Scenario ImportScenario(Scenario scenario)
        {
            var newLocation = new Location
            {
                Name = scenario.Location.Name,
                Comments = scenario.Location.Comments,
                GeoRect = new GeoRect(scenario.Location.GeoRect),
                StorageDirectory = scenario.Location.StorageDirectory,
            };
            Locations.Add(newLocation);

            EnvironmentalDataSet wind = null;
            EnvironmentalDataSet soundSpeed = null;
            EnvironmentalDataSet sediment = null;
            EnvironmentalDataSet bathymetry = null;
            if (scenario.Wind != null)
            {
                wind = new EnvironmentalDataSet
                {
                    FileName = scenario.Wind.FileName,
                    Resolution = scenario.Wind.Resolution,
                    TimePeriod = scenario.Wind.TimePeriod,
                    Location = newLocation,
                    SourcePlugin = scenario.Wind.SourcePlugin,
                };
                EnvironmentalDataSets.Add(wind);
            }

            if (scenario.SoundSpeed != null)
            {
                soundSpeed = new EnvironmentalDataSet
                {
                    FileName = scenario.SoundSpeed.FileName,
                    Resolution = scenario.SoundSpeed.Resolution,
                    TimePeriod = scenario.SoundSpeed.TimePeriod,
                    Location = newLocation,
                    SourcePlugin = scenario.SoundSpeed.SourcePlugin,
                };
                EnvironmentalDataSets.Add(soundSpeed);
            }

            if (scenario.Sediment != null)
            {
                sediment = new EnvironmentalDataSet
                {
                    FileName = scenario.Sediment.FileName,
                    Resolution = scenario.Sediment.Resolution,
                    TimePeriod = scenario.Sediment.TimePeriod,
                    Location = newLocation,
                    SourcePlugin = scenario.Sediment.SourcePlugin,
                };
                EnvironmentalDataSets.Add(sediment);
            }

            if (scenario.Bathymetry != null)
            {
                bathymetry = new EnvironmentalDataSet
                {
                    FileName = scenario.Bathymetry.FileName,
                    Resolution = scenario.Bathymetry.Resolution,
                    TimePeriod = scenario.Bathymetry.TimePeriod,
                    Location = newLocation,
                    SourcePlugin = scenario.Bathymetry.SourcePlugin,
                };
                EnvironmentalDataSets.Add(bathymetry);
            }

            var newScenario = new Scenario
            {
                Name = scenario.Name,
                Comments = scenario.Comments,
                StartTime = scenario.StartTime,
                Duration = new TimeSpan(scenario.Duration.Ticks),
                TimePeriod = scenario.TimePeriod,
                Location = newLocation,
                Wind = wind,
                SoundSpeed = soundSpeed,
                Sediment = sediment,
                Bathymetry = bathymetry,
            };
            Scenarios.Add(newScenario);

            foreach (var platform in scenario.Platforms)
            {
                var newPlatform = new Platform
                {
                    Description = platform.Description,
                    Guid = platform.Guid,
                    Launches = platform.Launches,
                    PSMPlatformGuid = platform.PSMPlatformGuid,
                    PlatformName = platform.PlatformName,
                    PlatformType = platform.PlatformType,
                    RepeatCount = platform.RepeatCount,
                    Scenario = newScenario,
                    Tows = platform.Tows,
                };
                Platforms.Add(newPlatform);
                foreach (var source in platform.Sources)
                {
                    var newSource = new Source
                    {
                        Guid = source.Guid,
                        PSMSourceGuid = source.PSMSourceGuid,
                        Platform = newPlatform,
                        SourceName = source.SourceName,
                        SourceType = source.SourceType
                    };
                    Sources.Add(newSource);
                    foreach (var mode in source.Modes)
                    {
                        var newMode = new Mode
                        {
                            ActiveTime = mode.ActiveTime,
                            DepressionElevationAngle = mode.DepressionElevationAngle,
                            Depth = mode.Depth,
                            Guid = mode.Guid,
                            HighFrequency = mode.HighFrequency,
                            LowFrequency = mode.LowFrequency,
                            HorizontalBeamWidth = mode.HorizontalBeamWidth,
                            MaxPropagationRadius = mode.MaxPropagationRadius,
                            ModeName = mode.ModeName,
                            ModeType = mode.ModeType,
                            PSMModeGuid = mode.PSMModeGuid,
                            PulseInterval = mode.PulseInterval,
                            PulseLength = mode.PulseLength,
                            RelativeBeamAngle = mode.RelativeBeamAngle,
                            SourceLevel = mode.SourceLevel,
                            VerticalBeamWidth = mode.VerticalBeamWidth,
                            Source = newSource,
                        };
                        Modes.Add(newMode);
                    }
                }
                Perimeter perimeter = null;
                if (platform.TrackDefinition != null)
                {
                    if (platform.TrackDefinition.Perimeter != null)
                    {
                        perimeter = (from p in Perimeters
                                     where p.Guid.Equals(platform.TrackDefinition.Perimeter.Guid)
                                     select p).FirstOrDefault();
                        if (perimeter == null)
                        {
                            perimeter = new Perimeter
                            {
                                Guid = platform.TrackDefinition.Perimeter.Guid,
                                Name = platform.TrackDefinition.Perimeter.Name,
                                Scenario = newScenario,
                            };
                            Perimeters.Add(perimeter);
                            if (platform.TrackDefinition.Perimeter.PerimeterCoordinates != null)
                            {
                                foreach (var coordinate in platform.TrackDefinition.Perimeter.PerimeterCoordinates)
                                    PerimeterCoordinates.Add(new PerimeterCoordinate
                                    {
                                        Geo = new Geo(coordinate.Geo),
                                        Order = coordinate.Order,
                                        Perimeter = perimeter,
                                    });
                            }
                        }
                    }
                    var trackDefinition = new TrackDefinition
                    {
                        Duration = platform.TrackDefinition.Duration,
                        InitialCourse = platform.TrackDefinition.InitialCourse,
                        InitialDepth = platform.TrackDefinition.InitialDepth,
                        InitialLatitude = platform.TrackDefinition.InitialLatitude,
                        InitialLongitude = platform.TrackDefinition.InitialLongitude,
                        InitialSpeed = platform.TrackDefinition.InitialSpeed,
                        OpsBounds = platform.TrackDefinition.OpsBounds,
                        OpsTimes = platform.TrackDefinition.OpsTimes,
                        Random = platform.TrackDefinition.Random,
                        StartTime = platform.TrackDefinition.StartTime,
                        TrackType = platform.TrackDefinition.TrackType,
                        Perimeter = perimeter,
                    };
                    newPlatform.TrackDefinition = trackDefinition;
                    TrackDefinitions.Add(trackDefinition);
                }
            }
            foreach (var species in scenario.ScenarioSpecies)
            {
                var newSpecies = new ScenarioSpecies
                {
                    Guid = species.Guid,
                    LatinName = species.LatinName,
                    Scenario = newScenario,
                    SpeciesFile = species.SpeciesFile,
                };
                ScenarioSpecies.Add(newSpecies);
                foreach (var animat in species.AnimatLocations) AnimatLocations.Add(new AnimatLocation
                {
                    ID = animat.ID,
                    Geo = new Geo(animat.Geo),
                    Depth = animat.Depth,
                    ScenarioSpecies = newSpecies,
                });
            }
            using (var transaction = new TransactionScope())
            {
                SaveChanges();
                transaction.Complete();
            }
            return newScenario;
        }

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

            modelBuilder.Entity<Actor>().HasKey(a => a.ID);
            modelBuilder.Entity<Actor>().HasOptional(a => a.Platform).WithOptionalDependent();
            modelBuilder.Entity<Actor>().HasOptional(a => a.AnimatLocation).WithOptionalDependent();
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