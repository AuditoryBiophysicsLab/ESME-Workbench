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
        public DbSet<ScenarioSpecies> ScenarioSpecies { get; set; }

        public DbSet<Actor> Actors { get; set; }

        public Scenario ImportScenario(Scenario scenario)
        {
            var newLocation = new Location
            {
                Name = scenario.Location.Name,
                Comments = scenario.Location.Comments,
                GeoRect = new GeoRect(scenario.Location.GeoRect),
                LayerSettings = { IsChecked = true },
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
            newLocation.Scenarios.Add(scenario);

            foreach (var platform in scenario.Platforms)
            {
                Perimeter perimeter = null;
                if (platform.Perimeter != null)
                {
                    perimeter = (from p in Perimeters
                                 where p.Guid.Equals(platform.Perimeter.Guid)
                                 select p).FirstOrDefault();
                    if (perimeter == null)
                    {
                        perimeter = new Perimeter
                        {
                            Guid = platform.Perimeter.Guid,
                            Name = platform.Perimeter.Name,
                            Scenario = newScenario,
                        };
                        Perimeters.Add(perimeter);
                        if (platform.Perimeter.PerimeterCoordinates != null)
                        {
                            foreach (var coordinate in platform.Perimeter.PerimeterCoordinates)
                                PerimeterCoordinates.Add(new PerimeterCoordinate
                                {
                                    Geo = new Geo(coordinate.Geo),
                                    Order = coordinate.Order,
                                    Perimeter = perimeter,
                                });
                        }
                    }
                }
                var newPlatform = new Platform
                {
                    Description = platform.Description,
                    Guid = platform.Guid,
                    Launches = platform.Launches,
                    PSMPlatformGuid = platform.PSMPlatformGuid,
                    PlatformName = platform.PlatformName,
                    PlatformType = platform.PlatformType,
                    RepeatCount = platform.RepeatCount,
                    Course = platform.Course,
                    Depth = platform.Depth,
                    Geo = new Geo(platform.Geo),
                    Speed = platform.Speed,
                    IsRandom = platform.IsRandom,
                    //StartTime = platform.TrackDefinition.StartTime,
                    TrackType = platform.TrackType,
                    Perimeter = perimeter,
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
            }
            foreach (var analysisPoint in scenario.AnalysisPoints)
            {
                var newAnalysisPoint = new AnalysisPoint
                {
                    Guid = analysisPoint.Guid,
                    Geo = new Geo(analysisPoint.Geo),
                    Scenario = newScenario,
                };
                foreach (var transmissionLoss in analysisPoint.TransmissionLosses)
                {
                    var newTransmissionLoss = new Scenarios.TransmissionLoss
                    {
                        Guid = transmissionLoss.Guid,
                        AnalysisPoint = newAnalysisPoint,
                        Mode = Modes.Find(transmissionLoss.Mode.Guid),
                        
                    };
                }
            }
            using (var transaction = new TransactionScope())
            {
                transaction.Complete();
            }
            return newScenario;
        }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            //modelBuilder.Entity<Platform>().HasRequired(p => p.TrackDefinition).WithRequiredPrincipal(t => t.Platform);
            //modelBuilder.Entity<TrackDefinition>().HasRequired(t => t.Platform).WithRequiredDependent(p => p.TrackDefinition);
            //modelBuilder.Entity<TrackDefinition>().HasOptional(p => p.Platform).WithRequired(t => t.TrackDefinition);
            //modelBuilder.Entity<TrackDefinition>().HasRequired(t => t.Platform).WithOptional(p => p.TrackDefinition);
            modelBuilder.Entity<Platform>().HasOptional(p => p.Perimeter).WithMany();
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