using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Data.Common;
using System.Data.Entity;
using System.IO;
using System.Linq;
using System.Threading;
using Devart.Data.SQLite;
using ESME.Behaviors;
using ESME.Database;
using ESME.Environment;
using HRC.Aspects;
using HRC.Navigation;
using NUnit.Framework;

namespace ESME.Tests.SqLite
{
    public class SqLiteTests
    {
        [Test]
        public void CreateEmptyDatabases()
        {
            var dbRootDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), "SQLite Tests");
            if (Directory.Exists(dbRootDirectory)) Directory.Delete(dbRootDirectory, true);
            for (var i = 0; i < 10; i++) if (Directory.Exists(dbRootDirectory)) Thread.Sleep(100); else break;
            Assert.IsFalse(Directory.Exists(dbRootDirectory));
            Directory.CreateDirectory(dbRootDirectory);
            Assert.IsTrue(Directory.Exists(dbRootDirectory));
            var locationDatabase = Path.Combine(dbRootDirectory, "locations.db");
            var scenarioDatabase = Path.Combine(dbRootDirectory, "scenario.db");
            var locationContext = new TestLocationContext(CreateOrConnectToDatabase(locationDatabase), true);
            Assert.AreEqual(0, locationContext.TestLocations.Count());
            var location = new TestLocation
            {
                Name = "Test location",
                Comments = "Random location comments",
                GeoRect = new GeoRect(44, 41, -69, -72),
                StorageDirectory = "StorageDirectory",
            };
            locationContext.TestLocations.Add(location);
            locationContext.SaveChanges();
            var scenarioContext = new TestScenarioContext(CreateOrConnectToDatabase(scenarioDatabase), true);
            Assert.AreEqual(0, scenarioContext.TestScenarios.Count());
            var scenario = new TestScenario
            {
                Name = "Test scenario",
                Comments = "Random scenario comments",
                StartTime = new TimeSpan(0, 12, 0, 0),
                Duration = new TimeSpan(1, 0, 0, 0),
                TimePeriod = TimePeriod.March,
                TestLocation = location,
            };
            scenarioContext.TestScenarios.Add(scenario);
            scenarioContext.SaveChanges();
        }

        [Test]
        public void ConnectAndAttachDatabases()
        {
            var dbRootDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), "SQLite Tests");
            Assert.IsTrue(Directory.Exists(dbRootDirectory));
            var locationDatabase = Path.Combine(dbRootDirectory, "locations.db");
            var scenarioDatabase = Path.Combine(dbRootDirectory, "scenario.db");
            var scenarioContext = new TestScenarioContext(ConnectAndAttachDatabase(scenarioDatabase, locationDatabase), true);
            Assert.AreEqual(1, scenarioContext.TestScenarios.Count());
            foreach (var scenario in scenarioContext.TestScenarios)
            {
                Console.WriteLine("Scenario name: {0}", scenario.Name);
                Console.WriteLine("     Comments: {0}", scenario.Comments);
                Console.WriteLine("   Start time: {0}", (TimeSpan)scenario.StartTime);
                Console.WriteLine("     Duration: {0}", (TimeSpan)scenario.Duration);
                Console.WriteLine("  Time period: {0}", scenario.TimePeriod);
                Console.WriteLine("     Location: {0}", scenario.TestLocation.Name);
            }
            //Console.WriteLine(scenarioContext.TestScenarios.First().TestLocation.Name);
        }

        public DbConnection CreateOrConnectToDatabase(string mainFilename)
        {
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.Workarounds.IgnoreSchemaName = true;
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.DmlOptions.BatchUpdates.Enabled = true;
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.DmlOptions.BatchUpdates.BatchSize = 30;
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.DmlOptions.BatchUpdates.AsynchronousBatch = true;
            var connectionStringBuilder = new SQLiteConnectionStringBuilder
            {
                FailIfMissing = false,
                DataSource = mainFilename,
                BinaryGUID = true,
            };

            return new SQLiteConnection(connectionStringBuilder.ToString());
        }

        public DbConnection ConnectAndAttachDatabase(string mainFilename, string attachedFilename)
        {
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.Workarounds.IgnoreSchemaName = true;
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.DmlOptions.BatchUpdates.Enabled = true;
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.DmlOptions.BatchUpdates.BatchSize = 30;
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.DmlOptions.BatchUpdates.AsynchronousBatch = true;
            var connectionStringBuilder = new SQLiteConnectionStringBuilder
            {
                Attach = attachedFilename,
                FailIfMissing = false,
                DataSource = mainFilename,
                BinaryGUID = true,
            };

            return new SQLiteConnection(connectionStringBuilder.ToString());
        }
    }

    public class TestLocationContext : DbContext
    {
        public TestLocationContext(DbConnection connection, bool contextOwnsConnection)
            : base(connection, contextOwnsConnection)
        {
            Configuration.AutoDetectChangesEnabled = false;
            Configuration.ProxyCreationEnabled = false;
            Configuration.LazyLoadingEnabled = true;
            Configuration.ValidateOnSaveEnabled = true;
            System.Data.Entity.Database.SetInitializer(new DatabaseInitializer());
        }

        public DbSet<TestLocation> TestLocations { get; set; }
        public DbSet<TestEnvironmentalDataSet> TestEnvironmentalDataSets { get; set; }
        public DbSet<TestLocationLogEntry> TestLocationLogs { get; set; }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            modelBuilder.Entity<TestTrackDefinition>().HasRequired(t => t.TestPlatform)
                .WithOptional();
        }

        public class DatabaseInitializer : CreateDatabaseIfNotExists<TestLocationContext>
        {
            protected override void Seed(TestLocationContext context)
            {
                //context.Database.ExecuteSqlCommand("PRAGMA journal_mode=WAL;");
            }
        }
    }

    public class TestScenarioContext : DbContext
    {
        public TestScenarioContext(DbConnection connection, bool contextOwnsConnection)
            : base(connection, contextOwnsConnection)
        {
            Configuration.AutoDetectChangesEnabled = false;
            Configuration.ProxyCreationEnabled = false;
            Configuration.LazyLoadingEnabled = true;
            Configuration.ValidateOnSaveEnabled = true;
            System.Data.Entity.Database.SetInitializer(new DatabaseInitializer());
        }

        public DbSet<TestScenarioLogEntry> TestScenarioLogs { get; set; }

        public DbSet<TestScenario> TestScenarios { get; set; }
        public DbSet<TestPlatform> TestPlatforms { get; set; }
        public DbSet<TestSource> TestSources { get; set; }
        public DbSet<TestMode> TestModes { get; set; }
        public DbSet<TestPerimeter> TestPerimeters { get; set; }
        public DbSet<TestPerimeterCoordinate> TestPerimeterCoordinates { get; set; }
        public DbSet<TestTrackDefinition> TestTrackDefinitions { get; set; }
        public DbSet<TestScenarioSpecies> TestScenarioSpecies { get; set; }
        public DbSet<TestAnimatLocation> TestAnimatLocations { get; set; }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            modelBuilder.Entity<TestTrackDefinition>().HasRequired(t => t.TestPlatform)
                               .WithOptional();
        }

        public class DatabaseInitializer : CreateDatabaseIfNotExists<TestScenarioContext>
        {
            protected override void Seed(TestScenarioContext context)
            {
                //context.Database.ExecuteSqlCommand("PRAGMA journal_mode=WAL;");
            }
        }
    }

    public class CombinedScenarioContext : DbContext
    {
        public CombinedScenarioContext(DbConnection connection, bool contextOwnsConnection)
            : base(connection, contextOwnsConnection)
        {
            Configuration.AutoDetectChangesEnabled = false;
            Configuration.ProxyCreationEnabled = false;
            Configuration.LazyLoadingEnabled = true;
            Configuration.ValidateOnSaveEnabled = true;
            System.Data.Entity.Database.SetInitializer(new DatabaseInitializer());
        }

        public DbSet<TestLocation> TestLocations { get; set; }
        public DbSet<TestEnvironmentalDataSet> TestEnvironmentalDataSets { get; set; }
        public DbSet<TestLocationLogEntry> TestLocationLogs { get; set; }

        public DbSet<TestScenarioLogEntry> TestScenarioLogs { get; set; }

        public DbSet<TestScenario> TestScenarios { get; set; }
        public DbSet<TestPlatform> TestPlatforms { get; set; }
        public DbSet<TestSource> TestSources { get; set; }
        public DbSet<TestMode> TestModes { get; set; }
        public DbSet<TestPerimeter> TestPerimeters { get; set; }
        public DbSet<TestPerimeterCoordinate> TestPerimeterCoordinates { get; set; }
        public DbSet<TestTrackDefinition> TestTrackDefinitions { get; set; }
        public DbSet<TestScenarioSpecies> TestScenarioSpecies { get; set; }
        public DbSet<TestAnimatLocation> TestAnimatLocations { get; set; }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            modelBuilder.Entity<TestTrackDefinition>().HasRequired(t => t.TestPlatform)
                               .WithOptional();
        }

        public class DatabaseInitializer : CreateDatabaseIfNotExists<TestScenarioContext>
        {
            protected override void Seed(TestScenarioContext context)
            {
                //context.Database.ExecuteSqlCommand("PRAGMA journal_mode=WAL;");
            }
        }
    }
    public interface IHaveGuid
    {
        string Guid { get; }
    }

    public class TestLocation : IHaveGuid
    {
        [Key, Initialize(IsGuid = true)]
        public string Guid { get; set; }
        public string Name { get; set; }
        public string Comments { get; set; }
        public DbGeoRect GeoRect { get; set; }
        public string StorageDirectory { get; set; }

        public virtual ICollection<TestEnvironmentalDataSet> TestEnvironmentalDataSets { get; set; }
        public virtual ICollection<TestScenario> TestScenarios { get; set; }
        public virtual ICollection<TestLocationLogEntry> TestLocationLogs { get; set; }
    }

    public class TestLocationLogEntry : IHaveGuid
    {
        public TestLocationLogEntry() { }
        public TestLocationLogEntry(IHaveGuid haveGuid) { Guid = haveGuid.Guid; }

        [Key, Initialize(IsGuid = true)]
        public string Guid { get; set; }
        public DbWhoWhenWhere MessageSource { get; set; }
        public string Message { get; set; }
        public string SourceGuid { get; set; }

        public virtual TestLocation TestLocation { get; set; }
        public virtual TestEnvironmentalDataSet TestEnvironmentalDataSet { get; set; }
    }

    public class TestEnvironmentalDataSet : IHaveGuid
    {
        [Key, Initialize(IsGuid = true)]
        public string Guid { get; set; }
        public float Resolution { get; set; }
        public int SampleCount { get; set; }
        public long FileSize { get; set; }

        [EnumDataType(typeof(TimePeriod))]
        public TimePeriod TimePeriod { get; set; }

        public string FileName { get; set; }
        public DbPluginIdentifier SourcePlugin { get; set; }

        public virtual TestLocation TestLocation { get; set; }

        public virtual ICollection<TestLocationLogEntry> TestLogs { get; set; }
    }
    
    public class TestScenario : IHaveGuid
    {
        [Key, Initialize(IsGuid = true)]
        public string Guid { get; set; }
        public string Name { get; set; }
        public string Comments { get; set; }
        public DbTimeSpan StartTime { get; set; }
        public DbTimeSpan Duration { get; set; }

        [EnumDataType(typeof(TimePeriod))]
        public TimePeriod TimePeriod { get; set; }

        public virtual TestLocation TestLocation { get; set; }

        public virtual ICollection<TestPlatform> TestPlatforms { get; set; }
        public virtual ICollection<TestScenarioLogEntry> TestScenarioLogs { get; set; }
        //public virtual ICollection<ScenarioSpecies> Species { get; set; }
    }

    public class TestScenarioLogEntry : IHaveGuid
    {
        public TestScenarioLogEntry() { }
        public TestScenarioLogEntry(IHaveGuid haveGuid) { Guid = haveGuid.Guid; }

        [Key, Initialize(IsGuid = true)]
        public string Guid { get; set; }
        public DbWhoWhenWhere MessageSource { get; set; }
        public string Message { get; set; }
        public string SourceGuid { get; set; }

        public virtual TestScenario TestScenario { get; set; }
        public virtual TestPlatform TestPlatform { get; set; }
        public virtual TestSource TestSource { get; set; }
        public virtual TestMode TestMode { get; set; }
        public virtual TestTrackDefinition TestTrackDefinition { get; set; }
        public virtual TestPerimeter TestPerimeter { get; set; }
    }


    public class TestPlatform : IHaveGuid
    {
        [Key, Initialize(IsGuid = true)]
        public string Guid { get; set; }
        public string Description { get; set; }
        public bool Launches { get; set; }
        public bool Tows { get; set; }
        public int RepeatCount { get; set; }

        // Copied from the PSM Platform
        public string PSMPlatformGuid { get; set; }
        public string PlatformName { get; set; }
        public string PlatformType { get; set; }

        public virtual TestScenario TestScenario { get; set; }
        //[Association("Platform_TrackDefinition", "Guid", "Guid")]
        public TestTrackDefinition TestTrackDefinition { get; set; }
        public virtual ICollection<TestSource> TestSources { get; set; }
    }

    public class TestSource : IHaveGuid
    {
        [Key, Initialize(IsGuid = true)]
        public string Guid { get; set; }
        public string PSMSourceGuid { get; set; }
        public string SourceName { get; set; }
        public string SourceType { get; set; }

        public virtual TestPlatform TestPlatform { get; set; }
        public virtual ICollection<TestMode> TestModes { get; set; }
    }

    public class TestMode : IHaveGuid
    {
        [Key, Initialize(IsGuid = true)]
        public string Guid { get; set; }
        public string PSMModeGuid { get; set; }
        public string ModeName { get; set; }
        public string ModeType { get; set; }
        public float? ActiveTime { get; set; }
        public float? Depth { get; set; }
        public float SourceLevel { get; set; }
        public float LowFrequency { get; set; }
        public float HighFrequency { get; set; }
        /// <summary>
        /// In seconds
        /// </summary>
        public float PulseInterval { get; set; }
        /// <summary>
        /// In milliseconds
        /// </summary>
        public float PulseLength { get; set; }
        public float HorizontalBeamWidth { get; set; }
        public float VerticalBeamWidth { get; set; }
        public float DepressionElevationAngle { get; set; }
        public float RelativeBeamAngle { get; set; }
        public float MaxPropagationRadius { get; set; }

        public virtual TestSource TestSource { get; set; }
    }

    public class TestTrackDefinition : IHaveGuid
    {
        [Key, Initialize(IsGuid = true)]
        public string Guid { get; set; }

        [EnumDataType(typeof(TrackType))]
        public TrackType TrackType { get; set; }
        public DbTimeSpan StartTime { get; set; }
        public DbTimeSpan Duration { get; set; }
        public bool Random { get; set; }
        public bool OpsBounds { get; set; }
        public bool OpsTimes { get; set; }
        public float InitialLatitude { get; set; }
        public float InitialLongitude { get; set; }
        public float InitialDepth { get; set; }
        public float InitialCourse { get; set; }
        public float InitialSpeed { get; set; }

        //[Association("Platform_TrackDefinition", "Guid", "Guid")]
        public TestPlatform TestPlatform { get; set; }
        public virtual TestPerimeter TestPerimeter { get; set; }
    }

    public class TestPerimeter : IHaveGuid
    {
        [Key, Initialize(IsGuid = true)]
        public string Guid { get; set; }
        public string Name { get; set; }
        public virtual ICollection<TestPerimeterCoordinate> TestPerimeterCoordinates { get; set; }
    }

    public class TestPerimeterCoordinate
    {
        [Key]
        public int PerimeterCoordinateID { get; set; }
        public int Order { get; set; }
        public DbGeo Geo { get; set; }

        public virtual TestPerimeter TestPerimeter { get; set; }
    }
    public class TestScenarioSpecies : IHaveGuid
    {
        [Key, Initialize(IsGuid = true)]
        public string Guid { get; set; }
        public string SpeciesFile { get; set; }
        public string Name { get; set; }

        public virtual TestScenario TestScenario { get; set; }
        public virtual ICollection<TestAnimatLocation> TestAnimatLocations { get; set; }
    }

    public class TestAnimatLocation
    {
        public int TestAnimatLocationID { get; set; }
        public DbGeo Geo { get; set; }
        public float Depth { get; set; }
    }
}
