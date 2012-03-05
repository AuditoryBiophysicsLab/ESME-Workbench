using System.Collections.Generic;
using System.Data.Common;
using System.Data.Entity;

namespace ESME.Database
{
    public class ScenarioContext : DbContext
    {
        public ScenarioContext(DbConnection connection, bool contextOwnsConnection, IDatabaseInitializer<ScenarioContext> initializer)
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

    public class Scenario
    {
        public int ScenarioID { get; set; }
        public string BuilderVersion { get; set; }
        public string EventName { get; set; }
        public DbDateTime CreationTime { get; set; }
        public string Description { get; set; }
        public string AnalystName { get; set; }
        public DbTimeSpan StartTime { get; set; }
        public DbTimeSpan Duration { get; set; }
        public string SimAreaName { get; set; }
        public string TimeFrame { get; set; }

        public virtual ICollection<ScenarioPlatform> ScenarioPlatforms { get; set; }
        public virtual ICollection<ScenarioSpecies> ScenarioSpecies { get; set; }
    }

    public class ScenarioPlatform : Platform
    {
        public int ScenarioPlatformID { get; set; }
        public string Description { get; set; }
        public bool Launches { get; set; }
        public bool Tows { get; set; }
        public int RepeatCount { get; set; }
        public virtual Scenario Scenario { get; set; }
        public virtual TrackDefinition TrackDefinition { get; set; }
        public virtual ICollection<ScenarioSource> ScenarioSources { get; set; }
    }

    public class ScenarioSource : Source
    {
        public int ScenarioSourceID { get; set; }
        public string Description { get; set; }

        public virtual ScenarioPlatform ScenarioPlatform { get; set; }
        public virtual ICollection<ScenarioMode> ScenarioModes { get; set; }
    }

    public class ScenarioMode : Mode
    {
        public int ScenarioModeID { get; set; }
        public string State { get; set; }
        public string Linked { get; set; }
        public int ClusterCount { get; set; }
    }

    public enum TrackType
    {
        Stationary = 0,
        StraightLine = 1,
        PerimeterBounce = 2,
    }

    public class Perimeter
    {
        public int PerimeterID { get; set; }
        public string Name { get; set; }
        public virtual ICollection<PerimeterCoordinate> PerimeterCoordinates { get; set; }
    }

    public class PerimeterCoordinate
    {
        public int PerimeterCoordinateID { get; set; }
        public DbGeo Geo { get; set; }

        public virtual Perimeter Perimeter { get; set; }
    }

    public class TrackDefinition
    {
        public int TrackDefinitionID { get; set; }
        public int TrackType { get; set; }
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
        public string LimitFileName { get; set; }

        public virtual Perimeter Perimeter { get; set; }
    }

    public class ScenarioSpecies
    {
        public int ScenarioSpeciesID { get; set; }
        public string SpeciesFile { get; set; }
        public string Name { get; set; }

        public virtual Scenario Scenario { get; set; }
        public virtual ICollection<AnimatLocation> AnimatLocations { get; set; }
    }

    public class AnimatLocation
    {
        public int AnimatLocationID { get; set; }
        public DbGeo Geo { get; set; }
        public float Depth { get; set; }

        public virtual ScenarioSpecies ScenarioSpecies { get; set; }
    }
}
