using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Data.Common;
using System.Data.Entity;
using HRC.Navigation;

namespace ESME.Databases
{
    public class ScenarioContext : DbContext
    {
        public ScenarioContext(DbConnection connection, bool contextOwnsConnection, IDatabaseInitializer<ScenarioContext> initializer)
            : base(connection, contextOwnsConnection) { Database.SetInitializer(initializer); }

        public DbSet<Scenario> Scenarios { get; set; }
        public DbSet<ScenarioPlatform> ScenarioPlatforms { get; set; }
        public DbSet<ScenarioSource> ScenarioSources { get; set; }
        public DbSet<ScenarioMode> ScenarioModes { get; set; }
        public DbSet<Perimeter> Perimeters { get; set; }
        public DbSet<PerimeterCoordinate> PerimeterCoordinates { get; set; }
        public DbSet<TrackDefinition> TrackDefinitions { get; set; }
        public DbSet<ScenarioSpecies> ScenarioSpecies { get; set; }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            DatabaseDateTime.ModelInitialization(modelBuilder);
            DatabaseTimeSpan.ModelInitialization(modelBuilder);
            DatabaseGeo.ModelInitialization(modelBuilder);
        }
    }

    [ComplexType]
    public class DatabaseDateTime
    {
        public DatabaseDateTime(DateTime dateTime) { Ticks = dateTime.Ticks; }
        public static implicit operator DatabaseDateTime(DateTime dateTime) { return new DatabaseDateTime(dateTime); }
        public static implicit operator DateTime(DatabaseDateTime databaseDateTime) { return new DateTime(databaseDateTime.Ticks); }
        public long Ticks { get; set; }

        public DateTime DateTime
        {
            get { return new DateTime(Ticks); }
            set { Ticks = value.Ticks; }
        }

        internal static void ModelInitialization(DbModelBuilder modelBuilder)
        {
            modelBuilder.ComplexType<DatabaseDateTime>()
                .Ignore(p => p.DateTime);
        }
    }

    [ComplexType]
    public class DatabaseTimeSpan
    {
        public DatabaseTimeSpan(DateTime dateTime) { Ticks = dateTime.Ticks; }
        public DatabaseTimeSpan(TimeSpan timeSpan) { Ticks = timeSpan.Ticks; }
        public static implicit operator DatabaseTimeSpan(TimeSpan timeSpan) { return new DatabaseTimeSpan(timeSpan); }
        public static implicit operator TimeSpan(DatabaseTimeSpan databaseTimeSpan) { return new TimeSpan(databaseTimeSpan.Ticks); }
        public long Ticks { get; set; }

        public TimeSpan TimeSpan
        {
            get { return new TimeSpan(Ticks); }
            set { Ticks = value.Ticks; }
        }

        internal static void ModelInitialization(DbModelBuilder modelBuilder)
        {
            modelBuilder.ComplexType<DatabaseTimeSpan>()
                .Ignore(p => p.TimeSpan);
        }
    }

    [ComplexType]
    public class DatabaseGeo
    {
        public DatabaseGeo(Geo geo) { _geo = new Geo(geo); }

        public static implicit operator DatabaseGeo(Geo geo) { return new DatabaseGeo(geo); }
        public static implicit operator Geo(DatabaseGeo databaseGeo) { return new Geo(databaseGeo._geo); }

        public double X { get { return _geo.X; } }
        public double Y { get { return _geo.Y; } }
        public double Z { get { return _geo.Z; } }

        readonly Geo _geo = new Geo();

        public double Latitude
        {
            get { return _geo.Latitude; }
            set { _geo.Latitude = value; }
        }
        public double Longitude
        {
            get { return _geo.Longitude; }
            set { _geo.Longitude = value; }
        }
        internal static void ModelInitialization(DbModelBuilder modelBuilder)
        {
            modelBuilder.ComplexType<DatabaseGeo>()
                .Ignore(p => p.X);
            modelBuilder.ComplexType<DatabaseGeo>()
                .Ignore(p => p.Y);
            modelBuilder.ComplexType<DatabaseGeo>()
                .Ignore(p => p.Z);
        }
    }

    public class Scenario
    {
        public int ScenarioID { get; set; }
        public string BuilderVersion { get; set; }
        public string EventName { get; set; }
        public DatabaseDateTime CreationTime { get; set; }
        public string Description { get; set; }
        public string AnalystName { get; set; }
        public DatabaseTimeSpan StartTime { get; set; }
        public DatabaseTimeSpan Duration { get; set; }
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
        public int Name { get; set; }
        public virtual ICollection<PerimeterCoordinate> PerimeterCoordinates { get; set; }
    }

    public class PerimeterCoordinate
    {
        public int PerimeterCoordinateID { get; set; }
        public DatabaseGeo Geo { get; set; }

        public virtual Perimeter Perimeter { get; set; }
    }

    public class TrackDefinition
    {
        public int TrackDefinitionID { get; set; }
        public int TrackType { get; set; }
        public DatabaseTimeSpan StartTime { get; set; }
        public DatabaseTimeSpan Duration { get; set; }
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
    }
}
