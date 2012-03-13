using System.Collections.Generic;
using ESME.Database;

namespace ESME.Locations
{
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

        public virtual Location Location { get; set; }

        public virtual ICollection<Platform> ScenarioPlatforms { get; set; }
        public virtual ICollection<ScenarioSpecies> ScenarioSpecies { get; set; }
    }

    public class Platform 
    {
        public int PlatformID { get; set; }
        public string Description { get; set; }
        public bool Launches { get; set; }
        public bool Tows { get; set; }
        public int RepeatCount { get; set; }

        // Copied from the PSM Platform
        public int PSMPlatformID { get; set; }
        public string PlatformName { get; set; }
        public string PlatformType { get; set; }

        public virtual Scenario Scenario { get; set; }
        public virtual TrackDefinition TrackDefinition { get; set; }
        public virtual ICollection<Source> Sources { get; set; }
    }

    public class Source
    {
        public int SourceID { get; set; }
        public string Description { get; set; }
        public int PSMSourceID { get; set; }
        public string SourceName { get; set; }
        public string SourceType { get; set; }

        public virtual Platform Platform { get; set; }
        public virtual ICollection<Mode> Modes { get; set; }
    }

    public class Mode
    {
        public int ModeID { get; set; }
        public int PSMModeID { get; set; }
        public string State { get; set; }
        public string Linked { get; set; }
        public int ClusterCount { get; set; }
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
    }

    public class TrackDefinition
    {
        public int TrackDefinitionID { get; set; }
        public DbTrackType TrackType { get; set; }
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
