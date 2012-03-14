using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using ESME.Behaviors;
using ESME.Database;
using ESME.Environment;
using HRC.Aspects;

namespace ESME.Locations
{
    public class Scenario : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string Name { get; set; }
        public string Comments { get; set; }
        public DbTimeSpan StartTime { get; set; }
        public DbTimeSpan Duration { get; set; }

        public DbTimePeriod TimePeriod { get; set; }

        //public virtual Location Location { get; set; }

        public virtual ICollection<Platform> Platforms { get; set; }
        public virtual ICollection<ScenarioSpecies> Species { get; set; }
    }

    public class Platform : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string Description { get; set; }
        public bool Launches { get; set; }
        public bool Tows { get; set; }
        public int RepeatCount { get; set; }

        // Copied from the PSM Platform
        public string PSMPlatformGuid { get; set; }
        public string PlatformName { get; set; }
        public string PlatformType { get; set; }

        public virtual Scenario Scenario { get; set; }
        //[Association("Platform_TrackDefinition", "Guid", "Guid")]
        public TrackDefinition TrackDefinition { get; set; }
        public virtual ICollection<Source> Sources { get; set; }
    }

    public class Source : IHaveGuid
    {
        [Key, Initialize(IsGuid = true)]
        public Guid Guid { get; set; }
        public string PSMSourceGuid { get; set; }
        public string SourceName { get; set; }
        public string SourceType { get; set; }

        public virtual Platform Platform { get; set; }
        public virtual ICollection<Mode> Modes { get; set; }
    }

    public class Mode : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
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

        public virtual Source Source { get; set; }
    }

    public class TrackDefinition : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }

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

        //[Association("Platform_TrackDefinition", "Guid", "Guid")]
        public Platform Platform { get; set; }
        public virtual Perimeter Perimeter { get; set; }
    }

    public class Perimeter : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string Name { get; set; }
        public virtual Scenario Scenario { get; set; }
        public virtual ICollection<PerimeterCoordinate> PerimeterCoordinates { get; set; }
    }

    public class PerimeterCoordinate
    {
        [Key]
        public int PerimeterCoordinateID { get; set; }
        public int Order { get; set; }
        public DbGeo Geo { get; set; }

        public virtual Perimeter Perimeter { get; set; }
    }

    public class ScenarioSpecies : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string SpeciesFile { get; set; }
        public string LatinName { get; set; }

        public virtual Scenario Scenario { get; set; }
        public virtual ICollection<AnimatLocation> AnimatLocations { get; set; }
    }

    public class AnimatLocation
    {
        public int AnimatLocationID { get; set; }
        public DbGeo Geo { get; set; }
        public float Depth { get; set; }
    }
}
