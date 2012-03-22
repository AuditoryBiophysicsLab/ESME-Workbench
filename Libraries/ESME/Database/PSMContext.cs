using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Data.Common;
using System.Data.Entity;
using HRC.Aspects;

namespace ESME.Database
{
    public class PSMContext : DbContext
    {
        public PSMContext(DbConnection connection, bool contextOwnsConnection, IDatabaseInitializer<PSMContext> initializer)
                : base(connection, contextOwnsConnection)
        {
            System.Data.Entity.Database.SetInitializer(initializer);
        }

        public DbSet<PSMPlatform> PSMPlatforms { get; set; }
        public DbSet<PSMSource> PSMSources { get; set; }
        public DbSet<PSMMode> PSMModes { get; set; }
    }

    public class PSMPlatform
    {
        [Key, Initialize(IsGuid = true)]
        public string Guid { get; set; }
        public string PlatformName { get; set; }
        public string PlatformType { get; set; }
        public virtual ICollection<PSMSource> PSMSources { get; set; }
    }

    public class PSMSource
    {
        [Key, Initialize(IsGuid = true)]
        public string Guid { get; set; }
        public string SourceName { get; set; }
        public string SourceType { get; set; }
        public virtual PSMPlatform PSMPlatform { get; set; }
        public virtual ICollection<PSMMode> PSMModes { get; set; }
    }

    public class PSMMode
    {
        [Key, Initialize(IsGuid = true)]
        public string Guid { get; set; }
        public string ModeName { get; set; }
        public string ModeType { get; set; }
        public float? ActiveTime { get; set; }
        public float? Depth { get; set; }
        public float SourceLevel { get; set; }
        public float LowFrequency { get; set; }
        public float HighFrequency { get; set; }
        public DbTimeSpan PulseInterval { get; set; }
        public DbTimeSpan PulseLength { get; set; }
        public float HorizontalBeamWidth { get; set; }
        public float VerticalBeamWidth { get; set; }
        public float DepressionElevationAngle { get; set; }
        public float RelativeBeamAngle { get; set; }
        public float MaxPropagationRadius { get; set; }
        public virtual PSMSource Source { get; set; }
    }
}