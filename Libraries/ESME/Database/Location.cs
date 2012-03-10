using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Data.Common;
using System.Data.Entity;
using ESME.Environment.Descriptors;

namespace ESME.Database
{
    public class LocationContext : DbContext
    {
        public LocationContext(DbConnection connection, bool contextOwnsConnection, IDatabaseInitializer<LocationContext> initializer)
            : base(connection, contextOwnsConnection)
        {
            Configuration.AutoDetectChangesEnabled = false;
            Configuration.ProxyCreationEnabled = false;
            Configuration.LazyLoadingEnabled = true;
            Configuration.ValidateOnSaveEnabled = true;
            System.Data.Entity.Database.SetInitializer(initializer);
        }

        public DbSet<Location> Locations { get; set; }
        public DbSet<LocationLogEntry> LocationLogEntries { get; set; }
        public DbSet<EnvironmentDataSet> EnvironmentDataSets { get; set; }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            DbDateTime.ModelInitialization(modelBuilder);
            DbTimeSpan.ModelInitialization(modelBuilder);
            DbGeo.ModelInitialization(modelBuilder);
        }
    }

    public class Location
    {
        public int LocationID { get; set; }
        public string Name { get; set; }
        public string Comments { get; set; }
        public DbGeoRect GeoRect { get; set; }
        public DbWhoWhenWhere Creator { get; set; }

        public virtual ICollection<EnvironmentDataSet> EnvironmentDataSets { get; set; }
        public virtual ICollection<LocationLogEntry> LogEntries { get; set; }
    }

    [ComplexType]
    public class LogEntry
    {
        public DbWhoWhenWhere MessageSource { get; set; }
        public string Message { get; set; }
        public int? OldSourceID { get; set; }        
    }
    public class LocationLogEntry
    {
        public string LocationLogEntryID { get; set; }
        public LogEntry LogEntry { get; set; }
        public virtual Location Source { get; set; }
    }

    public class EnvironmentDataSet
    {
        public int EnvironmentDataSetID { get; set; }
        public EnvironmentDataType EnvironmentDataType { get; set; }
        public string Filename { get; set; }
        public DbPluginIdentifier SourcePlugin { get; set; }
        public DbWhoWhenWhere Creator { get; set; }
        public virtual Location Location { get; set; }
    }
}
