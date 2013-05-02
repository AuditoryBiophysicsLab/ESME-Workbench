using System;
using System.Data;
using System.Data.Common;
using System.Data.Entity;
using System.Data.Entity.Infrastructure;
using System.Diagnostics;
using System.IO;
using System.Linq;
using ESME.Scenarios;

namespace ESME.PSM
{
    public class PSMContext : DbContext, IDbConnectionFactory
    {
        public DbSet<Platform> Platforms { get; set; }
        public DbSet<Source> Sources { get; set; }
        public DbSet<Mode> Modes { get; set; }

        PSMContext(DbConnection connection, bool contextOwnsConnection)
            : base(connection, contextOwnsConnection)
        {
            Configuration.AutoDetectChangesEnabled = true;
            Configuration.ProxyCreationEnabled = true;
            Configuration.LazyLoadingEnabled = true;
            Configuration.ValidateOnSaveEnabled = true;
            System.Data.Entity.Database.SetInitializer(new PSMDatabaseInitializer());
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
                    Debug.WriteLine(string.Format("{0}: Caught (and discarded) exception in PSMContext.IsModified: {1}", DateTime.Now, e.Message));
                    return true;
                }
            }
        }

        public static PSMContext Create(string directoryPath)
        {
            if (String.IsNullOrEmpty(directoryPath)) throw new ApplicationException("PSMDatabaseDirectory cannot be null or empty");
            if (!Directory.Exists(directoryPath)){Directory.CreateDirectory(directoryPath);}
            var connectionFactory = new SqlCeConnectionFactory("System.Data.SqlServerCe.4.0");
            var connection = connectionFactory.CreateConnection(Path.Combine(directoryPath, "psm.db"));
            return new PSMContext(connection, true);
        }

        protected override void OnModelCreating (DbModelBuilder modelBuilder)
        {
            //modelBuilder.Entity<Platform>().HasOptional(p => p.Sources);
        }


        public class PSMDatabaseInitializer : CreateDatabaseIfNotExists<PSMContext>
        {
            protected override void Seed(PSMContext context)
            {
                //context.Database.ExecuteSqlCommand("");
            }
        }

        public DbConnection CreateConnection(string nameOrConnectionString) { throw new NotImplementedException(); }
    }
}
