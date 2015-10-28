using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations.Schema;
using System.Data.Entity.Infrastructure;
using System.Data.Entity.ModelConfiguration.Conventions;
using System.Data.Entity.Validation;
using System.Data.SqlServerCe;
using System.Diagnostics;
using System.Data.Entity;
using System.Data.Entity.SqlServerCompact;
using System.IO;
using ESME.Scenarios;

namespace ESME.PSM
{
    [DbConfigurationType(typeof(PSMContextConfiguration))]
    public class PSMContext : DbContext
    {
        
        public DbSet<PlatformPSM> Platforms { get; set; }
        public DbSet<SourcePSM> Sources { get; set; }
        public DbSet<ModePSM> Modes { get; set; }

        static readonly string DatabaseFile = Path.Combine(Globals.MasterDatabaseService.MasterDatabaseDirectory, "psm.db.sdf"); 
            // Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.Desktop), "psm.db.sdf");  //for now
        static readonly string ConnectionString = new SqlCeConnectionStringBuilder
        {
            DataSource = DatabaseFile,
            FileMode = "Shared Read",
            MaxDatabaseSize = 4091,
        }.ToString();

        public PSMContext()
            : base(ConnectionString)
        {
            Configuration.AutoDetectChangesEnabled = false;
            Configuration.ProxyCreationEnabled = true;
            Configuration.LazyLoadingEnabled = true;
            Configuration.ValidateOnSaveEnabled = true;
            System.Data.Entity.Database.SetInitializer(new MigrateDatabaseToLatestVersion<PSMContext, Migrations.Configuration>());
            //Database.SetInitializer(new CreateDatabaseIfNotExists<GameContext>());
        }

        /// <summary>
        /// This is the main application interface to the database backing code for changing the state of the database
        ///  
        /// All changes to the database context are validated and saved on successful execution.
        /// </summary>
        /// <param name="action">a lambda expression, usually of the form c => {}, where c is an instance of the PSMContext.</param>
        public static void Modify(Action<PSMContext> action)
        {
            try
            {
                using (var c = new PSMContext())
                {
                    action(c);
                    c.SaveChanges();
                }
            }
            catch (DbEntityValidationException e)
            {
                foreach (var eve in e.EntityValidationErrors)
                {
                    Debug.WriteLine("Entity of type \"{0}\" in state \"{1}\" has the following validation errors:",
                        eve.Entry.Entity.GetType().Name, eve.Entry.State);
                    foreach (var ve in eve.ValidationErrors)
                    {
                        Debug.WriteLine("- Property: \"{0}\", Error: \"{1}\"",
                            ve.PropertyName, ve.ErrorMessage);
                    }
                }
                throw;
            }
            catch (DbUpdateException dbUpdateException)
            {
                Console.WriteLine("SaveChanges caught DbUpdateException");
                Console.WriteLine("  {0}", dbUpdateException.InnerException.Message);
                if (dbUpdateException.InnerException.InnerException != null) Console.WriteLine("    {0}", dbUpdateException.InnerException.InnerException.Message);
                throw;
            }
            catch (Exception e)
            {
                Debug.WriteLine(e.Message);
                Debug.WriteLine(e.InnerException.Message);
            }
        }

        /// <summary>
        /// This is the main application interface to the database backing code for querying the current state of the database. 
        /// The state of the database will not change through the use of this method.
        /// </summary>
        /// <typeparam name="TResult">The desired return type</typeparam>
        /// <param name="action">a lambda expression usually of the form c => {}, where c is an instance of the PSMContext.</param>
        /// <returns></returns>
        public static TResult Query<TResult>(Func<PSMContext, TResult> action)
        {
            using (var c = new PSMContext())
            {
                return action(c);
            }
        }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            //preliminary tasks
            modelBuilder.Conventions.Remove<PluralizingTableNameConvention>(); //no plural table names!

            //platform requirements
                //forward
            modelBuilder.Entity<PlatformPSM>().HasMany(p => p.Sources);

            //source requirements
                //forward
            modelBuilder.Entity<SourcePSM>().HasMany(s => s.Modes);
                //reverse
            modelBuilder.Entity<SourcePSM>().HasRequired(s => s.Platform);

            //mode requirements
                //reverse
            modelBuilder.Entity<ModePSM>().HasRequired(m => m.Source);

        }
    }

    public class PSMContextConfiguration : DbConfiguration
    {
        public PSMContextConfiguration() { SetProviderServices(SqlCeProviderServices.ProviderInvariantName, SqlCeProviderServices.Instance);}
    }

#if false
    public class Platform
    {
        //key 
        public Guid PlatformID { get; set; }

        //foreign keys and virtuals
        public virtual ICollection<Source> Sources { get; set; }

        //general attributes
        public PlatformType Type { get; set; }
        public string Name { get; set; }
        public float Speed { get; set; }
        //perimeter? 
        //behavior?
        public bool IsRandom { get; set; }

        public Platform() { PlatformID = Guid.NewGuid(); }
        public override string ToString() { return Name; }
    }

    public enum PlatformType
    {
        NotSpecified,

    }
    public class Source
    {
        //key
        public Guid SourceID { get; set; }

        //foreign keys and virtuals
        public virtual Platform Platform { get; set; }
        public virtual ICollection<Mode> Modes { get; set; }
        //general attributes
        public string Name { get; set; }
        public SourceType Type { get; set; }

        public Source() { SourceID = Guid.NewGuid(); }
        public override string ToString() { return Name; }
    }

    public enum SourceType
    {
        NotSpecified,
    }

    public class Mode
    {
        //key
        public Guid ModeID { get; set; }

        //foreign keys and virtuals
        public virtual Source Source { get; set; }
        //general attributes
        public string Name { get; set; }
        public ModeType Type { get; set; }
        //number of radials?
        public float MaxPropagationRadius { get; set; }
        //simulator?
        public float DepthOffset { get; set; }
        public float SourceLevel { get; set; }
        public float Frequency { get; set; }
        #region PulseLength
        //this is a workaround because sqlCE can't store TimeSpans natively.  
        public long PulseLengthTicks { get; set; }
        TimeSpan _pulseLength;
        [NotMapped]
        public TimeSpan PulseLength
        {
            get
            {
                return _pulseLength;
            }
            set
            {
                _pulseLength = value;
                PulseLengthTicks = _pulseLength.Ticks;
            }
        }
        #endregion
        #region PulseInterval
        public long PulseIntervalTicks { get; set; }
        TimeSpan _pulseInterval;
        [NotMapped]
        public TimeSpan PulseInterval
        {
            get
            {
                return _pulseInterval;
            }
            set
            {
                _pulseInterval = value;
                PulseIntervalTicks = _pulseInterval.Ticks;
            }
        }
        #endregion
        public float RelativeBeamAngle { get; set; }
        public float HorizontalBeamWidth { get; set; }
        public float VerticalBeamWidth { get; set; }
        public float SideLobeAttenuation { get; set; }
        public float DepressionElevationAngle { get; set; }

        public Mode() { ModeID = Guid.NewGuid(); }
        public override string ToString() { return Name; }
    }

    public enum ModeType
    {
        NotSpecified,
        Narrowband,
        Broadband,
        Explosive
    } 
#endif
}
