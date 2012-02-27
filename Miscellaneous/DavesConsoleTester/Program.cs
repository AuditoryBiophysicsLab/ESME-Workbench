#define sqlite
using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Data.Common;
using System.Data.Entity;
using System.IO;
using ESME.Environment;
using System.Linq;
using HRC.Navigation;
using ESME.Databases;

namespace DavesConsoleTester
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length != 1) throw new InvalidOperationException("Must pass a directory on the command line");
#if sqlite
            File.Delete("soundspeed.sqlite");
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.Workarounds.IgnoreSchemaName = true;
            var connection = new Devart.Data.SQLite.SQLiteConnection(string.Format("Data Source={0};FailIfMissing=False", "soundspeed.sqlite"));
#else
            var connection = new System.Data.SqlServerCe.SqlCeConnection(string.Format("Data Source={0}", "soundspeed.sqlce"));
#endif
            var files = Directory.GetFiles(args[0], "*.soundspeed");
            var startTime = DateTime.Now;
            Console.WriteLine("{0}: Starting import test", startTime);
            foreach (var file in files)
            {
                var sourceFile = Path.GetFileName(file);
                //Console.WriteLine("  Importing {0}                       ", );
                var soundSpeed = SoundSpeed.Load(file);
                foreach (var curField in soundSpeed.SoundSpeedFields)
                {
                    var context = new SoundSpeedContext(connection, false, new CreateDatabaseIfNotExists<SoundSpeedContext>());
                    var newField = ImportField(curField, context);
                    var batchSize = curField.EnvironmentData.Count;
                    var fieldStartTime = DateTime.Now;
                    //using (var scope = new TransactionScope())
                    //{
                    for (var batchIndex = 0; batchIndex < curField.EnvironmentData.Count; batchIndex++)
                    {
                        Console.Write("    Importing {0} profile {1} of {2} ({3:0.00%})\r", sourceFile, batchIndex, batchSize, ((float)batchIndex / batchSize));
                        ImportProfile(newField, curField.EnvironmentData[batchIndex], context);
                    }
                    //    scope.Complete();
                    //}
                    context.SaveChanges();
                    Console.WriteLine("{0}: Imported {1} ({2} elapsed)", DateTime.Now, sourceFile, DateTime.Now - fieldStartTime);
                }
            }
            Console.WriteLine("{0}: Finished import test. Elapsed time: {1}", DateTime.Now, DateTime.Now - startTime);
#if sqlite
            Console.WriteLine("{0}: Compacting database...", DateTime.Now);
            // Reclaim any extra space in the database file
            using (var context = new SoundSpeedContext(connection, true, new CreateDatabaseIfNotExists<SoundSpeedContext>())) 
                context.Database.ExecuteSqlCommand("VACUUM;");
            Console.WriteLine("{0}: Exiting", DateTime.Now);
#endif
        }

        static void ImportSoundSpeed(SoundSpeed soundSpeed, SoundSpeedContext context)
        {
            foreach (var curField in soundSpeed.SoundSpeedFields)
            {
                var newField = ImportField(curField, context);
                if (newField != null) foreach (var curProfile in curField.EnvironmentData) ImportProfile(newField, curProfile, context);
            }
            context.SaveChanges();
        }

        static NewSoundSpeedField ImportField(TimePeriodEnvironmentData<SoundSpeedProfile> curField, SoundSpeedContext context)
        {
            var existingField = (from f in context.NewSoundSpeedFields
                                 where f.TimePeriod == (int)curField.TimePeriod
                                 select f).FirstOrDefault();
#if sqlite
            context.Database.ExecuteSqlCommand("CREATE INDEX IF NOT EXISTS LatitudeIndex ON NewSoundSpeedProfiles(Latitude);");
            context.Database.ExecuteSqlCommand("CREATE INDEX IF NOT EXISTS LongitudeIndex ON NewSoundSpeedProfiles(Longitude);");
#endif
            if (existingField != null) return null;
            var newField = new NewSoundSpeedField { TimePeriod = (int)curField.TimePeriod };
            context.NewSoundSpeedFields.Add(newField);
            return newField;
        }

        static void ImportProfile(NewSoundSpeedField field, Geo<List<SoundSpeedSample>> curProfile, SoundSpeedContext context)
        {
            var newProfile = new NewSoundSpeedProfile
            {
                Latitude = curProfile.Latitude,
                Longitude = curProfile.Longitude,
                NewSoundSpeedField = field,
            };
            if (curProfile.Data.Count > 0)
            {
                using (var ms = new MemoryStream())
                {
                    using (var bw = new BinaryWriter(ms))
                    {
                        bw.Write(curProfile.Data.Count);
                        foreach (var curSample in curProfile.Data)
                        {
                            bw.Write(curSample.Depth);
                            bw.Write(curSample.Temperature);
                            bw.Write(curSample.Salinity);
                            bw.Write(curSample.Salinity);
                        }
                    }
                    newProfile.Blob = ms.GetBuffer();
                }
            }
            context.NewSoundSpeedProfiles.Add(newProfile);
        }
    }

    public class SoundSpeedContext : DbContext
    {
        public SoundSpeedContext(DbConnection connection, bool contextOwnsConnection, IDatabaseInitializer<SoundSpeedContext> initializer)
            : base(connection, contextOwnsConnection) { Database.SetInitializer(initializer); }

        public DbSet<NewSoundSpeedField> NewSoundSpeedFields { get; set; }
        public DbSet<NewSoundSpeedProfile> NewSoundSpeedProfiles { get; set; }
        public DbSet<NewSoundSpeedSample> NewSoundSpeedSamples { get; set; }
    }

    public class NewSoundSpeedField
    {
        public long NewSoundSpeedFieldID { get; set; }
        public int TimePeriod { get; set; }

        public virtual ICollection<NewSoundSpeedProfile> NewSoundSpeedProfiles { get; set; }
    }

    public class NewSoundSpeedProfile
    {
        public long NewSoundSpeedProfileID { get; set; }
        public double Latitude { get; set; }
        public double Longitude { get; set; }
#if sqlite
        [Column(TypeName = "BLOB")]
#else
        [Column(TypeName = "image")]
#endif
        public byte[] Blob { get; set; }

        //[ForeignKey("NewSoundSpeedFieldID")]
        public virtual NewSoundSpeedField NewSoundSpeedField { get; set; }
        public virtual ICollection<NewSoundSpeedSample> NewSoundSpeedSamples { get; set; }
    }

    public class NewSoundSpeedSample
    {
        public long NewSoundSpeedSampleID { get; set; }
        public float Depth { get; set; }
        public float Temperature { get; set; }
        public float Salinity { get; set; }
        public float? SoundSpeed { get; set; }

        //[ForeignKey("NewSoundSpeedProfileID")]
        public virtual NewSoundSpeedProfile NewSoundSpeedProfile { get; set; }
    }

    public class TimeStep
    {
        public long TimeStepID { get; set; }
        public TimeSpan SimulationTime { get; set; }

        public virtual ICollection<PlatformLocation> PlatformLocations { get; set; }
    }

    public class PlatformLocation
    {
        public long PlatformLocationID { get; set; }
        public float Latitude { get; set; }
        public float Longitude { get; set; }
        public float Depth { get; set; }

        public virtual TimeStep TimeStep { get; set; }
        public virtual Platform Platform { get; set; }
    }

    public class ActiveMode
    {
        public long ActiveModeID { get; set; }
    }
}
