#define sqlite
using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Data.Common;
using System.Data.Entity;
using System.IO;
using ESME.Environment;
using System.Linq;

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
            Console.WriteLine("Starting import test at {0}", startTime);
            foreach (var file in files)
            {
                Console.WriteLine("  Importing {0}                       ", Path.GetFileName(file));
                using (var context = new SoundSpeedContext(connection, false, new CreateDatabaseIfNotExists<SoundSpeedContext>()))
                    ImportSoundSpeed(SoundSpeed.Load(file), context);
            }
            var endTime = DateTime.Now;
            Console.WriteLine("Finished import test at {0}.  Elapsed time: {1}", endTime, endTime - startTime);
#if sqlite
            Console.WriteLine("Compacting database...");
            // Reclaim any extra space in the database file
            using (var context = new SoundSpeedContext(connection, true, new CreateDatabaseIfNotExists<SoundSpeedContext>())) 
                context.Database.ExecuteSqlCommand("VACUUM;");
#endif
            Console.WriteLine("Press any key to exit..");
            Console.ReadKey();
        }

        static void ImportSoundSpeed(SoundSpeed soundSpeed, SoundSpeedContext context)
        {
            foreach (var curField in soundSpeed.SoundSpeedFields)
            {
                var existingField = (from f in context.NewSoundSpeedFields 
                                     where f.TimePeriod == (int)curField.TimePeriod 
                                     select f).FirstOrDefault();
#if sqlite
                context.Database.ExecuteSqlCommand("CREATE INDEX IF NOT EXISTS LatitudeIndex ON NewSoundSpeedProfiles(Latitude);");
                context.Database.ExecuteSqlCommand("CREATE INDEX IF NOT EXISTS LongitudeIndex ON NewSoundSpeedProfiles(Longitude);");
#endif
                if (existingField != null) continue;
                var newField = new NewSoundSpeedField { TimePeriod = (int)curField.TimePeriod };
                context.NewSoundSpeedFields.Add(newField);
                var profileCount = curField.EnvironmentData.Count;
                foreach (var curProfile in curField.EnvironmentData)
                {
                    var curProfileIndex = (float)(curField.EnvironmentData.IndexOf(curProfile) + 1);
                    Console.Write("    Importing SoundSpeedProfile {0} of {1} ({2:###}%)         \r", curProfileIndex, profileCount, (curProfileIndex / profileCount) * 100);
                    var newProfile = new NewSoundSpeedProfile
                    {
                        Latitude = curProfile.Latitude,
                        Longitude = curProfile.Longitude,
                        NewSoundSpeedField = newField,
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
            context.SaveChanges();
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
}
