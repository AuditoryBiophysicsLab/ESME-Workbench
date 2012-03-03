﻿//#define USE_BLOB
using System;
using System.Collections.Generic;
#if USE_BLOB
using System.ComponentModel.DataAnnotations;
#endif
using System.Data.Common;
using System.Data.Entity;
using System.IO;
using System.Transactions;
using ESME.Environment;
using System.Linq;
using HRC.Navigation;

namespace DavesConsoleTester
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length != 1) throw new InvalidOperationException("Must pass a file name on the command line");
#if false
            if (!File.Exists(args[0])) throw new InvalidOperationException("Must pass an existing NEMO file on the command line");
            File.Delete("scenario.sqlite");
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.Workarounds.IgnoreSchemaName = true;
            var connection = new Devart.Data.SQLite.SQLiteConnection(string.Format("Data Source={0};FailIfMissing=False", "scenario.sqlite"));
            var context = new ScenarioContext(connection, false, new DropCreateDatabaseAlways<ScenarioContext>());
            const string scenarioDataDirectory = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas";
            ESME.Database.Importers.NemoFile.Import(args[0], scenarioDataDirectory, context);
#endif
#if true
            File.Delete("soundspeed.sqlite");
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.Workarounds.IgnoreSchemaName = true;
            var connection = new Devart.Data.SQLite.SQLiteConnection(string.Format("Data Source={0};FailIfMissing=False", "soundspeed.sqlite"));
            var context = new SoundSpeedContext(connection, false, new DropCreateDatabaseAlways<SoundSpeedContext>());
            var files = Directory.GetFiles(args[0], "*.soundspeed");
            var oneFile = new[] {files[0]};
            var startTime = DateTime.Now;
            Console.WriteLine("{0}: Starting import test", startTime);
            foreach (var file in oneFile)
            {
                var sourceFile = Path.GetFileName(file);
                //Console.WriteLine("  Importing {0}                       ", );
                var soundSpeed = SoundSpeed.Load(file);
                foreach (var curField in soundSpeed.SoundSpeedFields)
                {
                    //var context = new SoundSpeedContext(connection, false, new CreateDatabaseIfNotExists<SoundSpeedContext>());
                    var newField = ImportField(curField, context);
                    var batchSize = curField.EnvironmentData.Count;
                    var fieldStartTime = DateTime.Now;
                    using (var scope = new TransactionScope(TransactionScopeOption.Required, new TimeSpan(0, 1, 0, 0)))
                    {
                        for (var batchIndex = 0; batchIndex < curField.EnvironmentData.Count; batchIndex++)
                        {
                            Console.Write("    Importing {0} profile {1} of {2} ({3:0.00%})\r", sourceFile, batchIndex, batchSize, ((float)batchIndex / batchSize));
                            ImportProfile(newField, curField.EnvironmentData[batchIndex], context);
                        }
                        context.SaveChanges();
                        scope.Complete();
                    }
                    Console.WriteLine("{0}: Imported {1} ({2} elapsed)", DateTime.Now, sourceFile, DateTime.Now - fieldStartTime);
                }
            }
            Console.WriteLine("{0}: Finished import test. Elapsed time: {1}", DateTime.Now, DateTime.Now - startTime);
            Console.WriteLine("{0}: Compacting database...", DateTime.Now);
            // Reclaim any extra space in the database file
            //using (var context = new SoundSpeedContext(connection, true, new CreateDatabaseIfNotExists<SoundSpeedContext>())) 
            //    context.Database.ExecuteSqlCommand("VACUUM;");
            Console.WriteLine("{0}: Exiting", DateTime.Now);
#endif

        }

        static NewSoundSpeedField ImportField(TimePeriodEnvironmentData<SoundSpeedProfile> curField, SoundSpeedContext context)
        {
            var existingField = (from f in context.NewSoundSpeedFields
                                 where f.TimePeriod == (int)curField.TimePeriod
                                 select f).FirstOrDefault();
            context.Database.ExecuteSqlCommand("CREATE INDEX IF NOT EXISTS LatitudeIndex ON NewSoundSpeedProfiles(Latitude);");
            context.Database.ExecuteSqlCommand("CREATE INDEX IF NOT EXISTS LongitudeIndex ON NewSoundSpeedProfiles(Longitude);");
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
#if USE_BLOB
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
#else
                foreach (var curSample in curProfile.Data)
                {
                    context.NewSoundSpeedSamples.Add(new NewSoundSpeedSample
                    {
                        Depth = curSample.Depth,
                        Temperature = curSample.Temperature,
                        Salinity = curSample.Salinity,
                        SoundSpeed = float.IsNaN(curSample.SoundSpeed) ? (float?)null : curSample.SoundSpeed,
                        NewSoundSpeedProfile = newProfile,
                    });
                }
#endif
            }
            context.NewSoundSpeedProfiles.Add(newProfile);
        }
    }

    public class SimulationContext : DbContext
    {
        public SimulationContext(DbConnection connection, bool contextOwnsConnection, IDatabaseInitializer<SoundSpeedContext> initializer)
            : base(connection, contextOwnsConnection) { Database.SetInitializer(initializer); }
    }

    public class SoundSpeedContext : DbContext
    {
        public SoundSpeedContext(DbConnection connection, bool contextOwnsConnection, IDatabaseInitializer<SoundSpeedContext> initializer)
            : base(connection, contextOwnsConnection)
        {
            Configuration.AutoDetectChangesEnabled = false;
            Configuration.ProxyCreationEnabled = false;
            Configuration.LazyLoadingEnabled = true;
            Configuration.ValidateOnSaveEnabled = true;
            Database.SetInitializer(initializer);
        }

        public DbSet<NewSoundSpeedField> NewSoundSpeedFields { get; set; }
        public DbSet<NewSoundSpeedProfile> NewSoundSpeedProfiles { get; set; }
#if !USE_BLOB
        public DbSet<NewSoundSpeedSample> NewSoundSpeedSamples { get; set; }
#endif
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
#if USE_BLOB
        [Column(TypeName = "BLOB")]
        public byte[] Blob { get; set; }
#endif

        public virtual NewSoundSpeedField NewSoundSpeedField { get; set; }
#if !USE_BLOB
        public virtual ICollection<NewSoundSpeedSample> NewSoundSpeedSamples { get; set; }
#endif
    }

#if !USE_BLOB
    public class NewSoundSpeedSample
    {
        public long NewSoundSpeedSampleID { get; set; }
        public float Depth { get; set; }
        public float Temperature { get; set; }
        public float Salinity { get; set; }
        public float? SoundSpeed { get; set; }

        public virtual NewSoundSpeedProfile NewSoundSpeedProfile { get; set; }
    }
#endif
}
