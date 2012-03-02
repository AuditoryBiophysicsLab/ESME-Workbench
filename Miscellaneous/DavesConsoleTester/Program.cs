using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Data;
using System.Data.Common;
using System.Data.Entity;
using System.IO;
using System.Threading.Tasks;
using System.Transactions;
using ESME.Environment;
using System.Linq;
using ESME.NEMO;
using HRC.Navigation;
using ESME.Databases;

namespace DavesConsoleTester
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length != 1) throw new InvalidOperationException("Must pass a file name on the command line");
            if (!File.Exists(args[0])) throw new InvalidOperationException("Must pass an existing NEMO file on the command line");
            File.Delete("scenario.sqlite");
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.Workarounds.IgnoreSchemaName = true;
            var connection = new Devart.Data.SQLite.SQLiteConnection(string.Format("Data Source={0};FailIfMissing=False", "scenario.sqlite"));
            var context = new ScenarioContext(connection, false, new DropCreateDatabaseAlways<ScenarioContext>());
            const string simAreaDirectory = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas";
            var nemoFile = new NemoFile(args[0], simAreaDirectory);
            var scenario = new Scenario
            {
                BuilderVersion = nemoFile.Scenario.BuilderVersion,
                AnalystName = nemoFile.Scenario.AnalystName,
                CreationTime = nemoFile.Scenario.CreationTime,
                Description = nemoFile.Scenario.Description,
                Duration = nemoFile.Scenario.Duration,
                EventName = nemoFile.Scenario.EventName,
                SimAreaName = nemoFile.Scenario.SimAreaName,
                StartTime = nemoFile.Scenario.StartTime.TimeOfDay,
                TimeFrame = nemoFile.Scenario.TimeFrame,
            };
            context.Scenarios.Add(scenario);
            context.SaveChanges();
            foreach (var nemoPlatform in nemoFile.Scenario.Platforms)
            {
                var platform = new ScenarioPlatform
                {
                    PlatformID = int.Parse(nemoPlatform.Id),
                    PlatformName = nemoPlatform.Name,
                    Description = nemoPlatform.Description,
                    Launches = false,
                    Tows = false,
                    Scenario = scenario,
                };
                context.ScenarioPlatforms.Add(platform);
                context.SaveChanges();
                if (nemoPlatform.Trackdefs.Count > 0)
                {
                    var trackType = TrackType.Stationary;
                    switch (nemoPlatform.Trackdefs[0].TrackType.ToLower())
                    {
                        case "perimeter_bounce":
                            trackType = TrackType.PerimeterBounce;
                            break;
                        case "stationary":
                            trackType = TrackType.Stationary;
                            break;
                        case "straight_line":
                            trackType = TrackType.StraightLine;
                            break;
                    }
                    var trackDefinition = new TrackDefinition
                    {
                        Duration = nemoPlatform.Trackdefs[0].Duration,
                        InitialCourse = nemoPlatform.Trackdefs[0].InitialCourse,
                        InitialDepth = -1 * nemoPlatform.Trackdefs[0].InitialHeight,
                        InitialLatitude = nemoPlatform.Trackdefs[0].InitialLatitude,
                        InitialLongitude = nemoPlatform.Trackdefs[0].InitialLongitude,
                        InitialSpeed = nemoPlatform.Trackdefs[0].InitialSpeed,
                        LimitFileName = nemoPlatform.Trackdefs[0].LimitFileName,
                        OpsBounds = nemoPlatform.Trackdefs[0].OpsBounds,
                        OpsTimes = nemoPlatform.Trackdefs[0].OpsTimes,
                        Random = nemoPlatform.Trackdefs[0].Random,
                        StartTime = nemoPlatform.Trackdefs[0].StartTime.TimeOfDay,
                        TrackType = (int)trackType,
                    };
                    context.TrackDefinitions.Add(trackDefinition);
                    platform.TrackDefinition = trackDefinition;
                    context.SaveChanges();
                    if (trackDefinition.LimitFileName != null)
                    {
                        var perimeterName = Path.GetFileNameWithoutExtension(trackDefinition.LimitFileName);
                        var existingPerimeter = (from perimeter in context.Perimeters
                                                 where perimeter.Name == perimeterName
                                                 select perimeter).FirstOrDefault();
                        // If there is no perimeter with the current filename that already exists
                        if (existingPerimeter == null)
                        {
                            var perimeter = new Perimeter
                            {
                                Name = Path.GetFileNameWithoutExtension(trackDefinition.LimitFileName),
                            };
                            context.Perimeters.Add(perimeter);
                            context.SaveChanges();
                            foreach (var geo in nemoPlatform.Trackdefs[0].OverlayFile.Shapes[0].Geos)
                                context.PerimeterCoordinates.Add(new PerimeterCoordinate
                                {
                                    Geo = geo,
                                    Perimeter = perimeter,
                                });
                            context.SaveChanges();
                        }
                    }
                }
                foreach (var nemoSource in nemoPlatform.Sources)
                {
                    var source = new ScenarioSource
                    {
                        SourceID = int.Parse(nemoSource.Id),
                        SourceName = nemoSource.Name,
                        Description = nemoSource.Description,
                        Platform = platform,
                    };
                    context.ScenarioSources.Add(source);
                    context.SaveChanges();
                    foreach (var nemoMode in nemoSource.Modes)
                    {
                        var mode = new ScenarioMode
                        {
                            //ModeID = int.Parse(nemoMode.Id),
                            ActiveTime = nemoMode.ActiveTime,
                            ClusterCount = nemoMode.ClusterCount,
                            DepressionElevationAngle = nemoMode.DepressionElevationAngle,
                            Depth = nemoMode.DepthOffset,
                            HighFrequency = nemoMode.HighFrequency,
                            LowFrequency = nemoMode.LowFrequency,
                            HorizontalBeamWidth = nemoMode.HorizontalBeamWidth,
                            Linked = nemoMode.Linked,
                            MaxPropagationRadius = nemoMode.Radius,
                            ModeName = nemoMode.Name,
                            PulseInterval = (float)nemoMode.PulseInterval.TotalSeconds,
                            PulseLength = (float)nemoMode.PulseLength.TotalSeconds * 1000,
                            RelativeBeamAngle = nemoMode.RelativeBeamAngle,
                            SourceLevel = nemoMode.SourceLevel,
                            State = nemoMode.State,
                            VerticalBeamWidth = nemoMode.VerticalBeamWidth,
                            Source = source,
                        };
                        context.ScenarioModes.Add(mode);
                        context.SaveChanges();
                    }
                }
            }
            foreach (var nemoAnimals in nemoFile.Scenario.Animals)
            {
                foreach (var nemoSpecies in nemoAnimals.Species)
                {
                    var species = new ScenarioSpecies
                    {
                        Scenario = scenario,
                    };
                    context.ScenarioSpecies.Add(species);
                    context.SaveChanges();
                    using (var transaction = new TransactionScope())
                    {
                        nemoSpecies.AnimatDataTask.Start();
                        TaskEx.WhenAll(nemoSpecies.AnimatDataTask).Wait();
                        var result = nemoSpecies.AnimatDataTask.Result;
                        species.Name = result.LatinName;
                        var locationIndex = 1;
                        foreach (var location in result.AnimatStartPoints)
                        {
                            locationIndex++;
                            Console.Write("{0} Adding animat {1} of {2}\r", species.Name, locationIndex, result.AnimatStartPoints.Count);
                            context.AnimatLocations.Add(new AnimatLocation
                            {
                                Geo = new Geo(location.Latitude, location.Longitude),
                                Depth = location.Data,
                                ScenarioSpecies = species,
                            });
                        }
                        context.Entry(species).State = EntityState.Modified;
                        context.SaveChanges();
                        transaction.Complete();
                    }
                }
            }
            context.Database.ExecuteSqlCommand("VACUUM;");
#if false
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
                    //var context = new SoundSpeedContext(connection, false, new CreateDatabaseIfNotExists<SoundSpeedContext>());
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
            Console.WriteLine("{0}: Compacting database...", DateTime.Now);
            // Reclaim any extra space in the database file
            //using (var context = new SoundSpeedContext(connection, true, new CreateDatabaseIfNotExists<SoundSpeedContext>())) 
            //    context.Database.ExecuteSqlCommand("VACUUM;");
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

    public class SimulationContext : DbContext
    {
        public SimulationContext(DbConnection connection, bool contextOwnsConnection, IDatabaseInitializer<SoundSpeedContext> initializer)
            : base(connection, contextOwnsConnection) { Database.SetInitializer(initializer); }
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
