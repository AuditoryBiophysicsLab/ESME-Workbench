using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Data.Entity;
using System.IO;
using System.Linq;
using FileHelpers;

namespace grahams_little_sandbox
{
    class Program
    {
        static void Main()
        {
#if false
#if false
            var cassFile = @"C:\Users\Graham Voysey\Desktop\testoutput.BIN";

            var cassresult = CASSOutput.Load(cassFile, false);

            var tlf = TransmissionLossField.FromCASS(cassresult);

            var cassOut = @"C:\Users\Graham Voysey\Desktop\gvtestoutput.BIN";
            CASSOutput.Write(cassOut, tlf);
            var outresult = CASSOutput.Load(cassOut, false);


            // var tlf = TransmissionLossField.FromCASS(CASSOutput.Load(cassFile));  

            var ddbFile = @"C:\Users\Graham Voysey\Desktop\test2.ddb";
            var output = DDB.Load(ddbFile);
            var foo = output.AnimatStartPoints;
                var mmmbFile =
                    @"C:\Users\Graham Voysey\Desktop\test.3mb";
            var output = MMMB.Load(mmmbFile);
            var foo = output.AnimatStartPoints;
#endif

            string arrFile = @"C:\tests\score.arr";
            var z = 2500 / 3.2808;
            var w = 300 * 0.45359237;
            var fs = 88200;
            var T = 0.25;
            var model = BellhopNLInput.NLModelType.arons;

            const string infilePath = @"C:\tests\nlinput.bin";
            const string outfilePath = @"C:\tests\nloutput.bin";
            const string effectsPath = @"C:\tests\nloutput.effects";

#if true
#if false
           var bellhopInput = new BellhopRunFile
              {
                  TransmissionLossAlgorithm = TransmissionLossAlgorithm.Bellhop,
                  Filename = "",
                  Metadata = "",
                  RangeComplexName = "",
                  RangeDistanceIncrement = 0,
                  ReferenceLocation = new EarthCoordinate(),
                  ScenarioDataDirectory = "",
                  TransmissionLossJob = new TransmissionLossJob() { ModeName = "" },
                  TransmissionLossRunFileRadials = new List<TransmissionLossRunFileRadial>(),


              }; 
#endif
            new BellhopNLInput
               {
                   ChargeDepth = z,
                   ChargeMass = w,
                   OutputFreq = fs,
                   OutputTime = T,
                   ModelType = model.ToString(),
                   // Bearing = 180,
                   //BellhopConfiguration = "",
                   //BottomProfile = "",
                   //CalculationRange = 1000,
                   //Depths = "",
                   //EnvFilename = "",
                   //Ranges = "",
                   //TopReflectionCoefficients = "",
                   //WaterDepth = (float)z+500,


               }.Save(infilePath);
#endif

#if false
           var process = Process.Start(new ProcessStartInfo()
      {
          FileName = @"C:\Projects\ESME Deliverables\Utilities\BellhopNL\bin\Debug\BellhopNL.exe",
          Arguments = string.Format("-dataFile {0} -outFile {1} -isTest true -arrivalsFile {2} ", infilePath, outfilePath, arrFile)
      });
           process.WaitForExit(); 
#endif
            var nlOutput = BellhopNLOutput.Load(outfilePath);

            var effectsRecords = BellhopNL.Transform(nlOutput);

            EffectsFile.Write(effectsPath, effectsRecords, nlOutput.ModeName, nlOutput.ChargeDepth, nlOutput.OutputTime, nlOutput.TimePeriod); 
#endif
            const string graham = @"C:\Users\Graham Voysey\Documents\NAEMO\NAEMO demos\BU Test Sample2\Sim Areas\PSM.csv";
            const string dave = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas\PSM.csv";
            string sourceFile;
            if (File.Exists(graham)) sourceFile = graham;
            else if (File.Exists(dave)) sourceFile = dave;
            else throw new FileNotFoundException("Input file not found!");

            var sqlce = new System.Data.SqlServerCe.SqlCeConnection(string.Format("Data Source={0}", "psm.sqlce"));
            Console.WriteLine("populating database (sql ce)...");
            ImportPSM(sourceFile, sqlce);
            Console.WriteLine("dumping database (sql ce)...");
            Dump(sqlce);

            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.Workarounds.IgnoreSchemaName = true;
            var sqlite = new Devart.Data.SQLite.SQLiteConnection(string.Format("Data Source={0};FailIfMissing=False", "psm.sqlite"));
            Console.WriteLine("populating database (sqlite)...");
            ImportPSM(sourceFile, sqlite);
            Console.WriteLine("dumping database (sqlite)...");
            Dump(sqlite);

            Console.ReadLine();
        }

        static void Dump(DbConnection connection)
        {
            var psm = new PSMContext(connection, true, new CreateDatabaseIfNotExists<PSMContext>());
            foreach (var platform in psm.Platforms)
            {
                foreach (var source in platform.Sources)
                {
                    foreach (var mode in source.Modes)
                    {
                        Console.WriteLine("{0} {1} {2} {3} {4} {5}", platform.PlatformName, platform.PlatformType, source.SourceName, source.SourceType, mode.ModeName, mode.ModeType);
                    }
                }
            }
        }

        static void ImportPSM(string csvFilePath, DbConnection connection)
        {
            var psm = new PSMContext(connection, false, new DropCreateDatabaseAlways<PSMContext>());
            var engine = new FileHelperEngine(typeof (PSM));
            var entries = (PSM[])engine.ReadFile(csvFilePath);

            foreach (var entry in entries)
            {
                var platform = (from p in psm.Platforms
                                where p.PlatformName == entry.PlatformName
                                select p).FirstOrDefault();
                if (platform == null)
                {
                    platform = new Platform
                    {
                        PlatformName = entry.PlatformName,
                        PlatformType = entry.PlatformType,
                    };
                    psm.Platforms.Add(platform);
                    psm.SaveChanges();
                }
                Source source = null;
                if (platform.Sources != null)
                {
                    source = (from s in platform.Sources
                              where s.SourceName == entry.SourceName
                              select s).FirstOrDefault();

                }
                if (source == null)
                {
                    source = new Source
                    {
                        SourceName = entry.SourceName,
                        SourceType = entry.SourceType,

                        Platform = platform,
                    };
                    psm.Sources.Add(source);
                    psm.SaveChanges();
                }

                Mode mode = null;
                if (source.Modes != null)
                {
                    mode = (from m in source.Modes
                            where m.ModeName == entry.ModeName && m.ModeType == entry.ModeType
                            select m).FirstOrDefault();
                }
                if (mode == null)
                {
                    mode = new Mode
                    {
                        Source = source,
                        ActiveTime = entry.ActiveTime,
                        Depth = entry.Depth,
                        SourceLevel = entry.SourceLevel,
                        LowFrequency = entry.LowFrequency,
                        HighFrequency = entry.HighFrequency,
                        PulseInterval = entry.PulseInterval,
                        PulseLength = entry.PulseLength,
                        HorizontalBeamWidth = entry.HorizontalBeamwidth,
                        VerticalBeamWidth = entry.VerticalBeamwidth,
                        DepressionElevationAngle = entry.DepressionElevationAngle,
                        RelativeBeamAngle = entry.RelativeBeamAngle,
                        MaxPropagationRadius = entry.MaxPropagationRadius,
                        ModeName = entry.ModeName,
                        ModeType = entry.ModeType,
                    };
                    psm.Modes.Add(mode);
                    psm.SaveChanges();
                }
            }
#if false
            foreach (var entry in entries)
            {
                var myplatform = (from platform in context.Platforms
                                  where platform.PlatformName == entry.PlatformName
                                  select platform).FirstOrDefault();
                if (myplatform == null)
                {
                    myplatform = new Platform
                    {
                        PlatformName = entry.PlatformName,
                        PlatformType = entry.PlatformType
                    };
                    context.Platforms.InsertOnSubmit(myplatform);
                    context.SubmitChanges();
                }

                var mysource = (from source in context.Sources
                                where
                                        source.PlatformID == myplatform.PlatformID &&
                                        source.SourceName == entry.SourceName
                                select source).FirstOrDefault();
                if (mysource == null)
                {
                    mysource = new Source
                    {
                        PlatformID = myplatform.PlatformID,
                        SourceName = entry.SourceName,
                        SourceType = entry.SourceType,
                    };
                    context.Sources.InsertOnSubmit(mysource);
                    context.SubmitChanges();
                }

                var mymode = (from mode in context.Modes
                              where mode.SourceID == mysource.SourceID && mode.ModeName == entry.ModeName && mode.ModeType == entry.ModeType
                              select mode).FirstOrDefault();
                if (mymode == null)
                {
                    mymode = new Mode
                    {
                        SourceID = mysource.SourceID,
                        ActiveTime = entry.ActiveTime,
                        Depth = entry.Depth,
                        SourceLevel = entry.SourceLevel,
                        LowFrequency = entry.LowFrequency,
                        HighFrequency = entry.HighFrequency,
                        PulseInterval = entry.PulseInterval,
                        PulseLength = entry.PulseLength,
                        HorizontalBeamWidth = entry.HorizontalBeamwidth,
                        VerticalBeamWidth = entry.VerticalBeamwidth,
                        DepressionElevationAngle = entry.DepressionElevationAngle,
                        RelativeBeamAngle = entry.RelativeBeamAngle,
                        MaxPropagationRadius = entry.MaxPropagationRadius,
                        ModeName = entry.ModeName,
                        ModeType = entry.ModeType,
                    };
                    context.Modes.InsertOnSubmit(mymode);
                    context.SubmitChanges();
                }
            } //works with designer-dbase
#endif
        }
    }

    /// <summary>
    /// A Record class for Filehelpers CSV parsing.  Each public property corresponds, very possibly in order, to each field in a csv file's line. 
    /// We skip the first three lines of the file for PSM.CSV files corresponding to NUWC's NEMO2 system that is current as of 20 NOV 2011. 
    /// Not guaranteed to work with very early (pre-2011) PSM files (which may be missing the first line denoting CLASS/UNCLASS) or post JAN2012 files (which postdate the latest version available at this writing)
    /// 
    /// For valid files, line 0 is a !-flagged classification line, lines 1 and 2 are #-commented field name and corresponding unit identifiers.  Lines 3-n are record entries.
    /// </summary>
    [DelimitedRecord(",")]
    [IgnoreFirst(3)]
    public class PSM
    {
        public string PlatformType;
        public string PlatformName;
        public string SourceType;
        public string SourceName;
        public string ModeType;
        public string ModeName;
        public float? ActiveTime;
        public float? Depth;
        public float SourceLevel;
        public float LowFrequency;
        public float HighFrequency;
        public float PulseInterval;
        public float PulseLength;
        public float HorizontalBeamwidth;
        public float VerticalBeamwidth;
        public float DepressionElevationAngle;
        public float RelativeBeamAngle;
        public float MaxPropagationRadius;
    }

    public class Platform
    {
        public int PlatformID { get; set; }
        public string PlatformName { get; set; }
        public string PlatformType { get; set; }
        public virtual ICollection<Source> Sources { get; set; }
    }

    public class Source
    {

        public int SourceID { get; set; }
        public string SourceName { get; set; }
        public string SourceType { get; set; }
        public virtual Platform Platform { get; set; }
        public virtual ICollection<Mode> Modes { get; set; }
    }

    public class Mode
    {
        public int ModeID { get; set; }
        public string ModeName { get; set; }
        public string ModeType { get; set; }
        public float? ActiveTime { get; set; }
        public float? Depth { get; set; }
        public float SourceLevel { get; set; }
        public float LowFrequency { get; set; }
        public float HighFrequency { get; set; }
        public float PulseInterval { get; set; }
        public float PulseLength { get; set; }
        public float HorizontalBeamWidth { get; set; }
        public float VerticalBeamWidth { get; set; }
        public float DepressionElevationAngle { get; set; }
        public float RelativeBeamAngle { get; set; }
        public float MaxPropagationRadius { get; set; }
        public virtual Source Source { get; set; }
    }

    public class PSMContext : DbContext
    {
        public PSMContext(DbConnection connection, bool contextOwnsConnection, IDatabaseInitializer<PSMContext> initializer)
            : base(connection, contextOwnsConnection)
        {
            Database.SetInitializer(initializer);
        }

        public DbSet<Platform> Platforms { get; set; }
        public DbSet<Source> Sources { get; set; }
        public DbSet<Mode> Modes { get; set; }
    }
}
