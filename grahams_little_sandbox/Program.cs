using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Data.Entity;
using System.Linq;
using FileHelpers;



namespace grahams_little_sandbox
{
    class Program
    {
        static void Main(string[] args)
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
           // var monitor = new SQLiteMonitor() { IsActive = true };
            //var config = Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance;
            //config.Workarounds.IgnoreSchemaName = true;
            //Database.DefaultConnectionFactory = new SQLiteConnectionFactory(@"C:\Projects\ESME Deliverables\ESME WorkBench\Databases\", "FailIfMissing=False");
            
            
            Console.WriteLine("populating database...");
            using (var psm = new PSMContext(new DropCreateDatabaseAlways<PSMContext>()))
                PSM2SQLite(@"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas\PSM.csv", psm);
            Console.WriteLine("dumping database...");
            using(var psm2 = new PSMContext(new CreateDatabaseIfNotExists<PSMContext>()))
                PSMDump(psm2);
            Console.ReadLine();
        }

        static void PSMDump(PSMContext psm)
        {
            foreach (var platform in psm.EFPlatforms)
            {
                foreach (var source in platform.EFSources)
                {
                    foreach (var mode in source.EFModes)
                    {
                        Console.WriteLine("{0} {1} {2} {3} {4} {5}",platform.PlatformName,platform.PlatformType,source.SourceName,source.SourceType,mode.ModeName,mode.ModeType);
                    }
                }
            }
        }

        static void PSM2SQLite(string csvFilePath, PSMContext psm)
        {
            var engine = new FileHelperEngine(typeof(PSM));
            var entries = (PSM[])engine.ReadFile(csvFilePath);
            

            foreach (var entry in entries)
            {
                var platform = (from p in psm.EFPlatforms
                                where p.PlatformName == entry.PlatformName
                                select p).FirstOrDefault();
                if (platform == null)
                {
                    platform = new EFPlatform
                    {
                        PlatformName = entry.PlatformName,
                        PlatformType = entry.PlatformType,
                    };
                    psm.EFPlatforms.Add(platform);
                    psm.SaveChanges();
                }
                EFSource source = null;
                if (platform.EFSources != null)
                {
                    source = (from s in platform.EFSources
                              where s.SourceName == entry.SourceName
                              select s).FirstOrDefault();

                }
                if (source == null)
                {
                    source = new EFSource
                    {
                        SourceName = entry.SourceName,
                        SourceType = entry.SourceType,
                        
                        EFPlatform = platform,
                    };
                    psm.EFSources.Add(source);
                    psm.SaveChanges();
                }

                EFMode mode = null;
                if (source.EFModes != null)
                {
                    mode = (from m in source.EFModes
                            where m.ModeName == entry.ModeName && m.ModeType == entry.ModeType
                            select m).FirstOrDefault();
                }
                if (mode == null)
                {
                    mode = new EFMode
                    {
                        EFSource = source,
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
                    psm.EFModes.Add(mode);
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


    public class EFPlatform
    {
  
        public int EFPlatformID { get; set; }
        public string PlatformName { get; set; }
        public string PlatformType { get; set; }
        public virtual ICollection<EFSource> EFSources { get; set; }
    }


    public class EFSource
    {
      
        public int EFSourceID { get; set; }
        public string SourceName { get; set; }
        public string SourceType { get; set; }
        public virtual EFPlatform EFPlatform { get; set; }
        public virtual ICollection<EFMode> EFModes { get; set; }
    }
 
  public class EFMode
    {
        public int EFModeID { get; set; }
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
        public virtual EFSource EFSource { get; set; }
    }

    public class PSMContext : DbContext
    {
        public PSMContext(IDatabaseInitializer<PSMContext> initializer)
        {
            Database.SetInitializer(initializer);
        }

        public DbSet<EFPlatform> EFPlatforms { get; set; }
        public DbSet<EFSource> EFSources { get; set; }
        public DbSet<EFMode> EFModes { get; set; }
    }
}
