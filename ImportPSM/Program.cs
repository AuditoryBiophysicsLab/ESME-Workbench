using System;
using System.Data.Common;
using System.Data.Entity;
using System.IO;
using System.Linq;
using FileHelpers;

namespace ImportPSM
{
    class Program
    {
        public enum DatabaseType
        {
            SQLite,
            SQLCE4,
            Error,
        }
        static void Main(string[] args)
        {
            string sourceFile = null;
            string output = null;
            DatabaseType type = DatabaseType.Error;
            DbConnection connection = null;
            try
            {
                if (args.Length == 0)
                {
                    Usage("No arguments specified!");
                    return;
                }
                for (var i = 0; i < args.Length; i++)
                {
                    var arg = args[i];
                    switch (arg.ToLower())
                    {
                        case "-sourceFile":
                            sourceFile = args[++i];
                            if(!File.Exists(sourceFile)) Usage("source PSM file does not exist or is an invalid path.");
                            break;
                        case "-databaseType":
                            var res = args[++i];
                            if(!Enum.TryParse(res, true, out type)) Usage("Invalid/unsupported database type");
                            break;
                        case "-output":
                            output = args[++i];
                            break;
                        default :
                            Usage("Incorrect option specified.");
                            return;
                    }
                }
            }
            catch (Exception ex)
            {
                Usage(ex.Message);
            }
            
            switch (type)
            {
                case DatabaseType.SQLite:
                    Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.Workarounds.IgnoreSchemaName = true;
                    connection = new Devart.Data.SQLite.SQLiteConnection(string.Format("Data Source='{0}';FailIfMissing=False", output));
                    break;
                case DatabaseType.SQLCE4:
                    connection = new System.Data.SqlServerCe.SqlCeConnection(string.Format("Data Source='{0}'", output));
                    break;
                default:
                    throw new ApplicationException();
            }
            
            Console.WriteLine("populating database...");
            ImportPSM(sourceFile, connection);
            Console.WriteLine("done.");
        }

        static void Usage(string message = null)
        { 
            Console.WriteLine("Usage");
            Console.WriteLine("Valid database types are SQLite or SQLCE4");
            if (message != null) Console.WriteLine(message);
        
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
            var engine = new FileHelperEngine(typeof(PSM));
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
        }
       
    }
}