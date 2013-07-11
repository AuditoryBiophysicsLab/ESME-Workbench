using System;
using System.Data.Common;
using System.Data.Entity;
using System.IO;
using ESME.Database;

namespace ImportPSM
{
    class Program
    {
        static void Main(string[] args)
        {
            string sourceFile = null;
            string output = null;
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
                        case "-sourcefile":
                            sourceFile = args[++i];
                            if(!File.Exists(sourceFile)) Usage("source PSM file does not exist or is an invalid path.");
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
            
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.Workarounds.IgnoreSchemaName = true;
            DbConnection connection = new Devart.Data.SQLite.SQLiteConnection(string.Format("Data Source='{0}';FailIfMissing=False", output));
            var psm = new PSMContext(connection, false, new DropCreateDatabaseAlways<PSMContext>());

            Console.WriteLine("populating database...");
            ESME.Database.Importers.PSMFile.Import(sourceFile, psm);
            Console.WriteLine("done.");
        }

        static void Usage(string message = null)
        {
            Console.WriteLine("ImportPSM         - import tool for NUWC PSM.csv files.");
            Console.WriteLine("     -sourceFile  <sourceFile.csv>    : the path to a valid SimAreas.csv file");
            Console.WriteLine("     -output      <outputFile.sqlite> : the path to the target location of the normalized sqlite database output.");
            if (message != null) Console.WriteLine(message);
        
        }
    }
}