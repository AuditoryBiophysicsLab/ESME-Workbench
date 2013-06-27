using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace PurgeDeveloperAppData
{
    class Program
    {
        static void Main(string[] args)
        {
            #region input parsing
            try
            {
                if(args.Length > 0)
                    Usage("There are no options to specify!");
            }
            catch (Exception ex)
            {
                Usage(ex.Message);
            } 
            #endregion

            var esmeAppData = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME Workbench");
            var buAppData = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),"Boston_University");

            Console.WriteLine("Removing application configuration data directories for ESME Workbench and environmental database plugins...");
            if(Directory.Exists(esmeAppData))
            {
                Console.WriteLine("   Removing {0}", esmeAppData);
                Directory.Delete(esmeAppData,true);
                Console.WriteLine("   Done.");
            }

            if (Directory.Exists(buAppData))
            {
                Console.WriteLine("   Removing {0}", buAppData);
                Directory.Delete(buAppData,true);
                Console.WriteLine("   Done.");
            }
            Console.WriteLine("Done.");
        }

        private static void Usage(string message)
        {
            if (message != null) Console.WriteLine(message);
        }
    }
}
