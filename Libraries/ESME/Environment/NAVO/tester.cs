using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
    class Tester
    {
        static readonly List<EarthCoordinate3D[]> Testpoints = new List<EarthCoordinate3D[]>
                                             {
                                                 new EarthCoordinate3D[]
                                                 {
                                                     new EarthCoordinate3D(31.828,-78.685,0),//north east
                                                     new EarthCoordinate3D(28.795,-80.410,0), // south west
                                                     
                                                   },

                                             };
        public static void Main(string[] args)
        {
            DBDBTest();



        }
        static void DBDBTest()
        {
            var foo = new DBDB
                      {
                          DatabasePath = @"C:\Users\Graham Voysey\Desktop\DBDB-V\data\dbdbv5_level0c.h5",
                          ExtractionProgramPath = @"C:\Users\Graham Voysey\Desktop\DBDB-V\bin\Windows\dbv5_command.exe",
                      };

            var outfilename = @"C:\tests\dbtests\dbdb.dat";
            foo.GetAllResolutions();
            if (foo.Resolutions != null)
            {
                Console.WriteLine(@"Available Resolutions");
                foreach (var resolution in foo.Resolutions)
                {
                    Console.WriteLine(resolution);
                }
            }
            Console.WriteLine();
            foo.ExtractArea(outfilename, Testpoints[1][1].Latitude_degrees, Testpoints[1][2].Latitude_degrees, Testpoints[1][1].Longitude_degrees, Testpoints[1][2].Longitude_degrees);

        }

    }
}
