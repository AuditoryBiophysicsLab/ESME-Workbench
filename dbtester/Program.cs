using System;
using System.Collections.Generic;
using System.IO;
using ESME.Environment.NAVO;
using HRC.Navigation;

namespace dbtester
{
    internal class Program
    {
        static readonly List<EarthCoordinate3D[]> Testpoints = new List<EarthCoordinate3D[]>
                                                               {
                                                                   new[]
                                                                   {
                                                                       new EarthCoordinate3D(31.828, -78.685, 0), //north east
                                                                       new EarthCoordinate3D(28.795, -80.410, 0), // south west
                                                                   },
                                                               };

        public static void Main(string[] args)
        {
            //DBDBTest();
            //BSTTest();

        }

        static void DBDBTest()
        {
            var foo = new DBDB
                      {
                          DatabasePath = @"C:\Users\Graham Voysey\Desktop\DBDB-V\data\dbdbv5_level0c.h5",
                          ExtractionProgramPath = @"C:\Users\Graham Voysey\Desktop\DBDB-V\bin\Windows\dbv5_command.exe",
                      };

            string outfilename = @"C:\tests\dbtests\dbdb.txt";
            var test = Path.GetDirectoryName(outfilename);
            var testp = Path.Combine(Path.GetDirectoryName(outfilename), Path.GetFileNameWithoutExtension(outfilename));
            foo.GetAllResolutions();
            if (foo.Resolutions != null)
            {
                Console.WriteLine(@"DBDB: Available Resolutions");
                foreach (string resolution in foo.Resolutions)
                {
                    Console.WriteLine(resolution);
                }
            }
            Console.WriteLine();
            foo.SelectedResolution = (2.0).ToString();
            foo.ExtractArea(outfilename, Testpoints[0][1].Latitude_degrees, Testpoints[0][0].Latitude_degrees, Testpoints[0][0].Longitude_degrees, Testpoints[0][1].Longitude_degrees);
        }

        static void BSTTest()
        {
            var foo = new BST
                      {
                          DatabasePath = @"C:\Users\Graham Voysey\Desktop\BST\Sediments2.0_QAV_Analysis\Sediments\Version2.0\databases\hfevav2.h5",
                          ExtractionProgramPath = @"C:\Users\Graham Voysey\Desktop\BST\Sediments2.0_QAV_Analysis\Sediments\Version2.0\tools\Windows\with_hdf5_1.6\extract.exe",
                      };
            string outfilename = @"C:\tests\dbtests\bst.txt";
            foo.GetAllResolutions();
            if (foo.Resolutions != null)
            {
                Console.WriteLine(@"BST: Available Resolutions");
                foreach (string resolution in foo.Resolutions)
                {
                    Console.WriteLine(resolution);
                }
            }
            Console.WriteLine();
            foo.SelectedResolution = "5.0000";
            foo.ExtractArea(outfilename, Testpoints[0][1].Latitude_degrees, Testpoints[0][0].Latitude_degrees, Testpoints[0][0].Longitude_degrees, Testpoints[0][1].Longitude_degrees);
        }

        static void SMGCTest()
        {
            var foo = new SMGC()
                      {};

        }
    }
}