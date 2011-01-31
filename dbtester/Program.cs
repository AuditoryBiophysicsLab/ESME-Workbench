using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESME.Environment;
using ESME.Environment.NAVO;
using HRC.Navigation;

namespace dbtester
{
    internal class Program
    {
        static readonly List<EarthCoordinate3D[]> Testpoints = new List<EarthCoordinate3D[]>
                                                               {
                                                                   #if false
		new[]
                                                                   {
                                                                       new EarthCoordinate3D(31.828, -78.685, 0), //north east
                                                                       new EarthCoordinate3D(28.795, -80.410, 0), // south west
                                                                   },  
	#endif
                                                                   new[]
                                                                   {
                                                                       new EarthCoordinate3D(33,-77.2195,0),
                                                                       new EarthCoordinate3D(27.36,-81.8,0), 
    }
                                                               };

        public static void Main(string[] args)
        {
            DBDBTest();
            BSTTest();
            SMGCTest();
            GDEMTest();
            Console.WriteLine(@"press enter to exit.");
            Console.ReadLine();
        }

        static void DBDBTest()
        {
            Console.WriteLine(@"Now testing DBDB extraction routines ...");
            var foo = new DBDB
                      {
                          DatabasePath = @"C:\Users\Graham Voysey\Desktop\DBDB-V\data\dbdbv5_level0c.h5",
                          ExtractionProgramPath = @"C:\Users\Graham Voysey\Desktop\DBDB-V\bin\Windows\dbv5_command.exe",
                      };

            const string outfilename = @"C:\tests\dbtests\dbdb.txt";
            foo.GetAllResolutions();
            if (foo.Resolutions != null)
            {
                Console.WriteLine(@" DBDB: Available Resolutions");
                foreach (var resolution in foo.Resolutions)
                {
                    Console.WriteLine(resolution);
                }
            }
            Console.WriteLine();
            foo.SelectedResolution = (0.05).ToString();
            var packet = new NAVOExtractionPacket
                         {
                             Filename = outfilename,
                             North = Testpoints[0][0].Latitude_degrees,
                             South = Testpoints[0][1].Latitude_degrees,
                             East = Testpoints[0][0].Longitude_degrees,
                             West = Testpoints[0][1].Longitude_degrees,
                         };
            Console.WriteLine(@"Extracting area to " + Path.GetDirectoryName(outfilename) +@"...");
            foo.ExtractArea(packet);
            Console.WriteLine(@"Done!");
        }

        static void BSTTest()
        {
            Console.WriteLine(@"Now testing BST extraction routines ...");
            var foo = new BST
                      {
                          DatabasePath = @"C:\Users\Graham Voysey\Desktop\BST\Sediments2.0_QAV_Analysis\Sediments\Version2.0\databases\hfevav2.h5",
                          ExtractionProgramPath = @"C:\Users\Graham Voysey\Desktop\BST\Sediments2.0_QAV_Analysis\Sediments\Version2.0\tools\Windows\with_hdf5_1.6\extract.exe",
                      };
            const string outfilename = @"C:\tests\dbtests\bst.txt";
            foo.GetAllResolutions();
            if (foo.Resolutions != null)
            {
                Console.WriteLine(@" BST: Available Resolutions");
                foreach (var resolution in foo.Resolutions)
                {
                    Console.WriteLine(resolution);
                }
            }
            Console.WriteLine();
            foo.SelectedResolution = "5.0000";
            var packet = new NAVOExtractionPacket
            {
                Filename = outfilename,
                North = Testpoints[0][0].Latitude_degrees,
                South = Testpoints[0][1].Latitude_degrees,
                East = Testpoints[0][0].Longitude_degrees,
                West = Testpoints[0][1].Longitude_degrees,
            };
            Console.WriteLine(@"Extracting area to " + Path.GetDirectoryName(outfilename) + @"...");
            foo.ExtractArea(packet);
            Console.WriteLine(@"Done!");
        }

        static void SMGCTest()
        {
            Console.WriteLine(@"Now testing SMGC extraction routines ...");
            var foo = new SMGC
                      {
                          DatabasePath = @"C:\Users\Graham Voysey\Desktop\SMGC\alldata\",
                          ExtractionProgramPath = @"C:\Projects\ESME Deliverables\trunk\Debug\SMGCExtract.exe",
                          MinMonth = 1,
                          MaxMonth = 4,
                          WorkingDirectory = @"C:\Users\Graham Voysey\Desktop\",
                          GridSpacing = 1,
                      };

            const string outfilename = @"C:\tests\dbtests\smgc.txt";
            var packet = new NAVOExtractionPacket
            {
                Filename = outfilename,
                North = Testpoints[0][0].Latitude_degrees,
                South = Testpoints[0][1].Latitude_degrees,
                East = Testpoints[0][0].Longitude_degrees,
                West = Testpoints[0][1].Longitude_degrees,
            };
            Console.WriteLine(@"Extracting area to " + Path.GetDirectoryName(outfilename) + @"...");
            foo.ExtractArea(packet);
            Console.WriteLine(@"Done!");
        }

        static void GDEMTest()
        {
            Console.WriteLine(@"Now testing GDEM extraction routines ...");
            var foo = new GDEM
                      {
                          DatabasePath = @"C:\Users\Graham Voysey\Desktop\GDEM-V\uncompressed\",
                          ExtractionProgramPath = @"C:\Projects\ESME Deliverables\trunk\Utilities\NetCDFExtractor\bin\x86\Debug\ImportNetCDF.exe",
                          WorkingDirectory = "",
                          MinMonth = 1,
                          MaxMonth = 4,
                          GridSpacing = 0.25f,
                      };

            const string outfilename = @"C:\tests\dbtests\gdem.txt";
            Console.WriteLine(@" Extracting data ...");
            var packet = new NAVOExtractionPacket
            {
                Filename = outfilename,
                North = Testpoints[0][0].Latitude_degrees,
                South = Testpoints[0][1].Latitude_degrees,
                East = Testpoints[0][0].Longitude_degrees,
                West = Testpoints[0][1].Longitude_degrees,
            };
            Console.WriteLine(@"Extracting area to " + Path.GetDirectoryName(outfilename) + @"...");
            foo.ExtractArea(packet);
            Console.WriteLine(@"    done!");

            var eebFileName = Path.Combine(Path.GetDirectoryName(outfilename), Path.GetFileNameWithoutExtension(outfilename)) + ".eeb";
            var eebFile = DataFile.Create(eebFileName);
            var extractedArea = ((Environment3DData)foo.ExtractedArea);
            var eebLayer = new DataLayer("soundspeed", "spring", "various", "", new DataAxis("latitude", extractedArea.Latitudes.Select(x => (float)x).ToArray()), new DataAxis("longitude", extractedArea.Longitudes.Select(x => (float)x).ToArray()), new DataAxis("depth", extractedArea.Depths.Select(x => (float)x).ToArray()));
            eebFile.Layers.Add(eebLayer);
            var dataPoint = new DataPoint(eebLayer);
            var dataValues = new float[extractedArea.Depths.Length];

            for (var lonIndex = 0; lonIndex < extractedArea.Values.GetLength(0); lonIndex++)
                for (var latIndex = 0; latIndex < extractedArea.Values.GetLength(1); latIndex++)
                {
                    var curOutputPoint = extractedArea.Values[lonIndex, latIndex];
                    if (curOutputPoint == null) continue;
                    for (var depIndex = 0; depIndex < extractedArea.Depths.Length; depIndex++) dataValues[depIndex] = curOutputPoint.Count > depIndex ? curOutputPoint[depIndex] : float.NaN;
                    dataPoint.Data = dataValues;
                }
            Console.WriteLine(string.Format("GDEM : Data extracted to {0}", eebFileName));
            eebFile.Close();
            Console.WriteLine(@"Done!");
        }
    }
}