using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using ESME.Environment;
using HDF5DotNet;
using HRC.Navigation;

namespace DavesConsoleTester
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length != 2)
            {
                Console.WriteLine(@"Need 2 args - source CHB and destination text file names");
                return;
            }
            var result = SedimentOld.FromSedimentCHB(args[0]);
            using (var writer = new StreamWriter(args[1]))
            {
                for (var lat = 0; lat < result.Latitudes.Count; lat++)
                    for (var lon = 0; lon < result.Longitudes.Count; lon++)
                    {
                        writer.WriteLine(@"{0:0.00000} {1:0.00000} {2}", result.Latitudes[lat], result.Longitudes[lon], result.FieldData[lon, lat].Data);
                        if (result.FieldData[lon, lat].Data != 9) Console.WriteLine(@"non-sand found at location {0:0.00000} {1:0.00000} {2}", result.Latitudes[lat], result.Longitudes[lon], result.FieldData[lon, lat].Data);
                    }
            }
        }
    }
}
