using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using ESME.Environment;
using ESME.Environment.NAVO;
using HDF5DotNet;
using HRC.Navigation;

namespace DavesConsoleTester
{
    class Program
    {
        static void Main(string[] args)
        {
            var commandName = Path.GetFileNameWithoutExtension(Environment.GetCommandLineArgs()[0]).ToLower();
            switch (commandName)
            {
                case "chb2txt":
                    if (args.Length != 2)
                        Console.WriteLine("Need 2 args - source CHB and destination text file names");
                    else chb2txt(args[0], args[1]);
                    break;
                case "cmpxmlchb":
                default:
                    if (args.Length != 3)
                        Console.WriteLine("Need 3 args - XML file, low res CHB, high res CHB");
                    else cmpxmlchb(args[0], args[1], args[2]);
                    break;
            }
        }

        const float MaxDistanceForMatchMeters = 10f;

        static void cmpxmlchb(string xmlFile, string lowResCHB, string highResCHB)
        {
            float north, south, east, west;
            var sediment = Sediment.Load(xmlFile);
            Console.WriteLine("XML bounds: [N: {0}, S: {1}, E: {2}, W: {3}]", sediment.Samples.Last().Latitude, sediment.Samples.First().Latitude, sediment.Samples.Last().Longitude, sediment.Samples.First().Longitude);
            var hasLowResData = false;
            var hasHighResData = false;
            foreach (var sample in sediment.Samples)
            {
                if (sample.Data.Resolution == "5m") hasLowResData = true;
                if (sample.Data.Resolution == "6s") hasHighResData = true;
            }
            
            if (hasLowResData)
            {
                Console.WriteLine("XML contains low resolution data.  Comparing to low resolution CHB...");
                Console.Write("Converting low res CHB to text...");
                chb2txt(lowResCHB, Path.GetFileNameWithoutExtension(lowResCHB) + ".txt");
                Console.Write("done\r");
                var data = ReadSedimentCHB(lowResCHB, out north, out south, out east, out west);
                Console.WriteLine("CHB bounds: [N: {0}, S: {1}, E: {2}, W: {3}]", north, south, east, west);
                CompareXMLtoCHB(sediment, data, "5m");
            }
            else Console.WriteLine("XML does not contain low resolution data"); 

            if (hasHighResData)
            {
                Console.WriteLine("XML contains high resolution data.  Comparing to high resolution CHB...");
                Console.Write("Converting high res CHB to text...");
                chb2txt(highResCHB, Path.GetFileNameWithoutExtension(highResCHB) + ".txt");
                Console.WriteLine("done");
                var data = ReadSedimentCHB(highResCHB, out north, out south, out east, out west);
                Console.WriteLine("CHB bounds: [N: {0}, S: {1}, E: {2}, W: {3}]", north, south, east, west);
                CompareXMLtoCHB(sediment, data, "6s");
            }
            else Console.WriteLine("XML does not contain high resolution data"); 
        }

        static void CompareXMLtoCHB(Sediment sediment, IEnumerable<SedimentSampleOld> chb, string resolution)
        {
            var resolutionMismatchCount = 0;
            var matchCount = 0;
            var missingCount = 0;
            var progress = 0;
            var total = chb.Count();
            foreach (var sample in chb)
            {
                progress++;
                if ((progress % 1000) == 0)
                    Console.Write("Progress: {0:0.00}%\r", ((float)progress / total) * 100);

                var nearestSample = sediment.Samples[sample];

                if (sample.DistanceKilometers(nearestSample) > (MaxDistanceForMatchMeters / 1000f))
                {
                    missingCount++;
                    continue;
                }

                if (nearestSample.Data.Resolution != resolution)
                {
                    resolutionMismatchCount++;
                    continue;
                }
                matchCount++;
                
                if (!sample.Data.HasValue)
                    Console.WriteLine("XML/CHB mismatch at {0}, {1}, Resolution {2}, XML: {3}, CHB: No data", sample.Latitude, sample.Longitude, resolution, nearestSample.Data.SampleValue);

                if ((sample.Data.HasValue) && (sample.Data.Value != nearestSample.Data.SampleValue))
                    Console.WriteLine("XML/CHB mismatch at {0}, {1}, Resolution {2}, XML: {3}, CHB: {4}", sample.Latitude, sample.Longitude, resolution, nearestSample.Data.SampleValue, sample.Data.Value);
            }

            Console.WriteLine("CHB sample count: {0}", chb.Count());
            Console.WriteLine("     XML matches: {0} (res/value match, XML distance < {1}m)", matchCount, MaxDistanceForMatchMeters);
            Console.WriteLine("  Missing in XML: {0} (nearest XML sample distance > {1}m)", missingCount, MaxDistanceForMatchMeters);
            Console.WriteLine("  Res mismatches: {0} (nearest XML sample has different resolution)", resolutionMismatchCount);
            missingCount = resolutionMismatchCount = matchCount = 0;
            foreach (var curXml in sediment.Samples)
            {
                
                if (curXml.Data.Resolution != resolution)
                {
                    resolutionMismatchCount++;
                    continue;
                }

                var minDistance = double.MaxValue;
                SedimentSampleOld closestCHB = null;
                foreach (var item in chb)
                {
                    var curDistance = item.DistanceKilometers(curXml);
                    if (curDistance >= minDistance) continue;
                    minDistance = curDistance;
                    closestCHB = item;
                }

                if ((closestCHB == null) || (closestCHB.DistanceKilometers(curXml) > (MaxDistanceForMatchMeters / 1000f)))
                {
                    missingCount++;
                    continue;
                }

                matchCount++;
                if (!closestCHB.Data.HasValue)
                    Console.WriteLine("XML/CHB mismatch at {0}, {1}, Resolution {2}, XML: {3}, CHB: No data", closestCHB.Latitude, closestCHB.Longitude, resolution, curXml.Data.SampleValue);

                if ((closestCHB.Data.HasValue) && (closestCHB.Data.Value != curXml.Data.SampleValue))
                    Console.WriteLine("XML/CHB mismatch at {0}, {1}, Resolution {2}, XML: {3}, CHB: {4}", closestCHB.Latitude, closestCHB.Longitude, resolution, curXml.Data.SampleValue, closestCHB.Data.Value);
            }

            Console.WriteLine("XML sample count: {0}", sediment.Samples.Count());
            Console.WriteLine("     CHB matches: {0} (res/value match, CHB distance < {1}m)", matchCount, MaxDistanceForMatchMeters);
            Console.WriteLine("  Missing in CHB: {0} (nearest CHB sample distance > {1}m)", missingCount, MaxDistanceForMatchMeters);
            Console.WriteLine("  Res mismatches: {0} (current CHB file has different resolution)", resolutionMismatchCount);
        }

        static List<SedimentSampleOld> ReadSedimentCHB(string fileName, out float north, out float south, out float east, out float west)
        {
            var result = new List<SedimentSampleOld>();
            using (var stream = new BinaryReader(File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                west = stream.ReadSingle();
                // east (unused)
                stream.ReadSingle();
                south = stream.ReadSingle();
                // north (unused)
                stream.ReadSingle();
                var gridSpacing = stream.ReadSingle() / 60f; // Source is in minutes, we need degrees
                var width = stream.ReadInt32();
                var height = stream.ReadInt32();
                var endian = stream.ReadUInt32();
                if (endian != 0x00010203) throw new FileFormatException("Invalid CHRTR Binary file format - endian is incorrect");
                var minValue = stream.ReadSingle();
                var maxValue = stream.ReadSingle();
                var paddingWidth = (width - 10) * 4;
                if (paddingWidth < 0) throw new FileFormatException("Invalid Sediment File: File contains no data.  Likely cause is that selected area contains no sediment points, or too few of them.  This appears to be a bug in the sediment extraction program.");
                stream.ReadBytes(paddingWidth);
                for (var lat = 0; lat < height; lat++)
                {
                    var latitude = south + (lat * gridSpacing);
                    for (var lon = 0; lon < width; lon++)
                    {
                        var longitude = west + (lon * gridSpacing);
                        var curSampleValue = stream.ReadSingle();
                        var newSample = new SedimentSampleOld(latitude, lat, longitude, lon);
                        if ((minValue <= curSampleValue) && (curSampleValue <= maxValue)) newSample.Data = curSampleValue;
                        result.Add(newSample);
                    }
                }
                east = west + (width * gridSpacing);
                north = south + (height * gridSpacing);
                return result;
            }
        }


        static void chb2txt(string sourceCHB, string destinationText)
        {
            var result = SedimentOld.FromSedimentCHB(sourceCHB);
            using (var writer = new StreamWriter(destinationText))
            {
                for (var lat = 0; lat < result.Latitudes.Count; lat++)
                    for (var lon = 0; lon < result.Longitudes.Count; lon++)
                    {
                        writer.WriteLine("{0:0.00000} {1:0.00000} {2}", result.Latitudes[lat], result.Longitudes[lon], result.FieldData[lon, lat].Data);
                        //if (result.FieldData[lon, lat].Data != 9) Console.WriteLine("non-sand found at location {0:0.00000} {1:0.00000} {2}", result.Latitudes[lat], result.Longitudes[lon], result.FieldData[lon, lat].Data);
                    }
            }
        }
    }
}
