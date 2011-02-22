using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Text;
using ESME.Overlay;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.CASS;
using HRC.Navigation;

namespace cassdifftester
{
    class Program
    {
        static void Main(string[] args)
        {

            string esmeinfile = "";//@"C:\Users\Graham Voysey\Desktop\cass\esme_env_january.dat";
            string nuwcinfile = "";// @"C:\Users\Graham Voysey\Desktop\cass\env_january.dat";
            string outFileName = "";
            for (int i = 0; i < args.Length; i++)
            {
                switch (args[i])
                {
                    case "-esme":
                        esmeinfile = args[++i];
                        break;
                    case "-nuwc":
                        nuwcinfile = args[++i];
                        break;
                    case "-out":
                        outFileName = args[++i];
                        break;
                    default:
                        return;
                }

            }

            var esmeResult = (from packet in CASSFiles.ReadEnvironmentFile(esmeinfile)
                              orderby packet.Location.Latitude_degrees, packet.Location.Longitude_degrees
                              select packet).ToList();

            var nuwcResult = (from packet in CASSFiles.ReadEnvironmentFile(nuwcinfile)
                              orderby packet.Location.Latitude_degrees, packet.Location.Longitude_degrees
                              select packet).ToList();

            var joinedResult = (from esme in esmeResult
                                from nuwc in nuwcResult
                                where (esme.Location.Equals(nuwc.Location))
                                orderby nuwc.Location.Latitude_degrees, nuwc.Location.Longitude_degrees
                                select new
                                    {
                                        esme,
                                        nuwc
                                    }).ToList();
            var outFile = new StringBuilder();
            
            if (joinedResult.Count < esmeResult.Count)
            {
                outFile.AppendLine(string.Format("{0} records were extracted from ESME source.  {1} location matches were found in NUWC source. {2} locations were not matched.", esmeResult.Count, joinedResult.Count, nuwcResult.Count - joinedResult.Count));
                foreach (var esme in esmeResult)
                {
                    var matched = false;
                    foreach (var nuwc in nuwcResult.Where(nuwc => esme.Location.Equals(nuwc.Location)))
                    {
                        matched = true;
                    }
                    if (!matched) outFile.AppendLine(string.Format("esme location {0} is not a nuwc location.", esme.Location));
                }
            }
            else outFile.AppendLine(string.Format("All {0:000} esme records were matched on location\n", esmeResult.Count));

            var bottomcounter = 0;
            foreach (var result in joinedResult.Where(result => result.nuwc.BottomType != result.esme.BottomType))
            {
                if (bottomcounter == 0)
                {
                    outFile.AppendLine("Bottom Type Differences");
                    outFile.AppendLine("Lat/Lon           \tESME                 \tNUWC                ");
                    outFile.AppendLine("------------------\t-------------------- \t--------------------");
                }
                //outFile.AppendLine(string.Format("{0}: esme bottom type is {1} but nuwc is {2}", result.esme.Location, result.esme.BottomType, result.nuwc.BottomType));
                outFile.AppendLine(string.Format("{0,-18}\t{1,-20}\t{2,-20}", result.esme.Location, result.esme.BottomType, result.nuwc.BottomType));
                bottomcounter++;
            }
            outFile.AppendLine(bottomcounter == 0 ? "Bottom Types match." : string.Format("{0:00} Bottom Types differ", bottomcounter));
            outFile.AppendLine("");

            var depthcounter = 0;
            foreach (var result in joinedResult.Where(result => result.nuwc.Depths.Count != result.esme.Depths.Count))
            {
                if (depthcounter == 0)
                {
                    outFile.AppendLine("Depth Length Differences");
                    outFile.AppendLine("Lat/Lon           \tESME \tNUWC");
                    outFile.AppendLine("------------------\t---- \t----");
                }
                //outFile.AppendLine(string.Format("{0}: esme bottom type is {1} but nuwc is {2}", result.esme.Location, result.esme.Depths.Count, result.nuwc.Depths.Count));
                outFile.AppendLine(string.Format("{0,-18}\t{1,-4}\t{2,-4}", result.esme.Location, result.esme.Depths.Count, result.nuwc.Depths.Count));
                depthcounter++;
            }
            outFile.AppendLine(depthcounter == 0 ? "Depths match." : string.Format("{0:00} Depths differ", depthcounter));
            outFile.AppendLine("");

            var soundspeedcounter = 0;
            foreach (var result in joinedResult)
            {
                double esmefailspeed = 0;
                double nuwcfailspeed = 0;
                double faildepth = 0;
                if (soundspeedcounter == 0)
                {
                    outFile.AppendLine("Sound Speed Differences");
                    outFile.AppendLine("Lat/Lon           \tDepth\tESME     \tNUWC");
                    outFile.AppendLine("------------------\t-----\t---------\t----");
                }
                for (var i = 0; i < result.esme.Soundspeeds.Count; i++)
                {
                    var esmespeed = result.esme.Soundspeeds[i];
                    var nuwcspeed = result.nuwc.Soundspeeds[i];
                    if (esmespeed != nuwcspeed)
                    {
                        soundspeedcounter++;
                        esmefailspeed = esmespeed;
                        nuwcfailspeed = nuwcspeed;
                        faildepth = result.esme.Depths[i];
                        break;
                    }
                }

                //outFile.AppendLine(string.Format("{0}: esme bottom type is {1} but nuwc is {2}", result.esme.Location, result.esme.Depths.Count, result.nuwc.Depths.Count));
                outFile.AppendLine(string.Format("{0,-18}\t{1:0.0}\t{2:0.000}\t{3,-5}", result.esme.Location, faildepth, esmefailspeed, nuwcfailspeed));
                
            }
            outFile.AppendLine(soundspeedcounter == 0 ? "Soundspeeds match." : string.Format("{0:00} Soundspeeds differ", soundspeedcounter));
            outFile.AppendLine("");

            var windspeedcounter = 0;
            foreach (var result in joinedResult.Where(result => (result.nuwc.WindSpeed != result.esme.WindSpeed) && (result.esme.Location.Equals(result.nuwc.Location))))
            {
                if (windspeedcounter == 0)
                {
                    outFile.AppendLine("Wind Speed Differences");
                    outFile.AppendLine("Lat/Lon           \tESME \tNUWC");
                    outFile.AppendLine("------------------\t---- \t----");
                }
                //outFile.AppendLine(string.Format("{0}: esme bottom type is {1} but nuwc is {2}", result.esme.Location, result.esme.Depths.Count, result.nuwc.Depths.Count));
                outFile.AppendLine(string.Format("{0,-18}\t{1,-4}\t{2,-4}", result.esme.Location, result.esme.WindSpeed, result.nuwc.WindSpeed));
                windspeedcounter++;
            }
            outFile.AppendLine(depthcounter == 0 ? "Wind Speeds match." : string.Format("{0:00} Wind Speeds differ", windspeedcounter));
            File.WriteAllText(outFileName, outFile.ToString());
            Console.WriteLine("Comparison Data written to file " + outFileName);
            Console.Write(outFile);
            Console.ReadLine();
        }
    }


}
