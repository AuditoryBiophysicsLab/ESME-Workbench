using System;
using System.IO;
using System.Linq;
using System.Text;
using ESME.TransmissionLoss.CASS;

namespace cassdifftester
{
    internal class Program
    {
        static void Main(string[] args)
        {
            var esmeinfile = "";
            var nuwcinfile = "";
            var outFileName = "";

            for (var i = 0; i < args.Length; i++)
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
                              orderby packet.Location.Latitude , packet.Location.Longitude
                              select packet).ToList();

            var nuwcResult = (from packet in CASSFiles.ReadEnvironmentFile(nuwcinfile)
                              orderby packet.Location.Latitude , packet.Location.Longitude
                              select packet).ToList();

            var esmeMatchesNuwc = (from esme in esmeResult
                                   from nuwc in nuwcResult
                                   where (esme.Location.Equals(nuwc.Location))
                                   orderby nuwc.Location.Latitude, nuwc.Location.Longitude
                                   select new
                                   {
                                       esme,
                                       nuwc
                                   }).ToList();
            var nuwcMatchesEsme = (from nuwc in nuwcResult
                                   from esme in esmeResult
                                   where (nuwc.Location.Equals(esme.Location))
                                   orderby nuwc.Location.Latitude , nuwc.Location.Longitude
                                   select new
                                          {
                                              esme,
                                              nuwc
                                          }).ToList();
            var outFile = new StringBuilder();

            outFile.AppendLine(string.Format("ESME file contains {0} records", esmeResult.Count));
            outFile.AppendLine(string.Format("NUWC file contains {0} records", nuwcResult.Count));
            if (esmeMatchesNuwc.Count < esmeResult.Count)
            {
                outFile.AppendLine(string.Format("ESME locations matched {0} locations in NUWC file. {1} locations were not matched.", esmeMatchesNuwc.Count, esmeResult.Count - esmeMatchesNuwc.Count));
                foreach (var esme in esmeResult)
                {
                    var matched = false;
                    var esme1 = esme;
#pragma warning disable 168
                    foreach (var nuwc in nuwcResult.Where(nuwc => esme1.Location.Equals(nuwc.Location)))
#pragma warning restore 168
                    {
                        matched = true;
                    }
                    if (!matched) outFile.AppendLine(string.Format("ESME location {0} is not in the NUWC file", esme.Location));
                }
            }
            else
                outFile.AppendLine("All ESME records matched NUWC locations");
            if (nuwcMatchesEsme.Count < nuwcResult.Count)
            {
                outFile.AppendLine(string.Format("NUWC locations matched {0} locations in ESME file. {1} locations were not matched.", nuwcMatchesEsme.Count, nuwcResult.Count - nuwcMatchesEsme.Count));
                foreach (var nuwc in nuwcResult)
                {
                    var matched = false;
                    var nuwc1 = nuwc;
#pragma warning disable 168
                    foreach (var esme in esmeResult.Where(esme => nuwc1.Location.Equals(esme.Location)))
#pragma warning restore 168
                    {
                        matched = true;
                    }
                    if (!matched) outFile.AppendLine(string.Format("NUWC location {0} is not in the ESME file", nuwc.Location));
                }
            }
            else
                outFile.AppendLine("All ESME records matched NUWC locations");
            outFile.AppendLine();

            var bottomcounter = 0;
            foreach (var result in esmeMatchesNuwc.Where(result => result.nuwc.BottomType != result.esme.BottomType))
            {
                if (bottomcounter == 0)
                {
                    outFile.AppendLine("Bottom Type Differences");
                    outFile.AppendLine("    Location       ESME                 NUWC                ");
                    outFile.AppendLine("------------------ -------------------- --------------------");
                }
                //outFile.AppendLine(string.Format("{0}: esme bottom type is {1} but nuwc is {2}", result.esme.Location, result.esme.BottomType, result.nuwc.BottomType));
                outFile.AppendLine(string.Format("{0,-18} {1,-20} {2,-20}", result.esme.Location, result.esme.BottomType, result.nuwc.BottomType));
                bottomcounter++;
            }
            outFile.AppendLine(bottomcounter == 0 ? "Bottom Types match." : string.Format("{0} differences found", bottomcounter));
            outFile.AppendLine("");

            var depthcounter = 0;
            foreach (var result in esmeMatchesNuwc.Where(result => result.nuwc.Depths.Count != result.esme.Depths.Count))
            {
                if (depthcounter == 0)
                {
                    outFile.AppendLine("Depth Length Differences");
                    outFile.AppendLine("    Location       ESME NUWC");
                    outFile.AppendLine("------------------ ---- ----");
                }
                //outFile.AppendLine(string.Format("{0}: esme bottom type is {1} but nuwc is {2}", result.esme.Location, result.esme.Depths.Count, result.nuwc.Depths.Count));
                outFile.AppendLine(string.Format("{0,-18} {1,4} {2,4}", result.esme.Location, result.esme.Depths.Count, result.nuwc.Depths.Count));
                depthcounter++;
            }
            outFile.AppendLine(depthcounter == 0 ? "Depths match." : string.Format("{0} differences found", depthcounter));
            outFile.AppendLine("");

            var soundspeedcounter = 0;
            foreach (var result in esmeMatchesNuwc)
            {
                for (var i = 0; i < result.esme.Soundspeeds.Count; i++)
                {
                    var esmespeed = result.esme.Soundspeeds[i];
                    var nuwcspeed = result.nuwc.Soundspeeds[i];
                    var diff = esmespeed - nuwcspeed;
                    var pct = (1.0 - (esmespeed / nuwcspeed)) * 100.0;
                    //if (esmespeed != nuwcspeed)
                    //if ((1 - (Math.Min(esmespeed, nuwcspeed) / Math.Max(esmespeed, nuwcspeed))) > 0.005)
                    const double maxDiff = 0.1;
                    if (Math.Abs(esmespeed - nuwcspeed) > maxDiff)
                    {
                        if (soundspeedcounter == 0)
                        {
                            outFile.AppendLine("Sound Speed First Differences > " + maxDiff);
                            outFile.AppendLine("    Location       Depth      ESME      NUWC     Diff   Diff %");
                            outFile.AppendLine("------------------ ----- --------- --------- -------- --------");
                        }
                        outFile.AppendLine(string.Format("{0,-18} {1,5:0.0} {2,9:0.000} {3,9:0.000} {4,8:0.000} {5,8:0.00000}", result.esme.Location, result.esme.Depths[i], esmespeed, nuwcspeed, diff, pct));
                        soundspeedcounter++;
                        break;
                    }
                }
            }
            outFile.AppendLine(soundspeedcounter == 0 ? "Soundspeeds match." : string.Format("{0} differences found", soundspeedcounter));
            outFile.AppendLine("");

            var windspeedcounter = 0;
            foreach (var result in esmeMatchesNuwc.Where(result => (result.nuwc.WindSpeed != result.esme.WindSpeed) && (result.esme.Location.Equals(result.nuwc.Location))))
            {
                if (windspeedcounter == 0)
                {
                    outFile.AppendLine("Wind Speed Differences");
                    outFile.AppendLine("Lat/Lon               ESME    NUWC");
                    outFile.AppendLine("------------------ ------- -------");
                }
                //outFile.AppendLine(string.Format("{0}: esme bottom type is {1} but nuwc is {2}", result.esme.Location, result.esme.Depths.Count, result.nuwc.Depths.Count));
                outFile.AppendLine(string.Format("{0,-18} {1,7} {2,7}", result.esme.Location, result.esme.WindSpeed, result.nuwc.WindSpeed));
                windspeedcounter++;
            }
            outFile.AppendLine(depthcounter == 0 ? "Wind Speeds match." : string.Format("{0} differences found", windspeedcounter));
            File.WriteAllText(outFileName, outFile.ToString());
            Console.WriteLine("Comparison Data written to file " + outFileName);
            Console.Write(outFile);
            //Console.ReadLine();
        }
    }
}