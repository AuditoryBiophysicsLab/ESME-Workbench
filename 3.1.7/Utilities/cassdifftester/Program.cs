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
            string esmeInputFile = null;
            string nemoInputFile = null;
            string outputFile = null;

            for (var i = 0; i < args.Length; i++)
            {
                switch (args[i])
                {
                    case "-esme":
                        esmeInputFile = args[++i];
                        break;
                    case "-nemo":
                        nemoInputFile = args[++i];
                        break;
                    case "-out":
                        outputFile = args[++i];
                        break;
                    default:
                        return;
                }
            }

            if (string.IsNullOrEmpty(esmeInputFile) || string.IsNullOrEmpty(nemoInputFile) || string.IsNullOrEmpty(outputFile))
            {
                Usage();
                return;
            }

            if (!File.Exists(esmeInputFile))
            {
                Console.WriteLine(@"Specified ESME input file not found");
                Usage();
                return;
            }

            if (!File.Exists(nemoInputFile))
            {
                Console.WriteLine(@"Specified NEMO input file not found");
                Usage();
                return;
            }

            var esmeFile = NAEMOEnvironmentFile.Load(esmeInputFile);
            var esmeResult = (from packet in esmeFile.Locations
                              orderby packet.Latitude , packet.Longitude
                              select packet).ToList();

            var naemoFile = NAEMOEnvironmentFile.Load(nemoInputFile);
            var nemoResult = (from packet in naemoFile.Locations
                              orderby packet.Latitude , packet.Longitude
                              select packet).ToList();

            var esmeMatchesNemo = (from esme in esmeResult
                                   from nemo in nemoResult
                                   where (esme.Equals(nemo))
                                   orderby nemo.Latitude, nemo.Longitude
                                   select new
                                   {
                                       esme,
                                       nemo
                                   }).ToList();
            var nemoMatchesEsme = (from nemo in nemoResult
                                   from esme in esmeResult
                                   where (nemo.Equals(esme))
                                   orderby nemo.Latitude , nemo.Longitude
                                   select new
                                          {
                                              esme,
                                              nemo
                                          }).ToList();
            var outFile = new StringBuilder();

            outFile.AppendLine(string.Format("ESME file contains {0} records", esmeResult.Count));
            outFile.AppendLine(string.Format("NEMO file contains {0} records", nemoResult.Count));
            if (esmeMatchesNemo.Count < esmeResult.Count)
            {
                outFile.AppendLine(string.Format("ESME locations matched {0} locations in NEMO file. {1} locations were not matched.", esmeMatchesNemo.Count, esmeResult.Count - esmeMatchesNemo.Count));
                foreach (var esme in esmeResult)
                {
                    var matched = false;
                    foreach (var nemo in nemoResult.Where(nemo => esme.Equals(nemo)))
                    {
                        matched = true;
                    }
                    if (!matched) outFile.AppendLine(string.Format("ESME location {0} is not in the NEMO file", esme));
                }
            }
            else
                outFile.AppendLine("All ESME records matched NEMO locations");
            if (nemoMatchesEsme.Count < nemoResult.Count)
            {
                outFile.AppendLine(string.Format("NEMO locations matched {0} locations in ESME file. {1} locations were not matched.", nemoMatchesEsme.Count, nemoResult.Count - nemoMatchesEsme.Count));
                foreach (var nemo in nemoResult)
                {
                    var matched = false;
                    foreach (var esme in esmeResult.Where(esme => nemo.Equals(esme)))
                    {
                        matched = true;
                    }
                    if (!matched) outFile.AppendLine(string.Format("NEMO location {0} is not in the ESME file", nemo));
                }
            }
            else
                outFile.AppendLine("All ESME records matched NEMO locations");
            outFile.AppendLine();

            var bottomcounter = 0;
            outFile.AppendLine("Bottom Type Differences");
            outFile.AppendLine("    Location       ESME                 NEMO                ");
            outFile.AppendLine("------------------ -------------------- --------------------");
            foreach (var result in esmeMatchesNemo.Where(result => result.nemo.BottomType != result.esme.BottomType))
            {
                //outFile.AppendLine(string.Format("{0}: esme bottom type is {1} but NEMO is {2}", result.esme.Location, result.esme.BottomType, result.NEMO.BottomType));
                outFile.AppendLine(string.Format("{0,-18} {1,-20} {2,-20}", result.esme, result.esme.BottomType, result.nemo.BottomType));
                bottomcounter++;
            }
            outFile.AppendLine(string.Format("{0} differences found", bottomcounter));
            outFile.AppendLine("");

            var depthcounter = 0;
            outFile.AppendLine("Depth Vector Length Differences");
            outFile.AppendLine("    Location       ESME NEMO");
            outFile.AppendLine("------------------ ---- ----");
            foreach (var result in esmeMatchesNemo.Where(result => result.nemo.Depths.Count != result.esme.Depths.Count))
            {
                //outFile.AppendLine(string.Format("{0}: esme bottom type is {1} but NEMO is {2}", result.esme.Location, result.esme.Depths.Count, result.NEMO.Depths.Count));
                outFile.AppendLine(string.Format("{0,-18} {1,4} {2,4}", result.esme, result.esme.Depths.Count, result.nemo.Depths.Count));
                depthcounter++;
            }
            outFile.AppendLine(string.Format("{0} differences found", depthcounter));
            outFile.AppendLine("");

            var soundspeedcounter = 0;
            const double maxDiff = 0.1;
            outFile.AppendLine("First Sound Speed Differences > " + maxDiff);
            outFile.AppendLine("    Location       Depth      ESME      NEMO     Diff   Diff %");
            outFile.AppendLine("------------------ ----- --------- --------- -------- --------");
            foreach (var result in esmeMatchesNemo)
            {
                for (var i = 0; i < result.esme.Soundspeeds.Count; i++)
                {
                    var esmespeed = result.esme.Soundspeeds[i];
                    var nemospeed = result.nemo.Soundspeeds[i];
                    var diff = esmespeed - nemospeed;
                    var pct = (1.0 - (esmespeed / nemospeed)) * 100.0;
                    //if (esmespeed != NEMOspeed)
                    //if ((1 - (Math.Min(esmespeed, NEMOspeed) / Math.Max(esmespeed, NEMOspeed))) > 0.005)
                    if (Math.Abs(esmespeed - nemospeed) > maxDiff)
                    {
                        outFile.AppendLine(string.Format("{0,-18} {1,5:0.0} {2,9:0.000} {3,9:0.000} {4,8:0.000} {5,8:0.00000}", result.esme, result.esme.Depths[i], esmespeed, nemospeed, diff, pct));
                        soundspeedcounter++;
                        break;
                    }
                }
            }
            outFile.AppendLine(string.Format("{0} differences found", soundspeedcounter));
            outFile.AppendLine("");

            var windspeedcounter = 0;
            outFile.AppendLine("Wind Speed Differences");
            outFile.AppendLine("Lat/Lon               ESME    NEMO");
            outFile.AppendLine("------------------ ------- -------");
            foreach (var result in esmeMatchesNemo.Where(result => (Math.Abs(result.nemo.WindSpeed - result.esme.WindSpeed) > 0.0001) && (result.esme.Equals(result.nemo))))
            {
                //outFile.AppendLine(string.Format("{0}: esme bottom type is {1} but NEMO is {2}", result.esme.Location, result.esme.Depths.Count, result.NEMO.Depths.Count));
                outFile.AppendLine(string.Format("{0,-18} {1,7} {2,7}", result.esme, result.esme.WindSpeed, result.nemo.WindSpeed));
                windspeedcounter++;
            }
            outFile.AppendLine(string.Format("{0} differences found", windspeedcounter));
            File.WriteAllText(outputFile, outFile.ToString());
            Console.WriteLine("Comparison Data written to file " + outputFile);
            Console.Write(outFile);
            //Console.ReadLine();
        }

        static void Usage()
        {
            Console.WriteLine("Usage: cassdifftester -esme <ESME_created_env_file> -nemo <NEMO_created_env_file> -out <outputFilename>");
        }
    }
}