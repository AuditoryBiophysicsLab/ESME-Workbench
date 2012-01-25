using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SoundSpeedProfile
{
    class Program
    {
        static int Main(string[] args)
        {
            var latitude = double.NaN;
            List<double> depths = null;
            List<double> temperatures = null;
            List<double> salinities = null;
            string algorithm = null;
            if (args.Length == 0)
            {
                Usage("No arguments specified");
                return -1;
            }
            for (var argIndex = 0; argIndex < args.Length; argIndex++)
            {
                var arg = args[argIndex];
                string[] elements;
                switch (arg.ToLower())
                {
                    case "-latitude":
                        latitude = double.Parse(args[++argIndex]);
                        break;
                    case "-depths":
                        elements = args[++argIndex].Split(',');
                        if (!elements.Any()) Usage("Receiver range data was not specified");
                        depths = elements.Select(double.Parse).ToList();
                        for (var index = 0; index < depths.Count - 1; index++)
                            if (depths[index] >= depths[index + 1])
                            {
                                Usage("depths must increase with each entry");
                                return -1;
                            }
                        break;
                    case "-temperatures":
                        elements = args[++argIndex].Split(',');
                        if (!elements.Any()) Usage("Receiver range data was not specified");
                        temperatures = elements.Select(double.Parse).ToList();
                        break;
                    case "-salinities":
                        elements = args[++argIndex].Split(',');
                        if (!elements.Any()) Usage("Receiver range data was not specified");
                        salinities = elements.Select(double.Parse).ToList();
                        break;
                    case "-algorithm":
                        algorithm = args[++argIndex];
                        break;
                    case "-?":
                    case "-help":
                    default:
                        Usage();
                        return -1;
                }
            }
            if (double.IsNaN(latitude)) Usage("-latitude was not specified");
            if (algorithm == null) Usage("-algorithm was not specified");
            if (depths == null) Usage("-depths was not specified");
            if (temperatures == null) Usage("-temperatures was not specified");
            if (salinities == null) Usage("-salinities was not specified");
            try
            {
                //CreateBellhopEnvironment(outputDirectory, name, frequency, bathymetryRanges, bathymetryDepths, soundspeedDepths, soundspeedSpeeds, receiverRanges, depths, sedimentType);
            }
            catch (Exception ex)
            {
                Usage(ex.Message);
            }
            return 0;
        }

        public static void Usage(string additionalErrorInfo = null)
        {
            Console.WriteLine("Usage: SoundSpeedProfile -latitude <latitude>");
            Console.WriteLine("                         -depths <depths>");
            Console.WriteLine("                         -temperatures <temperatures>");
            Console.WriteLine("                        [-salinities <salinities>]");
            Console.WriteLine("                        [-algorithm <algorithmSelector>]");
            Console.WriteLine();
            Console.WriteLine("Description: Calculate an underwater sound speed profile given a temperature");
            Console.WriteLine("             and optional salinity vector, using a specified algorithm");
            Console.WriteLine();
            Console.WriteLine("Where: <latitude> is the latitude, in degrees, for which the sound speed profile");
            Console.WriteLine("                  is being calculated. Latitude is needed because the rotational");
            Console.WriteLine("                  speed of the Earth has a small effect on the the water pressure");
            Console.WriteLine("                  at a given depth, which is in turn needed to calculate the");
            Console.WriteLine("                  sound speed profile.");
            Console.WriteLine();
            Console.WriteLine("       <depths> is a comma-separated list of depths that correspond to the");
            Console.WriteLine("                temperature and (optional) salinity values.  These depths");
            Console.WriteLine("                should start at zero and increase to the maximum depth for");
            Console.WriteLine("                which you have temperature and (optionally) salinity data.");
            Console.WriteLine();
            Console.WriteLine("       <temperatures> is a comma-separated list of water temperatures measured");
            Console.WriteLine("                      or estimated for each of the depths in the <depths> list.");
            Console.WriteLine("                      Temperature data is specified in degrees C.");
            Console.WriteLine();
            Console.WriteLine("       <salinities> is an optional, comma-separated list of salinity values");
            Console.WriteLine("                    measured or estimated for each of the depths in the <depths>.");
            Console.WriteLine("                    list. Salinity data is specified in parts per thousand (ppt).");
            Console.WriteLine();
            Console.WriteLine("       <algorithmSelector> is the name of one of the algorithms supported by this");
            Console.WriteLine("                           utility.  Currently, the only supported algorithm is");
            Console.WriteLine("                           chen.millero.li, which is also the default for this");
            Console.WriteLine("                           parameter.");
            Console.WriteLine();
            if (additionalErrorInfo != null) Console.WriteLine(additionalErrorInfo);
        }
    }
}
