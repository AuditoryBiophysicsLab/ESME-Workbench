using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using ESME.Environment.NAVO;
using HRC.Navigation;

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
            var isCSV = false;
            var separators = new[] {",", " "};
            if (args.Length == 0)
            {
                Usage();
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
                        elements = args[++argIndex].Split(separators, StringSplitOptions.RemoveEmptyEntries);
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
                        elements = args[++argIndex].Split(separators, StringSplitOptions.RemoveEmptyEntries);
                        if (!elements.Any()) Usage("Receiver range data was not specified");
                        temperatures = elements.Select(double.Parse).ToList();
                        break;
                    case "-salinities":
                        elements = args[++argIndex].Split(separators, StringSplitOptions.RemoveEmptyEntries);
                        if (!elements.Any()) Usage("Receiver range data was not specified");
                        salinities = elements.Select(double.Parse).ToList();
                        break;
                    case "-algorithm":
                        algorithm = args[++argIndex];
                        break;
                    case "-csv":
                        isCSV = true;
                        break;
                    default:
                        Usage();
                        return -1;
                }
            }
            if (double.IsNaN(latitude))
            {
                Usage("-latitude was not specified");
                return -1;
            }

            if (depths == null)
            {
                Usage("-depths was not specified");
                return -1;
            }
            if (temperatures == null)
            {
                Usage("-temperatures was not specified");
                return -1;
            }
            if (algorithm != null && string.Compare(algorithm, "chen.millero.li", true, CultureInfo.InvariantCulture) != 0)
            {
                Usage("-algorithm was specified and was not 'chen.millero.li', which is currently the only supported algorithm");
                return -1;
            }

            if (depths.Count() != temperatures.Count())
            {
                Usage("-depths and -temperatures must contain the same number of elements");
                return -1;
            }
            if (salinities != null && depths.Count() != salinities.Count())
            {
                Usage("-salinities, when specified, must contain the same number of elements as -depths and -temperatures");
                return -1;
            }

            try
            {
                var location = new EarthCoordinate(latitude, 0);
                var curSalinity = 0.0;
                if (isCSV) Console.WriteLine("\"Depth (m)\", \"Sound Speed (m/s)\"");
                for (var index = 0; index < depths.Count(); index++)
                {
                    if (salinities != null) curSalinity = salinities[index];
                    var result = ChenMilleroLi.SoundSpeed(location, (float)depths[index], (float)temperatures[index], (float)curSalinity);
                    Console.WriteLine("{0:0.####}{1}{2:0.####}", depths[index], isCSV ? ", " : " ", result);
                }
                return 0;
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
            Console.WriteLine("                        [-csv]");
            Console.WriteLine();
            Console.WriteLine("Description: Calculate an underwater sound speed profile given a temperature");
            Console.WriteLine("             and optional salinity vector, using a specified algorithm");
            Console.WriteLine();
            Console.WriteLine("Where: <latitude> is the latitude, in degrees, for which the sound speed");
            Console.WriteLine("                  profile is being calculated. Latitude is needed because the");
            Console.WriteLine("                  rotational speed of the Earth has a small effect on the the");
            Console.WriteLine("                  water pressure at a given depth, which is in turn needed to");
            Console.WriteLine("                  calculate the sound speed profile.");
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
            Console.WriteLine("                    measured or estimated for each of the depths in the");
            Console.WriteLine("                    <depths> list. Salinity data is specified in parts per");
            Console.WriteLine("                    thousand (ppt).");
            Console.WriteLine();
            Console.WriteLine("       <algorithmSelector> is the name of one of the algorithms supported by");
            Console.WriteLine("                           this utility.  Currently, the only supported");
            Console.WriteLine("                           algorithm is chen.millero.li, which is also the");
            Console.WriteLine("                           default for this parameter.");
            Console.WriteLine();
            Console.WriteLine("       -csv will write the resulting sound speed profile to standard output");
            Console.WriteLine("            in CSV (comma-separated values) format, with a header for each");
            Console.WriteLine("            of the two columns of data.  If -csv is not specified, the");
            Console.WriteLine("            sound speed profile will be written to standard output without");
            Console.WriteLine("            column headers and spaces between the values in each column.");
            Console.WriteLine();
            Console.WriteLine("            In either case, the first column of output is depth in meters");
            Console.WriteLine("            and the second column is sound speed in meters per second at");
            Console.WriteLine("            the indicated depth.");
            Console.WriteLine();
            if (additionalErrorInfo != null) Console.WriteLine(additionalErrorInfo);
        }
    }
}
