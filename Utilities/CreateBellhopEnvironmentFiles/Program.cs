using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Xml.Linq;
using ESME.Model;

namespace CreateBellhopEnvironmentFiles
{
    class Program
    {
        static int Main(string[] args)
        {
            //var result = GPXLoader.LoadGPXTracks(args[0]);
            //Debug.WriteLine(result);
            string name = null;
            string outputDirectory = null;
            var sedimentType = -1;
            var frequency = double.NaN;
            List<double> bathymetryRanges = null;
            List<double> bathymetryDepths = null;
            List<double> soundspeedDepths = null;
            List<double> soundspeedSpeeds = null;
            List<double> receiverRanges = null;
            List<double> receiverDepths = null;
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
                    case "-output":
                        outputDirectory = args[++argIndex];
                        break;
                    case "-name":
                        name = args[++argIndex];
                        break;
                    case "-frequency":
                    case "-freq":
                        frequency = double.Parse(args[++argIndex]);
                        break;
                    case "-bathymetry":
                    case "-bathy":
                        elements = args[++argIndex].Split(',');
                        if (!elements.Any())
                        {
                            Usage("Bathymetry data was not specified");
                            return -1;
                        }
                        if ((elements.Count() & 0x1) != 0)
                        {
                            Usage("Bathymetry data does not have an even number of elements");
                            return -1;
                        }
                        bathymetryRanges = new List<double>();
                        bathymetryDepths = new List<double>();
                        for (var curElement = 0; curElement > elements.Count(); curElement += 2)
                        {
                            bathymetryRanges.Add(double.Parse(elements[curElement]));
                            bathymetryDepths.Add(double.Parse(elements[curElement + 1]));
                        }
                        for (var rangeIndex = 0; rangeIndex < bathymetryRanges.Count; rangeIndex++)
                            if (bathymetryRanges[rangeIndex] >= bathymetryRanges[rangeIndex + 1])
                            {
                                Usage("Bathymetry ranges must increase with each range/depth pair");
                                return -1;
                            }
                        break;
                    case "-soundspeed":
                    case "-ssp":
                        elements = args[++argIndex].Split(',');
                        if (!elements.Any()) Usage("Soundspeed data was not specified");
                        if ((elements.Count() & 0x1) != 0) Usage("Soundspeed data does not have an even number of elements");
                        soundspeedDepths = new List<double>();
                        soundspeedSpeeds = new List<double>();
                        for (var curElement = 0; curElement > elements.Count(); curElement += 2)
                        {
                            soundspeedDepths.Add(double.Parse(elements[curElement]));
                            soundspeedSpeeds.Add(double.Parse(elements[curElement + 1]));
                        }
                        for (var depthIndex = 0; depthIndex < soundspeedDepths.Count; depthIndex++)
                            if (soundspeedDepths[depthIndex] >= soundspeedDepths[depthIndex + 1])
                            {
                                Usage("Soundspeed depths must increase with each depth/speed pair");
                                return -1;
                            }
                        break;
                    case "-ranges":
                        elements = args[++argIndex].Split(',');
                        if (!elements.Any()) Usage("Receiver range data was not specified");
                        receiverRanges = elements.Select(double.Parse).ToList();
                        for (var rangeIndex = 0; rangeIndex < receiverRanges.Count; rangeIndex++)
                            if (receiverRanges[rangeIndex] >= receiverRanges[rangeIndex + 1])
                            {
                                Usage("Receiver ranges must increase with each entry");
                                return -1;
                            }
                        break;
                    case "-depths":
                        elements = args[++argIndex].Split(',');
                        if (!elements.Any()) Usage("Receiver range data was not specified");
                        receiverDepths = elements.Select(double.Parse).ToList();
                        for (var depthIndex = 0; depthIndex < receiverDepths.Count; depthIndex++)
                            if (receiverDepths[depthIndex] >= receiverDepths[depthIndex + 1])
                            {
                                Usage("Receiver depths must increase with each entry");
                                return -1;
                            }
                        break;
                    case "-sediment":
                        sedimentType = int.Parse(args[++argIndex]);
                        break;
                    case "-?":
                    case "-help":
                    default:
                        Usage();
                        return -1;
                }
            }
            if (name == null) Usage("-name was not specified");
            if (outputDirectory == null) Usage("-output was not specified");
            if (double.IsNaN(frequency)) Usage("-frequency was not specified");
            if (frequency <= 0) Usage("Specified -frequency value is not valid");
            if (bathymetryRanges == null) Usage("-bathymetry was not specified");
            if (soundspeedDepths == null) Usage("-soundspeed was not specified");
            if (receiverRanges == null) Usage("-ranges was not specified");
            if (receiverDepths == null) Usage("-depths was not specified");
            if (sedimentType == -1) Usage("-sediment was not specified");
            if (sedimentType < 1 || sedimentType > 23) Usage("-sediment value must be between 1 and 23");
            try
            {
                CreateBellhopEnvironment(outputDirectory, name, frequency, bathymetryRanges, bathymetryDepths, soundspeedDepths, soundspeedSpeeds, receiverRanges, receiverDepths, sedimentType);
            }
            catch (Exception ex)
            {
                Usage(ex.Message);
            }
            return 0;
        }

        public static void CreateBellhopEnvironment(string outputDirectory, string name, double frequency, List<double> bathymetryRanges, 
                                                    List<double> bathymetryDepths, List<double> soundspeedDepths, List<double> soundspeedSpeeds,
                                                    List<double> receiverRanges, List<double> receiverDepths, int sedimentType)
        {
            if (!Directory.Exists(outputDirectory)) Directory.CreateDirectory(outputDirectory);
            var bathymetryFilename = Path.Combine(outputDirectory, name + ".bty");
            var sediment = SedimentTypes.Find(sedimentType);
        }

        public static void Usage(string additionalErrorInfo = null)
        {
            Console.WriteLine("Usage: CreateBellhopEnvironmentFiles -output <outputPath>");
            Console.WriteLine("                                     -name <baseName>");
            Console.WriteLine("                                     -frequency <frequencyHz>");
            Console.WriteLine("                                     -bathymetry <rangeDepthPairs>");
            Console.WriteLine("                                     -soundspeed <depthSpeedPairs>");
            Console.WriteLine("                                     -ranges <rangeList>");
            Console.WriteLine("                                     -depths <depthList>");
            Console.WriteLine("                                     -sediment <sedimentType>");
            Console.WriteLine();
            Console.WriteLine("Description: Create a set of configuration and data files suitable for running");
            Console.WriteLine("             the Bellhop acoustic simulator along a single transect.");
            Console.WriteLine();
            Console.WriteLine("Where: <outputPath> is the full path of the directory into which the output");
            Console.WriteLine("                    files will be placed.");
            Console.WriteLine();
            Console.WriteLine("       <baseName> is the base filename that the output files will have");
            Console.WriteLine("                  Output files will be named <baseName>.env, <baseName>.bty,");
            Console.WriteLine("                  etc.");
            Console.WriteLine();
            Console.WriteLine("       <frequencyHz> is the frequency of the source to be simulated by Bellhop.");
            Console.WriteLine();
            Console.WriteLine("       <rangeDepthPairs> is the bathymetry profile along the transect, given as");
            Console.WriteLine("                         a comma-separated list of range/depth value pairs.");
            Console.WriteLine("                         Ranges arespecified in kilometers from the source,");
            Console.WriteLine("                         depths are specified in meters.");
            Console.WriteLine();
            Console.WriteLine("           For example: -bathymetry 0,1000,1,1100,2,1150,5.5,800,10,2000");
            Console.WriteLine("                         would mean that under the source (0 km) the depth is");
            Console.WriteLine("                         1000m, at 1km the depth is 1100m, at 5.5km the depth");
            Console.WriteLine("                         is 800m, and at at 10km the depth is 2000m.");
            Console.WriteLine();
            Console.WriteLine("       <depthSpeedPairs> is the sound speed profile to be used for this");
            Console.WriteLine("                         transect, given as a a comma-separated list of");
            Console.WriteLine("                         depth/speed pairs.  Depths are specified in meters");
            Console.WriteLine("                         from the surface, sound speeds are specified in meters");
            Console.WriteLine("                         per second.");
            Console.WriteLine();
            Console.WriteLine("           For example: -soundspeed 1,1500,10,1502.3,20,1500,50,1497,100,1503");
            Console.WriteLine("                         gives sound speeds for depths of 1m, 10m, 20m, 50m");
            Console.WriteLine("                         and 100m.");
            Console.WriteLine();
            Console.WriteLine("       <rangeList> List of ranges at which receiver data are to be calculated.");
            Console.WriteLine("                   Ranges are specified in kilometers from the sound source");
            Console.WriteLine();
            Console.WriteLine("       <depthList> List of depths at which receiver data are to be calculated.");
            Console.WriteLine("                   Depths are specified in meters from the surface.");
            Console.WriteLine();
            Console.WriteLine("       <sedimentType is one of the following values:");
            Console.WriteLine("                     1......Rough Rock");
            Console.WriteLine("                     2......Rock");
            Console.WriteLine("                     3......Cobble or Gravel or Pebble");
            Console.WriteLine("                     4......Sandy Gravel");
            Console.WriteLine("                     5......Very Coarse Sand");
            Console.WriteLine("                     6......Muddy Sandy Gravel");
            Console.WriteLine("                     7......Coarse Sand or Gravelly Sand");
            Console.WriteLine("                     8......Gravelly Muddy Sand");
            Console.WriteLine("                     9......Medium Sand or Sand");
            Console.WriteLine("                    10......Muddy Gravel");
            Console.WriteLine("                    11......Fine Sand or Silty Sand");
            Console.WriteLine("                    12......Muddy Sand");
            Console.WriteLine("                    13......Very Fine Sand");
            Console.WriteLine("                    14......Clayey Sand");
            Console.WriteLine("                    15......Coarse Silt");
            Console.WriteLine("                    16......Gravelly Mud or Sandy Silt");
            Console.WriteLine("                    17......Medium Silt or Sand-Silt-Clay");
            Console.WriteLine("                    18......Sandy Mud or Silt");
            Console.WriteLine("                    19......Fine Silt or Clayey Silt");
            Console.WriteLine("                    20......Sandy Clay");
            Console.WriteLine("                    21......Very Fine Silt");
            Console.WriteLine("                    22......Silty Clay");
            Console.WriteLine("                    23......Clay");
            Console.WriteLine();
            if (additionalErrorInfo != null) Console.WriteLine(additionalErrorInfo);
        }
    }

    public static class GPXLoader
    {
        /// <summary> 
        /// Load the Xml document for parsing 
        /// </summary> 
        /// <param name="filename">Fully qualified file name (local)</param> 
        /// <returns>XDocument</returns> 
        private static XDocument GetGpxDoc(string filename)
        {
            var gpxDoc = XDocument.Load(filename);
            return gpxDoc;
        }

        /// <summary> 
        /// Load the namespace for a standard GPX document 
        /// </summary> 
        /// <returns></returns> 
        private static XNamespace GetGpxNameSpace()
        {
            var gpx = XNamespace.Get("http://www.topografix.com/GPX/1/1");
            return gpx;
        }

        /// <summary> 
        /// When passed a file, open it and parse all waypoints from it. 
        /// </summary> 
        /// <param name="filename">Fully qualified file name (local)</param> 
        /// <returns>string containing line delimited waypoints from 
        /// the file (for test)</returns> 
        /// <remarks>Normally, this would be used to populate the 
        /// appropriate object model</remarks> 
        public static string LoadGPXWaypoints(string filename)
        {
            var gpxDoc = GetGpxDoc(filename);
            var gpx = GetGpxNameSpace();

            var waypoints = from waypoint in gpxDoc.Descendants(gpx + "wpt")
                            let latAttribute = waypoint.Attribute("lat")
                            where latAttribute != null
                            let lonAttribute = waypoint.Attribute("lon")
                            where lonAttribute != null
                            let elevationAttribute = waypoint.Element(gpx + "ele")
                            where elevationAttribute != null
                            let nameAttribute = waypoint.Element(gpx + "name")
                            where nameAttribute != null
                            let cmtAttribute = waypoint.Element(gpx + "cmt")
                            where cmtAttribute != null
                            select new
                            {
                                Latitude = latAttribute.Value,
                                Longitude = lonAttribute.Value,
                                Elevation = elevationAttribute != null ? elevationAttribute.Value : null,
                                Name = nameAttribute != null ? nameAttribute.Value : null,
                                Dt = cmtAttribute != null ? cmtAttribute.Value : null
                            };

            var sb = new StringBuilder();
            foreach (var wpt in waypoints)
            {
                // This is where we'd instantiate data 
                // containers for the information retrieved. 
                sb.Append(string.Format("Name:{0} Latitude:{1} Longitude:{2} Elevation:{3} Date:{4}\n",
                          wpt.Name, wpt.Latitude, wpt.Longitude, wpt.Elevation, wpt.Dt));
            }

            return sb.ToString();
        }

        /// <summary> 
        /// When passed a file, open it and parse all tracks 
        /// and track segments from it. 
        /// </summary> 
        /// <param name="filename">Fully qualified file name (local)</param> 
        /// <returns>string containing line delimited waypoints from the 
        /// file (for test)</returns> 
        public static string LoadGPXTracks(string filename)
        {
            var gpxDoc = GetGpxDoc(filename);
            var gpx = GetGpxNameSpace();
            var gpxx = XNamespace.Get("http://www.garmin.com/xmlschemas/GpxExtensions/v3");

            var tracks = from track in gpxDoc.Descendants(gpx + "trk")
                         select new
                         {
                             Name = track.Element(gpx + "name") != null ? track.Element(gpx + "name").Value : null,
                             Segs = (
                                  from trackpoint in track.Descendants(gpx + "trkpt")
                                  select new
                                  {
                                      Latitude = trackpoint.Attribute("lat").Value,
                                      Longitude = trackpoint.Attribute("lon").Value,
                                      Elevation = trackpoint.Element(gpx + "ele") != null ? trackpoint.Element(gpx + "ele").Value : null,
                                      Time = trackpoint.Element(gpx + "time") != null ? trackpoint.Element(gpx + "time").Value : null,
                                      Temperature = trackpoint.Element(gpx + "extensions") != null ? trackpoint.Element(gpx + "extensions").Descendants().Elements(gpxx + "Temperature").First().Value : null,
                                      Depth = trackpoint.Element(gpx + "extensions") != null ? trackpoint.Element(gpx + "extensions").Descendants().Elements(gpxx + "Depth").First().Value : null,
                                  }
                                )
                         };

            var sb = new StringBuilder();
            foreach (var trk in tracks)
            {
                // Populate track data objects. 
                foreach (var trkSeg in trk.Segs)
                {
                    // Populate detailed track segments 
                    // in the object model here. 
                    sb.Append(string.Format("Track:{0} - Latitude:{1} Longitude:{2} " + "Elevation:{3} Date:{4} Temperature:{5} Depth:{6}\n",
                              trk.Name, trkSeg.Latitude, trkSeg.Longitude, trkSeg.Elevation, trkSeg.Time, trkSeg.Temperature, trkSeg.Depth));
                }
            }
            return sb.ToString();
        } 
    } 
}
