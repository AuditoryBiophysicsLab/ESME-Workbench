using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;

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
            var frequency = double.NaN;
            List<double> bathymetryRanges = null;
            List<double> bathymetryDepths = null;
            List<double> soundspeedDepths = null;
            List<double> soundspeedSpeeds = null;
            List<double> receiverRanges = null;
            List<double> receiverDepths = null;
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
                        if (!elements.Any()) Usage("Bathymetry data was not specified");
                        if ((elements.Count() & 0x1) != 0) Usage("Bathymetry data does not have an even number of elements");
                        bathymetryRanges = new List<double>();
                        bathymetryDepths = new List<double>();
                        for (var curElement = 0; curElement > elements.Count(); curElement += 2)
                        {
                            bathymetryRanges.Add(double.Parse(elements[curElement]));
                            bathymetryDepths.Add(double.Parse(elements[curElement + 1]));
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
                        break;
                    case "-ranges":
                        elements = args[++argIndex].Split(',');
                        if (!elements.Any()) Usage("Receiver range data was not specified");
                        receiverRanges = elements.Select(double.Parse).ToList();
                        break;
                    case "-depths":
                        elements = args[++argIndex].Split(',');
                        if (!elements.Any()) Usage("Receiver range data was not specified");
                        receiverDepths = elements.Select(double.Parse).ToList();
                        break;
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
            CreateBellhopEnvironment(name, frequency, bathymetryRanges, bathymetryDepths, soundspeedDepths, soundspeedSpeeds, receiverRanges, receiverDepths, outputDirectory);
            return 0;
        }

        public static void CreateBellhopEnvironment(string name, double frequency, List<double> bathymetryRanges, List<double> bathymetryDepths,
                                                    List<double> soundspeedDepths, List<double> soundspeedSpeeds,
                                                    List<double> receiverRanges, List<double> receiverDepths, string outputDirectory)
        {
            
        }

        public static void Usage(string additionalErrorInfo = null)
        {
            Console.WriteLine("Usage: CreateBellhopEnvironmentFiles -output <outputPath> -name <baseName> -frequency <frequencyHz>");
            Console.WriteLine("                                     -bathymetry <rangeDepthPairs> -soundspeed <depthSpeedPairs>");
            Console.WriteLine("                                     -ranges <rangeList> -depths <depthList>");
            Console.WriteLine();
            Console.WriteLine("Description: Create a set of configuration and data files suitable for running the Bellhop acoustic");
            Console.WriteLine("             simulator along single transect.");
            Console.WriteLine();
            Console.WriteLine("Where: <outputPath> is the full path of the directory into which the output files will be placed.");
            Console.WriteLine();
            Console.WriteLine("       <baseName> base filename that the output files will have.  Output files will be named <baseName>.env,");
            Console.WriteLine("                  <baseName>.bty, etc.");
            Console.WriteLine();
            Console.WriteLine("       <frequencyHz> The frequency of the source to be simulated by Bellhop");
            Console.WriteLine();
            Console.WriteLine("       <rangeDepthPairs> Bathymetry along the transect, given as a list of comma-separated range/depth value pairs.");
            Console.WriteLine("                         Ranges are specified in kilometers from the source, depths are specified in meters");
            Console.WriteLine();
            Console.WriteLine("       <depthSpeedPairs> Sound speed profile to be used for this transect, given as a list of comma-separated");
            Console.WriteLine("                         depth/speed pairs.  Depths are specified in meters from the surface, sound speeds are");
            Console.WriteLine("                         specified in meters per second.");
            Console.WriteLine();
            Console.WriteLine("       <rangeList> List of ranges at which receiver data are to be calculated. Ranges are specified in kilometers from");
            Console.WriteLine("                   the sound source");
            Console.WriteLine();
            Console.WriteLine("       <depthList> List of depths at which receiver data are to be calculated. Depths are specified in meters from");
            Console.WriteLine("                   the surface.");
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
