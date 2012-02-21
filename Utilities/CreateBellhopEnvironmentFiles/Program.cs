using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Xml.Linq;
using ESME.Environment;
using ESME.Model;
using ESME.NEMO;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using HRC.Navigation;

namespace CreateBellhopEnvironmentFiles
{
    class Program
    {
        static int Main(string[] args)
        {
            //var result = GPXLoader.LoadGPXTracks(args[0]);
            //Debug.WriteLine(result);
            string name = null;
            string sspFile = null;
            string outputDirectory = null;
            var sedimentType = -1;
            var frequency = double.NaN;
            var sourceDepth = double.NaN;
            var verticalBeamWidth = double.NaN;
            var depressionElevationAngle = double.NaN;
            var beamCount = 100;
            var separators = new[] { ",", " " };
            List<double> bathymetryRanges = null;
            List<double> bathymetryDepths = null;
            List<double> receiverRanges = null;
            List<double> receiverDepths = null;
            try
            {
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
                        case "-sourcedepth":
                            sourceDepth = double.Parse(args[++argIndex]);
                            break;
                        case "-beamwidth":
                            verticalBeamWidth = double.Parse(args[++argIndex]);
                            break;
                        case "-beamangle":
                            depressionElevationAngle = double.Parse(args[++argIndex]);
                            break;
                        case "-rays":
                            beamCount = int.Parse(args[++argIndex]);
                            break;
                        case "-frequency":
                        case "-freq":
                            frequency = double.Parse(args[++argIndex]);
                            break;
                        case "-bathymetry":
                        case "-bathy":
                            elements = args[++argIndex].Split(separators, StringSplitOptions.RemoveEmptyEntries);
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
                            for (var curElement = 0; curElement < elements.Count() - 1; curElement += 2)
                            {
                                bathymetryRanges.Add(double.Parse(elements[curElement]));
                                bathymetryDepths.Add(double.Parse(elements[curElement + 1]));
                            }
                            for (var rangeIndex = 0; rangeIndex < bathymetryRanges.Count - 1; rangeIndex++)
                                if (bathymetryRanges[rangeIndex] >= bathymetryRanges[rangeIndex + 1])
                                {
                                    Usage("Bathymetry ranges must increase with each range/depth pair");
                                    return -1;
                                }
                            break;
                        case "-soundspeed":
                        case "-ssp":
                            sspFile = args[++argIndex];
                            break;
                        case "-ranges":
                            elements = args[++argIndex].Split(separators, StringSplitOptions.RemoveEmptyEntries);
                            if (!elements.Any()) Usage("Receiver range data was not specified");
                            receiverRanges = elements.Select(double.Parse).ToList();
                            for (var rangeIndex = 0; rangeIndex < receiverRanges.Count - 1; rangeIndex++)
                                if (receiverRanges[rangeIndex] >= receiverRanges[rangeIndex + 1])
                                {
                                    Usage("Receiver ranges must increase with each entry");
                                    return -1;
                                }
                            break;
                        case "-depths":
                            elements = args[++argIndex].Split(separators, StringSplitOptions.RemoveEmptyEntries);
                            if (!elements.Any()) Usage("Receiver depth data was not specified");
                            receiverDepths = elements.Select(double.Parse).ToList();
                            for (var depthIndex = 0; depthIndex < receiverDepths.Count - 1; depthIndex++)
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
                if (name == null)
                {
                    Usage("-name was not specified");
                    return -1;
                }
                if (outputDirectory == null)
                {
                    Usage("-output was not specified");
                    return -1;
                }
                if (double.IsNaN(frequency))
                {
                    Usage("-frequency was not specified");
                    return -1;
                }
                if (double.IsNaN(sourceDepth))
                {
                    Usage("-sourcedepth was not specified");
                    return -1;
                }
                if (double.IsNaN(verticalBeamWidth))
                {
                    Usage("-beamwidth was not specified");
                    return -1;
                }
                if (double.IsNaN(depressionElevationAngle))
                {
                    Usage("-beamangle was not specified");
                    return -1;
                }
                if (frequency <= 0)
                {
                    Usage("Specified -frequency value is not valid");
                    return -1;
                }
                if (bathymetryRanges == null)
                {
                    Usage("-bathymetry was not specified");
                    return -1;
                }
                if (sspFile == null)
                {
                    Usage("-soundspeed was not specified");
                    return -1;
                }
                if (receiverRanges == null)
                {
                    Usage("-ranges was not specified");
                    return -1;
                }
                if (receiverDepths == null)
                {
                    Usage("-depths was not specified");
                    return -1;
                }
                if (beamCount <= 0)
                {
                    Usage("-rays must be a positive integer");
                    return -1;
                }
                if (sedimentType == -1)
                {
                    Usage("-sediment was not specified");
                    return -1;
                }
                if (sedimentType < 1 || sedimentType > 23)
                {
                    Usage("-sediment value must be between 1 and 23");
                    return -1;
                }

                if (!Path.IsPathRooted(sspFile)) sspFile = Path.Combine(outputDirectory, sspFile);
                if (!File.Exists(sspFile))
                {
                    Usage("-soundspeed file does not exist");
                    return -1;
                }


                // Read in the sound speed profile from the provided file
                var soundspeedDepths = new List<double>();
                var soundspeedSpeeds = new List<double>();
                var sspLines = File.ReadAllLines(sspFile);
                var spaceSeparators = new[] {" "};
                foreach (var line in sspLines)
                {
                    var fields = line.Split(spaceSeparators, StringSplitOptions.RemoveEmptyEntries);
                    if (fields.Length != 2)
                    {
                        Usage("-soundspeed file is not in the expected format");
                        return -1;
                    }
                    soundspeedDepths.Add(double.Parse(fields[0]));
                    soundspeedSpeeds.Add(double.Parse(fields[1]));
                }

                // Make the max depth 10% greater than the deeper of the deepest point of the bathymetry or the deepest receiver depth
                var maxDepth = Math.Max(bathymetryDepths.Last(), receiverDepths.Max()) * 1.1;

                CreateBellhopEnvironment(outputDirectory,
                                         name,
                                         sourceDepth,
                                         frequency,
                                         verticalBeamWidth,
                                         depressionElevationAngle,
                                         bathymetryRanges,
                                         bathymetryDepths,
                                         soundspeedDepths,
                                         soundspeedSpeeds,
                                         receiverRanges,
                                         receiverDepths,
                                         sedimentType,
                                         beamCount,
                                         maxDepth);
            }
            catch (Exception ex)
            {
                Usage(ex.Message);
            }
            return 0;
        }

        public static void CreateBellhopEnvironment(string outputDirectory, string name, 
                                                    double sourceDepth, double frequency, double verticalBeamWidth, double depressionElevationAngle, 
                                                    List<double> bathymetryRanges, List<double> bathymetryDepths, List<double> soundspeedDepths, 
                                                    List<double> soundspeedSpeeds, List<double> receiverRanges, List<double> receiverDepths, 
                                                    int sedimentType, int beamCount, double maxDepth)
        {
            if (!Directory.Exists(outputDirectory)) Directory.CreateDirectory(outputDirectory);

            // Write the bathymetry file
            var bathymetryFilename = Path.Combine(outputDirectory, name + ".bty");
            using (var writer = new StreamWriter(bathymetryFilename))
            {
                writer.WriteLine("'C'");
                writer.WriteLine(bathymetryRanges.Count);
                for (var index = 0; index < bathymetryRanges.Count; index++)
                    writer.WriteLine("{0} {1}", bathymetryRanges[index], bathymetryDepths[index]);
            }

            var acousticProperties = new AcousticProperties
            {
                HighFrequency = (float)frequency,
                LowFrequency = (float)frequency,
                DepressionElevationAngle = (float)depressionElevationAngle,
                SourceDepth = (float)sourceDepth,
                VerticalBeamWidth = (float)verticalBeamWidth,
            };
            var maxRadius = (int)Math.Ceiling(receiverRanges.Last() * 1.01); // Allow an extra 1% of range so the beams don't run off the end before they hit the last column of receivers

            var sspData = new DepthValuePairs<float>();
            for (var index = 0; index < soundspeedDepths.Count; index++)
                sspData.Add(new DepthValuePair<float>((float)soundspeedDepths[index], (float)soundspeedSpeeds[index]));
            var soundSpeedProfile = new SoundSpeedProfileGeneric
            {
                Data = sspData
            };
            var result = GetRadialConfiguration(acousticProperties, soundSpeedProfile, SedimentTypes.Find(sedimentType), 
                                                maxDepth, maxRadius, receiverRanges, receiverDepths, 
                                                false, true, true, beamCount);
            File.WriteAllText(Path.Combine(outputDirectory, name + ".env"), result, new ASCIIEncoding());
        }

        public static string GetRadialConfiguration(AcousticProperties acousticProperties, SoundSpeedProfileGeneric ssp, SedimentType sediment, 
                                                    double maxDepth, double maxRadius, List<double> ranges, List<double> depths, 
                                                    bool useSurfaceReflection, bool useVerticalBeamforming, bool generateArrivalsFile, int beamCount)
        {
            using (var sw = new StringWriter())
            {
                sw.WriteLine("'TL' ! Title");
                sw.WriteLine("{0} ! Frequency (Hz)", acousticProperties.Frequency);
                sw.WriteLine("1 ! NMedia"); // was NMEDIA in gui_genbellhopenv.m
                sw.WriteLine(useSurfaceReflection ? "'CFFT ' ! Top Option" : "'CVFT ' ! Top Option");

                sw.WriteLine("0  0.00 {0} ! N sigma depth", ssp.Data[ssp.Data.Count - 1].Depth);

                // If SSP is shallower than the bathymetry then extrapolate an SSP entry for the deepest part of the water
                //if (SSP.DepthVector[SSP.DepthVector.Length - 1] < RealBottomDepth_Meters)
                //    SoundSpeedProfile = ExtrapolateSSP(SoundSpeedProfile, RealBottomDepth_Meters);

                foreach (var depthValuePair in ssp.Data)
                    sw.WriteLine("{0:0.00} {1:0.00} 0.00 1.00 0.00 0.00 / ! z c cs rho", depthValuePair.Depth, depthValuePair.Value);

                sw.WriteLine("'A*' 0.00 ! Bottom Option, sigma"); // A = Acoustic halfspace, ~ = read bathymetry file, 0.0 = bottom roughness (currently ignored)
                sw.WriteLine("{0} {1} {2} {3} {4} {5} / ! lower halfspace", maxDepth, sediment.CompressionWaveSpeed, sediment.ShearWaveSpeed, sediment.Density, sediment.LossParameter, 0);
                // Source and Receiver Depths and Ranges
                sw.WriteLine("1"); // Number of Source Depths
                sw.WriteLine("{0} /", acousticProperties.SourceDepth); // source depth
                sw.WriteLine("{0}", depths.Count); // Number of Receiver Depths
                foreach (var depth in depths) sw.Write("{0} ", depth);
                sw.WriteLine("/ ! Receiver Depths (m)");
                sw.WriteLine("{0}", ranges.Count); // Number of Receiver Ranges
                foreach (var range in ranges) sw.Write("{0} ", range);
                sw.WriteLine("/ ! Receiver Ranges (km)");

                if (generateArrivalsFile) sw.WriteLine("'aG'");  // aB
                else sw.WriteLine(useVerticalBeamforming ? "'IG*'" : "'I'");
                // if useVerticalBeamforming is true, then SBPFIL must be present (Source Beam Pattern file)
                sw.WriteLine("{0}", beamCount); // Number of beams
                //sw.WriteLine("0"); // Number of beams
                var verticalHalfAngle = acousticProperties.VerticalBeamWidth / 2;
                var angle1 = acousticProperties.DepressionElevationAngle - verticalHalfAngle;
                var angle2 = acousticProperties.DepressionElevationAngle + verticalHalfAngle;
                sw.WriteLine("{0} {1} /", angle1, angle2); // Beam fan half-angles (negative angles are toward the surface
                //sw.WriteLine("-60.00 60.00 /"); // Beam fan half-angles (negative angles are toward the surface
                //sw.WriteLine("{0:F} {1:F} {2:F} ! step zbox(meters) rbox(km)", experiment.TransmissionLossSettings.DepthCellSize, RealBottomDepth_Meters + 100, (bottomProfile.Length / 1000.0) * 1.01);
                sw.WriteLine("{0} {1} {2}", (ranges[0] / 2) * 1000, maxDepth, maxRadius);
                return sw.ToString();
            }
        }

        public static void Usage(string additionalErrorInfo = null)
        {
            Console.WriteLine("Usage: CreateBellhopEnvironmentFiles -output <outputPath>");
            Console.WriteLine("                                     -name <baseName>");
            Console.WriteLine("                                     -sourcedepth <sourceDepth>");
            Console.WriteLine("                                     -frequency <frequencyHz>");
            Console.WriteLine("                                     -beamwidth <beamWidth>");
            Console.WriteLine("                                     -beamangle <beamAngle>");
            Console.WriteLine("                                     -bathymetry <rangeDepthPairs>");
            Console.WriteLine("                                     -soundspeed <sspFile>");
            Console.WriteLine("                                     -ranges <rangeList>");
            Console.WriteLine("                                     -depths <depthList>");
            Console.WriteLine("                                     -sediment <sedimentType>");
            Console.WriteLine("                                    [-rays <rayCount>]");
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
            Console.WriteLine("       <sourceDepth> is the depth of the acoustic source, in meters.");
            Console.WriteLine();
            Console.WriteLine("       <frequencyHz> is the frequency of the acoustic source.");
            Console.WriteLine();
            Console.WriteLine("       <beamWidth> is the vertical beam width, in degrees, of the acoustic");
            Console.WriteLine("                   source.");
            Console.WriteLine();
            Console.WriteLine("       <beamAngle> is the vertical beam \"look angle\", in degrees, of the");
            Console.WriteLine("                   acoustic source. Zero degrees means the acoustic source");
            Console.WriteLine("                   emits its sound parallel to the surface, while a negative");
            Console.WriteLine("                   angle means that the acoustic source emits its sound towards");
            Console.WriteLine("                   the surface.  A positive angle indicates that the acoustic");
            Console.WriteLine("                   source is oriented towards the bottom.");
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
            Console.WriteLine("       <sspFile> is the name of the file containing the sound speed profile to");
            Console.WriteLine("                 be used for this transect.  This filename can either be fully");
            Console.WriteLine("                 specified or relative to the directory given as <outputPath>.");
            Console.WriteLine("                 The file should consist of space-separated depth/speed pairs");
            Console.WriteLine("                 with one pair per line. Depths are specified in meters from");
            Console.WriteLine("                 the surface, sound speeds are specified in meters per second");
            Console.WriteLine();
            Console.WriteLine("       <rangeList> List of ranges at which receiver data are to be calculated.");
            Console.WriteLine("                   Ranges are specified in kilometers from the sound source");
            Console.WriteLine();
            Console.WriteLine("       <depthList> List of depths at which receiver data are to be calculated.");
            Console.WriteLine("                   Depths are specified in meters from the surface.");
            Console.WriteLine();
            Console.WriteLine("       <rayCount> is an optional parameter which specifies the number of rays.");
            Console.WriteLine("                  that will be launched by Bellhop during its numerical");
            Console.WriteLine("                  simulation of the acoustic environment.  More rays may");
            Console.WriteLine("                  yield a more accurate result but at the expense of longer");
            Console.WriteLine("                  computation time.  The default value for this parameter is");
            Console.WriteLine("                  100.");
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
