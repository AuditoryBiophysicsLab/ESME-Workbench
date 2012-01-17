using System;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Xml.Linq;

namespace CreateBellhopEnvironmentFiles
{
    class Program
    {
        static void Main(string[] args)
        {
            var result = GPXLoader.LoadGPXTracks(args[0]);
            Debug.WriteLine(result);


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
