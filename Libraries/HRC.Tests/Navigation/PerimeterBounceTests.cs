using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Windows.Media;
using HRC.Navigation;
using NUnit.Framework;
using KMLib;
using KMLib.Feature;
using TimeSpan = System.TimeSpan;

namespace HRC.Tests.Navigation
{
    public class PerimeterBounceTests
    {
        [Test]
        public void PerimeterBounce()
        {
            var jaxOpsArea = new GeoArray(
                new Geo(29.3590, -79.2195),
                new Geo(31.1627, -79.2195),
                new Geo(31.1627, -81.2789),
                new Geo(30.1627, -81.2789),
                new Geo(29.3590, -80.8789),
                new Geo(29.3590, -79.2195));

            var kml = new KMLRoot();
            var folder = new Folder("Jacksonville");
            jaxOpsArea.Placemark.name = "Jacksonville OpArea";
            jaxOpsArea.Placemark.Snippet = "The operational area";
            jaxOpsArea.Placemark.Snippet.maxLines = 1;
            folder.Add(jaxOpsArea.Placemark);

            var result = jaxOpsArea.PerimeterBounce(null, double.NaN, 10, new TimeSpan(0, 0, 0, 1), 1000);
            var startLocation = result[0];
            startLocation.Placemark.name = "Start location";
            startLocation.Placemark.Snippet = "The start of the track";
            startLocation.Placemark.Snippet.maxLines = 1;
            startLocation.Placemark.AddStyle(new IconStyle(System.Drawing.Color.Green) { Icon = new Icon() { href = @"http://www.clker.com/cliparts/q/y/S/n/A/V/green-pin-th.png" } });
            folder.Add(startLocation.Placemark);

            result.Placemark.name = "Platform track";
            result.Placemark.Snippet = "The track of the platform";
            result.Placemark.Snippet.maxLines = 1;
            result.Placemark.Geometry.AltitudeMode = AltitudeMode.clampedToGround;
            folder.Add(result.Placemark);

            var endPoint = result.Segments.Last()[1];
            if (!jaxOpsArea.Contains(endPoint))
            {
                var goodBounceSegment = result.Segments.ToArray()[result.Segments.Count() - 2];
                var goodBouncePoint = goodBounceSegment[0];
                var goodBounce = goodBouncePoint.Placemark;
                Debug.WriteLine("Test failed"); 
                goodBounce.name = string.Format("Last Good Bounce {0:000}", result.Segments.Count() - 1);
                Debug.WriteLine(goodBounce.name);
                goodBounce.Snippet = string.Format("Lat: {0:0.#####} Lon: {1:0.#####}", goodBouncePoint.Latitude, goodBouncePoint.Longitude);
                goodBounce.Snippet.maxLines = 1;
                folder.Add(goodBounce);

                goodBounceSegment.Placemark.name = "Last good bounce segment";
                goodBounceSegment.Placemark.AddStyle(new LineStyle(System.Drawing.Color.Red, 5));
                folder.Add(goodBounceSegment.Placemark);

                var badBouncePoint = goodBounceSegment[1];
                var badBounce = badBouncePoint.Placemark;
                badBounce.name = string.Format("Bad Bounce {0:000}", result.Segments.Count());
                badBounce.Snippet = string.Format("Lat: {0:0.#####} Lon: {1:0.#####}", badBouncePoint.Latitude, badBouncePoint.Longitude);
                badBounce.Snippet.maxLines = 1;
                folder.Add(badBounce);
            }
            else Debug.WriteLine("Test passed");
#if false
            var segments = result.Segments.ToArray();
            for (var segmentIndex = 0; segmentIndex < segments.Length; segmentIndex++)
            {
                var bounce = segments[segmentIndex][1].Placemark;
                bounce.name = string.Format("Bounce {0:000}", segmentIndex + 1);
                bounce.Snippet = string.Format("Lat: {0:0.#####} Lon: {1:0.#####}", segments[segmentIndex][1].Latitude, segments[segmentIndex][1].Longitude);
                bounce.Snippet.maxLines = 1;
                folder.Add(bounce);
            }
#endif

            var endLocation = result[result.Length - 1];
            endLocation.Placemark.name = "End location";
            endLocation.Placemark.Snippet = "The end of the track";
            endLocation.Placemark.Snippet.maxLines = 1;
            endLocation.Placemark.AddStyle(new IconStyle(System.Drawing.Color.Red) { Icon = new Icon { href = @"http://www.clker.com/cliparts/Z/x/U/0/B/3/red-pin-th.png" } });
            folder.Add(endLocation.Placemark);

            kml.Document.Add(folder);

            var savePath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "Perimeter Bounce Tests", "PerimeterBounce.kml");
            Debug.WriteLine("Saving KML...");
            kml.Save(savePath);
        }
    }
}
