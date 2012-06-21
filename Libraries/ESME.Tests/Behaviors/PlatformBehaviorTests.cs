using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using ESME.Behaviors;
using ESME.Scenarios;
using HRC.Navigation;
using HRC.Utility;
using KMLib;
using KMLib.Feature;
using NUnit.Framework;
using TimeSpan = System.TimeSpan;

namespace ESME.Tests.Behaviors
{
    public class PlatformBehaviorTests
    {
        [Test]
        public void PlatformBehaviorToKML()
        {
            var jaxOpsArea = new GeoArray(
                new Geo(29.3590, -79.2195),
                new Geo(31.1627, -79.2195),
                new Geo(31.1627, -81.2789),
                new Geo(30.1627, -81.2789),
                new Geo(29.3590, -80.8789),
                new Geo(29.3590, -79.2195));

            var platform = new Platform
            {
                PlatformName = "Test Platform",
                Perimeter = jaxOpsArea,
                Depth = 0,
                IsRandom = true,
                TrackType = TrackType.PerimeterBounce,
                Sources = new ObservableList<Source>(),
                Speed = 20,
            };
            var source = new Source
            {
                SourceName = "Test Source",
                Modes = new ObservableList<Mode>(),
                Platform = platform,
            };
            platform.Sources.Add(source);
            var mode = new Mode
            {
                ModeName = "Test Mode",
                PulseInterval = new TimeSpan(0, 0, 0, 10),
                PulseLength = new TimeSpan(0, 0, 0, 0, 500),
                Depth = 5,
                HighFrequency = 1000,
                LowFrequency = 1000,
                DepressionElevationAngle = 10,
                VerticalBeamWidth = 90,
                SourceLevel = 200,
                Source = source,
            };
            source.Modes.Add(mode);

            var behavior = new PlatformBehavior(platform, new TimeSpan(0, 0, 0, 1), 86400);
#if true
            var kml = new KMLRoot();
            var folder = new Folder("Jacksonville");
            jaxOpsArea.Placemark.name = "Jacksonville OpArea";
            jaxOpsArea.Placemark.Snippet = "The operational area";
            jaxOpsArea.Placemark.Snippet.maxLines = 1;
            folder.Add(jaxOpsArea.Placemark);

#if true
            var timeStep = 0;
            foreach (var state in behavior.PlatformStates)
            {
                if (timeStep % 100 == 0)
                {
                    state.PlatformLocation.Location.Placemark.name = string.Format("TimeStep {0}", timeStep);
                    folder.Add(state.PlatformLocation.Location.Placemark);
                }
                timeStep++;
            }
#else
            result.Placemark.name = "Platform track";
            result.Placemark.Snippet = "The track of the platform";
            result.Placemark.Snippet.maxLines = 1;
            result.Placemark.Geometry.AltitudeMode = AltitudeMode.clampedToGround;
            folder.Add(result.Placemark);
#endif

            kml.Document.Add(folder);

            var savePath = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "Platform Behavior Tests", "PlatformBehavior.kml");
            Debug.WriteLine("Saving KML...");
            kml.Save(savePath);
#endif
        }
    }
}
