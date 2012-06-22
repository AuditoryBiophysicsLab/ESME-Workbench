using System;
using System.IO;
using ESME.Animats;
using ESME.Behaviors;
using ESME.Database;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Locations;
using ESME.Plugins;
using ESME.Scenarios;
using HRC.Navigation;
using NUnit.Framework;

namespace ESME.Tests.Animats
{
    class AnimatSeedingTests
    {
        readonly string _databaseDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), @"ESME Workbench\Database");
        const string PluginDirectory = @"C:\Projects\ESME Deliverables\Libraries\ESME.Tests\bin\Debug";

        [Test]
        public void StaticDensitySeed()
        {
            var geoRect = new GeoRect(1, -1, 1, -1);
            var bathymetry = new Bathymetry();
            bathymetry.Samples.Add(new Geo<float>(geoRect.Center.Latitude,geoRect.Center.Longitude,100));
            var list = AnimatFile.Seed("Orca orca", .2, geoRect,bathymetry);
            foreach (var animatLocation in list)
            {
                Assert.IsTrue(geoRect.Contains(animatLocation.Geo));
                Assert.IsTrue(animatLocation.Depth < 100 && animatLocation.Depth > 0);
            }
        }

        
    }
}
