using System;
using System.IO;
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
        [Test]
        public void StaticDensitySeed()
        {
            var geoRect = new GeoRect(1, -1, 1, -1);
            var bathymetry = new Bathymetry();
            bathymetry.Samples.Add(new Geo<float>(geoRect.Center.Latitude,geoRect.Center.Longitude,100));
            var list = Animat.Seed(new ScenarioSpecies{LatinName = "Orca orca", PopulationDensity = .2f}, geoRect,bathymetry);
            foreach (var animatLocation in list.Locations)
            {
                Assert.IsTrue(geoRect.Contains(animatLocation));
                Assert.IsTrue(animatLocation.Data < 100 && animatLocation.Data > 0);
            }
        }

        
    }
}
