using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using HRC.Navigation;
using NUnit.Framework;

namespace HRC.Tests.Navigation
{
    class GeoTests
    {
        [Test]
        public void ShouldBeBoston()
        {
            var geo = new Geo(42.37, 71.03);
            Assert.That(geo.Latitude, Is.EqualTo(42.37).Within(0.000001));
            Assert.That(geo.Longitude, Is.EqualTo(71.03).Within(0.000001));
        }

        [Test]
        public void ShouldStayAsInitialized(
            [Random(-90.0, 90.0, 50)] double lat, 
            [Random(-180.0, 180.0, 50)] double lon)
        {
            var geo = new Geo(lat, lon);
            Assert.That(geo.Latitude, Is.EqualTo(lat).Within(0.000001));
            Assert.That(geo.Longitude, Is.EqualTo(lon).Within(0.000001));
        }

        [Test]
        public void ShouldWrapOutOfRangeValues(
            [Random(-1000, 1000.0, 50)] double lat,
            [Random(-1000.0, 1000.0, 50)] double lon)
        {
            var wrappedLat = lat;
            while (wrappedLat < -90) wrappedLat += 180.0;
            while (wrappedLat > 90.0) wrappedLat -= 180.0;
            var wrappedLon = lon;
            while (wrappedLon < -180) wrappedLon += 360.0;
            while (wrappedLon > 180.0) wrappedLon -= 360.0;
            var geo = new Geo(lat, lon);
            Assert.That(geo.Latitude, Is.EqualTo(wrappedLat).Within(0.000001));
            Assert.That(geo.Longitude, Is.EqualTo(wrappedLon).Within(0.000001));
        }

        [Test]
        public void ShouldHaveConsistentDistance(
            [Random(-1000, 1000.0, 5)] double lat,
            [Random(-1000.0, 1000.0, 5)] double lon,
            [Random(-360.0, 360.0, 10)] double bearing,
            [Random(0.0, 1000.0, 10)] double distance)
        {
            var sourceGeo = new Geo(lat, lon);
            var destGeo = sourceGeo.Offset(Geo.KilometersToRadians(distance), Geo.DegreesToRadians(bearing));
            Assert.That(sourceGeo.DistanceKilometers(destGeo), Is.EqualTo(distance).Within(0.0003));
        }
    }

}
