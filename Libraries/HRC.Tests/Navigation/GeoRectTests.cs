using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using HRC.Navigation;
using NUnit.Framework;

namespace HRC.Tests.Navigation
{
    public class GeoRectTests
    {
        [Test]
        public void Test()
        {
            var geoRect = new GeoRect(44, 41, -69, -72);
            var insideGeo = new Geo(42, -70);
            var outsideGeo = new Geo(0, 0);
            Assert.IsTrue(geoRect.Contains(insideGeo));
            Assert.IsFalse(geoRect.Contains(outsideGeo));
        }
    }
}
