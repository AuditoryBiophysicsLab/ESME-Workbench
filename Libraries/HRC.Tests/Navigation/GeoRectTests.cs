using System;
using System.ComponentModel;
using HRC.Navigation;
using NUnit.Framework;
using PostSharp;

namespace HRC.Tests.Navigation
{
    public class GeoRectTests
    {
        [Test]
        public void Test()
        {
            var geoRect = new GeoRect(44, 41, -69, -72);
            Post.Cast<GeoRect, INotifyPropertyChanged>(geoRect).PropertyChanged += (s, e) => Console.WriteLine("PropertyChanged: {0}", e.PropertyName);
            var insideGeo = new Geo(42, -70);
            var outsideGeo = new Geo(0, 0);
            Assert.IsTrue(geoRect.Contains(insideGeo));
            Assert.IsFalse(geoRect.Contains(outsideGeo));
            Console.WriteLine("Setting North to 45");
            geoRect.North = 45;
        }
    }
}
