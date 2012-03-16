using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Windows;
using HRC.Navigation;
using NUnit.Framework;
using PostSharp;

namespace HRC.Tests.Navigation
{
    public class GeoRectTests
    {
        [Test]
        public void ConstructionTest()
        {
            var rect1 = new GeoRect(44, 41, -69, -72);
            var rect2 = new GeoRect(44, 41, -69, -72);
            Assert.IsTrue(rect1.Equals(rect2));

            var rect3 = new GeoRect(rect1);
            Assert.IsTrue(rect1.Equals(rect3));

            var rect4 = new GeoRect(new List<Geo>
                                        {
                                            new Geo(44, -69),
                                            new Geo(41, -72),
                                        });
            Assert.IsTrue(rect1.Equals(rect4));
        }

        [Test]
        public void PropertyChangedTest()
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

        [Test]
        public void AreEqual()
        {
            var rect1 = new GeoRect(44, 41, -69, -72);
            var rect2 = new GeoRect(44, 41, -69, -72);
            var rect3 = new GeoRect(41, 44, -60, -70);
            Assert.IsTrue(rect1.Equals(rect2));
            Assert.IsFalse(rect1.Equals(rect3));
        }

        [Test]
        public void UnionRect()
        {
            var baseRect = new GeoRect(43, 41, -69, -71);
            var containedWithin = new GeoRect(42.5, 41.5, -69.5, -70.5);
            var alsoContained = new GeoRect(42.5, 41.5, -69.5, -70.5);
            var array = new[] { baseRect, containedWithin, alsoContained };
            Assert.IsTrue(GeoRect.Union(baseRect, containedWithin).Equals(baseRect));
            Assert.IsTrue(GeoRect.Union(array).Equals(baseRect));

            var disjoint = new GeoRect(44, 42, -72, -74);
            Assert.IsTrue(GeoRect.Union(baseRect,disjoint).Equals(new GeoRect(44,41,-69,-74)));

            var overlaps = new GeoRect(44, 42, -68, -70);
            Assert.IsTrue(GeoRect.Union(overlaps,baseRect).Equals(new GeoRect(44,41,-68,-71)));
        }

        [Test]
        public void UnionGeo()
        {
            var baseRect = new GeoRect(43, 41, -69, -71);
            var containedWithin = new Geo(42, -70);
            var onWesternEdge = new Geo(42, -71);
            var onCorner = new Geo(43, -69);
            var outside = new Geo(44, -60);

            Assert.IsTrue(GeoRect.Union(baseRect,containedWithin).Equals(baseRect));
            Assert.IsTrue(GeoRect.Union(baseRect, onWesternEdge).Equals(baseRect));
            Assert.IsTrue(GeoRect.Union(baseRect, onCorner).Equals(baseRect));
            Assert.IsFalse(GeoRect.Union(baseRect,outside).Equals(baseRect));
            Assert.IsTrue(GeoRect.Union(baseRect,outside).Equals(new GeoRect(44,41,-60,-71)));
        }

        [Test]
        public void ContainsRect()
        {
            var baseRect = new GeoRect(43, 41, -69, -71);
            var containedWithin = new GeoRect(42.5, 41.5, -69.5, -70.5);
            Assert.IsTrue(baseRect.Contains(containedWithin));
        }

        [Test]
        public void ContainsGeo()
        {
            var baseRect = new GeoRect(43, 41, -69, -71);
            var containedWithin = new Geo(42, -70);
            Assert.IsTrue(baseRect.Contains(containedWithin));
        }
        
       [Test]
        public void Inflations()
        {
            
        }
    }
}
