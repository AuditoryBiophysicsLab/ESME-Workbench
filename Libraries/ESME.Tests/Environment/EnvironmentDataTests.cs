using System;
using System.Diagnostics;
using ESME.Environment;
using HRC.Navigation;
using NUnit.Framework;

namespace ESME.Tests.Environment
{
    public class EnvironmentDataTests
    {
        EnvironmentData<EarthCoordinate> _list;

        [SetUp]
        public void Initialize()
        {
            _list = new EnvironmentData<EarthCoordinate>();
        }

        [Test]
        public void EmptyList()
        {
            Assert.AreEqual(0, _list.Count);
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => Debug.WriteLine(_list[0]));
            Assert.Throws(typeof(IndexOutOfRangeException), () => Debug.WriteLine(_list[0, 0]));
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => Debug.WriteLine(_list.GetNearestPoint(new EarthCoordinate(0, 0))));
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => Debug.WriteLine(_list.GetExactPoint(0, 0)));
            EarthCoordinate result;
            Assert.IsFalse(_list.TryGetExactPoint(0, 0, out result));
            Assert.IsFalse(_list.TryGetNearestPoint(0, 0, out result));
        }


        [Test]
        public void OneEntry()
        {
            var zeroZero = new EarthCoordinate(0, 0);
            var oneOne = new EarthCoordinate(1, 1);
            EarthCoordinate result;

            _list.Add(zeroZero);
            Assert.AreEqual(1, _list.Count);
            Assert.AreSame(zeroZero, _list[0]);
            Assert.AreSame(zeroZero, _list[0, 0]);
            
            Assert.IsTrue(_list.TryGetNearestPoint(oneOne, out result));
            Assert.AreSame(result, zeroZero);
            
            Assert.IsFalse(_list.TryGetNearestPoint(oneOne, out result, 100));
            Assert.IsTrue(_list.TryGetNearestPoint(oneOne, out result, 200));
            Assert.AreSame(result, zeroZero);
            
            Assert.IsTrue(_list.TryGetExactPoint(zeroZero, out result));
            Assert.AreSame(result, zeroZero);

            result = _list.GetExactPoint(0, 0);
            Assert.AreSame(result, zeroZero);

            result = _list.GetNearestPoint(0, 1);
            Assert.AreSame(result, zeroZero);

            result = _list.GetNearestPoint(0, 0, 0);
            Assert.AreSame(result, zeroZero);
        }

        [Test]
        public void TwoByTwoSquare()
        {
            var zeroZero = new EarthCoordinate(0, 0);
            var zeroOne = new EarthCoordinate(0, 1);
            var oneZero = new EarthCoordinate(1, 0);
            var oneOne = new EarthCoordinate(1, 1);

            _list.Add(oneOne);
            _list.Add(oneZero);
            _list.Add(zeroOne);
            _list.Add(zeroZero);
            Assert.AreEqual(4, _list.Count);
            
            Assert.AreSame(oneOne, _list[0]);
            Assert.AreSame(oneZero, _list[1]);
            Assert.AreSame(zeroOne, _list[2]);
            Assert.AreSame(zeroZero, _list[3]);

            Assert.IsFalse(_list.IsSorted);
            _list.Sort();
            Assert.IsTrue(_list.IsSorted);

            Assert.AreSame(zeroZero, _list[0]);
            Assert.AreSame(zeroOne, _list[1]);
            Assert.AreSame(oneZero, _list[2]);
            Assert.AreSame(oneOne, _list[3]);

            Assert.AreSame(zeroZero, _list[0, 0]);
            Assert.AreSame(zeroOne, _list[1, 0]);
            Assert.AreSame(oneOne, _list[1, 1]);
            Assert.AreSame(oneZero, _list[0, 1]);

            TestNearestAndExact(zeroZero, 0, -1);
            TestNearestAndExact(zeroZero, -1, -1);
            TestNearestAndExact(zeroZero, -1, 0);
            TestNearestAndExact(zeroZero, 0.4, 0.4);

            TestNearestAndExact(oneZero, 1, -1);
            TestNearestAndExact(oneZero, 2, -1);
            TestNearestAndExact(oneZero, 2, 0);
            TestNearestAndExact(oneZero, 0.6, 0.4);

            TestNearestAndExact(oneOne, 2, 1);
            TestNearestAndExact(oneOne, 2, 2);
            TestNearestAndExact(oneOne, 1, 2);
            TestNearestAndExact(oneOne, 0.6, 0.6);

            TestNearestAndExact(zeroOne, 0, 2);
            TestNearestAndExact(zeroOne, -1, 2);
            TestNearestAndExact(zeroOne, -1, 1);
            TestNearestAndExact(zeroOne, 0.4, 0.6);

            TestNearestAndExact(oneOne, 0.5, 0.5);
        }

        [TestCase(-10, 1, 21, -10, 1, 21, 5, -5, 5, -5, 11, 11)]
        [TestCase(-10, 1, 21, -10, 1, 21, 20, 5, 20, 4, 6, 7)]
        public void TrimToNearestPoints(double minLat, double latStep, int latStepCount, double minLon, double lonStep, int lonStepCount, double north, double south, double east, double west, int expectedLatitudeCount, int expectedLongitudeCount)
        {
            for (var latIndex = 0; latIndex < latStepCount; latIndex++)
                for (var lonIndex = 0; lonIndex < lonStepCount; lonIndex++)
                    _list.Add(new EarthCoordinate(minLat + (latIndex * latStep), minLon + (lonIndex * lonStep)));
            Debug.WriteLine("Number of latitudes is {0}", _list.Latitudes.Count);
            Debug.WriteLine("Number of longitudes is {0}", _list.Longitudes.Count);
            Debug.WriteLine("Number of elements is {0}", _list.Count);
            Assert.AreEqual(latStepCount, _list.Latitudes.Count);
            Assert.AreEqual(lonStepCount, _list.Longitudes.Count);
            Assert.AreEqual(latStepCount * lonStepCount, _list.Count);
            var trimRect = new GeoRect(north, south, east, west);
            _list.TrimToNearestPoints(trimRect, 1);
            Debug.WriteLine("Number of latitudes is {0}", _list.Latitudes.Count);
            Debug.WriteLine("Number of longitudes is {0}", _list.Longitudes.Count);
            Debug.WriteLine("Number of elements is {0}", _list.Count);
        }

        void TestNearestAndExact(Geo nearest, double latitude, double longitude)
        {
            EarthCoordinate result;

            Assert.IsTrue(_list.TryGetNearestPoint(latitude, longitude, out result));
            Assert.AreSame(result, nearest);

            Assert.IsTrue(_list.TryGetNearestPoint(new Geo(latitude, longitude), out result));
            Assert.AreSame(result, nearest);

            result = _list.GetNearestPoint(latitude, longitude);
            Assert.AreSame(result, nearest);

            result = _list.GetNearestPoint(new Geo(latitude, longitude));
            Assert.AreSame(result, nearest);
            
            Assert.IsTrue(_list.TryGetExactPoint(nearest.Latitude, nearest.Longitude, out result));
            Assert.AreSame(result, nearest);
            
            Assert.IsTrue(_list.TryGetExactPoint(nearest, out result));
            Assert.AreSame(result, nearest);

            result = _list.GetExactPoint(nearest.Latitude, nearest.Longitude);
            Assert.AreSame(result, nearest);

            result = _list.GetExactPoint(nearest);
            Assert.AreSame(result, nearest);

        }
    }
}
