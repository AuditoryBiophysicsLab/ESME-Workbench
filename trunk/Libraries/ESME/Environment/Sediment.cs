using System.Collections.Generic;
using System.IO;
using System.Linq;
using HRC.Navigation;

namespace ESME.Environment
{
    public class Sediment
    {
        private Sediment()
        {
            Latitudes = new List<double>();
            Longitudes = new List<double>();
        }

        public List<double> Latitudes { get; private set; }
        public List<double> Longitudes { get; private set; }

        public SedimentSample[,] SedimentSamples { get; private set; }

        // Return the nearest sample to the given location
        public SedimentSample this[EarthCoordinate location]
        {
            get
            {
                int latStartIndex;
                var latEndIndex = latStartIndex = Latitudes.IndexOf(location.Latitude_degrees);
                if (latStartIndex == -1)
                {
                    var southLats = Latitudes.FindAll(y => y <= location.Latitude_degrees);
                    if (southLats.Count() > 0) latStartIndex = Latitudes.IndexOf(southLats.Last());
                }
                if (latEndIndex == -1)
                {
                    var northLats = Latitudes.FindAll(y => y >= location.Latitude_degrees);
                    if (northLats.Count() > 0) latEndIndex = Latitudes.IndexOf(northLats.First());
                }
                if (latStartIndex == -1) latStartIndex = latEndIndex;
                if (latEndIndex == -1) latEndIndex = latStartIndex;

                int lonStartIndex;
                var lonEndIndex = lonStartIndex = Longitudes.IndexOf(location.Longitude_degrees);
                if (lonStartIndex == -1)
                {
                    var westLons = Longitudes.FindAll(x => x <= location.Longitude_degrees);
                    if (westLons.Count() > 0) lonStartIndex = Longitudes.IndexOf(westLons.Last());
                }
                if (lonEndIndex == -1)
                {
                    var eastLons = Latitudes.FindAll(x => x >= location.Longitude_degrees);
                    if (eastLons.Count() > 0) lonEndIndex = Longitudes.IndexOf(eastLons.First());
                }
                if (lonStartIndex == -1) lonStartIndex = lonEndIndex;
                if (lonEndIndex == -1) lonEndIndex = lonStartIndex;

                var searchList = new List<SedimentSample>();
                for (var latIndex = latStartIndex; latIndex <= latEndIndex; latIndex++) 
                    for (var lonIndex = lonStartIndex; lonIndex <= lonEndIndex; lonIndex++) 
                        searchList.Add(SedimentSamples[lonIndex, latIndex]);
                var closestSample = searchList.First();
                var closestDistance = location.GetDistanceTo_Meters(closestSample);
                foreach (var curSample in searchList)
                {
                    var curDistance = location.GetDistanceTo_Meters(curSample);
                    if (curDistance >= closestDistance) continue;
                    closestDistance = curDistance;
                    closestSample = curSample;
                }
                return closestSample;
            }
        }

        public static Sediment ReadChrtrBinaryFile(string fileName)
        {
            var result = new Sediment();
            using (var stream = new BinaryReader(File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                var west = stream.ReadSingle();
                var east = stream.ReadSingle();
                var south = stream.ReadSingle();
                var north = stream.ReadSingle();
                var gridSpacing = stream.ReadSingle() / 60f; // Source is in minutes, we need degrees
                var width = stream.ReadInt32();
                var height = stream.ReadInt32();
                var endian = stream.ReadUInt32();
                if (endian != 0x00010203) throw new FileFormatException("Invalid CHRTR Binary file format - endian is incorrect");
                var minValue = stream.ReadSingle();
                var maxValue = stream.ReadSingle();
                var paddingWidth = (width - 10) * 4;
                stream.ReadBytes(paddingWidth);
                result.SedimentSamples = new SedimentSample[width, height];
                for (var lat = 0; lat < height; lat++)
                {
                    var latitude = south + (lat * gridSpacing);
                    for (var lon = 0; lon < width; lon++)
                    {
                        var longitude = west + (lon * gridSpacing);
                        var curSampleValue = stream.ReadSingle();
                        var curSample = new SedimentSample(latitude, lat, longitude, lon);
                        if ((minValue <= curSampleValue) && (curSampleValue <= maxValue)) curSample.SedimentType = curSampleValue;
                        result.SedimentSamples[lon, lat] = curSample;
                    }
                }
                for (var lat = 0; lat < height; lat++)
                    result.Latitudes.Add(south + (lat * gridSpacing));
                for (var lon = 0; lon < width; lon++)
                    result.Longitudes.Add(west + (lon * gridSpacing));
                result.FillTheDamnHoles();
                return result;
            }
        }

        private void FillTheDamnHoles()
        {
            var thereIsAtLeastOneHole = true;
            var passCount = 0;
            while (thereIsAtLeastOneHole)
            {
                thereIsAtLeastOneHole = false;
                for (var lat = 0; lat < SedimentSamples.GetLength(1); lat++)
                {
                    for (var lon = 0; lon < SedimentSamples.GetLength(0); lon++)
                    {
                        if (!SedimentSamples[lon, lat].SedimentType.HasValue) 
                            if (!FillOneDamnHole(SedimentSamples[lon, lat])) 
                                thereIsAtLeastOneHole = true;
                    }
                }
                passCount++;
            }
            System.Diagnostics.Debug.WriteLine("FillTheDamnHoles: Sediment sample holes filled after " + passCount + " passes.");
        }

        private bool FillOneDamnHole(SedimentSample curSample)
        {
            float nearbyValue;
            var possibleValues = new List<float>();

            // See if there's a value to the south, north, west and east first.  Those are the closest to the current point.
            if (TryGetValue(curSample.LonIndex - 1, curSample.LatIndex, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (TryGetValue(curSample.LonIndex + 1, curSample.LatIndex, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (TryGetValue(curSample.LonIndex, curSample.LatIndex - 1, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (TryGetValue(curSample.LonIndex, curSample.LatIndex + 1, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (possibleValues.Count > 0)
            {
                var actualValues = from value in possibleValues
                                   where ((1 <= value) && (value <= 23))
                                   select value;

                if (actualValues.Count() > 0)
                {
                    curSample.SedimentType = actualValues.First();
                    return true;
                }
            }

            // If nothing was found above, try the southwest, southeast, northwest and northeast corners
            if (TryGetValue(curSample.LonIndex - 1, curSample.LatIndex - 1, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (TryGetValue(curSample.LonIndex - 1, curSample.LatIndex + 1, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (TryGetValue(curSample.LonIndex - 1, curSample.LatIndex - 1, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (TryGetValue(curSample.LonIndex + 1, curSample.LatIndex + 1, out nearbyValue)) possibleValues.Add(nearbyValue);
            if (possibleValues.Count > 0)
            {
                var actualValues = from value in possibleValues
                                   where ((1 <= value) && (value <= 23))
                                   select value;

                if (actualValues.Count() > 0)
                {
                    curSample.SedimentType = actualValues.First();
                    return true;
                }

                curSample.SedimentType = possibleValues.Max();
                return true;
            }
            // If no values were found, then don't put anything in the current cell, for now.
            return false;
        }

        private bool TryGetValue(int lonIndex, int latIndex, out float value)
        {
            value = 0;
            if ((lonIndex < 0) || (lonIndex >= SedimentSamples.GetLength(0))) return false;
            if ((latIndex < 0) || (latIndex >= SedimentSamples.GetLength(1))) return false;
            if (!SedimentSamples[lonIndex, latIndex].SedimentType.HasValue) return false;
            value = SedimentSamples[lonIndex, latIndex].SedimentType.Value;
            return true;
        }

#if false
        public virtual bool ClosestTo(EarthCoordinate coordinate, ref T value)
        {
            var north = SedimentSamples[0].Latitude_degrees;
            var south = SedimentSamples[0].Latitude_degrees;
            var east = SedimentSamples[0].Longitude_degrees;
            var west = SedimentSamples[0].Longitude_degrees;
            var minDistance = coordinate.GetDistanceTo_Meters(SedimentSamples[0]);
            foreach (var sample in SedimentSamples)
            {
                
            }
            if (SedimentSamples.ContainsCoordinate(coordinate))
            {
                var closestCoordinate = new EarthCoordinate(Latitudes.First(), Longitudes.First());
                foreach (var curCoordinate in Latitudes.SelectMany(latitude => Longitudes, (latitude, longitude) => new EarthCoordinate(latitude, longitude)).Where(curCoordinate => coordinate.GetDistanceTo_Meters(closestCoordinate) > coordinate.GetDistanceTo_Meters(curCoordinate)))
                    closestCoordinate = curCoordinate;
                var latIndex = LookupIndex(closestCoordinate.Latitude_degrees, Latitudes);
                var lonIndex = LookupIndex(closestCoordinate.Longitude_degrees, Longitudes);
                if ((latIndex >= 0) && (lonIndex >= 0))
                {
                    value = Values[latIndex, lonIndex];
                    return true;
                }
            }
            return false;
        }

        public bool ClosestTo(EarthCoordinate coordinate, out SedimentSample value)
        {
            if ((SedimentSamples == null) || (SedimentSamples.Count == 0)) throw new ApplicationException("No sediment data available");
            value = SedimentSamples[0];
            var closestDistance = coordinate.GetDistanceTo_Meters(value);
            foreach (var sample in SedimentSamples)
                if (coordinate.GetDistanceTo_Meters(value) < closestDistance) 
                    value = sample;
            return true;
        }

        public bool ClosestTo(EarthCoordinate coordinate, out float value)
        {
            SedimentSample sample;
            var result = ClosestTo(coordinate, out sample);
            value = sample.SedimentType;
            return result;
        }

        public static Sediment ReadESMEEnvironmentBinaryFile(string fileName, float north, float south, float east, float west)
        {
            var source = new Environment2DData(fileName, "bottomtype", north, west, south, east);
            var result = new Sediment();
            for (var latIndex = 0; latIndex < source.Latitudes.Length; latIndex++) 
                for (var lonIndex = 0; lonIndex < source.Longitudes.Length; lonIndex++) 
                    result.SedimentSamples.Add(new SedimentSample(source.Latitudes[latIndex], source.Longitudes[lonIndex], source.Values[latIndex, lonIndex]));
            return result;
        }
#endif
    }

    public class SedimentSample : EarthCoordinate
    {
        internal SedimentSample(double latitude, int latIndex, double longitude, int lonIndex)
            : base(latitude, longitude)
        {
            LonIndex = lonIndex;
            LatIndex = latIndex;
            SedimentType = null;
        }
        public float? SedimentType { get; internal set; }
        internal int LonIndex { get; set; }
        internal int LatIndex { get; set; }
    }
}