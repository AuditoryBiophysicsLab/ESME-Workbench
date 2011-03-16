using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using HRC.Navigation;

namespace ESME.Environment
{
    public class Sediment : Environment2DData<SedimentSample>
    {
        private Sediment(IEnumerable<SedimentSample> data) : base(data) { }

        public static Sediment FromSedimentCHB(string fileName)
        {
            var data = new List<SedimentSample>();
            using (var stream = new BinaryReader(File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                var west = stream.ReadSingle();
                // east (unused)
                stream.ReadSingle(); 
                var south = stream.ReadSingle();
                // north (unused)
                stream.ReadSingle(); 
                var gridSpacing = stream.ReadSingle() / 60f; // Source is in minutes, we need degrees
                var width = stream.ReadInt32();
                var height = stream.ReadInt32();
                var endian = stream.ReadUInt32();
                if (endian != 0x00010203) throw new FileFormatException("Invalid CHRTR Binary file format - endian is incorrect");
                var minValue = stream.ReadSingle();
                var maxValue = stream.ReadSingle();
                var paddingWidth = (width - 10) * 4;
                if (paddingWidth < 0) return null;
                stream.ReadBytes(paddingWidth);
                for (var lat = 0; lat < height; lat++)
                {
                    var latitude = south + (lat * gridSpacing);
                    for (var lon = 0; lon < width; lon++)
                    {
                        var longitude = west + (lon * gridSpacing);
                        var curSampleValue = stream.ReadSingle();
                        var newSample = new SedimentSample(latitude, lat, longitude, lon);
                        if ((minValue <= curSampleValue) && (curSampleValue <= maxValue)) newSample.Data = curSampleValue;
                        data.Add(newSample);
                    }
                }
                var result = new Sediment(data);
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
                for (var lat = 0; lat < FieldData.GetLength(1); lat++)
                {
                    for (var lon = 0; lon < FieldData.GetLength(0); lon++)
                    {
                        if (!FieldData[lon, lat].Data.HasValue)
                            if (!FillOneDamnHole(FieldData[lon, lat])) 
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
                    curSample.Data = actualValues.First();
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
                    curSample.Data = actualValues.First();
                    return true;
                }

                curSample.Data = possibleValues.Max();
                return true;
            }
            // If no values were found, then don't put anything in the current cell, for now.
            return false;
        }

        private bool TryGetValue(int lonIndex, int latIndex, out float value)
        {
            value = 0;
            if ((lonIndex < 0) || (lonIndex >= FieldData.GetLength(0))) return false;
            if ((latIndex < 0) || (latIndex >= FieldData.GetLength(1))) return false;

            if (FieldData[lonIndex, latIndex] == null) return false;
            var curValue = FieldData[lonIndex, latIndex].Data;
            value = curValue.HasValue ? curValue.Value : 0;
            return curValue.HasValue;
        }

#if false
        public virtual bool ClosestTo(EarthCoordinate coordinate, ref T value)
        {
            var north = SedimentSamples[0].Latitude;
            var south = SedimentSamples[0].Latitude;
            var east = SedimentSamples[0].Longitude;
            var west = SedimentSamples[0].Longitude;
            var minDistance = coordinate.GetDistanceTo_Meters(SedimentSamples[0]);
            foreach (var sample in SedimentSamples)
            {
                
            }
            if (SedimentSamples.ContainsCoordinate(coordinate))
            {
                var closestCoordinate = new EarthCoordinate(Latitudes.First(), Longitudes.First());
                foreach (var curCoordinate in Latitudes.SelectMany(latitude => Longitudes, (latitude, longitude) => new EarthCoordinate(latitude, longitude)).Where(curCoordinate => coordinate.GetDistanceTo_Meters(closestCoordinate) > coordinate.GetDistanceTo_Meters(curCoordinate)))
                    closestCoordinate = curCoordinate;
                var latIndex = LookupIndex(closestCoordinate.Latitude, Latitudes);
                var lonIndex = LookupIndex(closestCoordinate.Longitude, Longitudes);
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
        public override void Save(BinaryWriter stream) { throw new NotImplementedException(); }
    }

    public class SedimentSample : EarthCoordinate<float?>, IComparable<SedimentSample>
    {
        internal SedimentSample(double latitude, int latIndex, double longitude, int lonIndex)
            : base(latitude, longitude)
        {
            LonIndex = lonIndex;
            LatIndex = latIndex;
            Data = null;
        }

        internal int LatIndex { get; private set; }
        internal int LonIndex { get; private set; }

        public int CompareTo(SedimentSample other)
        {
            if ((Data == null) || (other.Data == null)) throw new ApplicationException("SedimentSample: Can't compare samples when one has null data");
            if (!Data.HasValue) throw new ApplicationException("SedimentSample: Can't compare samples when my data value has not been");
            if (!other.Data.HasValue) throw new ApplicationException("SedimentSample: Can't compare samples when the other data value has not been set");
            return Data.Value.CompareTo(other.Data.Value);
        }
    }
}