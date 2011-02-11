using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment
{
    public class Sediment : SerializableData<Sediment>
    {
        public Sediment() { SedimentSamples = new List<SedimentSample>(); }

        public List<SedimentSample> SedimentSamples { get; set; }

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
                for (var lat = 0; lat < height; lat++)
                {
                    var latitude = south + (lat * gridSpacing);
                    for (var lon = 0; lon < width; lon++)
                    {
                        var longitude = west + (lon * gridSpacing);
                        var curSample = stream.ReadSingle();
                        if ((minValue <= curSample) && (curSample <= maxValue)) result.SedimentSamples.Add(new SedimentSample(latitude, longitude, curSample));
                    }
                }
                return result;
            }
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
#endif

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
            value = sample.Value;
            return result;
        }

        public static Sediment ReadESMEEnvironmentBinaryFile(string fileName, float north, float south, float east, float west)
        {
            var source = new Environment2DData(fileName, "bottomtype", north, west, south, east);
            var result = new Sediment();
            for (var latIndex = 0; latIndex < source.Latitudes.Length; latIndex++) for (var lonIndex = 0; lonIndex < source.Longitudes.Length; lonIndex++) result.SedimentSamples.Add(new SedimentSample(source.Latitudes[latIndex], source.Longitudes[lonIndex], source.Values[latIndex, lonIndex]));
            return result;
        }
    }

    public class SedimentSample : EarthCoordinate
    {
        public SedimentSample(double latitude, double longitude, float value) : base(latitude, longitude) { Value = value; }
        public float Value { get; set; }
    }
}