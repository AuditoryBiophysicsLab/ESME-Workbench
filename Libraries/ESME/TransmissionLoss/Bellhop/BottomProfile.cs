using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using ESME.Environment;
using ESME.Model;
using HRC.Navigation;

namespace ESME.TransmissionLoss.Bellhop
{
    public class BottomProfile
    {
        public BottomProfile(int numberOfPointsInTransect, Transect transect, Bathymetry bathymetry)
        {
            MaxDepth = double.MinValue;
            Profile = new List<BottomProfilePoint>();
            Length = EarthCoordinate.DistanceBetween(transect.StartPoint, transect.EndPoint);
            var stepLength = Length / (numberOfPointsInTransect - 1);
            var currentPoint = transect.StartPoint;
            var curRange = 0.0;
            for (var i = 0; i < numberOfPointsInTransect; i++)
            {
                var curDepth = Math.Round(Math.Abs(TwoDBilinearApproximation(bathymetry, currentPoint)), 2);
                Profile.Add(new BottomProfilePoint { Depth = curDepth, Range = (curRange / 1000.0) });
                if (MaxDepth < curDepth)
                {
                    MaxDepth = curDepth;
                    DeepestPoint = currentPoint;
                }
                currentPoint = new EarthCoordinate(currentPoint, transect.Bearing, stepLength);
                curRange += stepLength;
            }
            //Profile = profile.ToList();
        }

        static IEnumerable<BottomProfilePoint> Filter(IList<BottomProfilePoint> source)
        {
            var curProfilePoint = source[0];
            yield return curProfilePoint;
            foreach (var point in source.Where(point => curProfilePoint.Depth != point.Depth)) 
            {
                curProfilePoint = point;
                yield return point;
            }
            if (curProfilePoint.Range < source.Last().Range) yield return source.Last();
        }

        public List<BottomProfilePoint> Profile { get; private set; }

        public double Length { get; private set; }

        public EarthCoordinate DeepestPoint { get; private set; }

        public double MaxDepth { get; set; }

        public void WriteBathymetryFile(string fileName)
        {
            using (var sw = new StreamWriter(fileName)) 
                sw.Write(ToBellhopString());
        }

        public string ToBellhopString()
        {
            var sb = new StringBuilder();
            sb.AppendFormat("'L' \n");
            sb.AppendFormat("{0} \n", Profile.Count);
            foreach (var point in Profile) sb.AppendFormat("{0:0.#####} {1:0.#####} \n", point.Range, point.Depth);
            return sb.ToString();
        }

        public static double TwoDBilinearApproximation(Bathymetry bathymetry, EarthCoordinate pt)
        {
            if (!bathymetry.Samples.GeoRect.Contains(pt)) 
                throw new BathymetryOutOfBoundsException("TwoDBilinearApproximation: XCoord and YCoord must be within the provided data set.  This is an interpolation routine not an extrapolation one.");
            var lat = pt.Latitude;
            var lon = pt.Longitude;
            for (var i = 0; i < bathymetry.Samples.Latitudes.Length - 1; i++)
            {
                // elevations.Latitudes go from south to north, so a southern elevations.Latitudes come before northern ones
                if ((bathymetry.Samples.Latitudes[i] > lat) || (lat > bathymetry.Samples.Latitudes[i + 1])) continue;
                for (var j = 0; j < bathymetry.Samples.Longitudes.Length - 1; j++)
                {
                    // elevations.Longitudes go from west to east, so western elevations.Longitudes come before eastern ones
                    if ((bathymetry.Samples.Longitudes[j] > lon) || (lon > bathymetry.Samples.Longitudes[j + 1])) continue;
                    var north = i + 1;
                    var south = i;
                    var east = j + 1;
                    var west = j;
                    var northEast = bathymetry.Samples[(uint)east, (uint)north];
                    var northWest = bathymetry.Samples[(uint)west, (uint)north];
                    var southEast = bathymetry.Samples[(uint)east, (uint)south];
                    var southWest = bathymetry.Samples[(uint)west, (uint)south];

                    return BilinearRecursive(pt, // Point to interpolate
                        northEast, northWest, southEast, southWest);
                } // for j
            } // for i
            throw new BathymetryOutOfBoundsException("TwoDBilinearApproximation: Desired point does not appear to be in data set.  This message should never appear!");
        }

        // TwoDBilinearApproximation

        static double BilinearRecursive(Geo pointToInterpolate, EarthCoordinate<float> northEast, EarthCoordinate<float> northWest, EarthCoordinate<float> southEast, EarthCoordinate<float> southWest)
        {
            var zList = new List<double>();

            if ((pointToInterpolate.Latitude < southEast.Latitude) || (northWest.Latitude < pointToInterpolate.Latitude) || (pointToInterpolate.Longitude < northWest.Longitude) || (southEast.Longitude < pointToInterpolate.Longitude)) throw new ApplicationException("BilinearRecursive: PointToInterpolate is not within the southeast to northwest region");
            zList.Add(northEast.Data);
            zList.Add(northWest.Data);
            zList.Add(southEast.Data);
            zList.Add(southWest.Data);
            zList.Sort();
            var zMin = zList[0];
            var zMax = zList[3];

            if ((zMax - zMin) < 1) return (zMax + zMin) / 2;

            var east = new EarthCoordinate<float>((northEast + southEast) / 2, (northEast.Data + southEast.Data) / 2);
            var west = new EarthCoordinate<float>((northWest + southWest) / 2, (northWest.Data + southWest.Data) / 2);
            var north = new EarthCoordinate<float>((northEast + northWest) / 2, (northEast.Data + northWest.Data) / 2);
            var south = new EarthCoordinate<float>((southEast + southWest) / 2, (southEast.Data + southWest.Data) / 2);
            var middle = new EarthCoordinate<float>((north + south) / 2, (north.Data + south.Data) / 2);

            // if the latitude is less than or equal to the middle latitude, then the point is in the south half
            // if the longitude is less than or equal to the middle longitude, then the point is in the southwest quadrant
            // if the longitude is greater than the middle longitude, then the point is in the southeast quadrant
            if (pointToInterpolate.Latitude <= middle.Latitude)
                return pointToInterpolate.Longitude <= middle.Longitude ? BilinearRecursive(pointToInterpolate, middle, west, south, southWest) : BilinearRecursive(pointToInterpolate, east, middle, southEast, south);

            // if the latitude is greater than the middle latitude, then the point is in the north half
            // if the longitude is less than or equal to the middle longitude, then the point is in the northwest quadrant
            // if the longitude is greater than the middle longitude, then the point is in the northeast quadrant
            return pointToInterpolate.Longitude <= middle.Longitude ? BilinearRecursive(pointToInterpolate, north, northWest, middle, west) : BilinearRecursive(pointToInterpolate, northEast, north, east, middle);
        }
    }

    public class BottomProfilePoint
    {
        public double Range;
        public double Depth;
        public override string ToString() { return string.Format("{0},{1}", Range, Depth); }
    }
}