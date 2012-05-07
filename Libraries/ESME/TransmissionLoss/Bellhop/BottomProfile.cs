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
        private BottomProfile() {}
        public BottomProfile(int numberOfPointsInTransect, GeoSegment geoSegment , Bathymetry bathymetry)
        {
            MaxDepth = double.MinValue;
            Profile = new List<BottomProfilePoint>();
            Length = Geo.RadiansToKilometers(geoSegment.LengthRadians) * 1000;
            //Length = transect.StartPoint.DistanceKilometers(transect.EndPoint) * 1000;
            var stepLength = Length / (numberOfPointsInTransect - 1);
            var stepFraction = 1.0 / numberOfPointsInTransect;
            //var currentPoint = transect.StartPoint;
            var currentPoint = geoSegment[0];
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
                currentPoint = geoSegment.Slerp(stepFraction * i);
                //currentPoint = currentPoint.Offset(Geo.KilometersToRadians(stepLength / 1000f), Geo.DegreesToRadians(transect.Bearing));
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

        public Geo DeepestPoint { get; private set; }

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

        public static BottomProfilePoint[] FromBellhopFile(string profileFile)
        {
            var lines = File.ReadAllLines(profileFile);
            var result = new List<BottomProfilePoint>();
            var lineCount = int.Parse(lines[1]);
            for (var line = 0; line < lineCount; line++)
            {
                var fields = lines[line + 2].Split(' ');
                result.Add(new BottomProfilePoint{Range = float.Parse(fields[0]), Depth = float.Parse(fields[1])});
            }
            return result.ToArray();
        }

        public static double TwoDBilinearApproximation(Bathymetry bathymetry, Geo pt)
        {
            var bounds = GeoRect.Inflate(bathymetry.Samples.GeoRect, 0.01);
            if (!bounds.Contains(pt)) 
                throw new BathymetryOutOfBoundsException("TwoDBilinearApproximation: XCoord and YCoord must be within the provided data set.  This is an interpolation routine not an extrapolation one.");
            var lat = pt.Latitude;
            var lon = pt.Longitude;
            for (var i = 0; i < bathymetry.Samples.Latitudes.Count - 1; i++)
            {
                // elevations.Latitudes go from south to north, so a southern elevations.Latitudes come before northern ones
                if ((bathymetry.Samples.Latitudes[i] > lat) || (lat > bathymetry.Samples.Latitudes[i + 1])) continue;
                for (var j = 0; j < bathymetry.Samples.Longitudes.Count - 1; j++)
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

        static double BilinearRecursive(Geo pointToInterpolate, Geo<float> northEast, Geo<float> northWest, Geo<float> southEast, Geo<float> southWest)
        {
            var bounds = GeoRect.Inflate(new GeoRect(northEast.Latitude, southWest.Latitude, northEast.Longitude, southWest.Longitude), 0.01);
            if (!bounds.Contains(pointToInterpolate)) 
                throw new ApplicationException("BilinearRecursive: PointToInterpolate is not within the southeast to northwest region");

            var zList = new List<double> { northEast.Data, northWest.Data, southEast.Data, southWest.Data };
            var zMin = zList.Min();
            var zMax = zList.Max();

            if ((zMax - zMin) < 1) return (zMax + zMin) / 2;

            var east = new Geo<float>(northEast.MidPoint(southEast), (northEast.Data + southEast.Data) / 2);
            var west = new Geo<float>(northWest.MidPoint(southWest), (northWest.Data + southWest.Data) / 2);
            var north = new Geo<float>(northEast.MidPoint(northWest), (northEast.Data + northWest.Data) / 2);
            var south = new Geo<float>(southEast.MidPoint(southWest), (southEast.Data + southWest.Data) / 2);
            var middle = new Geo<float>(north.MidPoint(south), (north.Data + south.Data) / 2);

            if (pointToInterpolate.Latitude <= middle.Latitude)   // If the point's latitude is less than or equal to the middle latitude, it's in the southern half of the current region
                return pointToInterpolate.Longitude <= middle.Longitude ? // If the point's longitude is is less than or equal to the middle longitude, it's in the western half of the current region
                    BilinearRecursive(pointToInterpolate, middle, west, south, southWest) : // Return the southwest quadrant
                    BilinearRecursive(pointToInterpolate, east, middle, southEast, south);  // Return the southeast quadrant

            // If we get here, the point is in the northern half of the current region 
            return pointToInterpolate.Longitude <= middle.Longitude ? // If the point's longitude is is less than or equal to the middle longitude, it's in the western half of the current region
                BilinearRecursive(pointToInterpolate, north, northWest, middle, west) : // Return the northwest quadrant
                BilinearRecursive(pointToInterpolate, northEast, north, east, middle);  // Return the northeast quadrant
        }
    }

    public class BottomProfilePoint
    {
        public double Range;
        public double Depth;
        public override string ToString() { return string.Format("{0},{1}", Range, Depth); }
    }
}