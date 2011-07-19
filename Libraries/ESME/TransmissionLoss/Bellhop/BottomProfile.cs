using System;
using System.Collections.Generic;
using System.IO;
using ESME.Environment;
using ESME.Model;
using HRC.Navigation;

namespace ESME.TransmissionLoss.Bellhop
{
    public class BottomProfile
    {
        const UInt32 Magic = 0x2bf6f6e5;

        public BottomProfile(int numberOfPointsInTransect, Transect transect, Bathymetry bathymetry)
        {
            MaxDepth = double.MinValue;
            Profile = new double[numberOfPointsInTransect];
            Length = EarthCoordinate.DistanceBetween(transect.StartPoint, transect.EndPoint);
            var delta = (transect.EndPoint - transect.StartPoint)/(numberOfPointsInTransect - 1);
            var currentPoint = transect.StartPoint;
            for (var i = 0; i < numberOfPointsInTransect; i++)
            {
                Profile[i] = Math.Abs(TwoDBilinearApproximation(bathymetry, currentPoint));
                if (MaxDepth < Profile[i])
                {
                    MaxDepth = Profile[i];
                    DeepestPoint = currentPoint;
                }
                currentPoint += delta;
            }
        }

        public BottomProfile(BinaryReader stream)
        {
            int i;
            if (stream.ReadUInt32() != Magic) throw new FormatException("Format error reading BottomProfile from file");
            Length = stream.ReadSingle();
            MaxDepth = stream.ReadSingle();
            Profile = new double[stream.ReadInt32()];
            for (i = 0; i < Profile.Length; i++) Profile[i] = stream.ReadSingle();
        }

        public double[] Profile { get; private set; }

        public double Length { get; private set; }

        public EarthCoordinate DeepestPoint { get; private set; }

        public double MaxDepth { get; set; }

        public void Write(BinaryWriter stream)
        {
            stream.Write(Magic);
            stream.Write((float) Length);
            stream.Write((float) MaxDepth);
            stream.Write(Profile.Length);
            foreach (var t in Profile) stream.Write((float) t);
        }

        public void WriteBathymetryFile(string fileName)
        {
            using (var sw = new StreamWriter(fileName)) 
                sw.Write(ToBellhopString());
        }

        public string ToBellhopString()
        {
            var interPointDistanceKm = ((Length/(Profile.Length - 1))/1000);

            using (var sw = new StringWriter())
            {
                sw.WriteLine(Profile.Length);
                for (var i = 0; i < Profile.Length; i++) sw.WriteLine("{0:F} {1:F}", interPointDistanceKm*i, Profile[i]);
                return sw.ToString();
            }
        }

        public static double TwoDBilinearApproximation(Bathymetry bathymetry, EarthCoordinate point)
        {
            if ((point.Latitude < bathymetry.Samples.Latitudes[0]) || (point.Latitude > bathymetry.Samples.Latitudes[bathymetry.Samples.Latitudes.Count - 1]) || (point.Longitude < bathymetry.Samples.Longitudes[0]) || (point.Longitude > bathymetry.Samples.Longitudes[bathymetry.Samples.Longitudes.Count - 1])) throw new BathymetryOutOfBoundsException("TwoDBilinearApproximation: XCoord and YCoord must be within the provided data set.  This is an interpolation routine not an extrapolation one.");
            for (var i = 0; i < bathymetry.Samples.Latitudes.Count - 1; i++)
            {
                // elevations.Latitudes go from south to north, so a southern elevations.Latitudes come before northern ones
                if ((bathymetry.Samples.Latitudes[i] > point.Latitude) || (point.Latitude > bathymetry.Samples.Latitudes[i + 1])) continue;
                for (var j = 0; j < bathymetry.Samples.Longitudes.Count - 1; j++)
                {
                    // elevations.Longitudes go from west to east, so western elevations.Longitudes come before eastern ones
                    if ((bathymetry.Samples.Longitudes[j] > point.Longitude) || (point.Longitude > bathymetry.Samples.Longitudes[j + 1])) continue;
                    var north = i + 1;
                    var south = i;
                    var east = j + 1;
                    var west = j;
                    return BilinearRecursive(point, // Point to interpolate
                                             new EarthCoordinate3D(bathymetry.Samples.Latitudes[north], bathymetry.Samples.Longitudes[east], bathymetry.Samples[(uint)east, (uint)north].Data), // northeast corner
                                             new EarthCoordinate3D(bathymetry.Samples.Latitudes[north], bathymetry.Samples.Longitudes[west], bathymetry.Samples[(uint)west, (uint)north].Data), // northwest corner
                                             new EarthCoordinate3D(bathymetry.Samples.Latitudes[south], bathymetry.Samples.Longitudes[east], bathymetry.Samples[(uint)east, (uint)south].Data), // southeast corner
                                             new EarthCoordinate3D(bathymetry.Samples.Latitudes[south], bathymetry.Samples.Longitudes[west], bathymetry.Samples[(uint)west, (uint)south].Data) // southwest corner
                        );
                } // for j
            } // for i
            throw new BathymetryOutOfBoundsException("TwoDBilinearApproximation: Desired point does not appear to be in data set.  This message should never appear!");
        }

        // TwoDBilinearApproximation

        static double BilinearRecursive(EarthCoordinate pointToInterpolate, EarthCoordinate3D northeast, EarthCoordinate3D northwest, EarthCoordinate3D southeast, EarthCoordinate3D southwest)
        {
            var zList = new List<double>();

            if ((pointToInterpolate.Latitude < southeast.Latitude) || (northwest.Latitude < pointToInterpolate.Latitude) || (pointToInterpolate.Longitude < northwest.Longitude) || (southeast.Longitude < pointToInterpolate.Longitude)) throw new ApplicationException("BilinearRecursive: PointToInterpolate is not within the southeast to northwest region");
            zList.Add(northeast.Elevation);
            zList.Add(northwest.Elevation);
            zList.Add(southeast.Elevation);
            zList.Add(southwest.Elevation);
            zList.Sort();
            var zMin = zList[0];
            var zMax = zList[3];

            if ((zMax - zMin) < 1) return (zMax + zMin)/2;

            var east = (northeast + southeast)/2;
            var west = (northwest + southwest)/2;
            var north = (northeast + northwest)/2;
            var south = (southeast + southwest)/2;
            var middle = (north + south)/2;

            // if the latitude is less than or equal to the middle latitude, then the point is in the south half
            // if the longitude is less than or equal to the middle longitude, then the point is in the southwest quadrant
            // if the longitude is greater than the middle longitude, then the point is in the southeast quadrant
            if (pointToInterpolate.Latitude <= middle.Latitude)
                return pointToInterpolate.Longitude <= middle.Longitude ? BilinearRecursive(pointToInterpolate, middle, west, south, southwest) : BilinearRecursive(pointToInterpolate, east, middle, southeast, south);

            // if the latitude is greater than the middle latitude, then the point is in the north half
            // if the longitude is less than or equal to the middle longitude, then the point is in the northwest quadrant
            // if the longitude is greater than the middle longitude, then the point is in the northeast quadrant
            return pointToInterpolate.Longitude <= middle.Longitude ? BilinearRecursive(pointToInterpolate, north, northwest, middle, west) : BilinearRecursive(pointToInterpolate, northeast, north, east, middle);
        }
    }
}