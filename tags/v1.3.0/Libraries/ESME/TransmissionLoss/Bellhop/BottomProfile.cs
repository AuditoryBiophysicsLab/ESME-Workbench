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
            Length = EarthCoordinate.DistanceBetween_Meters(transect.StartPoint, transect.EndPoint);
            var delta = (transect.EndPoint - transect.StartPoint)/(numberOfPointsInTransect - 1);
            var currentPoint = transect.StartPoint;
            for (var i = 0; i < numberOfPointsInTransect; i++)
            {
                Profile[i] = Math.Abs(TwoDBilinearApproximation(bathymetry.Latitudes, bathymetry.Longitudes, bathymetry.Elevations, currentPoint));
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

        public static double TwoDBilinearApproximation(double[] latitudes, double[] longitudes, float[,] elevations, EarthCoordinate point)
        {
            if (latitudes.Length != elevations.GetLength(0)) 
                throw new ApplicationException("TwoDBilinearApproximation: Latitudes length must be the same as dimension 0 of Elevations");
            if (longitudes.Length != elevations.GetLength(1)) 
                throw new ApplicationException("TwoDBilinearApproximation: Longitudes length must be the same as dimension 1 of Elevations");

            if ((point.Latitude_degrees < latitudes[0]) || (point.Latitude_degrees > latitudes[latitudes.Length - 1]) || (point.Longitude_degrees < longitudes[0]) || (point.Longitude_degrees > longitudes[longitudes.Length - 1])) throw new BathymetryOutOfBoundsException("TwoDBilinearApproximation: XCoord and YCoord must be within the provided data set.  This is an interpolation routine not an extrapolation one.");
            for (var i = 0; i < latitudes.Length - 1; i++)
            {
                // Latitudes go from south to north, so a southern latitudes come before northern ones
                if ((latitudes[i] > point.Latitude_degrees) || (point.Latitude_degrees > latitudes[i + 1])) continue;
                for (var j = 0; j < longitudes.Length - 1; j++)
                {
                    // Longitudes go from west to east, so western longitudes come before eastern ones
                    if ((longitudes[j] > point.Longitude_degrees) || (point.Longitude_degrees > longitudes[j + 1])) continue;
                    var north = i + 1;
                    var south = i;
                    var east = j + 1;
                    var west = j;
                    return BilinearRecursive(point, // Point to interpolate
                                             new EarthCoordinate3D(latitudes[north], longitudes[east], elevations[north, east]), // northeast corner
                                             new EarthCoordinate3D(latitudes[north], longitudes[west], elevations[north, west]), // northwest corner
                                             new EarthCoordinate3D(latitudes[south], longitudes[east], elevations[south, east]), // southeast corner
                                             new EarthCoordinate3D(latitudes[south], longitudes[west], elevations[south, west]) // southwest corner
                        );
                } // for j
            } // for i
            throw new BathymetryOutOfBoundsException("TwoDBilinearApproximation: Desired point does not appear to be in data set.  This message should never appear!");
        }

        // TwoDBilinearApproximation

        static double BilinearRecursive(EarthCoordinate pointToInterpolate, EarthCoordinate3D northeast, EarthCoordinate3D northwest, EarthCoordinate3D southeast, EarthCoordinate3D southwest)
        {
            var zList = new List<double>();

            if ((pointToInterpolate.Latitude_degrees < southeast.Latitude_degrees) || (northwest.Latitude_degrees < pointToInterpolate.Latitude_degrees) || (pointToInterpolate.Longitude_degrees < northwest.Longitude_degrees) || (southeast.Longitude_degrees < pointToInterpolate.Longitude_degrees)) throw new ApplicationException("BilinearRecursive: PointToInterpolate is not within the southeast to northwest region");
            zList.Add(northeast.Elevation_meters);
            zList.Add(northwest.Elevation_meters);
            zList.Add(southeast.Elevation_meters);
            zList.Add(southwest.Elevation_meters);
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
            if (pointToInterpolate.Latitude_degrees <= middle.Latitude_degrees)
                return pointToInterpolate.Longitude_degrees <= middle.Longitude_degrees ? BilinearRecursive(pointToInterpolate, middle, west, south, southwest) : BilinearRecursive(pointToInterpolate, east, middle, southeast, south);

            // if the latitude is greater than the middle latitude, then the point is in the north half
            // if the longitude is less than or equal to the middle longitude, then the point is in the northwest quadrant
            // if the longitude is greater than the middle longitude, then the point is in the northeast quadrant
            return pointToInterpolate.Longitude_degrees <= middle.Longitude_degrees ? BilinearRecursive(pointToInterpolate, north, northwest, middle, west) : BilinearRecursive(pointToInterpolate, northeast, north, east, middle);
        }
    }
}