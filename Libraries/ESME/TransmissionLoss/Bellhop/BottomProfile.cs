using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using HRC.Navigation;
using ESME.Environment;

namespace ESME.TransmissionLoss
{
    public class BottomProfile
    {
        private double TransectLength_Meters;
        private EarthCoordinate DeepestTransectPoint;
        private double maxDepth_Meters;
        private double[] depthArray_Meters;
        private const UInt32 Magic = 0x2bf6f6e5;

        public double[] Profile { get { return depthArray_Meters; } }
        public double Length_Meters { get { return TransectLength_Meters; } }
        public EarthCoordinate DeepestPoint { get { return DeepestTransectPoint; } }
        public double MaxDepth_Meters { get { return maxDepth_Meters; } set { maxDepth_Meters = value; } }

        public BottomProfile(int NumberOfPointsInTransect, ESME.Environment.Transect transect, Bathymetry bathymetry)
        {
            EarthCoordinate Delta, CurrentPoint;

            maxDepth_Meters = double.MinValue;
            depthArray_Meters = new double[NumberOfPointsInTransect];
            TransectLength_Meters = EarthCoordinate.DistanceBetween_Meters(transect.StartPoint, transect.EndPoint);
            Delta = (transect.EndPoint - transect.StartPoint) / (NumberOfPointsInTransect - 1);
            CurrentPoint = transect.StartPoint;
            for (int i = 0; i < NumberOfPointsInTransect; i++)
            {
                depthArray_Meters[i] = Math.Abs(TwoDBilinearApproximation(bathymetry.Latitudes_degrees, bathymetry.Longitudes_degrees, bathymetry.Elevations_meters, CurrentPoint));
                if (maxDepth_Meters < depthArray_Meters[i])
                {
                    maxDepth_Meters = depthArray_Meters[i];
                    DeepestTransectPoint = CurrentPoint;
                }
                CurrentPoint += Delta;
            }
        }

        public BottomProfile(BinaryReader stream)
        {
            int i;
            if (stream.ReadUInt32() != Magic)
                throw new FormatException("Format error reading BottomProfile from file");
            TransectLength_Meters = stream.ReadSingle();
            maxDepth_Meters = stream.ReadSingle();
            depthArray_Meters = new double[stream.ReadInt32()];
            for (i = 0; i < this.Profile.Length; i++)
                depthArray_Meters[i] = stream.ReadSingle();

        }

        public void Write(BinaryWriter stream)
        {
            int i;
            stream.Write(Magic);
            stream.Write((float)TransectLength_Meters);
            stream.Write((float)maxDepth_Meters);
            stream.Write(this.Profile.Length);
            for (i = 0; i < this.Profile.Length; i++)
                stream.Write((float)this.Profile[i]);
        }

        public void WriteBathymetryFile(string FileName)
        {
            using (StreamWriter sw = new StreamWriter(FileName))
                sw.Write(ToBellhopString());
        }

        public string ToBellhopString()
        {
            double InterPointDistance_km = ((TransectLength_Meters / (depthArray_Meters.Length - 1)) / 1000);

            using (StringWriter sw = new StringWriter())
            {
                sw.WriteLine(depthArray_Meters.Length);
                for (int i = 0; i < depthArray_Meters.Length; i++)
                    sw.WriteLine("{0:F} {1:F}", InterPointDistance_km * i, depthArray_Meters[i]);
                return sw.ToString();
            }
        }

        public static double TwoDBilinearApproximation(double[] Latitudes, double[] Longitudes, float[,] Elevations, EarthCoordinate Point)
        {
            int i, j, east, north, west, south;

            if (Latitudes.Length != Elevations.GetLength(0))
                throw new ApplicationException("TwoDBilinearApproximation: Latitudes length must be the same as dimension 0 of Elevations");
            if (Longitudes.Length != Elevations.GetLength(1))
                throw new ApplicationException("TwoDBilinearApproximation: Longitudes length must be the same as dimension 1 of Elevations");
            if ((Point.Latitude_degrees < Latitudes[0]) || (Point.Latitude_degrees > Latitudes[Latitudes.Length - 1]) || (Point.Longitude_degrees < Longitudes[0]) || (Point.Longitude_degrees > Longitudes[Longitudes.Length - 1]))
                throw new BathymetryOutOfBoundsException("TwoDBilinearApproximation: XCoord and YCoord must be within the provided data set.  This is an interpolation routine not an extrapolation one.");
            for (i = 0; i < Latitudes.Length - 1; i++)
            {
                // Latitudes go from south to north, so a southern latitudes come before northern ones
                if ((Latitudes[i] <= Point.Latitude_degrees) && (Point.Latitude_degrees <= Latitudes[i + 1]))
                {
                    for (j = 0; j < Longitudes.Length - 1; j++)
                    {
                        // Longitudes go from west to east, so western longitudes come before eastern ones
                        if ((Longitudes[j] <= Point.Longitude_degrees) && (Point.Longitude_degrees <= Longitudes[j + 1]))
                        {
                            north = i + 1;
                            south = i;
                            east = j + 1;
                            west = j;
                            return BottomProfile.BilinearRecursive(Point,                                                   // Point to interpolate
                                new EarthCoordinate3D(Latitudes[north], Longitudes[east], (double)Elevations[north, east]), // northeast corner
                                new EarthCoordinate3D(Latitudes[north], Longitudes[west], (double)Elevations[north, west]), // northwest corner
                                new EarthCoordinate3D(Latitudes[south], Longitudes[east], (double)Elevations[south, east]), // southeast corner
                                new EarthCoordinate3D(Latitudes[south], Longitudes[west], (double)Elevations[south, west])  // southwest corner
                                );
                        } // if ((Longitudes[i] <= Longitude) && (Longitude <= Longitudes[i + 1]))
                    } // for j
                } // if ((Latitudes[i] <= Latitude) && (Latitude <= Latitudes[i + 1]))
            } // for i
            throw new BathymetryOutOfBoundsException("TwoDBilinearApproximation: Desired point does not appear to be in data set.  This message should never appear!");
        } // TwoDBilinearApproximation

        private static double BilinearRecursive(EarthCoordinate PointToInterpolate, EarthCoordinate3D northeast, EarthCoordinate3D northwest, EarthCoordinate3D southeast, EarthCoordinate3D southwest)
        {
            EarthCoordinate3D east, west, north, south, middle;
            double ZMin, ZMax;
            List<double> ZList = new List<double>();

            if ((PointToInterpolate.Latitude_degrees < southeast.Latitude_degrees) ||
                (northwest.Latitude_degrees < PointToInterpolate.Latitude_degrees) ||
                (PointToInterpolate.Longitude_degrees < northwest.Longitude_degrees) ||
                (southeast.Longitude_degrees < PointToInterpolate.Longitude_degrees))
                throw new ApplicationException("BilinearRecursive: PointToInterpolate is not within the southeast to northwest region");
            ZList.Add(northeast.Elevation_meters);
            ZList.Add(northwest.Elevation_meters);
            ZList.Add(southeast.Elevation_meters);
            ZList.Add(southwest.Elevation_meters);
            ZList.Sort();
            ZMin = ZList[0];
            ZMax = ZList[3];

            if ((ZMax - ZMin) < 1)
                return (ZMax + ZMin) / 2;

            east = (northeast + southeast) / 2;
            west = (northwest + southwest) / 2;
            north = (northeast + northwest) / 2;
            south = (southeast + southwest) / 2;
            middle = (north + south) / 2;

            // Find which quadrant the point is in
            if (PointToInterpolate.Latitude_degrees <= middle.Latitude_degrees)
            {
                // if the latitude is less than or equal to the middle latitude, then the point is in the south half
                if (PointToInterpolate.Longitude_degrees <= middle.Longitude_degrees)
                {
                    // if the longitude is less than or equal to the middle longitude, then the point is in the southwest quadrant
                    return BilinearRecursive(PointToInterpolate, middle, west, south, southwest);
                }
                else
                {
                    // if the longitude is greater than the middle longitude, then the point is in the southeast quadrant
                    return BilinearRecursive(PointToInterpolate, east, middle, southeast, south);
                }
            }
            else
            {
                // if the latitude is greater than the middle latitude, then the point is in the north half
                if (PointToInterpolate.Longitude_degrees <= middle.Longitude_degrees)
                {
                    // if the longitude is less than or equal to the middle longitude, then the point is in the northwest quadrant
                    return BilinearRecursive(PointToInterpolate, north, northwest, middle, west);
                }
                else
                {
                    // if the longitude is greater than the middle longitude, then the point is in the northeast quadrant
                    return BilinearRecursive(PointToInterpolate, northeast, north, east, middle);
                }
            }
        }
    }
}
