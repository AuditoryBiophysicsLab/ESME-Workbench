using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.IO;
using ESME.Overlay;
using HRC.Navigation;

namespace ESME.Environment
{
    public class Bathymetry
    {
        #region Public properties
        /// <summary>
        /// Elevations of all points in the bathymetry, in meters.  Positive values are ABOVE sea level
        /// </summary>
        public float[,] Elevations_meters { get; internal set; }

        /// <summary>
        /// List of latitudes for which we have elevation points
        /// </summary>
        public double[] Latitudes_degrees { get; internal set; }

        /// <summary>
        /// List of longitudes for which we have elevation points
        /// </summary>
        public double[] Longitudes_degrees { get; internal set; }

        /// <summary>
        /// Lowest elevation in the bathymetry.  Usually below sea level (negative).
        /// </summary>
        public float MinElevation_meters { get; internal set; }

        /// <summary>
        /// Highest elevation in the bathymetry.  May be above sea level (positive).
        /// </summary>
        public float MaxElevation_meters { get; internal set; }

        /// <summary>
        /// Corner of the bathymetry that has the minimum lat and lon values
        /// </summary>
        public EarthCoordinate MinCoordinate { get; internal set; }

        /// <summary>
        /// Corner of the bathymetry that has the maximum lat and lon values
        /// </summary>
        public EarthCoordinate MaxCoordinate { get; internal set; }

        public string Filename { get; internal set; }
        #endregion

        private readonly static UInt32 Magic = 0x728dcde6;

        #region Public constructors
        public Bathymetry(string Filename, float[,] Elevations_meters, double[] Latitudes_degrees, double[] Longitudes_degrees, float MinElevation_meters, float MaxElevation_meters)
        {
            this.Elevations_meters = Elevations_meters;
            this.Latitudes_degrees = Latitudes_degrees;
            this.Longitudes_degrees = Longitudes_degrees;
            this.MinElevation_meters = MinElevation_meters;
            this.MaxElevation_meters = MaxElevation_meters;
            this.Filename = Filename;

            MinCoordinate = new EarthCoordinate(Latitudes_degrees[0], this.Longitudes_degrees[0]);
            MaxCoordinate = new EarthCoordinate(Latitudes_degrees[Latitudes_degrees.Length - 1], Longitudes_degrees[Longitudes_degrees.Length - 1]);
        }

        public Bathymetry(string Filename)
        {
            MinCoordinate = null;
            MaxCoordinate = null;
            MaxElevation_meters = 0;
            MinElevation_meters = 0;
            Latitudes_degrees = null;
            Longitudes_degrees = null;
            Elevations_meters = null;
            this.Filename = Filename;
            if (Path.GetExtension(Filename) == ".eeb")
            {
                //float[, ,] array;
                //float curValue;
                DataFile file = DataFile.Open(Filename);
                foreach (ESME.Environment.DataLayer layer in file.Layers)
                {
                    if (layer.Name == "bathymetry")
                    {
                        Latitudes_degrees = layer.LatitudeAxis.UnwrappedValues;
                        Longitudes_degrees = layer.LongitudeAxis.UnwrappedValues;
                        //array = layer.DataArray.Data;

                        MinCoordinate = new EarthCoordinate(Latitudes_degrees[0], Longitudes_degrees[0]);
                        MaxCoordinate = new EarthCoordinate(Latitudes_degrees[Latitudes_degrees.Length - 1], Longitudes_degrees[Longitudes_degrees.Length - 1]);

                        MinElevation_meters = float.MaxValue;
                        MaxElevation_meters = float.MinValue;

                        Elevations_meters = layer.Get2DData(0, layer.RowCount - 1, layer.RowCount,
                            0, layer.ColumnCount - 1, layer.ColumnCount);
                        for (int row = 0; row < Elevations_meters.GetLength(0); row++)
                            for (int col = 0; col < Elevations_meters.GetLength(1); col++)
                            {
                                MinElevation_meters = Math.Min(MinElevation_meters, Elevations_meters[row, col]);
                                MaxElevation_meters = Math.Max(MaxElevation_meters, Elevations_meters[row, col]);
                            }

#if false
                        Elevations_meters = new float[Latitudes_degrees.Length, Longitudes_degrees.Length];
                        foreach (DataRow row in layer.Rows)
                            foreach (DataPoint point in row.Points)
                            {
                                curValue = point.Data[0];
                                MinElevation_meters = Math.Min(MinElevation_meters, curValue);
                                MaxElevation_meters = Math.Max(MaxElevation_meters, curValue);
                                Elevations_meters[point.RowIndex, point.ColumnIndex] = curValue;
                            }
#endif
#if false
                        for (lat = 0; lat < Latitudes_degrees.Length; lat++)
                        {
                            for (lon = 0; lon < Longitudes_degrees.Length; lon++)
                            {
                                curValue = array[lon, lat, 0];
                                MinElevation_meters = Math.Min(MinElevation_meters, curValue);
                                MaxElevation_meters = Math.Max(MaxElevation_meters, curValue);
                                Elevations_meters[lat, lon] = curValue;
                            }
                        }
#endif
                    }
                }
            }
            else
            {
                using (BinaryReader stream = new BinaryReader(new FileStream(Filename, FileMode.Open)))
                {
                    Load(stream);
                }
            }
            this.Filename = Filename;
        }
        #endregion

        public void Save(string Filename)
        {
            using (BinaryWriter stream = new BinaryWriter(new FileStream(Filename, FileMode.Create)))
            {
                Save(stream);
            }
        }

        public void Load(BinaryReader stream)
        {
            int lat, lon;

            if (stream.ReadUInt32() != Magic)
                throw new FormatException("Attempted to read invalid data into Bathymetry");

            MinElevation_meters = stream.ReadSingle();
            MaxElevation_meters = stream.ReadSingle();

            MinCoordinate = new EarthCoordinate(stream);
            MaxCoordinate = new EarthCoordinate(stream);

            Longitudes_degrees = new double[stream.ReadInt32()];
            for (lon = 0; lon < Longitudes_degrees.Length; lon++)
                Longitudes_degrees[lon] = stream.ReadDouble();

            Latitudes_degrees = new double[stream.ReadInt32()];
            for (lat = 0; lat < Latitudes_degrees.Length; lat++)
                Latitudes_degrees[lat] = stream.ReadDouble();

            Elevations_meters = new float[Latitudes_degrees.Length, Longitudes_degrees.Length];
            for (lat = 0; lat < Latitudes_degrees.Length; lat++)
                for (lon = 0; lon < Longitudes_degrees.Length; lon++)
                    Elevations_meters[lat, lon] = stream.ReadSingle();
        }

        public void Save(BinaryWriter stream)
        {
            stream.Write(Magic);

            stream.Write(MinElevation_meters);
            stream.Write(MaxElevation_meters);

            MinCoordinate.Write(stream);
            MaxCoordinate.Write(stream);

            stream.Write(Longitudes_degrees.Length);
            foreach (double lon in Longitudes_degrees)
                stream.Write(lon);

            stream.Write(Latitudes_degrees.Length);
            foreach (double lat in Latitudes_degrees)
                stream.Write(lat);

            for (int lat = 0; lat < Latitudes_degrees.Length; lat++)
                for (int lon = 0; lon < Longitudes_degrees.Length; lon++)
                    stream.Write(Elevations_meters[lat, lon]);
        }

        public bool CheckFile(string Filename)
        {
            using (BinaryReader stream = new BinaryReader(new FileStream(Filename, FileMode.Open)))
            {
                if (stream.ReadUInt32() != Magic)
                    return false;
            }
            return true;
        }

        public bool ContainsCoordinate(EarthCoordinate Coordinate)
        {
            if ((MinCoordinate.Longitude_degrees <= Coordinate.Longitude_degrees) && (Coordinate.Longitude_degrees <= MaxCoordinate.Longitude_degrees) &&
                (MinCoordinate.Latitude_degrees <= Coordinate.Latitude_degrees) && (Coordinate.Latitude_degrees <= MaxCoordinate.Latitude_degrees))
                return true;
            else
                return false;
        }

        public OverlayLineSegments BoundingBox
        {
            get
            {
                var bathyBox = new[]
                                   {
                                       //northeast corner:                   
                                       new EarthCoordinate(MaxCoordinate.Latitude_degrees,
                                                           MaxCoordinate.Longitude_degrees),
                                       //southeast corner: 
                                       new EarthCoordinate(MinCoordinate.Latitude_degrees,
                                                           MaxCoordinate.Longitude_degrees),
                                       //southwest corner: 
                                       new EarthCoordinate(MinCoordinate.Latitude_degrees,
                                                           MinCoordinate.Longitude_degrees),
                                       //northwest corner: 
                                       new EarthCoordinate(MaxCoordinate.Latitude_degrees,
                                                           MinCoordinate.Longitude_degrees),
                                       //northeast corner again to close the loop.
                                       new EarthCoordinate(MaxCoordinate.Latitude_degrees,
                                                           MaxCoordinate.Longitude_degrees),
                                   };

                var shape = new OverlayLineSegments(bathyBox, Color.Black, 1, LineStyle.Solid);
                return shape;
            }
        }

        // lookup a coordinate in the current bathymetry dataset.
        // The elevation at the requested coordinate is returned in the 'out' parameter Elevation
        // If the requested coordinate is contained in the bathymetry dataset, the function return value is 'true', 'false' otherwise
        public bool LookupElevation(EarthCoordinate3D Coordinate, out float Elevation)
        {
            int latIndex, longIndex;

            if (ContainsCoordinate(Coordinate))
            {
                latIndex = LookupIndex(Coordinate.Latitude_degrees, Latitudes_degrees);
                longIndex = LookupIndex(Coordinate.Longitude_degrees, Longitudes_degrees);
                if ((latIndex >= 0) && (longIndex >= 0))
                {
                    Elevation = Elevations_meters[latIndex, longIndex];
                    return true;
                }
            }
            Elevation = float.NaN;
            return false;
        }


        // lookup a coordinate in the current bathymetry dataset.
        // The elevation at the requested coordinate is returned in the 'out' parameter Elevation
        // If the requested coordinate is contained in the bathymetry dataset, the function return value is 'true', 'false' otherwise
        public bool LookupElevation(EarthCoordinate Coordinate, out float Elevation)
        {
            int latIndex, longIndex;

            if (ContainsCoordinate(Coordinate))
            {
                latIndex = LookupIndex(Coordinate.Latitude_degrees, Latitudes_degrees);
                longIndex = LookupIndex(Coordinate.Longitude_degrees, Longitudes_degrees);
                if ((latIndex >= 0) && (longIndex >= 0))
                {
                    Elevation = Elevations_meters[latIndex, longIndex];
                    return true;
                }
            }
            Elevation = float.NaN;
            return false;
        }

        // lookup a value in a latitude or longitude array.  the array is presumed to be sorted in ascending order (lowest values first)
        // and if the value being sought is contained within the interval between index N and index N+1, then N will be returned.  if the
        // value is not found within the array in this fashion, -1 is returned.
        private int LookupIndex(double value, double[] array)
        {
            for (int index = 0; index < array.Length - 1; index++)
            {
                if ((array[index] <= value) && (value <= array[index + 1]))
                    return index;
            }
            return -1; // value not found within the array
        }
    }

    public class BathymetryOutOfBoundsException : Exception
    {
        public BathymetryOutOfBoundsException(string Message) : base(Message) { }
    }

}
