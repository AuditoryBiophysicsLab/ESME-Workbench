using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Windows.Media;
using ESME.Overlay;
using HRC.Navigation;

namespace ESME.Environment
{
    public class Bathymetry
    {
        #region Public properties

        /// <summary>
        ///   Elevations of all points in the bathymetry, in meters.  Positive values are ABOVE sea level
        /// </summary>
        public float[,] Elevations { get; internal set; }

        /// <summary>
        ///   List of latitudes (in degrees) for which we have elevation points
        /// </summary>
        public double[] Latitudes { get; internal set; }

        /// <summary>
        ///   List of longitudes (in degrees) for which we have elevation points
        /// </summary>
        public double[] Longitudes { get; internal set; }

        /// <summary>
        ///   Lowest elevation in the bathymetry.  Usually below sea level (negative).
        /// </summary>
        public float MinElevation { get; internal set; }

        /// <summary>
        ///   Highest elevation in the bathymetry.  May be above sea level (positive).
        /// </summary>
        public float MaxElevation { get; internal set; }

        /// <summary>
        ///   Corner of the bathymetry that has the minimum lat and lon values
        /// </summary>
        public EarthCoordinate MinCoordinate { get; internal set; }

        /// <summary>
        ///   Corner of the bathymetry that has the maximum lat and lon values
        /// </summary>
        public EarthCoordinate MaxCoordinate { get; internal set; }

        public string Filename { get; internal set; }

        #endregion

        const UInt32 Magic = 0x728dcde6;

        Bathymetry()
        {
            MinCoordinate = null;
            MaxCoordinate = null;
            MaxElevation = 0;
            MinElevation = 0;
            Latitudes = null;
            Longitudes = null;
            Elevations = null;
        }

        #region Public constructors

        public Bathymetry(string filename, float[,] elevations, double[] latitudes, double[] longitudes, float minElevation, float maxElevation) : this()
        {
            Filename = filename;

            MinCoordinate = new EarthCoordinate(latitudes[0], Longitudes[0]);
            MaxCoordinate = new EarthCoordinate(latitudes[latitudes.Length - 1], longitudes[longitudes.Length - 1]);
        }

        public Bathymetry(string filename, float north, float west, float south, float east)
        {
            Filename = filename;
            if (Path.GetExtension(filename) != ".eeb") throw new FileFormatException(string.Format("Bathymetry: Unknown file type \"{0}\"", Path.GetFileName(filename)));
            var file = DataFile.Open(filename);

            var layer = file["bathymetry"];
            if (layer == null) throw new FileFormatException(string.Format("Bathymetry: Specified environment file \"{0}\"does not contain a bathymetry layer", filename));

            Longitudes = layer.LongitudeAxis.DoubleValuesBetween(west, east);
            Latitudes = layer.LatitudeAxis.DoubleValuesBetween(south, north);
            var lonIndices = layer.LongitudeAxis.IndicesBetween(west, east);
            var latIndices = layer.LatitudeAxis.IndicesBetween(south, north);

            MinCoordinate = new EarthCoordinate(Latitudes[0], Longitudes[0]);
            MaxCoordinate = new EarthCoordinate(Latitudes[Latitudes.Length - 1], Longitudes[Longitudes.Length - 1]);
            MinElevation = float.MaxValue;
            MaxElevation = float.MinValue;
            Elevations = layer.Get2DData(latIndices[0], latIndices[latIndices.Length - 1], lonIndices[0], lonIndices[lonIndices.Length - 1]);
            for (var row = 0; row < Elevations.GetLength(0); row++)
                for (var col = 0; col < Elevations.GetLength(1); col++)
                {
                    MinElevation = Math.Min(MinElevation, Elevations[row, col]);
                    MaxElevation = Math.Max(MaxElevation, Elevations[row, col]);
                }
        }

        public Bathymetry(string filename) : this()
        {
            Filename = filename;
            if (Path.GetExtension(filename) == ".eeb")
            {
                //float[, ,] array;
                //float curValue;
                var file = DataFile.Open(filename);
                foreach (var layer in file.Layers)
                {
                    if (layer.Name != "bathymetry") continue;
                    Latitudes = layer.LatitudeAxis.UnwrappedValues;
                    Longitudes = layer.LongitudeAxis.UnwrappedValues;
                    //array = layer.DataArray.Data;

                    MinCoordinate = new EarthCoordinate(Latitudes[0], Longitudes[0]);
                    MaxCoordinate = new EarthCoordinate(Latitudes[Latitudes.Length - 1], Longitudes[Longitudes.Length - 1]);

                    MinElevation = float.MaxValue;
                    MaxElevation = float.MinValue;

                    Elevations = layer.Get2DData(0, layer.RowCount - 1, layer.RowCount, 0, layer.ColumnCount - 1, layer.ColumnCount);
                    for (var row = 0; row < Elevations.GetLength(0); row++)
                        for (var col = 0; col < Elevations.GetLength(1); col++)
                        {
                            MinElevation = Math.Min(MinElevation, Elevations[row, col]);
                            MaxElevation = Math.Max(MaxElevation, Elevations[row, col]);
                        }
                }
            }
            else
            {
                using (var stream = new BinaryReader(new FileStream(filename, FileMode.Open)))
                {
                    Load(stream);
                }
            }
            Filename = filename;
        }

        #endregion

        public OverlayLineSegments BoundingBox
        {
            get
            {
                var bathyBox = new[]
                               {
                                   //northeast corner:                   
                                   new EarthCoordinate(MaxCoordinate.Latitude_degrees, MaxCoordinate.Longitude_degrees), //southeast corner: 
                                   new EarthCoordinate(MinCoordinate.Latitude_degrees, MaxCoordinate.Longitude_degrees), //southwest corner: 
                                   new EarthCoordinate(MinCoordinate.Latitude_degrees, MinCoordinate.Longitude_degrees), //northwest corner: 
                                   new EarthCoordinate(MaxCoordinate.Latitude_degrees, MinCoordinate.Longitude_degrees), //northeast corner again to close the loop.
                                   new EarthCoordinate(MaxCoordinate.Latitude_degrees, MaxCoordinate.Longitude_degrees),
                               };

                var shape = new OverlayLineSegments(bathyBox, Colors.Black, 1, LineStyle.Solid);
                return shape;
            }
        }

        public void Save(string filename)
        {
            using (var stream = new BinaryWriter(new FileStream(filename, FileMode.Create)))
            {
                Save(stream);
            }
        }

        public void Load(BinaryReader stream)
        {
            int lat,
                lon;

            if (stream.ReadUInt32() != Magic) throw new FormatException("Attempted to read invalid data into Bathymetry");

            MinElevation = stream.ReadSingle();
            MaxElevation = stream.ReadSingle();

            MinCoordinate = new EarthCoordinate(stream);
            MaxCoordinate = new EarthCoordinate(stream);

            Longitudes = new double[stream.ReadInt32()];
            for (lon = 0; lon < Longitudes.Length; lon++) Longitudes[lon] = stream.ReadDouble();

            Latitudes = new double[stream.ReadInt32()];
            for (lat = 0; lat < Latitudes.Length; lat++) Latitudes[lat] = stream.ReadDouble();

            Elevations = new float[Latitudes.Length,Longitudes.Length];
            for (lat = 0; lat < Latitudes.Length; lat++) for (lon = 0; lon < Longitudes.Length; lon++) Elevations[lat, lon] = stream.ReadSingle();
        }

        public void Save(BinaryWriter stream)
        {
            stream.Write(Magic);

            stream.Write(MinElevation);
            stream.Write(MaxElevation);

            MinCoordinate.Write(stream);
            MaxCoordinate.Write(stream);

            stream.Write(Longitudes.Length);
            foreach (var lon in Longitudes) stream.Write(lon);

            stream.Write(Latitudes.Length);
            foreach (var lat in Latitudes) stream.Write(lat);

            for (var lat = 0; lat < Latitudes.Length; lat++) for (var lon = 0; lon < Longitudes.Length; lon++) stream.Write(Elevations[lat, lon]);
        }

        public bool ContainsCoordinate(EarthCoordinate coordinate) { return (MinCoordinate.Longitude_degrees <= coordinate.Longitude_degrees) && (coordinate.Longitude_degrees <= MaxCoordinate.Longitude_degrees) && (MinCoordinate.Latitude_degrees <= coordinate.Latitude_degrees) && (coordinate.Latitude_degrees <= MaxCoordinate.Latitude_degrees); }

        // lookup a coordinate in the current bathymetry dataset.
        // The elevation at the requested coordinate is returned in the 'out' parameter Elevation
        // If the requested coordinate is contained in the bathymetry dataset, the function return value is 'true', 'false' otherwise
        public bool LookupElevation(EarthCoordinate3D coordinate, out float elevation)
        {
            if (ContainsCoordinate(coordinate))
            {
                var latIndex = LookupIndex(coordinate.Latitude_degrees, Latitudes);
                var longIndex = LookupIndex(coordinate.Longitude_degrees, Longitudes);
                if ((latIndex >= 0) && (longIndex >= 0))
                {
                    elevation = Elevations[latIndex, longIndex];
                    return true;
                }
            }
            elevation = float.NaN;
            return false;
        }


        // lookup a coordinate in the current bathymetry dataset.
        // The elevation at the requested coordinate is returned in the 'out' parameter Elevation
        // If the requested coordinate is contained in the bathymetry dataset, the function return value is 'true', 'false' otherwise
        public bool LookupElevation(EarthCoordinate coordinate, out float elevation)
        {
            if (ContainsCoordinate(coordinate))
            {
                var latIndex = LookupIndex(coordinate.Latitude_degrees, Latitudes);
                var longIndex = LookupIndex(coordinate.Longitude_degrees, Longitudes);
                if ((latIndex >= 0) && (longIndex >= 0))
                {
                    elevation = Elevations[latIndex, longIndex];
                    return true;
                }
            }
            elevation = float.NaN;
            return false;
        }

        // lookup a value in a latitude or longitude array.  the array is presumed to be sorted in ascending order (lowest values first)
        // and if the value being sought is contained within the interval between index N and index N+1, then N will be returned.  if the
        // value is not found within the array in this fashion, -1 is returned.
        static int LookupIndex(double value, IList<double> array)
        {
            for (var index = 0; index < array.Count - 1; index++)
            {
                if ((array[index] <= value) && (value <= array[index + 1])) return index;
            }
            return -1; // value not found within the array
        }
    }
}