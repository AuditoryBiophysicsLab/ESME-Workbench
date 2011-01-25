using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Windows.Media;
using ESME.Overlay;
using HRC.Navigation;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.Environment
{
    public abstract class EnvironmentData
    {
        #region Public properties

        /// <summary>
        ///   List of latitudes (in degrees) for which we have values
        /// </summary>
        public double[] Latitudes { get; internal set; }

        /// <summary>
        ///   List of longitudes (in degrees) for which we have values
        /// </summary>
        public double[] Longitudes { get; internal set; }

        /// <summary>
        ///   Corner of the data set that has the minimum lat and lon values
        /// </summary>
        public EarthCoordinate MinCoordinate { get; internal set; }

        /// <summary>
        ///   Corner of the data set that has the maximum lat and lon values
        /// </summary>
        public EarthCoordinate MaxCoordinate { get; internal set; }

        public string Filename { get; internal set; }

        #endregion

        protected EnvironmentData()
        {
            MinCoordinate = null;
            MaxCoordinate = null;
            Latitudes = null;
            Longitudes = null;
        }

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

        public abstract void Save(BinaryWriter stream);
        public abstract void Load(BinaryReader stream);


        public bool ContainsCoordinate(EarthCoordinate coordinate) { return (MinCoordinate.Longitude_degrees <= coordinate.Longitude_degrees) && (coordinate.Longitude_degrees <= MaxCoordinate.Longitude_degrees) && (MinCoordinate.Latitude_degrees <= coordinate.Latitude_degrees) && (coordinate.Latitude_degrees <= MaxCoordinate.Latitude_degrees); }

        // lookup a value in a latitude or longitude array.  the array is presumed to be sorted in ascending order (lowest values first)
        // and if the value being sought is contained within the interval between index N and index N+1, then N will be returned.  if the
        // value is not found within the array in this fashion, -1 is returned.
        protected static int LookupIndex(double value, IList<double> array)
        {
            for (int index = 0; index < array.Count - 1; index++)
            {
                if ((array[index] <= value) && (value <= array[index + 1])) return index;
            }
            return -1; // value not found within the array
        }
    }

    public abstract class EnvironmentData<T> : EnvironmentData
    {
        #region Public properties

        /// <summary>
        ///   Values of all points in the data set
        /// </summary>
        public T[,] Values { get; internal set; }

        /// <summary>
        /// Look up a value in the current data set.
        /// </summary>
        /// <param name="coordinate">
        /// The coordinate to search the data set for
        /// </param>
        /// <param name="value">
        /// The value at the requested coordinate
        /// </param>
        /// <returns>
        /// If the requested coordinate is contained in the data set, the function return value is 'true', 'false' otherwise
        /// </returns>
        protected virtual bool Lookup(EarthCoordinate coordinate, ref T value)
        {
            if (ContainsCoordinate(coordinate))
            {
                int latIndex = LookupIndex(coordinate.Latitude_degrees, Latitudes);
                int lonIndex = LookupIndex(coordinate.Longitude_degrees, Longitudes);
                if ((latIndex >= 0) && (lonIndex >= 0))
                {
                    value = Values[latIndex, lonIndex];
                    return true;
                }
            }
            return false;
        }

        #endregion
    }

    public sealed class Environment2DData : EnvironmentData<float>
    {
        #region Public properties

        /// <summary>
        ///   Lowest value in the data set.
        /// </summary>
        public float MinValue { get; internal set; }

        /// <summary>
        ///   Highest value in the data set
        /// </summary>
        public float MaxValue { get; internal set; }

        #endregion

        const UInt32 Magic = 0x728dcde6;

        public static Environment2DData ReadChrtrBinaryFile(string fileName)
        {
            using (var stream = new BinaryReader(File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                float west = stream.ReadSingle();
                float east = stream.ReadSingle();
                float south = stream.ReadSingle();
                float north = stream.ReadSingle();
                float gridSpacing = stream.ReadSingle();
                int width = stream.ReadInt32();
                int height = stream.ReadInt32();
                uint endian = stream.ReadUInt32();
                if (endian != 0x00010203) throw new FileFormatException("Invalid CHRTR Binary file format - endian is incorrect");
                float minDepth = stream.ReadSingle();
                float maxDepth = stream.ReadSingle();
                int paddingWidth = (width - 10) * 4;
                byte[] padding = stream.ReadBytes(paddingWidth);
                var depths = new float[height, width];
                for (int lon = 0; lon < width; lon++)
                    for (int lat = 0; lat < height; lat++)
                    {
                        depths[lat, lon] = stream.ReadSingle();
                        if (depths[lat, lon] == 1e16f) depths[lat, lon] = float.NaN;
                    }
                return new Environment2DData(north, south, east, west, gridSpacing, depths, minDepth, maxDepth);
            }
        }

        public override void Load(BinaryReader stream)
        {
            if (stream.ReadUInt32() != Magic) throw new FormatException("Attempted to read invalid data into environment2DData");

            MinValue = stream.ReadSingle();
            MaxValue = stream.ReadSingle();

            MinCoordinate = new EarthCoordinate(stream);
            MaxCoordinate = new EarthCoordinate(stream);

            Longitudes = new double[stream.ReadInt32()];
            for (int lon = 0; lon < Longitudes.Length; lon++) Longitudes[lon] = stream.ReadDouble();

            Latitudes = new double[stream.ReadInt32()];
            for (int lat = 0; lat < Latitudes.Length; lat++) Latitudes[lat] = stream.ReadDouble();

            Values = new float[Latitudes.Length, Longitudes.Length];
            for (int lat = 0; lat < Latitudes.Length; lat++) for (int lon = 0; lon < Longitudes.Length; lon++) Values[lat, lon] = stream.ReadSingle();
        }

        public override void Save(BinaryWriter stream)
        {
            stream.Write(Magic);

            stream.Write(MinValue);
            stream.Write(MaxValue);

            MinCoordinate.Write(stream);
            MaxCoordinate.Write(stream);

            stream.Write(Longitudes.Length);
            foreach (double lon in Longitudes) stream.Write(lon);

            stream.Write(Latitudes.Length);
            foreach (double lat in Latitudes) stream.Write(lat);

            for (int lat = 0; lat < Latitudes.Length; lat++) for (int lon = 0; lon < Longitudes.Length; lon++) stream.Write(Values[lat, lon]);
        }

        /// <summary>
        /// Saves a data set to a text file in YXZ format.
        /// The output data file will be a three-column text file 
        /// LatitudeValue LongitudeValue ScaleFactor*DataValueAtLatLon
        /// </summary>
        /// <param name="fileName"></param>
        /// <param name="scaleFactor"></param>
        public void SaveToYXZ(string fileName, float scaleFactor)
        {
            using (var stream = new StreamWriter(File.Create(fileName)))
            {
                for (int lat = 0; lat < Latitudes.Length; lat++) for (int lon = 0; lon < Longitudes.Length; lon++) stream.WriteLine(string.Format("{0:##.######} {1:###.######} {2:#.###}", Latitudes[lat], Longitudes[lon], scaleFactor * Values[lat, lon]));
            }
        }

        /// <summary>
        /// Look up a value in the current data set.
        /// </summary>
        /// <param name="coordinate">
        /// The coordinate to search the data set for
        /// </param>
        /// <param name="value">
        /// The value at the requested coordinate
        /// </param>
        /// <returns>
        /// If the requested coordinate is contained in the data set, the function return value is 'true', 'false' otherwise
        /// </returns>
        public bool Lookup(EarthCoordinate coordinate, out float value)
        {
            value = float.NaN;
            return Lookup(coordinate, ref value);
        }

        #region Public constructors

        public Environment2DData(double north, double south, double east, double west, float gridSpacing, float[,] values, float minValue, float maxValue)
        {
            MinCoordinate = new EarthCoordinate(south, west);
            MaxCoordinate = new EarthCoordinate(north, east);
            MinValue = minValue;
            MaxValue = maxValue;
            Longitudes = new double[values.GetLength(0)];
            Latitudes = new double[values.GetLength(1)];
            for (int lon = 0; lon < Longitudes.Length; lon++) Longitudes[lon] = west + (lon * gridSpacing);
            for (int lat = 0; lat < Latitudes.Length; lat++) Latitudes[lat] = south + (lat * gridSpacing);
            Values = values;
        }

        public Environment2DData(string fileName, string layerName, float north, float west, float south, float east)
        {
            Filename = fileName;
            if (Path.GetExtension(fileName) != ".eeb") throw new System.IO.FileFormatException(string.Format("Environment2DData: Unknown file type \"{0}\"", Path.GetFileName(fileName)));
            DataFile file = DataFile.Open(fileName);

            DataLayer layer = file[layerName];
            if (layer == null) throw new System.IO.FileFormatException(string.Format("Environment2DData: Specified environment file \"{0}\"does not contain a environment2DData layer", fileName));

            Longitudes = layer.LongitudeAxis.DoubleValuesBetween(west, east);
            Latitudes = layer.LatitudeAxis.DoubleValuesBetween(south, north);
            int[] lonIndices = layer.LongitudeAxis.IndicesBetween(west, east);
            int[] latIndices = layer.LatitudeAxis.IndicesBetween(south, north);

            MinCoordinate = new EarthCoordinate(Latitudes[0], Longitudes[0]);
            MaxCoordinate = new EarthCoordinate(Latitudes[Latitudes.Length - 1], Longitudes[Longitudes.Length - 1]);
            MinValue = float.MaxValue;
            MaxValue = float.MinValue;
            Values = layer.Get2DData(latIndices[0], latIndices[latIndices.Length - 1], lonIndices[0], lonIndices[lonIndices.Length - 1]);
            for (int row = 0; row < Values.GetLength(0); row++)
                for (int col = 0; col < Values.GetLength(1); col++)
                {
                    MinValue = Math.Min(MinValue, Values[row, col]);
                    MaxValue = Math.Max(MaxValue, Values[row, col]);
                }
        }

        public Environment2DData(string fileName)
        {
            Filename = fileName;
            if (Path.GetExtension(fileName) == ".eeb")
            {
                //float[, ,] array;
                //float curValue;
                DataFile file = DataFile.Open(fileName);
                foreach (DataLayer layer in file.Layers)
                {
                    if (layer.Name != "environment2DData") continue;
                    Latitudes = layer.LatitudeAxis.UnwrappedValues;
                    Longitudes = layer.LongitudeAxis.UnwrappedValues;
                    //array = layer.DataArray.Data;

                    MinCoordinate = new EarthCoordinate(Latitudes[0], Longitudes[0]);
                    MaxCoordinate = new EarthCoordinate(Latitudes[Latitudes.Length - 1], Longitudes[Longitudes.Length - 1]);

                    MinValue = float.MaxValue;
                    MaxValue = float.MinValue;

                    Values = layer.Get2DData(0, layer.RowCount - 1, layer.RowCount, 0, layer.ColumnCount - 1, layer.ColumnCount);
                    for (int row = 0; row < Values.GetLength(0); row++)
                        for (int col = 0; col < Values.GetLength(1); col++)
                        {
                            MinValue = Math.Min(MinValue, Values[row, col]);
                            MaxValue = Math.Max(MaxValue, Values[row, col]);
                        }
                }
            }
            else
            {
                using (var stream = new BinaryReader(new FileStream(fileName, FileMode.Open)))
                {
                    Load(stream);
                }
            }
            Filename = fileName;
        }

        #endregion
    }

    public abstract class Environment3DData<T> : EnvironmentData<List<T>>
    {
        #region Public properties

        /// <summary>
        ///   List of Depths (in meters) for which we have values
        /// </summary>
        public double[] Depths { get; internal set; }

        #endregion

        #region Public constructors

        protected Environment3DData(double north, double south, double east, double west, float gridSpacing, IList<float> depths, List<T>[,] values)
        {
            MinCoordinate = new EarthCoordinate(south, west);
            MaxCoordinate = new EarthCoordinate(north, east);
            Longitudes = new double[values.GetLength(0)];
            Latitudes = new double[values.GetLength(1)];
            Depths = new double[depths.Count];
            for (int lon = 0; lon < Longitudes.Length; lon++) Longitudes[lon] = west + (lon * gridSpacing);
            for (int lat = 0; lat < Latitudes.Length; lat++) Latitudes[lat] = south + (lat * gridSpacing);
            for (int dep = 0; dep < Depths.Length; dep++) Depths[dep] = depths[dep];
            Values = values;
        }

        #endregion

    }
    public class AverageDatum
    {
        public float Value { get; set; }
        public int Count { get; set; }

        public void Add(float newValue)
        {
            Value += newValue;
            Count++;
        }

        public void Average()
        {
            if (Count > 0) Value /= Count;
        }

        public AverageDatum()
        {
            Value = 0;
            Count = 0;
        }

        public AverageDatum(float value)
        {
            Value = value;
            Count = 1;
        }
    }

    public sealed class Environment3DAverager : Environment3DData<AverageDatum>
    {
        public Environment3DAverager(double north, double south, double east, double west, float gridSpacing, IList<float> depths, List<AverageDatum>[,] values)
            : base(north, south, east, west, gridSpacing, depths, values)
        {
        }

        internal void Add(Environment3DData that)
        {
            // Verify that this.Latitudes[] that.Latitudes[] (same with Longitudes[]) are the same length AND have the same contents
            // use VerifyArrays, below
            // Same thing with Depths[]
            // Loop through all the Values[,] and make sure that Count is equal to the corresponding Count of the 'that' data
            // If all checks pass, then just loop through Values[,] as above and add (+=) the corresponding Values from 'that'

            VerifyArrays(Latitudes, that.Latitudes, "latitude");
            VerifyArrays(Longitudes, that.Longitudes, "longitude");
            // Make sure the Depths array is copied from the longest Depths array we are presented with
            if (Depths.Length < that.Depths.Length) Array.Copy(that.Depths, Depths, that.Depths.Length);
            //VerifyArrays(Depths, that.Depths, "depth");)

            for (int lonIndex = 0; lonIndex < Values.GetLength(0); lonIndex++)
                for (int latIndex = 0; latIndex < Values.GetLength(1); latIndex++)
                {
                    if (Values[lonIndex, latIndex] == null)
                        Values[lonIndex, latIndex] = new List<AverageDatum>();
                    for (int depthIndex = 0; depthIndex < that.Values[lonIndex, latIndex].Count; depthIndex++)
                    {
                        var thatValue = that.Values[lonIndex, latIndex][depthIndex];
                        if (Values[lonIndex, latIndex].Count <= depthIndex)
                            Values[lonIndex, latIndex].Add(new AverageDatum(thatValue));
                        else Values[lonIndex, latIndex][depthIndex].Add(thatValue);
                    }
                }
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <param name="name"></param>
        /// <returns></returns>
        static void VerifyArrays(double[] left, double[] right, string name)
        {
            if (left.Length != right.Length) throw new ApplicationException(name + " array length mismatch");
            if (left.Where((t, i) => t != right[i]).Any()) throw new ApplicationException(name + " array element value mismatch");
        }

        internal void Average()
        {
            for (int lonIndex = 0; lonIndex < Values.GetUpperBound(0); lonIndex++)
                for (int latIndex = 0; latIndex < Values.GetUpperBound(1); latIndex++)
                {
                    for (int depthIndex = 0; depthIndex < Values[lonIndex, latIndex].Count; depthIndex++)
                    {
                        Values[lonIndex, latIndex][depthIndex].Average();
                    }
                }
        }

        public override void Save(BinaryWriter stream) { throw new NotImplementedException(); }
        public override void Load(BinaryReader stream) { throw new NotImplementedException(); }
    }

    public sealed class Environment3DData : Environment3DData<float>
    {

        const UInt32 Magic = 0x3d8dcde6;

        public override void Load(BinaryReader stream)
        {
            if (stream.ReadUInt32() != Magic) throw new FormatException("Attempted to read invalid data into environment2DData");

            MinCoordinate = new EarthCoordinate(stream);
            MaxCoordinate = new EarthCoordinate(stream);

            Longitudes = new double[stream.ReadInt32()];
            for (int lon = 0; lon < Longitudes.Length; lon++) Longitudes[lon] = stream.ReadDouble();

            Latitudes = new double[stream.ReadInt32()];
            for (int lat = 0; lat < Latitudes.Length; lat++) Latitudes[lat] = stream.ReadDouble();

            Depths = new double[stream.ReadInt32()];
            for (int dep = 0; dep < Depths.Length; dep++) Depths[dep] = stream.ReadDouble();

            for (int lat = 0; lat < Latitudes.Length; lat++)
                for (int lon = 0; lon < Longitudes.Length; lon++)
                {
                    var curData = Depths.Select(t => stream.ReadSingle()).Where(curValue => !float.IsNaN(curValue)).ToList();
                    Values[lat, lon] = curData;
                }
        }

        public override void Save(BinaryWriter stream)
        {
            stream.Write(Magic);

            MinCoordinate.Write(stream);
            MaxCoordinate.Write(stream);

            stream.Write(Longitudes.Length);
            foreach (double lon in Longitudes) stream.Write(lon);

            stream.Write(Latitudes.Length);
            foreach (double lat in Latitudes) stream.Write(lat);

            stream.Write(Depths.Length);
            foreach (double dep in Depths) stream.Write(dep);

            for (int lat = 0; lat < Latitudes.Length; lat++)
                for (int lon = 0; lon < Longitudes.Length; lon++)
                {
                    List<float> curValue = Values[lat, lon];
                    for (int dep = 0; dep < Depths.Length; dep++) stream.Write(curValue.Count < dep ? curValue[dep] : float.NaN);
                }
        }

        /// <summary>
        /// Look up a value in the current data set.
        /// </summary>
        /// <param name="coordinate">
        /// The coordinate to search the data set for
        /// </param>
        /// <param name="value">
        /// The value at the requested coordinate
        /// </param>
        /// <returns>
        /// If the requested coordinate is contained in the data set, the function return value is 'true', 'false' otherwise
        /// </returns>
        public bool Lookup(EarthCoordinate coordinate, out List<float> value)
        {
            value = null;
            return Lookup(coordinate, ref value);
        }


        #region Public constructors

        public Environment3DData(double north, double south, double east, double west, float gridSpacing, IList<float> depths, List<float>[,] values)
            : base(north, south, east, west, gridSpacing, depths, values)
        {
        }

        #endregion
    }
}