using System;
using System.Collections.Generic;
using System.IO;
using System.Windows.Media;
using ESME.Overlay;
using HRC.Navigation;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.Environment
{
    public class Environment2DData
    {
        #region Public properties

        /// <summary>
        ///   Values of all points in the data set
        /// </summary>
        public float[,] Values { get; internal set; }

        /// <summary>
        ///   List of latitudes (in degrees) for which we have values
        /// </summary>
        public double[] Latitudes { get; internal set; }

        /// <summary>
        ///   List of longitudes (in degrees) for which we have values
        /// </summary>
        public double[] Longitudes { get; internal set; }

        /// <summary>
        ///   Lowest value in the data set.
        /// </summary>
        public float MinValue { get; internal set; }

        /// <summary>
        ///   Highest value in the data set
        /// </summary>
        public float MaxValue { get; internal set; }

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

        const UInt32 Magic = 0x728dcde6;

        Environment2DData()
        {
            MinCoordinate = null;
            MaxCoordinate = null;
            MaxValue = 0;
            MinValue = 0;
            Latitudes = null;
            Longitudes = null;
            Values = null;
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
                int paddingWidth = (width - 10)*4;
                byte[] padding = stream.ReadBytes(paddingWidth);
                var depths = new float[height,width];
                for (int lon = 0; lon < width; lon++)
                    for (int lat = 0; lat < height; lat++)
                    {
                        depths[lat, lon] = stream.ReadSingle();
                        if (depths[lat, lon] == 1e16f) depths[lat, lon] = float.NaN;
                    }
                return new Environment2DData(north, south, east, west, gridSpacing, depths, minDepth, maxDepth);
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

            if (stream.ReadUInt32() != Magic) throw new FormatException("Attempted to read invalid data into environment2DData");

            MinValue = stream.ReadSingle();
            MaxValue = stream.ReadSingle();

            MinCoordinate = new EarthCoordinate(stream);
            MaxCoordinate = new EarthCoordinate(stream);

            Longitudes = new double[stream.ReadInt32()];
            for (lon = 0; lon < Longitudes.Length; lon++) Longitudes[lon] = stream.ReadDouble();

            Latitudes = new double[stream.ReadInt32()];
            for (lat = 0; lat < Latitudes.Length; lat++) Latitudes[lat] = stream.ReadDouble();

            Values = new float[Latitudes.Length,Longitudes.Length];
            for (lat = 0; lat < Latitudes.Length; lat++) for (lon = 0; lon < Longitudes.Length; lon++) Values[lat, lon] = stream.ReadSingle();
        }

        public void Save(BinaryWriter stream)
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
                for (int lat = 0; lat < Latitudes.Length; lat++) for (int lon = 0; lon < Longitudes.Length; lon++) stream.WriteLine(string.Format("{0:##.######} {1:###.######} {2:#.###}", Latitudes[lat], Longitudes[lon], scaleFactor*Values[lat, lon]));
            }
        }

        public bool ContainsCoordinate(EarthCoordinate coordinate) { return (MinCoordinate.Longitude_degrees <= coordinate.Longitude_degrees) && (coordinate.Longitude_degrees <= MaxCoordinate.Longitude_degrees) && (MinCoordinate.Latitude_degrees <= coordinate.Latitude_degrees) && (coordinate.Latitude_degrees <= MaxCoordinate.Latitude_degrees); }

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
        public bool Lookup(EarthCoordinate3D coordinate, out float value)
        {
            if (ContainsCoordinate(coordinate))
            {
                int latIndex = LookupIndex(coordinate.Latitude_degrees, Latitudes);
                int longIndex = LookupIndex(coordinate.Longitude_degrees, Longitudes);
                if ((latIndex >= 0) && (longIndex >= 0))
                {
                    value = Values[latIndex, longIndex];
                    return true;
                }
            }
            value = float.NaN;
            return false;
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
            if (ContainsCoordinate(coordinate))
            {
                int latIndex = LookupIndex(coordinate.Latitude_degrees, Latitudes);
                int longIndex = LookupIndex(coordinate.Longitude_degrees, Longitudes);
                if ((latIndex >= 0) && (longIndex >= 0))
                {
                    value = Values[latIndex, longIndex];
                    return true;
                }
            }
            value = float.NaN;
            return false;
        }

        // lookup a value in a latitude or longitude array.  the array is presumed to be sorted in ascending order (lowest values first)
        // and if the value being sought is contained within the interval between index N and index N+1, then N will be returned.  if the
        // value is not found within the array in this fashion, -1 is returned.
        static int LookupIndex(double value, IList<double> array)
        {
            for (int index = 0; index < array.Count - 1; index++)
            {
                if ((array[index] <= value) && (value <= array[index + 1])) return index;
            }
            return -1; // value not found within the array
        }

#if false
        internal static Environment2DData ReadSMGCFile(string fileName)
        {//INCOMPLETE
            using (var stream = new BinaryReader(File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                var checkBit1 = stream.ReadSingle();
                var checkBit2 = stream.ReadSingle();
                if ((checkBit1 != 99.0) || (checkBit2 != 380.0)) throw new FileFormatException("Invalid SMGC File check bits.");
                var count = 0;
                var reclen = new List<int>();
                var sampleSize = new List<int>();
                var min = new List<int>();
                var max = new List<int>();
                var mean = new List<int>();
                var std = new List<int>();
                var median = new List<int>();
                var mode = new List<int>();
                var h = new List<int[]>();
                while (stream.BaseStream.Position != stream.BaseStream.Length)
                {
                    reclen.Add(stream.ReadInt32()); //add the record length
                    if (reclen[count] != 0)
                    {
                        sampleSize.Add(stream.ReadInt32());
                        min.Add(stream.ReadInt32());
                        max.Add(stream.ReadInt32());
                        mean.Add(stream.ReadInt32());
                        std.Add(stream.ReadInt32());
                        median.Add(stream.ReadInt32());
                        mode.Add(stream.ReadInt32());
                        var hh = new int[(reclen[count] / 4) - 7];
                        for (var i = 0; i < hh.Length; i++)
                        {
                            hh[i] = stream.ReadInt32();
                        }
                        h.Add(hh);
                    }
                    count++;
                }
                //now convert each one to its correct precision by multiplying to get a float.  or something. 

                //return new Environment2DData(north, south, east, west, gridSpacing, depths, minDepth, maxDepth);
            }
        } 
#endif
       
        #region Public constructors

        public Environment2DData(double north, double south, double east, double west, float gridSpacing, float[,] values, float minValue, float maxValue) : this()
        {
            MinCoordinate = new EarthCoordinate(south, west);
            MaxCoordinate = new EarthCoordinate(north, east);
            Longitudes = new double[values.GetLength(0)];
            Latitudes = new double[values.GetLength(1)];
            for (int lon = 0; lon < Longitudes.Length; lon++) Longitudes[lon] = west + (lon*gridSpacing);
            for (int lat = 0; lat < Latitudes.Length; lat++) Latitudes[lat] = south + (lat*gridSpacing);
            Values = values;
        }

        public Environment2DData(string fileName, string layerName, float north, float west, float south, float east)
        {
            Filename = fileName;
            if (Path.GetExtension(fileName) != ".eeb") throw new System.IO.FileFormatException(string.Format("environment2DData: Unknown file type \"{0}\"", Path.GetFileName(fileName)));
            DataFile file = DataFile.Open(fileName);

            DataLayer layer = file[layerName];
            if (layer == null) throw new System.IO.FileFormatException(string.Format("environment2DData: Specified environment file \"{0}\"does not contain a environment2DData layer", fileName));

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

        public Environment2DData(string fileName) : this()
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
}