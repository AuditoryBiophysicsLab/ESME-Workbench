using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using ESME.Overlay;
using HRC.Navigation;
using Color = System.Windows.Media.Color;
using FileFormatException = ESME.Model.FileFormatException;
using PixelFormat = System.Drawing.Imaging.PixelFormat;

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

        static readonly List<Color> ColorMap = new List<Color>
                                               {
                                                   Color.FromArgb(255, 0, 0, 143),
                                                   Color.FromArgb(255, 0, 0, 147),
                                                   Color.FromArgb(255, 0, 0, 151),
                                                   Color.FromArgb(255, 0, 0, 155),
                                                   Color.FromArgb(255, 0, 0, 159),
                                                   Color.FromArgb(255, 0, 0, 163),
                                                   Color.FromArgb(255, 0, 0, 167),
                                                   Color.FromArgb(255, 0, 0, 171),
                                                   Color.FromArgb(255, 0, 0, 175),
                                                   Color.FromArgb(255, 0, 0, 179),
                                                   Color.FromArgb(255, 0, 0, 183),
                                                   Color.FromArgb(255, 0, 0, 187),
                                                   Color.FromArgb(255, 0, 0, 191),
                                                   Color.FromArgb(255, 0, 0, 195),
                                                   Color.FromArgb(255, 0, 0, 199),
                                                   Color.FromArgb(255, 0, 0, 203),
                                                   Color.FromArgb(255, 0, 0, 207),
                                                   Color.FromArgb(255, 0, 0, 211),
                                                   Color.FromArgb(255, 0, 0, 215),
                                                   Color.FromArgb(255, 0, 0, 219),
                                                   Color.FromArgb(255, 0, 0, 223),
                                                   Color.FromArgb(255, 0, 0, 227),
                                                   Color.FromArgb(255, 0, 0, 231),
                                                   Color.FromArgb(255, 0, 0, 235),
                                                   Color.FromArgb(255, 0, 0, 239),
                                                   Color.FromArgb(255, 0, 0, 243),
                                                   Color.FromArgb(255, 0, 0, 247),
                                                   Color.FromArgb(255, 0, 0, 251),
                                                   Color.FromArgb(255, 0, 0, 255),
                                                   Color.FromArgb(255, 0, 3, 255),
                                                   Color.FromArgb(255, 0, 7, 255),
                                                   Color.FromArgb(255, 0, 11, 255),
                                                   Color.FromArgb(255, 0, 15, 255),
                                                   Color.FromArgb(255, 0, 19, 255),
                                                   Color.FromArgb(255, 0, 23, 255),
                                                   Color.FromArgb(255, 0, 27, 255),
                                                   Color.FromArgb(255, 0, 31, 255),
                                                   Color.FromArgb(255, 0, 35, 255),
                                                   Color.FromArgb(255, 0, 39, 255),
                                                   Color.FromArgb(255, 0, 43, 255),
                                                   Color.FromArgb(255, 0, 47, 255),
                                                   Color.FromArgb(255, 0, 51, 255),
                                                   Color.FromArgb(255, 0, 55, 255),
                                                   Color.FromArgb(255, 0, 59, 255),
                                                   Color.FromArgb(255, 0, 63, 255),
                                                   Color.FromArgb(255, 0, 67, 255),
                                                   Color.FromArgb(255, 0, 71, 255),
                                                   Color.FromArgb(255, 0, 75, 255),
                                                   Color.FromArgb(255, 0, 79, 255),
                                                   Color.FromArgb(255, 0, 83, 255),
                                                   Color.FromArgb(255, 0, 87, 255),
                                                   Color.FromArgb(255, 0, 91, 255),
                                                   Color.FromArgb(255, 0, 95, 255),
                                                   Color.FromArgb(255, 0, 99, 255),
                                                   Color.FromArgb(255, 0, 103, 255),
                                                   Color.FromArgb(255, 0, 107, 255),
                                                   Color.FromArgb(255, 0, 111, 255),
                                                   Color.FromArgb(255, 0, 115, 255),
                                                   Color.FromArgb(255, 0, 119, 255),
                                                   Color.FromArgb(255, 0, 123, 255),
                                                   Color.FromArgb(255, 0, 127, 255),
                                                   Color.FromArgb(255, 0, 131, 255),
                                                   Color.FromArgb(255, 0, 135, 255),
                                                   Color.FromArgb(255, 0, 139, 255),
                                                   Color.FromArgb(255, 0, 143, 255),
                                                   Color.FromArgb(255, 0, 147, 255),
                                                   Color.FromArgb(255, 0, 151, 255),
                                                   Color.FromArgb(255, 0, 155, 255),
                                                   Color.FromArgb(255, 0, 159, 255),
                                                   Color.FromArgb(255, 0, 163, 255),
                                                   Color.FromArgb(255, 0, 167, 255),
                                                   Color.FromArgb(255, 0, 171, 255),
                                                   Color.FromArgb(255, 0, 175, 255),
                                                   Color.FromArgb(255, 0, 179, 255),
                                                   Color.FromArgb(255, 0, 183, 255),
                                                   Color.FromArgb(255, 0, 187, 255),
                                                   Color.FromArgb(255, 0, 191, 255),
                                                   Color.FromArgb(255, 0, 195, 255),
                                                   Color.FromArgb(255, 0, 199, 255),
                                                   Color.FromArgb(255, 0, 203, 255),
                                                   Color.FromArgb(255, 0, 207, 255),
                                                   Color.FromArgb(255, 0, 211, 255),
                                                   Color.FromArgb(255, 0, 215, 255),
                                                   Color.FromArgb(255, 0, 219, 255),
                                                   Color.FromArgb(255, 0, 223, 255),
                                                   Color.FromArgb(255, 0, 227, 255),
                                                   Color.FromArgb(255, 0, 231, 255),
                                                   Color.FromArgb(255, 0, 235, 255),
                                                   Color.FromArgb(255, 0, 239, 255),
                                                   Color.FromArgb(255, 0, 243, 255),
                                                   Color.FromArgb(255, 0, 247, 255),
                                                   Color.FromArgb(255, 0, 251, 255),
                                                   Color.FromArgb(255, 0, 255, 255),
                                                   Color.FromArgb(255, 3, 255, 251),
                                                   Color.FromArgb(255, 7, 255, 247),
                                                   Color.FromArgb(255, 11, 255, 243),
                                                   Color.FromArgb(255, 15, 255, 239),
                                                   Color.FromArgb(255, 19, 255, 235),
                                                   Color.FromArgb(255, 23, 255, 231),
                                                   Color.FromArgb(255, 27, 255, 227),
                                                   Color.FromArgb(255, 31, 255, 223),
                                                   Color.FromArgb(255, 35, 255, 219),
                                                   Color.FromArgb(255, 39, 255, 215),
                                                   Color.FromArgb(255, 43, 255, 211),
                                                   Color.FromArgb(255, 47, 255, 207),
                                                   Color.FromArgb(255, 51, 255, 203),
                                                   Color.FromArgb(255, 55, 255, 199),
                                                   Color.FromArgb(255, 59, 255, 195),
                                                   Color.FromArgb(255, 63, 255, 191),
                                                   Color.FromArgb(255, 67, 255, 187),
                                                   Color.FromArgb(255, 71, 255, 183),
                                                   Color.FromArgb(255, 75, 255, 179),
                                                   Color.FromArgb(255, 79, 255, 175),
                                                   Color.FromArgb(255, 83, 255, 171),
                                                   Color.FromArgb(255, 87, 255, 167),
                                                   Color.FromArgb(255, 91, 255, 163),
                                                   Color.FromArgb(255, 95, 255, 159),
                                                   Color.FromArgb(255, 99, 255, 155),
                                                   Color.FromArgb(255, 103, 255, 151),
                                                   Color.FromArgb(255, 107, 255, 147),
                                                   Color.FromArgb(255, 111, 255, 143),
                                                   Color.FromArgb(255, 115, 255, 139),
                                                   Color.FromArgb(255, 119, 255, 135),
                                                   Color.FromArgb(255, 123, 255, 131),
                                                   Color.FromArgb(255, 127, 255, 127),
                                                   Color.FromArgb(255, 131, 255, 123),
                                                   Color.FromArgb(255, 135, 255, 119),
                                                   Color.FromArgb(255, 139, 255, 115),
                                                   Color.FromArgb(255, 143, 255, 111),
                                                   Color.FromArgb(255, 147, 255, 107),
                                                   Color.FromArgb(255, 151, 255, 103),
                                                   Color.FromArgb(255, 155, 255, 99),
                                                   Color.FromArgb(255, 159, 255, 95),
                                                   Color.FromArgb(255, 163, 255, 91),
                                                   Color.FromArgb(255, 167, 255, 87),
                                                   Color.FromArgb(255, 171, 255, 83),
                                                   Color.FromArgb(255, 175, 255, 79),
                                                   Color.FromArgb(255, 179, 255, 75),
                                                   Color.FromArgb(255, 183, 255, 71),
                                                   Color.FromArgb(255, 187, 255, 67),
                                                   Color.FromArgb(255, 191, 255, 63),
                                                   Color.FromArgb(255, 195, 255, 59),
                                                   Color.FromArgb(255, 199, 255, 55),
                                                   Color.FromArgb(255, 203, 255, 51),
                                                   Color.FromArgb(255, 207, 255, 47),
                                                   Color.FromArgb(255, 211, 255, 43),
                                                   Color.FromArgb(255, 215, 255, 39),
                                                   Color.FromArgb(255, 219, 255, 35),
                                                   Color.FromArgb(255, 223, 255, 31),
                                                   Color.FromArgb(255, 227, 255, 27),
                                                   Color.FromArgb(255, 231, 255, 23),
                                                   Color.FromArgb(255, 235, 255, 19),
                                                   Color.FromArgb(255, 239, 255, 15),
                                                   Color.FromArgb(255, 243, 255, 11),
                                                   Color.FromArgb(255, 247, 255, 7),
                                                   Color.FromArgb(255, 251, 255, 3),
                                                   Color.FromArgb(255, 255, 255, 0),
                                                   Color.FromArgb(255, 255, 251, 0),
                                                   Color.FromArgb(255, 255, 247, 0),
                                                   Color.FromArgb(255, 255, 243, 0),
                                                   Color.FromArgb(255, 255, 239, 0),
                                                   Color.FromArgb(255, 255, 235, 0),
                                                   Color.FromArgb(255, 255, 231, 0),
                                                   Color.FromArgb(255, 255, 227, 0),
                                                   Color.FromArgb(255, 255, 223, 0),
                                                   Color.FromArgb(255, 255, 219, 0),
                                                   Color.FromArgb(255, 255, 215, 0),
                                                   Color.FromArgb(255, 255, 211, 0),
                                                   Color.FromArgb(255, 255, 207, 0),
                                                   Color.FromArgb(255, 255, 203, 0),
                                                   Color.FromArgb(255, 255, 199, 0),
                                                   Color.FromArgb(255, 255, 195, 0),
                                                   Color.FromArgb(255, 255, 191, 0),
                                                   Color.FromArgb(255, 255, 187, 0),
                                                   Color.FromArgb(255, 255, 183, 0),
                                                   Color.FromArgb(255, 255, 179, 0),
                                                   Color.FromArgb(255, 255, 175, 0),
                                                   Color.FromArgb(255, 255, 171, 0),
                                                   Color.FromArgb(255, 255, 167, 0),
                                                   Color.FromArgb(255, 255, 163, 0),
                                                   Color.FromArgb(255, 255, 159, 0),
                                                   Color.FromArgb(255, 255, 155, 0),
                                                   Color.FromArgb(255, 255, 151, 0),
                                                   Color.FromArgb(255, 255, 147, 0),
                                                   Color.FromArgb(255, 255, 143, 0),
                                                   Color.FromArgb(255, 255, 139, 0),
                                                   Color.FromArgb(255, 255, 135, 0),
                                                   Color.FromArgb(255, 255, 131, 0),
                                                   Color.FromArgb(255, 255, 127, 0),
                                                   Color.FromArgb(255, 255, 123, 0),
                                                   Color.FromArgb(255, 255, 119, 0),
                                                   Color.FromArgb(255, 255, 115, 0),
                                                   Color.FromArgb(255, 255, 111, 0),
                                                   Color.FromArgb(255, 255, 107, 0),
                                                   Color.FromArgb(255, 255, 103, 0),
                                                   Color.FromArgb(255, 255, 99, 0),
                                                   Color.FromArgb(255, 255, 95, 0),
                                                   Color.FromArgb(255, 255, 91, 0),
                                                   Color.FromArgb(255, 255, 87, 0),
                                                   Color.FromArgb(255, 255, 83, 0),
                                                   Color.FromArgb(255, 255, 79, 0),
                                                   Color.FromArgb(255, 255, 75, 0),
                                                   Color.FromArgb(255, 255, 71, 0),
                                                   Color.FromArgb(255, 255, 67, 0),
                                                   Color.FromArgb(255, 255, 63, 0),
                                                   Color.FromArgb(255, 255, 59, 0),
                                                   Color.FromArgb(255, 255, 55, 0),
                                                   Color.FromArgb(255, 255, 51, 0),
                                                   Color.FromArgb(255, 255, 47, 0),
                                                   Color.FromArgb(255, 255, 43, 0),
                                                   Color.FromArgb(255, 255, 39, 0),
                                                   Color.FromArgb(255, 255, 35, 0),
                                                   Color.FromArgb(255, 255, 31, 0),
                                                   Color.FromArgb(255, 255, 27, 0),
                                                   Color.FromArgb(255, 255, 23, 0),
                                                   Color.FromArgb(255, 255, 19, 0),
                                                   Color.FromArgb(255, 255, 15, 0),
                                                   Color.FromArgb(255, 255, 11, 0),
                                                   Color.FromArgb(255, 255, 7, 0),
                                                   Color.FromArgb(255, 255, 3, 0),
                                                   Color.FromArgb(255, 255, 0, 0),
                                                   Color.FromArgb(255, 251, 0, 0),
                                                   Color.FromArgb(255, 247, 0, 0),
                                                   Color.FromArgb(255, 243, 0, 0),
                                                   Color.FromArgb(255, 239, 0, 0),
                                                   Color.FromArgb(255, 235, 0, 0),
                                                   Color.FromArgb(255, 231, 0, 0),
                                                   Color.FromArgb(255, 227, 0, 0),
                                                   Color.FromArgb(255, 223, 0, 0),
                                                   Color.FromArgb(255, 219, 0, 0),
                                                   Color.FromArgb(255, 215, 0, 0),
                                                   Color.FromArgb(255, 211, 0, 0),
                                                   Color.FromArgb(255, 207, 0, 0),
                                                   Color.FromArgb(255, 203, 0, 0),
                                                   Color.FromArgb(255, 199, 0, 0),
                                                   Color.FromArgb(255, 195, 0, 0),
                                                   Color.FromArgb(255, 191, 0, 0),
                                                   Color.FromArgb(255, 187, 0, 0),
                                                   Color.FromArgb(255, 183, 0, 0),
                                                   Color.FromArgb(255, 179, 0, 0),
                                                   Color.FromArgb(255, 175, 0, 0),
                                                   Color.FromArgb(255, 171, 0, 0),
                                                   Color.FromArgb(255, 167, 0, 0),
                                                   Color.FromArgb(255, 163, 0, 0),
                                                   Color.FromArgb(255, 159, 0, 0),
                                                   Color.FromArgb(255, 155, 0, 0),
                                                   Color.FromArgb(255, 151, 0, 0),
                                                   Color.FromArgb(255, 147, 0, 0),
                                                   Color.FromArgb(255, 143, 0, 0),
                                                   Color.FromArgb(255, 139, 0, 0),
                                                   Color.FromArgb(255, 135, 0, 0),
                                                   Color.FromArgb(255, 131, 0, 0),
                                                   Color.FromArgb(255, 127, 0, 0),
                                               };

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
                                   //edit: Modified this routine to take the horizontal and vertical resolution into account
                                   //      It now places the bounding box such that the lines are coincident with the edges of
                                   //      the edge samples of the selected data (extends by half the horizontal/vertical resolution)
                                   //northeast corner:                   
                                   new EarthCoordinate(MaxCoordinate.Latitude_degrees + (VerticalResolution/2), MaxCoordinate.Longitude_degrees + (HorizontalResolution/2)), //southeast corner: 
                                   new EarthCoordinate(MinCoordinate.Latitude_degrees - (VerticalResolution/2), MaxCoordinate.Longitude_degrees + (HorizontalResolution/2)), //southwest corner: 
                                   new EarthCoordinate(MinCoordinate.Latitude_degrees - (VerticalResolution/2), MinCoordinate.Longitude_degrees - (HorizontalResolution/2)), //northwest corner: 
                                   new EarthCoordinate(MaxCoordinate.Latitude_degrees + (VerticalResolution/2), MinCoordinate.Longitude_degrees - (HorizontalResolution/2)), //northeast corner again to close the loop.
                                   new EarthCoordinate(MaxCoordinate.Latitude_degrees + (VerticalResolution/2), MaxCoordinate.Longitude_degrees + (HorizontalResolution/2)),
                               };

                var shape = new OverlayLineSegments(bathyBox, Colors.Black, 1, LineStyle.Solid);
                return shape;
            }
        }

        public double HorizontalResolution
        {
            get { return Math.Abs(Longitudes[1] - Longitudes[0]); }
        }

        public double VerticalResolution
        {
            get { return Math.Abs(Latitudes[1] - Latitudes[0]); }
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

        static Color Lookup(float value, float minValue, float maxValue, float dataRange)
        {
            if (value >= maxValue) return ColorMap.Last();
            if (value <= minValue) return ColorMap.First();

            if (dataRange != 0.0)
            {
                double fraction = (value - minValue)/dataRange;
                var index = (int) (fraction*ColorMap.Count);
                return ColorMap[index];
            }
            return Colors.Black;
        }

        // data[lats,lons]
        protected static Bitmap ToBitmap(float[,] data)
        {
            if (data == null) throw new ApplicationException("ToBitmap: data cannot be null");

            int height = data.GetLength(0);
            int width = data.GetLength(1);
            IEnumerable<float> minMaxSource = data.Cast<float>();
            float dataMin = minMaxSource.Min();
            float dataMax = minMaxSource.Max();
            float dataRange = dataMax - dataMin;
            var writeableBitmap = new WriteableBitmap(width, height, 96, 96, PixelFormats.Bgr32, null);

            writeableBitmap.Lock();
            unsafe
            {
                var curOffset = (int) writeableBitmap.BackBuffer;
                for (int y = 0; y < height; y++)
                {
                    for (int x = 0; x < width; x++)
                    {
                        // Draw from the bottom up, which matches the default render order.  This may change as the UI becomes
                        // more fully implemented, especially if we need to flip the canvas and render from the top.  Time will tell.
                        Color curColor = Lookup(data[height - 1 - y, x], dataMin, dataMax, dataRange);
                        *((int*) curOffset) = ((curColor.A << 24) | (curColor.R << 16) | (curColor.G << 8) | (curColor.B));
                        curOffset += sizeof (Int32);
                    }
                }
            }
            writeableBitmap.AddDirtyRect(new Int32Rect(0, 0, width, height));
            writeableBitmap.Unlock();
            var result = new Bitmap(width, height, 4*width, PixelFormat.Format32bppArgb, writeableBitmap.BackBuffer);
            result.Save(@"C:\Users\Dave Anderson\Desktop\test_bathymetry.jpg", ImageFormat.Jpeg);
            return result;
        }


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

        public Bitmap Bitmap
        {
            get { return ToBitmap(Values); }
        }

        public MemoryStream MemoryStream
        {
            get
            {
                var memoryStream = new MemoryStream();
                Bitmap.Save(memoryStream, ImageFormat.Jpeg);
                return memoryStream;
            }
        }

        #region Public constructors

        public Environment2DData(double north, double south, double east, double west, float gridSpacing, float[,] values, float minValue, float maxValue)
        {
            MinCoordinate = new EarthCoordinate(south, west);
            MaxCoordinate = new EarthCoordinate(north, east);
            MinValue = minValue;
            MaxValue = maxValue;
            Longitudes = new double[values.GetLength(1)];
            Latitudes = new double[values.GetLength(0)];
            for (int lon = 0; lon < Longitudes.Length; lon++) Longitudes[lon] = west + (lon*gridSpacing);
            for (int lat = 0; lat < Latitudes.Length; lat++) Latitudes[lat] = south + (lat*gridSpacing);
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

        public static Environment2DData ReadChrtrBinaryFile(string fileName)
        {
            using (var stream = new BinaryReader(File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                float west = stream.ReadSingle();
                float east = stream.ReadSingle();
                float south = stream.ReadSingle();
                float north = stream.ReadSingle();
                float gridSpacing = stream.ReadSingle()/60f; // Source is in minutes, we need degrees
                int width = stream.ReadInt32();
                int height = stream.ReadInt32();
                uint endian = stream.ReadUInt32();
                if (endian != 0x00010203) throw new FileFormatException("Invalid CHRTR Binary file format - endian is incorrect");
                float maxDepth = -stream.ReadSingle();
                float minDepth = -stream.ReadSingle();
                int paddingWidth = (width - 10)*4;
                stream.ReadBytes(paddingWidth);
                var depths = new float[height,width];
                for (int lat = 0; lat < height; lat++)
                    for (int lon = 0; lon < width; lon++)
                    {
                        float curSample = stream.ReadSingle();
                        depths[lat, lon] = curSample == 1e16f ? float.NaN : -curSample;
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

            Values = new float[Latitudes.Length,Longitudes.Length];
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
                for (int lat = 0; lat < Latitudes.Length; lat++) for (int lon = 0; lon < Longitudes.Length; lon++) stream.WriteLine(string.Format("{0:##.######} {1:###.######} {2:#.###}", Latitudes[lat], Longitudes[lon], scaleFactor*Values[lat, lon]));
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
            for (int lon = 0; lon < Longitudes.Length; lon++) Longitudes[lon] = west + (lon*gridSpacing);
            for (int lat = 0; lat < Latitudes.Length; lat++) Latitudes[lat] = south + (lat*gridSpacing);
            for (int dep = 0; dep < Depths.Length; dep++) Depths[dep] = depths[dep];
            Values = values;
        }

        #endregion
    }

    public class AverageDatum
    {
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

        public float Value { get; set; }
        public int Count { get; set; }

        public void Add(float newValue)
        {
            Value += newValue;
            Count++;
        }

        public void Average() { if (Count > 0) Value /= Count; }
    }

    public sealed class Environment3DAverager : Environment3DData<AverageDatum>
    {
        public Environment3DAverager(double north, double south, double east, double west, float gridSpacing, IList<float> depths, List<AverageDatum>[,] values) : base(north, south, east, west, gridSpacing, depths, values) { }

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
                    if (Values[lonIndex, latIndex] == null) Values[lonIndex, latIndex] = new List<AverageDatum>();
                    for (int depthIndex = 0; depthIndex < that.Values[lonIndex, latIndex].Count; depthIndex++)
                    {
                        float thatValue = that.Values[lonIndex, latIndex][depthIndex];
                        if (Values[lonIndex, latIndex].Count <= depthIndex) Values[lonIndex, latIndex].Add(new AverageDatum(thatValue));
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
                    List<float> curData = Depths.Select(t => stream.ReadSingle()).Where(curValue => !float.IsNaN(curValue)).ToList();
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

        public Environment3DData(double north, double south, double east, double west, float gridSpacing, IList<float> depths, List<float>[,] values) : base(north, south, east, west, gridSpacing, depths, values) { }

        #endregion
    }
}