﻿using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using HRC.Navigation;
using Color = System.Windows.Media.Color;
using PixelFormat = System.Drawing.Imaging.PixelFormat;

namespace HRC.Utility
{
    // HRC.Resources.haxby.map
    // HRC.Resources.jet.map
    // HRC.Resources.summer.map
    public class Colormap
    {
        readonly List<Color> _map = new List<Color>();

        /// <summary>
        /// Create a Colormap from a matrix of normalized values.
        /// </summary>
        /// <param name="rgbNormalizedValues">
        /// An N-by-3 matrix of double values.  N (the number of 'rows') specifies the number of discrete colors
        /// in the Colormap.  The columns specify normalized Red, Green, and Blue values, respectively.
        /// These normalized values must fall within the range 0.0 to 1.0, inclusive. 
        /// For any given row M in the matrix, rgbNormalizedValues[M, 0] is Red, rgbNormalizedValues[M, 1] is Green,
        /// rgbNormalizedValues[M, 2] is Blue
        /// </param>
        public Colormap(IEnumerable<RGBNormalizedValue> rgbNormalizedValues)
        {
            if (rgbNormalizedValues == null) throw new ArgumentException("Colormap: The color values list cannot be null");

            foreach (var curValue in rgbNormalizedValues)
                _map.Add(curValue.Color);
        }

        /// <summary>
        /// Create a colormap from a stream.
        /// The stream is expected to contain ascii numeric data of N rows by 3 columns, which will be parsed
        /// to create a normalized array of RGB values which will in turn used to construct a Colormap
        /// </summary>
        /// <param name="inputStream"></param>
        /// <returns></returns>
        public static Colormap FromStream(Stream inputStream)
        {
            char[] separators = {' '};
            var values = new List<RGBNormalizedValue>();

            if (inputStream == null) throw new ArgumentException("Colormap.FromStream: inputStream cannot be null");
            if (!inputStream.CanRead) throw new ArgumentException("Colormap.FromStream: inputStream must be readable");
            using (var reader = new StreamReader(inputStream))
            {
                string curLine;
                int lineNumber = 1;
                while ((curLine = reader.ReadLine()) != null)
                {
                    try
                    {
                        values.Add(new RGBNormalizedValue(curLine, separators));
                    }
                    catch (InvalidDataException e)
                    {
                        throw new FileFormatException("Error encountered on line " + lineNumber, e);
                    }

                    lineNumber++;
                }
            }
            return new Colormap(values);
        }

        internal static Colormap FromEmbeddedResource(string embeddedResourceName)
        {
            var myAssembly = Assembly.GetExecutingAssembly();
            return FromStream(myAssembly.GetManifestResourceStream(embeddedResourceName));
        }

        static Colormap _haxby;
        public static Colormap Haxby { get { return _haxby ?? (_haxby = FromEmbeddedResource("HRC.Resources.haxby.map")); } }

        static Colormap _jet;
        public static Colormap Jet { get { return _jet ?? (_jet = FromEmbeddedResource("HRC.Resources.jet.map")); } }

        static Colormap _summer;
        public static Colormap Summer { get { return _summer ?? (_summer = FromEmbeddedResource("HRC.Resources.summer.map")); } }

        // data[lats,lons]
        public Bitmap ToBitmap(float[,] data, float minValue, float maxValue)
        {
            if (data == null) throw new ApplicationException("ToBitmap: data cannot be null");

            var height = data.GetLength(0);
            var width = data.GetLength(1);
            var dataRange = maxValue - minValue;
            var writeableBitmap = new WriteableBitmap(width, height, 96, 96, PixelFormats.Bgr32, null);

            writeableBitmap.Lock();
            unsafe
            {
                var curOffset = (int)writeableBitmap.BackBuffer;
                for (var y = 0; y < height; y++)
                {
                    for (var x = 0; x < width; x++)
                    {
                        // Draw from the bottom up, which matches the default render order.  This may change as the UI becomes
                        // more fully implemented, especially if we need to flip the canvas and render from the top.  Time will tell.
                        var curColor = Lookup(data[height - 1 - y, x], minValue, maxValue, dataRange);
                        *((int*)curOffset) = ((curColor.A << 24) | (curColor.R << 16) | (curColor.G << 8) | (curColor.B));
                        curOffset += sizeof(Int32);
                    }
                }
            }
            writeableBitmap.AddDirtyRect(new Int32Rect(0, 0, width, height));
            writeableBitmap.Unlock();
            return new Bitmap(width, height, 4 * width, PixelFormat.Format32bppArgb, writeableBitmap.BackBuffer);
        }

        public Bitmap ToBitmap(float[,] data)
        {
            if (data == null) throw new ApplicationException("ToBitmap: data cannot be null");

            var minMaxSource = data.Cast<float>();
            var dataMin = minMaxSource.Min();
            var dataMax = minMaxSource.Max();
            return (ToBitmap(data, dataMin, dataMax));
        }

        internal Color Lookup(float value, float minValue, float maxValue, float dataRange)
        {
            if (value >= maxValue) return _map.Last();
            if (value <= minValue) return _map.First();

            if (dataRange != 0.0)
            {
                double fraction = (value - minValue) / dataRange;
                if (IsInverted) fraction = 1.0 - fraction;
                var index = (int)(fraction * _map.Count);
                return _map[index];
            }
            return Colors.Black;
        }

        public bool IsInverted { set; get; }
    }

    public class DualColormap
    {
        public DualColormap(Colormap aboveThresholdColormap, Colormap belowThresholdColormap) 
        { 
            AboveThresholdColormap = aboveThresholdColormap;
            BelowThresholdColormap = belowThresholdColormap;
        }
        public Colormap AboveThresholdColormap { get; private set; }
        public Colormap BelowThresholdColormap { get; private set; }
        public float Threshold { get; set; }

        // data[lats,lons]
        public Bitmap ToBitmap<T>(T[,] data, float minValue, float maxValue) 
            where T : EarthCoordinate<float> 
        {
            if (data == null) throw new ApplicationException("ToBitmap: data cannot be null");

            var height = data.GetLength(1);
            var width = data.GetLength(0);
            var writeableBitmap = new WriteableBitmap(width, height, 96, 96, PixelFormats.Bgr32, null);

            writeableBitmap.Lock();
            unsafe
            {
                Color curColor;
                var curOffset = (int)writeableBitmap.BackBuffer;
                for (var y = 0; y < height; y++)
                {
                    for (var x = 0; x < width; x++)
                    {
                        var curValue = data[x, height - 1 - y].Data;
                        curColor = curValue <= Threshold ? BelowThresholdColormap.Lookup(curValue, minValue, Threshold, Threshold - minValue) : AboveThresholdColormap.Lookup(curValue, Threshold, maxValue, maxValue - Threshold);
                        // Draw from the bottom up, which matches the default render order.  This may change as the UI becomes
                        // more fully implemented, especially if we need to flip the canvas and render from the top.  Time will tell.
                        *((int*)curOffset) = ((curColor.A << 24) | (curColor.R << 16) | (curColor.G << 8) | (curColor.B));
                        curOffset += sizeof(Int32);
                    }
                }
            }
            writeableBitmap.AddDirtyRect(new Int32Rect(0, 0, width, height));
            writeableBitmap.Unlock();
            return new Bitmap(width, height, 4 * width, PixelFormat.Format32bppArgb, writeableBitmap.BackBuffer);
        }

        public Bitmap ToBitmap(EarthCoordinate<float>[,] data)
        {
            if (data == null) throw new ApplicationException("ToBitmap: data cannot be null");

            var minMaxSource = data.Cast<float>();
            var dataMin = minMaxSource.Min();
            var dataMax = minMaxSource.Max();
            return (ToBitmap(data, dataMin, dataMax));
        }
    }

    public class RGBNormalizedValue
    {
        public RGBNormalizedValue(double red, double green, double blue)
        {
            Initialize(red, green, blue);
        }

        internal RGBNormalizedValue(string curLine, char[] separators)
        {
            double red,
                   green,
                   blue;

            var fields = curLine.Trim().Split(separators, StringSplitOptions.RemoveEmptyEntries);

            if (!double.TryParse(fields[0], out red) || !double.TryParse(fields[1], out green) || !double.TryParse(fields[2], out blue))
                throw new InvalidDataException("Colormap.FromStream: Invalid data encountered");

            Initialize(red, green, blue);
        }

        void Initialize(double red, double green, double blue)
        {
            if ((red > 1.0) || (red < 0.0)) throw new ArgumentException("RGBNormalizedValue: Red must be between 0.0 and 1.0");
            if ((green > 1.0) || (green < 0.0)) throw new ArgumentException("RGBNormalizedValue: Green must be between 0.0 and 1.0");
            if ((blue > 1.0) || (blue < 0.0)) throw new ArgumentException("RGBNormalizedValue: Blue must be between 0.0 and 1.0");

            Red = red;
            Green = green;
            Blue = blue;
        }
        public double Red { get; private set; }
        public double Green { get; private set; }
        public double Blue { get; private set; }

        public Color Color
        {
            get { return Color.FromRgb((byte) (Red * 255.0), (byte) (Green * 255.0), (byte) (Blue * 255.0)); }
        }
    }
}