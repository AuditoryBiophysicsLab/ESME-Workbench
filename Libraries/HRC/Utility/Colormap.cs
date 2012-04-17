using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using HRC.Aspects;
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
        [Initialize, UsedImplicitly] public List<Color> Map { get; private set; }

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
                Map.Add(curValue.Color);
        }

        private Colormap() {}

        public Colormap(IList<ColormapComponent> components, int stepCount)
        {
            if (components[0].Fraction != 0.0) throw new ArgumentOutOfRangeException("components", "First component's fraction must be 0.0");
            if (components.Last().Fraction != 1.0) throw new ArgumentOutOfRangeException("components", "Last component's fraction must be 1.0");
            for (var componentIndex = 0; componentIndex < components.Count - 1; componentIndex++) if (components[componentIndex].Fraction >= components[componentIndex + 1].Fraction) throw new ArgumentException("Fraction values must increase with each entry in the component list", "components");

            var curAlpha = components[0].Alpha;
            var curRed = components[0].Red;
            var curGreen = components[0].Green;
            var curBlue = components[0].Blue;
            for (var componentIndex = 1; componentIndex < components.Count; componentIndex++)
            {
                var numSteps = (int)((components[componentIndex].Fraction - components[componentIndex - 1].Fraction) * stepCount);
                var deltaAlpha = (components[componentIndex].Alpha - curAlpha) / numSteps;
                var deltaRed = (components[componentIndex].Red - curRed) / numSteps;
                var deltaGreen = (components[componentIndex].Green - curGreen) / numSteps;
                var deltaBlue = (components[componentIndex].Blue - curBlue) / numSteps;
                for (var step = 0; step < numSteps; step++)
                {
                    Map.Add(Color.FromScRgb(curAlpha, curRed, curGreen, curBlue));
                    curAlpha += deltaAlpha;
                    curRed += deltaRed;
                    curGreen += deltaGreen;
                    curBlue += deltaBlue;
                }
                curAlpha = components[componentIndex].Alpha;
                curRed = components[componentIndex].Red;
                curGreen = components[componentIndex].Green;
                curBlue = components[componentIndex].Blue;
            }
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
                var lineNumber = 1;
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

        public readonly static ColormapComponent[] HotComponents = new[]
        {
            new ColormapComponent(0.00000f, 1.0000f, 0.0000f, 0.0000f, 0.0000f),
            new ColormapComponent(0.37500f, 1.0000f, 1.0000f, 0.0000f, 0.0000f),
            new ColormapComponent(0.75000f, 1.0000f, 1.0000f, 1.0000f, 0.0000f),
            new ColormapComponent(1.00000f, 1.0000f, 1.0000f, 1.0000f, 1.0000f)
        };
        public readonly static ColormapComponent[] CopperComponents = new[]
        {
            new ColormapComponent(0.00000f, 1.0000f, 0.0000f, 0.0000f, 0.0000f),
            new ColormapComponent(0.79690f, 1.0000f, 1.0000f, 0.6235f, 0.4000f),
            new ColormapComponent(1.00000f, 1.0000f, 1.0000f, 0.7843f, 0.5020f)
        };
        public readonly static ColormapComponent[] CoolComponents = new[]
        {
            new ColormapComponent(0.00000f, 1.0000f, 1.0000f, 1.0000f, 0.0000f),
            new ColormapComponent(1.00000f, 1.0000f, 1.0000f, 0.0000f, 1.0000f)
        };
        public readonly static ColormapComponent[] OceanComponents = new[]
        {
            new ColormapComponent(0.00000f, 1.0000f, 0.0000f, 0.0000f, 0.0000f),
            new ColormapComponent(0.12500f, 1.0000f, 0.0000f, 0.0196f, 0.0980f),
            new ColormapComponent(0.25000f, 1.0000f, 0.0000f, 0.0392f, 0.1961f),
            new ColormapComponent(0.37500f, 1.0000f, 0.0000f, 0.3137f, 0.4902f),
            new ColormapComponent(0.50000f, 1.0000f, 0.0000f, 0.5882f, 0.7843f),
            new ColormapComponent(0.62500f, 1.0000f, 0.3373f, 0.7725f, 0.7216f),
            new ColormapComponent(0.75000f, 1.0000f, 0.6745f, 0.9608f, 0.6588f),
            new ColormapComponent(0.87500f, 1.0000f, 0.8275f, 0.9804f, 0.8275f),
            new ColormapComponent(1.00000f, 1.0000f, 0.9804f, 1.0000f, 1.0000f),
        };

        static Colormap _haxby;
        public static Colormap Haxby { get { return _haxby ?? (_haxby = FromEmbeddedResource("HRC.Resources.haxby.map")); } }

        static Colormap _jet;
        public static Colormap Jet { get { return _jet ?? (_jet = FromEmbeddedResource("HRC.Resources.jet.map")); } }

        static Colormap _summer;
        public static Colormap Summer { get { return _summer ?? (_summer = FromEmbeddedResource("HRC.Resources.summer.map")); } }

        static Colormap _sediment;

        public static Colormap Sediment
        {
            get
            {
                if (_sediment != null) return _sediment;
                _sediment = new Colormap();
                _sediment.Map.AddRange(new List<Color>
                {
                    Colors.Pink,                            //  0 (should be invalid)
                    Color.FromScRgb(1, 1, 0, 0),            //  1 Rough Rock
                    Color.FromScRgb(1, .666f, 0, 0),        //  2 Rock
                    Color.FromScRgb(1, .333f, 0, 0),        //  3 Cobble or Gravel or Pebble
                    Color.FromScRgb(1, 1, .666f, 0),        //  4 Sandy Gravel
                    Color.FromScRgb(1, .666f, .444f, 0),    //  5 Very Coarse Sand
                    Color.FromScRgb(1, .333f, .222f, 0),    //  6 Muddy Sandy Gravel
                    Color.FromScRgb(1, 1, 1, 0),            //  7 Coarse Sand or Gravelly Sand
                    Color.FromScRgb(1, .666f, .666f, 0),    //  8 Gravelly Muddy Sand
                    Color.FromScRgb(1, .333f, .333f, 0),    //  9 Medium Sand or Sand
                    Color.FromScRgb(1, 0, 1, 0),            // 10 Muddy Gravel
                    Color.FromScRgb(1, 0, .666f, 0),        // 11 Fine Sand or Silty Sand
                    Color.FromScRgb(1, 0, .333f, 0),        // 12 Muddy Sand
                    Color.FromScRgb(1, 0, 1, 1),            // 13 Very Fine Sand,
                    Color.FromScRgb(1, 0, .666f, .666f),    // 14 Clayey Sand
                    Color.FromScRgb(1, 0, .333f, .333f),    // 15 Coarse Silt
                    Color.FromScRgb(1, 0, 0, 1),            // 16 Gravelly Mud or Sandy Silt
                    Color.FromScRgb(1, 0, 0, .666f),        // 17 Medium Silt or Sand-Silt-Clay
                    Color.FromScRgb(1, 0, 0, .333f),        // 18 Sandy Mud or Silt
                    Color.FromScRgb(1, 1, 0, 1),            // 19 Fine Silt or Clayey Silt
                    Color.FromScRgb(1, .666f, 0, .666f),    // 20 Sandy Clay
                    Color.FromScRgb(1, .333f, 0, .333f),    // 21 Very Fine Silt
                    Colors.Olive,                           // 22 Silty Clay
                    Colors.OliveDrab,                       // 23 Clay
                    Colors.Black,   
                });
                return _sediment;
            }
        }

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

            var minMaxSource = data.Cast<float>().ToList();
            var dataMin = minMaxSource.Min();
            var dataMax = minMaxSource.Max();
            return (ToBitmap(data, dataMin, dataMax));
        }

        internal Color Lookup(float value, float minValue, float maxValue, float dataRange)
        {
            if (value >= maxValue) return Map.Last();
            if (value <= minValue) return Map.First();

            if (Math.Abs(dataRange - 0.0) > 0.0001)
            {
                double fraction = (value - minValue) / dataRange;
                if (IsInverted) fraction = 1.0 - fraction;
                var index = (int)(fraction * Map.Count);
                return Map[index];
            }
            return Colors.Black;
        }

        public bool IsInverted { set; get; }

        // data[lats,lons]
        public uint[,] ToPixelValues(float[,] data, float minValue, float maxValue)
        {
            if (data == null) throw new ApplicationException("ToBitmap: data cannot be null");

            var height = data.GetLength(1);
            var width = data.GetLength(0);
            var dataRange = maxValue - minValue;
            var pixelValues = new uint[width, height];

            for (var y = 0; y < height; y++)
            {
                for (var x = 0; x < width; x++)
                {
                    var curColor = Lookup(data[x, height - 1 - y], minValue, maxValue, dataRange);
                    // Draw from the bottom up, which matches the default render order.  This may change as the UI becomes
                    // more fully implemented, especially if we need to flip the canvas and render from the top.  Time will tell.
                    pixelValues[x, y] = (uint)((curColor.A << 24) | (curColor.R << 16) | (curColor.G << 8) | (curColor.B));
                }
            }
            return pixelValues;
        }
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
            where T : Geo<float>
        {
            if (data == null) throw new ApplicationException("ToBitmap: data cannot be null");

            var height = data.GetLength(1);
            var width = data.GetLength(0);
            var writeableBitmap = new WriteableBitmap(width, height, 96, 96, PixelFormats.Bgr32, null);

            writeableBitmap.Lock();
            unsafe
            {
                var curOffset = (int)writeableBitmap.BackBuffer;
                for (var y = 0; y < height; y++)
                {
                    for (var x = 0; x < width; x++)
                    {
                        var curValue = data[x, height - 1 - y].Data;
                        var curColor = curValue <= Threshold ? BelowThresholdColormap.Lookup(curValue, minValue, Threshold, Threshold - minValue) : AboveThresholdColormap.Lookup(curValue, Threshold, maxValue, maxValue - Threshold);
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

        // data[lats,lons]
        public Bitmap ToBitmap(float[,] data, float minValue, float maxValue)
        {
            if (data == null) throw new ApplicationException("ToBitmap: data cannot be null");

            var height = data.GetLength(1);
            var width = data.GetLength(0);
            var writeableBitmap = new WriteableBitmap(width, height, 96, 96, PixelFormats.Bgr32, null);

            writeableBitmap.Lock();
            unsafe
            {
                var curOffset = (int)writeableBitmap.BackBuffer;
                for (var y = 0; y < height; y++)
                {
                    for (var x = 0; x < width; x++)
                    {
                        var curValue = data[x, height - 1 - y];
                        var curColor = curValue <= Threshold ? BelowThresholdColormap.Lookup(curValue, minValue, Threshold, Threshold - minValue) : AboveThresholdColormap.Lookup(curValue, Threshold, maxValue, maxValue - Threshold);
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

        // data[lats,lons]
        public uint[,] ToPixelValues(float[,] data, float minValue, float maxValue, Color aboveThresholdColor = default(Color))
        {
            if (data == null) throw new ApplicationException("ToBitmap: data cannot be null");

            var height = data.GetLength(1);
            var width = data.GetLength(0);
            var pixelValues = new uint[width,height];

            for (var y = 0; y < height; y++)
            {
                for (var x = 0; x < width; x++)
                {
                    var curValue = data[x, height - 1 - y];
                    var curColor = curValue <= Threshold
                                       ? BelowThresholdColormap.Lookup(curValue, minValue, Threshold, Threshold - minValue)
                                       : aboveThresholdColor == default(Color) ? AboveThresholdColormap.Lookup(curValue, Threshold, maxValue, maxValue - Threshold) : aboveThresholdColor;
                    // Draw from the bottom up, which matches the default render order.  This may change as the UI becomes
                    // more fully implemented, especially if we need to flip the canvas and render from the top.  Time will tell.
                    pixelValues[x, y] = (uint)((curColor.A << 24) | (curColor.R << 16) | (curColor.G << 8) | (curColor.B));
                }
            }
            return pixelValues;
        }

        public Bitmap ToBitmap(Geo<float>[,] data)
        {
            if (data == null) throw new ApplicationException("ToBitmap: data cannot be null");

            var dataMin = float.MaxValue;
            var dataMax = float.MinValue;
            foreach (var d in data)
            {
                dataMin = Math.Min(d.Data, dataMin);
                dataMax = Math.Max(d.Data, dataMax);
            }
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

    public class ColormapComponent
    {
        public ColormapComponent(float fraction, float alpha, float red, float green, float blue)
        {
            const string rangeExceptionString = "Must be between 0.0 and 1.0, inclusive";
            if (!RangeCheck(fraction)) throw new ArgumentOutOfRangeException("fraction", rangeExceptionString);
            if (!RangeCheck(alpha)) throw new ArgumentOutOfRangeException("alpha", rangeExceptionString);
            if (!RangeCheck(red)) throw new ArgumentOutOfRangeException("red", rangeExceptionString);
            if (!RangeCheck(green)) throw new ArgumentOutOfRangeException("green", rangeExceptionString);
            if (!RangeCheck(blue)) throw new ArgumentOutOfRangeException("blue", rangeExceptionString);
            Fraction = fraction;
            Alpha = alpha;
            Red = red;
            Green = green;
            Blue = blue;
        }

        static bool RangeCheck(float value) { return 0f <= value && value <= 1f; }
        public float Fraction { get; private set; }
        public float Alpha { get; private set; }
        public float Red { get; private set; }
        public float Green { get; private set; }
        public float Blue { get; private set; }
    }
}
