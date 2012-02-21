using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.IO;
using System.Linq;
using HRC.Navigation;

namespace HRC.Utility
{
    public static class ExtensionMethods
    {
        public static TimeSpan Modulo(this TimeSpan source, TimeSpan modulus)
        {
            var sourceMs = source.TotalMilliseconds;
            var modulusMs = modulus.TotalMilliseconds;
            var modulo = sourceMs - (modulusMs * (int) (sourceMs / modulusMs));
            return new TimeSpan(0, 0, 0, 0, (int) modulo);
        }

        public static double DivideBy(this TimeSpan numerator, TimeSpan denominator) { return numerator.TotalSeconds / denominator.TotalSeconds; }

#if false
        /// <summary>
        ///   Draw a circle with the CENTER at the specified EarthCoordinate, with the radius given in pixels
        /// </summary>
        /// <param name = "g"></param>
        /// <param name = "center">Center of the circle</param>
        /// <param name = "radius">Radius of the circle, in pixels</param>
        /// <param name = "lineColor">Color of the perimeter of the circle</param>
        /// <param name = "fillColor">Color to fill the circle with, or null if an unfilled circle is desired</param>
        public static void Circle(this Graphics g, EarthCoordinate center, float radius, Color? lineColor, Color? fillColor)
        {
            var unitsPerPixelX = g.GetUnitsPerPixelX();
            var unitsPerPixelY = g.GetUnitsPerPixelY();
            var offset = new EarthCoordinate(-radius * unitsPerPixelY, radius * unitsPerPixelX);
            var topLeft = center - offset;
            var size = new SizeF((float) (offset.Longitude * 2), (float) (offset.Latitude * 2));
            var bounds = new RectangleF((PointF) topLeft, size);
            if (lineColor != null)
            {
                var pen = new Pen(lineColor.Value, (2 * Math.Max(unitsPerPixelX, unitsPerPixelY)));
                g.DrawEllipse(pen, bounds);
            }
            if (fillColor != null)
            {
                Brush brush = new SolidBrush(fillColor.Value);
                g.FillEllipse(brush, bounds);
            }
        }
#endif

        /// <summary>
        ///   Set georeferenced coordinates for the current graphics object
        /// </summary>
        /// <param name = "g"></param>
        /// <param name = "p1">One of the corners of the current drawable area</param>
        /// <param name = "p2">Another corner of the current drawable area</param>
        public static void SetGeoCoordinates(this Graphics g, Geo p1, Geo p2)
        {
            var north = Math.Max(p1.Latitude, p2.Latitude);
            var south = Math.Min(p1.Latitude, p2.Latitude);
            var west = Math.Min(p1.Longitude, p2.Longitude);
            var east = Math.Max(p1.Longitude, p2.Longitude);
            var latitudeCoverage = north - south;
            var longitudeCoverage = east - west;

            g.Transform = new Matrix(1, 0, 0, 1, 0, 0);

            var pixelsPerUnitX = (g.VisibleClipBounds.Width - 1) / longitudeCoverage;
            var pixelsPerUnitY = (g.VisibleClipBounds.Height - 1) / latitudeCoverage;

            var mm = new Matrix(1, 0, 0, -1, 0, 0);
            mm.Scale((float) pixelsPerUnitX, (float) pixelsPerUnitY);
            mm.Translate((float) (-west), (float) (-north));
            g.Transform = mm;
        }

#if false
        /// <summary>
        ///   Draw a square with the CENTER at the specified EarthCoordinate, with the radius given in pixels
        /// </summary>
        /// <param name = "g"></param>
        /// <param name = "center">Center of the square</param>
        /// <param name = "width">Radius of the circle, in pixels</param>
        /// <param name = "lineColor">Color of the perimeter of the circle</param>
        /// <param name = "fillColor">Color to fill the circle with, or null if an unfilled circle is desired</param>
        public static void Square(this Graphics g, EarthCoordinate center, float width, Color? lineColor, Color? fillColor)
        {
            var radius = width / 2f;
            var unitsPerPixelX = g.GetUnitsPerPixelX();
            var unitsPerPixelY = g.GetUnitsPerPixelY();
            var offset = new EarthCoordinate(radius * unitsPerPixelY, radius * unitsPerPixelX);
            var p1 = center + offset;
            var p2 = center - offset;
            var north = (float) Math.Max(p1.Latitude, p2.Latitude);
            var south = (float) Math.Min(p1.Latitude, p2.Latitude);
            var west = (float) Math.Min(p1.Longitude, p2.Longitude);
            var east = (float) Math.Max(p1.Longitude, p2.Longitude);
            PointF[] points = {
                                  new PointF(west, north), new PointF(east, north), new PointF(east, south), new PointF(west, south)
                              };
            if (lineColor != null)
            {
                var pen = new Pen(lineColor.Value, (width * Math.Max(unitsPerPixelX, unitsPerPixelY)));
                g.DrawPolygon(pen, points);
            }
            if (fillColor != null)
            {
                Brush brush = new SolidBrush(fillColor.Value);
                g.FillPolygon(brush, points);
            }
        }
#endif

        public static void Lines(this Graphics g, Geo[] points, float width, Color color)
        {
            var unitsPerPixelX = g.GetUnitsPerPixelX();
            var unitsPerPixelY = g.GetUnitsPerPixelY();
            var pen = new Pen(color, (width * Math.Max(unitsPerPixelX, unitsPerPixelY)));
            for (var i = 0; i < points.Length - 1; i++) g.DrawLine(pen, points[i], points[i + 1]);
        }

        /// <summary>
        ///   Draw a polygon from a list of EarthCoordinates
        /// </summary>
        /// <param name = "g"></param>
        /// <param name="points"></param>
        /// <param name = "width">Width of the perimeter line, in pixels</param>
        /// <param name = "lineColor">Color of the perimeter of the square</param>
        /// <param name = "fillColor">Color to fill the square with, or null if an unfilled square is desired</param>
        public static void Polygon(this Graphics g, Geo[] points, float width, Color? lineColor, Color? fillColor)
        {
            var pointFs = new PointF[points.Length];
            for (var i = 0; i < points.Length; i++) pointFs[i] = points[i];
            var unitsPerPixelX = g.GetUnitsPerPixelX();
            var unitsPerPixelY = g.GetUnitsPerPixelY();
            if (lineColor != null)
            {
                var pen = new Pen(lineColor.Value, (width * Math.Max(unitsPerPixelX, unitsPerPixelY)));
                g.DrawPolygon(pen, pointFs);
            }
            if (fillColor == null) return;
            Brush brush = new SolidBrush(fillColor.Value);
            g.FillPolygon(brush, pointFs);
        }

        /// <summary>
        ///   Draw a line from the start point to the end point, with the specified color
        /// </summary>
        /// <param name = "g"></param>
        /// <param name = "start">EarthCoordinate to start drawing</param>
        /// <param name = "end">EarthCoordinate to stop drawing</param>
        /// <param name = "width">Width of the line, in pixels</param>
        /// <param name = "color">Color of the line</param>
        public static void Line(this Graphics g, Geo start, Geo end, float width, Color color)
        {
            var unitsPerPixelX = g.GetUnitsPerPixelX();
            var unitsPerPixelY = g.GetUnitsPerPixelY();
            var pen = new Pen(color, (width * Math.Max(unitsPerPixelX, unitsPerPixelY)));
            g.DrawLine(pen, start, end);
        }

        /// <summary>
        ///   Returns the number of X pixels in each graphics unit in the current Graphics object (X Scale)
        /// </summary>
        /// <param name = "g"></param>
        /// <returns></returns>
        public static float GetPixelsPerUnitX(this Graphics g) { return Math.Abs(g.Transform.Elements[0]); }

        /// <summary>
        ///   Returns the number of Y pixels in each graphics unit in the current Graphics object (Y Scale)
        /// </summary>
        /// <param name = "g"></param>
        /// <returns></returns>
        public static float GetPixelsPerUnitY(this Graphics g) { return Math.Abs(g.Transform.Elements[3]); }

        /// <summary>
        ///   Returns the number of graphics units per X pixel in the current Graphics object (1 / (X Scale))
        /// </summary>
        /// <param name = "g"></param>
        /// <returns></returns>
        public static float GetUnitsPerPixelX(this Graphics g) { return 1f / Math.Abs(g.Transform.Elements[0]); }

        /// <summary>
        ///   Returns the number of graphics units per Y pixel in the current Graphics object (1 / (Y Scale))
        /// </summary>
        /// <param name = "g"></param>
        /// <returns></returns>
        public static float GetUnitsPerPixelY(this Graphics g) { return 1f / Math.Abs(g.Transform.Elements[3]); }

        /// <summary>
        ///   Returns the number of X pixels the origin has been offset in the current Graphics object (X Translate)
        /// </summary>
        /// <param name = "g"></param>
        /// <returns></returns>
        public static float GetOriginOffsetX(this Graphics g) { return g.Transform.Elements[4]; }

        /// <summary>
        ///   Returns the number of Y pixels the origin has been offset in the current Graphics object (Y Translate)
        /// </summary>
        /// <param name = "g"></param>
        /// <returns></returns>
        public static float GetOriginOffsetY(this Graphics g) { return g.Transform.Elements[5]; }

#if false
        public static void ToHSV(this System.Windows.Media.Color color, out double hue, out double saturation, out double value)
        {
            int max = Math.Max(color.R, Math.Max(color.G, color.B));
            int min = Math.Min(color.R, Math.Min(color.G, color.B));

            hue = color.GetHue();
            saturation = (max == 0) ? 0 : 1d - (1d * min / max);
            value = max / 255d;
        }
#endif

        public static System.Windows.Media.Color ColorFromHSV(double hue, double saturation, double value)
        {
            var hi = Convert.ToInt32(Math.Floor(hue / 60)) % 6;
            var f = hue / 60 - Math.Floor(hue / 60);

            value = value * 255;
            var v = Convert.ToByte(value);
            var p = Convert.ToByte(value * (1 - saturation));
            var q = Convert.ToByte(value * (1 - f * saturation));
            var t = Convert.ToByte(value * (1 - (1 - f) * saturation));

            switch (hi)
            {
                case 0:
                    return System.Windows.Media.Color.FromArgb(255, v, t, p);
                case 1:
                    return System.Windows.Media.Color.FromArgb(255, q, v, p);
                case 2:
                    return System.Windows.Media.Color.FromArgb(255, p, v, t);
                case 3:
                    return System.Windows.Media.Color.FromArgb(255, p, q, v);
                case 4:
                    return System.Windows.Media.Color.FromArgb(255, t, p, v);
                default:
                    return System.Windows.Media.Color.FromArgb(255, v, p, q);
            }
        }

        public static System.Windows.Media.Color[] CreateHSVPalette(int paletteLength) 
        {
            var result = new System.Windows.Media.Color[paletteLength];
            var angleIncrement = 360.0 / ((double)paletteLength / 3);
            var index = 0;
            var saturation = 100;
            while (true)
            {
                var hueOffset = index * angleIncrement;
                result[index++] = ColorFromHSV(hueOffset, saturation / 100.0, 1.0);
                if (index == paletteLength) break;
                result[index++] = ColorFromHSV(120.0 + hueOffset, saturation / 100.0, 1.0);
                if (index == paletteLength) break;
                result[index++] = ColorFromHSV(240.0 + hueOffset, saturation / 100.0, 1.0);
                if (index == paletteLength) break;
                switch (saturation)
                {
                    case 100:
                        saturation = 90;
                        break;
                    case 90:
                        saturation = 100;
                        break;
                }
            }

            return result;
        }

        public static bool IsValidFilename(this string fileName)
        {
            var charList = new List<char>();
            charList.AddRange(Path.GetInvalidFileNameChars());
            charList.AddRange(Path.GetInvalidPathChars());
            var invalidChars = charList.Distinct().ToList();
            var charsToCheck = fileName.ToCharArray();
            return !(from cur in charsToCheck from invalid in invalidChars where cur == invalid select cur).Any();
        }
    }
}