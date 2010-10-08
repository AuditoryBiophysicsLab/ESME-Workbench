using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml;
using System.Drawing;
using System.Drawing.Drawing2D;
using HRC.Navigation;

namespace HRC.Utility
{
    public static class ExtensionMethods
    {
        public static TimeSpan Modulo(this TimeSpan Source, TimeSpan Modulus)
        {
            double source = Source.TotalMilliseconds;
            double modulus = Modulus.TotalMilliseconds;
            double modulo = source - (modulus * (int)(source / modulus));
            return new TimeSpan(0, 0, 0, 0, (int)modulo);
        }

        public static double DivideBy(this TimeSpan Numerator, TimeSpan Denominator)
        {
            return Numerator.TotalSeconds / Denominator.TotalSeconds;
        }

        /// <summary>
        /// Draw a circle with the CENTER at the specified EarthCoordinate, with the radius given in pixels
        /// </summary>
        /// <param name="g"></param>
        /// <param name="center">Center of the circle</param>
        /// <param name="Radius_pixels">Radius of the circle, in pixels</param>
        /// <param name="LineColor">Color of the perimeter of the circle</param>
        /// <param name="FillColor">Color to fill the circle with, or null if an unfilled circle is desired</param>
        public static void Circle(this Graphics g, EarthCoordinate center, float Radius_pixels, Color? LineColor, Color? FillColor)
        {
            float UnitsPerPixelX = g.GetUnitsPerPixelX();
            float UnitsPerPixelY = g.GetUnitsPerPixelY();
            EarthCoordinate Offset = new EarthCoordinate(-Radius_pixels * UnitsPerPixelY, Radius_pixels * UnitsPerPixelX);
            EarthCoordinate TopLeft = center - Offset;
            SizeF size = new SizeF((float)(Offset.Longitude_degrees * 2), (float)(Offset.Latitude_degrees * 2));
            RectangleF bounds = new RectangleF((PointF)TopLeft, size);
            if (LineColor != null)
            {
                Pen pen = new Pen(LineColor.Value, (float)(2 * Math.Max(UnitsPerPixelX, UnitsPerPixelY)));
                g.DrawEllipse(pen, bounds);
            }
            if (FillColor != null)
            {
                Brush brush = new SolidBrush(FillColor.Value);
                g.FillEllipse(brush, bounds);
            }
        }

        /// <summary>
        /// Set georeferenced coordinates for the current graphics object
        /// </summary>
        /// <param name="g"></param>
        /// <param name="p1">One of the corners of the current drawable area</param>
        /// <param name="p2">Another corner of the current drawable area</param>
        public static void SetGeoCoordinates(this Graphics g, EarthCoordinate p1, EarthCoordinate p2)
        {
            double North = Math.Max(p1.Latitude_degrees, p2.Latitude_degrees);
            double South = Math.Min(p1.Latitude_degrees, p2.Latitude_degrees);
            double West = Math.Min(p1.Longitude_degrees, p2.Longitude_degrees);
            double East = Math.Max(p1.Longitude_degrees, p2.Longitude_degrees);
            double LatitudeCoverage = North - South;
            double LongitudeCoverage = East - West;

            g.Transform = new Matrix(1, 0, 0, 1, 0, 0);

            double PixelsPerUnitX = (g.VisibleClipBounds.Width - 1) / LongitudeCoverage;
            double PixelsPerUnitY = (g.VisibleClipBounds.Height - 1) / LatitudeCoverage;

            Matrix mm = new System.Drawing.Drawing2D.Matrix(1, 0, 0, -1, 0, 0);
            mm.Scale((float)PixelsPerUnitX, (float)PixelsPerUnitY);
            mm.Translate((float)(-West), (float)(-North));
            g.Transform = mm;
        }

        /// <summary>
        /// Draw a square with the CENTER at the specified EarthCoordinate, with the radius given in pixels
        /// </summary>
        /// <param name="g"></param>
        /// <param name="center">Center of the square</param>
        /// <param name="Radius_pixels">Radius of the circle, in pixels</param>
        /// <param name="LineColor">Color of the perimeter of the circle</param>
        /// <param name="FillColor">Color to fill the circle with, or null if an unfilled circle is desired</param>
        public static void Square(this Graphics g, EarthCoordinate center, float Width_pixels, Color? LineColor, Color? FillColor)
        {
            float Radius_pixels = Width_pixels / 2f;
            float UnitsPerPixelX = g.GetUnitsPerPixelX();
            float UnitsPerPixelY = g.GetUnitsPerPixelY();
            EarthCoordinate Offset = new EarthCoordinate(Radius_pixels * UnitsPerPixelY, Radius_pixels * UnitsPerPixelX);
            EarthCoordinate p1 = center + Offset;
            EarthCoordinate p2 = center - Offset;
            float North = (float)Math.Max(p1.Latitude_degrees, p2.Latitude_degrees);
            float South = (float)Math.Min(p1.Latitude_degrees, p2.Latitude_degrees);
            float West = (float)Math.Min(p1.Longitude_degrees, p2.Longitude_degrees);
            float East = (float)Math.Max(p1.Longitude_degrees, p2.Longitude_degrees);
            PointF[] Points = { new PointF(West, North), new PointF(East, North), new PointF(East, South), new PointF(West, South) };
            if (LineColor != null)
            {
                Pen pen = new Pen(LineColor.Value, (float)(Width_pixels * Math.Max(UnitsPerPixelX, UnitsPerPixelY)));
                g.DrawPolygon(pen, Points);
            }
            if (FillColor != null)
            {
                Brush brush = new SolidBrush(FillColor.Value);
                g.FillPolygon(brush, Points);
            }
        }

        public static void Lines(this Graphics g, EarthCoordinate[] Points, float Width_pixels, Color Color)
        {
            float UnitsPerPixelX = g.GetUnitsPerPixelX();
            float UnitsPerPixelY = g.GetUnitsPerPixelY();
            Pen pen = new Pen(Color, (float)(Width_pixels * Math.Max(UnitsPerPixelX, UnitsPerPixelY)));
            for (int i = 0; i < Points.Length - 1; i++)
                g.DrawLine(pen, (PointF)Points[i], (PointF)Points[i + 1]);
        }

        /// <summary>
        /// Draw a polygon from a list of EarthCoordinates
        /// </summary>
        /// <param name="g"></param>
        /// <param name="center">Center of the square</param>
        /// <param name="Width_pixels">Width of the perimeter line, in pixels</param>
        /// <param name="LineColor">Color of the perimeter of the square</param>
        /// <param name="FillColor">Color to fill the square with, or null if an unfilled square is desired</param>
        public static void Polygon(this Graphics g, EarthCoordinate[] Points, float Width_pixels, Color? LineColor, Color? FillColor)
        {
            PointF[] PointFs = new PointF[Points.Length];
            for (int i = 0; i < Points.Length; i++)
                PointFs[i] = (PointF)Points[i];
            float UnitsPerPixelX = g.GetUnitsPerPixelX();
            float UnitsPerPixelY = g.GetUnitsPerPixelY();
            if (LineColor != null)
            {
                Pen pen = new Pen(LineColor.Value, (float)(Width_pixels * Math.Max(UnitsPerPixelX, UnitsPerPixelY)));
                g.DrawPolygon(pen, PointFs);
            }
            if (FillColor != null)
            {
                Brush brush = new SolidBrush(FillColor.Value);
                g.FillPolygon(brush, PointFs);
            }
        }

        /// <summary>
        /// Draw a line from the start point to the end point, with the specified color
        /// </summary>
        /// <param name="g"></param>
        /// <param name="Start">EarthCoordinate to start drawing</param>
        /// <param name="End">EarthCoordinate to stop drawing</param>
        /// <param name="Width_pixels">Width of the line, in pixels</param>
        /// <param name="Color">Color of the line</param>
        public static void Line(this Graphics g, EarthCoordinate Start, EarthCoordinate End, float Width_pixels, Color Color)
        {
            float UnitsPerPixelX = g.GetUnitsPerPixelX();
            float UnitsPerPixelY = g.GetUnitsPerPixelY();
            Pen pen = new Pen(Color, (float)(Width_pixels * Math.Max(UnitsPerPixelX, UnitsPerPixelY)));
            g.DrawLine(pen, (PointF)Start, (PointF)End);
        }

        /// <summary>
        /// Returns the number of X pixels in each graphics unit in the current Graphics object (X Scale)
        /// </summary>
        /// <param name="g"></param>
        /// <returns></returns>
        public static float GetPixelsPerUnitX(this Graphics g) { return Math.Abs(g.Transform.Elements[0]); }

        /// <summary>
        /// Returns the number of Y pixels in each graphics unit in the current Graphics object (Y Scale)
        /// </summary>
        /// <param name="g"></param>
        /// <returns></returns>
        public static float GetPixelsPerUnitY(this Graphics g) { return Math.Abs(g.Transform.Elements[3]); }

        /// <summary>
        /// Returns the number of graphics units per X pixel in the current Graphics object (1 / (X Scale))
        /// </summary>
        /// <param name="g"></param>
        /// <returns></returns>
        public static float GetUnitsPerPixelX(this Graphics g) { return 1f / Math.Abs(g.Transform.Elements[0]); }

        /// <summary>
        /// Returns the number of graphics units per Y pixel in the current Graphics object (1 / (Y Scale))
        /// </summary>
        /// <param name="g"></param>
        /// <returns></returns>
        public static float GetUnitsPerPixelY(this Graphics g) { return 1f / Math.Abs(g.Transform.Elements[3]); }

        /// <summary>
        /// Returns the number of X pixels the origin has been offset in the current Graphics object (X Translate)
        /// </summary>
        /// <param name="g"></param>
        /// <returns></returns>
        public static float GetOriginOffsetX_pixels(this Graphics g) { return g.Transform.Elements[4]; }

        /// <summary>
        /// Returns the number of Y pixels the origin has been offset in the current Graphics object (Y Translate)
        /// </summary>
        /// <param name="g"></param>
        /// <returns></returns>
        public static float GetOriginOffsetY_pixels(this Graphics g) { return g.Transform.Elements[5]; }
    }
}
