using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;

namespace HRC.WPF
{
    public static class PlotHelpers
    {
        public static Func<double, double, double, string> GetGlyphRenderFunc(GlyphStyle glyphStyle)
        {
            switch (glyphStyle)
            {
                case GlyphStyle.Line:
                    return (x, y, size) => String.Format("L {0},{1} ", x, y);  //todo: needs M ? 
                case GlyphStyle.Circle:
                    return (x, y, size) => String.Format("M {0},{1} m -{2},0 a {2},{2} 180 1 1 {3},0 {2},{2} 180 1 1 -{3},0 ", x, y, size / 2, size);
                case GlyphStyle.Square:
                    return (x, y, size) => String.Format("M {0},{1} m {2},{2} h {3} v {3} h -{3} v -{3}", x, y, -size / 2, size);
                case GlyphStyle.UpTriangle:
                    return (x, y, size) => String.Format("M {0},{1} m {2},{3} l {4},{5} l {6},{7} z ", x, y, -Math.Cos(Math.PI / 6) * (size / 2), Math.Sin(Math.PI / 6) * (size / 2), Math.Cos(Math.PI / 3) * size, -Math.Sin(Math.PI / 3) * size, Math.Cos(Math.PI / 3) * size, Math.Sin(Math.PI / 3) * size);
                default:
                    throw new InvalidEnumArgumentException();
            }
        }

        public static string GetGrid(ICollection<double> horizontalTicks, ICollection<double> verticalTicks, int skipFactor, double height, double width)
        {
            var sb = new StringBuilder();
            foreach (var tick in horizontalTicks.Skip(skipFactor))
                sb.Append(String.Format("M 0,{0} H {1}", tick, width));
            foreach (var tick in verticalTicks.Skip(skipFactor))
                sb.Append(String.Format("M {0},0 V {1}", tick, height));
            return sb.ToString();
        }
    }

    public enum GlyphStyle
    {
        Line,
        UpTriangle,
        Circle,
        Square,
    }
}
