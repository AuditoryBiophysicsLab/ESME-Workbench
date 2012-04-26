using System;
using System.ComponentModel;
using System.Linq;
using System.Text;

namespace ESME.Environment
{
    public static class SoundSpeedProfileExtensions
    {
        public static string GetGeometry(this SoundSpeedProfile profile, double height, double width, double glyphSize = 5, GlyphStyle glyphStyle = GlyphStyle.Line)
        {
            var speeds = (from p in profile.Data select p.SoundSpeed).ToArray();
            var depths = (from d in profile.Data select d.Depth).ToArray();
            var sMin = speeds.Min(); 
            var sMax = speeds.Max();
            var sDiff = sMax - sMin;
            sMin -= (float).1 * sDiff;
            sMax += (float).1 * sDiff;
            sDiff = sMax - sMin;
            var dMax = depths.Max();
            var orderedData = (from d in profile.Data orderby d.Depth select d);

            Func<double, double, double, string> renderFunc;
            switch (glyphStyle)
            {
                case GlyphStyle.Line:
                    renderFunc = (x, y, size) => string.Format("L {0},{1} ", x, y);
                    break;
                case GlyphStyle.Circle:
                    renderFunc = (x, y, size) => string.Format("M {0},{1} m -{2},0 a {2},{2} 180 1 1 {3},0 {2},{2} 180 1 1 -{3},0 ", x, y, size / 2, size);
                    break;
                case GlyphStyle.Square:
                    renderFunc = (x, y, size) => string.Format("M {0},{1} m {2},{2} h {3} v {3} h -{3} v -{3}", x, y, -size / 2, size);
                    break;
                case GlyphStyle.Triangle:
                    renderFunc = (x, y, size) => string.Format("M {0},{1} m {2},{3} l {4},{5} l {6},{7} z ", x, y, -Math.Cos(Math.PI / 6) * (size / 2), Math.Sin(Math.PI / 6) * (size / 2), Math.Cos(Math.PI / 3) * size, -Math.Sin(Math.PI / 3) * size, Math.Cos(Math.PI / 3) * size, Math.Sin(Math.PI / 3) * size);
                    break;
                default:
                    throw new InvalidEnumArgumentException();
            }

            var sb = new StringBuilder();
            foreach (var t in orderedData)
            {
                var y = t.Depth * (height / dMax);
                var x = (t.SoundSpeed - sMin) * (width / sDiff);
                sb.Append(sb.Length == 0 ? string.Format("M {0},{1} ", x, y) : renderFunc(x, y, glyphSize));    
            }
            return sb.ToString();

        }
    }

    public enum GlyphStyle
    {
        Line,
        Triangle,
        Circle,
        Square,
    }
}
