using System.Linq;
using System.Text;
using HRC.WPF;

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
            var orderedData = (from d in profile.Data orderby d.Depth select d).ToList();
            var renderFunc = PlotHelpers.GetGlyphRenderFunc(glyphStyle);
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

}
