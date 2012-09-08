using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using DavesWPFTester.AxisLabeling.Language.Types;
using ESME.Views.Controls;

namespace DavesWPFTester.AxisLabeling.Layout.AxisLabelers
{
    class WilkinsonAxisLabeler : AxisLabeler
    {
        public override Axis Generate(AxisLabelerOptions options, double density) { return Generate(options, density, 0.8); }

        public Axis Generate(AxisLabelerOptions options, double density, double mincoverage)
        {
            var m = (((options.AxisLocation == AxisLocation.Top || options.AxisLocation == AxisLocation.Bottom) ? options.Screen.Width : options.Screen.Height) * density);
            m = Math.Max(m, 2);

            Axis best = null;

            for (var i = 2; i < 12; i++)
            {
                var b = Helper(options, i, mincoverage);
                var granularity = 1 - Math.Abs(i - m) / m;
                if (b == null || (best != null && b.Score + granularity <= best.Score)) continue;
                best = b;
                best.Score += granularity;
            }

            if (best != null) best.VisibleRange = new Range(Math.Min(options.VisibleRange.Min, best.Labels.Min(t => t.Value)), Math.Max(options.VisibleRange.Max, best.Labels.Max(t => t.Value)));

            return best;
        }

        static Axis Helper(AxisLabelerOptions options, double m, double mincoverage = 0.8)
        {
            var snice = double.NegativeInfinity;

            var intervals = (int)Math.Max(Math.Floor(m), 2) - 1;

            var v = (Numeric)options.Symbol;
            var min = v.Range.Min;
            var max = v.Range.Max;

            var Q = new List<double> { 1.0, 10.0, 5.0, 2.0, 2.5, 3.0, 4.0, 1.5, 7.0, 6.0, 8.0, 9.0 };

            var range = max - min;
            var dc = range / intervals;
            var dbase = Math.Pow(10, Math.Floor(Math.Log10(dc)));

            Axis best = null;

            foreach (var q in Q)
            {
                var tdelta = q * dbase;
                var tmin = Math.Floor(min / tdelta) * tdelta;
                var tmax = tmin + intervals * tdelta;

                var i = Q.IndexOf(q);
                var roundness = 1.0 - ((i + 1) - ((tmin <= 0 && tmax >= 0) ? 1.0 : 0.0)) / Q.Count();
                var coverage = (max - min) / (tmax - tmin);

                if (coverage <= mincoverage || tmin > min || tmax < max) continue;
                var score = roundness + coverage;

                if (score <= snice) continue;
                var stepSequence = Enumerable.Range(0, intervals + 1).Select(x => tmin + x * tdelta).ToList();
                //var newlabels = stepSequence.Select(value => Tuple.Create(value, value.ToString(CultureInfo.InvariantCulture))).ToList();
                var newlabels = stepSequence.Select(value => new AxisLabel(value, value.ToString(CultureInfo.InvariantCulture))).ToList();

                var candidate = options.DefaultAxis();
                candidate.Score = score;
                candidate.Labels = newlabels;
                best = candidate;

                snice = score;
            }

            return best;
        }
    }
}