using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using DavesWPFTester.AxisLabeling.Language;
using DavesWPFTester.AxisLabeling.Layout.Formatters;
using ESME.Views.Controls;

namespace DavesWPFTester.AxisLabeling.Layout.AxisLabelers
{
    /* Implements the axis labeling routine described in 
     *  Talbot, Lin, and Hanrahan. An Extension of Wilkinson’s Algorithm for Positioning Tick Labels on Axes, Infovis 2010.
     */

    class ExtendedAxisLabeler : AxisLabeler
    {
        readonly List<double> Q = new List<double> { 1.0, 5.0, 2.0, 2.5, 4.0, 3.0 };
        readonly List<double> w = new List<double> { 0.25, 0.2, 0.5, 0.05 };
        readonly List<Format> formats;

        void AddUnitFormat(double unit, string name, Range logRange, double weight, double factoredWeight)
        {
            formats.Add(new UnitFormat(unit, name, logRange, false, false, weight));
            formats.Add(new UnitFormat(unit, name, logRange, false, true, weight));
            formats.Add(new UnitFormat(unit, name, logRange, true, false, factoredWeight));
            formats.Add(new UnitFormat(unit, name, logRange, true, true, factoredWeight));
        }

        void AddUnitFormat(double unit, string name, Range logRange, double factoredWeight)
        {
            formats.Add(new UnitFormat(unit, name, logRange, true, false, factoredWeight));
            formats.Add(new UnitFormat(unit, name, logRange, true, true, factoredWeight));
        }

        public ExtendedAxisLabeler()
        {
            formats = new List<Format> { new UnitFormat(1.0, "", new Range(-4, 6), false, false, 1) };
            AddUnitFormat(1000.0, "K", new Range(3, 6), 0.75, 0.4);
            AddUnitFormat(1000000.0, "M", new Range(6, 9), 0.75, 0.4);
            AddUnitFormat(1000000000.0, "B", new Range(9, 12), 0.75, 0.4);
            AddUnitFormat(100.0, "hundred", new Range(2, 3), 0.35);
            AddUnitFormat(1000.0, "thousand", new Range(3, 6), 0.5);
            AddUnitFormat(1000000.0, "million", new Range(6, 9), 0.5);
            AddUnitFormat(1000000000.0, "billion", new Range(9, 12), 0.5);
            AddUnitFormat(0.01, "hundredth", new Range(-2, -3), 0.3);
            AddUnitFormat(0.001, "thousandth", new Range(-3, -6), 0.5);
            AddUnitFormat(0.000001, "millionth", new Range(-6, -9), 0.5);
            AddUnitFormat(0.000000001, "billionth", new Range(-9, -12), 0.5);
            formats.Add(new ScientificFormat(true, false, 0.3));
            formats.Add(new ScientificFormat(true, true, 0.3));
            formats.Add(new ScientificFormat(false, false, 0.25));
            formats.Add(new ScientificFormat(false, true, 0.25));
        }

        protected double floored_mod(double a, double n) { return a - n * Math.Floor(a / n); }

        protected double simplicity(double stepSize, List<double> stepSizes, int j, double lmin, double lmax, double lstep)
        {
            const double eps = 1e-10;
            var n = stepSizes.Count;
            var i = stepSizes.IndexOf(stepSize) + 1;
            var v = (floored_mod(lmin, lstep) < eps && lmin <= 0 && lmax >= 0) ? 1 : 0;
            if (n <= 1) return 1 - j + v;
            return 1 - (i - 1) / (n - 1) - j + v;
        }

        protected double max_simplicity(double stepSize, List<double> stepSizes, int j)
        {
            var n = stepSizes.Count;
            double i = stepSizes.IndexOf(stepSize) + 1;
            const double v = 1;
            if (n == 1) return 1 - j + v;
            return 1 - (i - 1) / (n - 1) - j + v;
        }

        protected double coverage(double dmin, double dmax, double lmin, double lmax) { return 1 - 0.5 * (((dmax - lmax) * (dmax - lmax) + (dmin - lmin) * (dmin - lmin)) / ((0.1 * (dmax - dmin)) * (0.1 * (dmax - dmin)))); }

        protected double max_coverage(double dmin, double dmax, double span)
        {
            var range = dmax - dmin;

            if (span > range)
            {
                var half = (span - range) / 2;
                return 1 - 0.5 * ((half * half + half * half) / ((0.1 * (dmax - dmin)) * (0.1 * (dmax - dmin))));
            }
            return 1;
        }

        protected double density(double r, double rt) { return (2 - Math.Max(r / rt, rt / r)); }

        protected double max_density(double r, double rt)
        {
            if (r >= rt) return 2 - r / rt;
            return 1;
        }

        public override Axis Generate(AxisLabelerOptions options, double density)
        {
            var formatter = new QuantitativeFormatter(options.Typeface.FontFamily);
            var space = ((options.AxisLocation == AxisLocation.Top || options.AxisLocation == AxisLocation.Bottom) ? options.Screen.Width : options.Screen.Height);

            var dmax = options.DataRange.Max;
            var dmin = options.DataRange.Min;

            if (dmax == dmin) return null;

            Axis best = null;
            double bestScore = -2;

            var j = 1;
            while (j < int.MaxValue)
            {
                foreach (var q in Q)
                {
                    var sm = max_simplicity(q, Q, j);
                    if (w[0] * sm + w[1] + w[2] + w[3] < bestScore)
                    {
                        j = int.MaxValue - 1;
                        break;
                    }

                    var k = 2;
                    while (k < int.MaxValue)
                    {
                        var dm = max_density(k / space, density);

                        if (w[0] * sm + w[1] + w[2] * dm + w[3] < bestScore) break;

                        var delta = (dmax - dmin) / (k + 1) / (j * q);
                        var z = (int)Math.Ceiling(Math.Log10(delta));

                        while (z < int.MaxValue)
                        {
                            var step = j * q * Math.Pow(10, z);
                            var cm = max_coverage(dmin, dmax, step * (k - 1));

                            if (w[0] * sm + w[1] * cm + w[2] * dm + w[3] < bestScore) break;

                            for (var start = (int)(Math.Floor(dmax / step - (k - 1)) * j); start <= (int)(Math.Ceiling(dmin / step)) * j; start++)
                            {
                                var lmin = start * step / j;
                                var lmax = lmin + step * (k - 1);

                                var s = simplicity(q, Q, j, lmin, lmax, step);
                                var d = this.density(k / space, density);
                                var c = coverage(dmin, dmax, lmin, lmax);

                                if (w[0] * s + w[1] * c + w[2] * d + w[3] < bestScore) continue;

                                var option = options.DefaultAxis();

                                var stepSequence = Enumerable.Range(0, k).Select(x => lmin + x * step).ToList();
                                var newlabels = stepSequence.Select(value => new AxisLabel(value, value.ToString(CultureInfo.InvariantCulture))).ToList();

                                option.Labels = newlabels;
                                option.Granularity = d;
                                option.Coverage = c;
                                option.Simplicity = s;
                                option.Score = s + c + d;

                                //format and choose best
                                var subPossibilities = new List<Axis> { option };
                                var optionFormatted = formatter.Format(
                                                                       formatter.varyOrientation(formatter.varyFontSize(subPossibilities, options)),
                                                                       formats,
                                                                       options,
                                                                       a => w[0] * a.Simplicity + w[1] * a.Coverage + w[2] * a.Granularity + w[3] * a.Legibility,
                                                                       bestScore);

                                var score = w[0] * optionFormatted.Simplicity + w[1] * optionFormatted.Coverage +
                                            w[2] * optionFormatted.Granularity + w[3] * optionFormatted.Legibility;

                                if (score <= bestScore) continue;
                                bestScore = score;
                                optionFormatted.Score = score;
                                best = optionFormatted;
                            }

                            z = z + 1;
                        }

                        k = k + 1;
                    }
                }

                j = j + 1;
            }

            if (best == null) Console.WriteLine(@"WARNING: Extended algorithm found 0 solutions");
            else best.VisibleRange = new Range(Math.Min(options.VisibleRange.Min, best.Labels.Min(t => t.Value)), Math.Max(options.VisibleRange.Max, best.Labels.Max(t => t.Value)));
            return best;
        }
    }
}