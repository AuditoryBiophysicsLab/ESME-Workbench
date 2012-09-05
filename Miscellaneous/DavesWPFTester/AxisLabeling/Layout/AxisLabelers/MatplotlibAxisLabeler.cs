using System;
using System.Collections.Generic;
using System.Globalization;
using DavesWPFTester.AxisLabeling.Language;
using ESME.Views.Controls;

namespace DavesWPFTester.AxisLabeling.Layout.AxisLabelers
{
    class MatplotlibAxisLabeler : AxisLabeler
    {
        public override Axis Generate(AxisLabelerOptions options, double density)
        {
            //int nbins = 9;      // in the actual Matplotlib implementation this was fixed at 9. here we let it vary like the other methods for a better comparison
            var m = (((options.AxisLocation == AxisLocation.Top || options.AxisLocation == AxisLocation.Bottom) ? options.Screen.Width : options.Screen.Height) * density);
            var nbins = (int)Math.Max(m, 2);

            var steps = new List<double> { 1, 2, 5, 10 };

            var vMin = options.DataRange.Min;
            var vMax = options.DataRange.Max;

            var value = matPlotLibScaleRange(vMin, vMax, nbins);
            var scale = value.Item1;
            var offset = value.Item2;

            vMin -= offset;
            vMax -= offset;

            var rawStep = (vMax - vMin) / nbins;
            var scaledRawStep = rawStep / scale;

            var bestMax = vMax;
            var bestMin = vMin;

            double scaledStep = 1;
            foreach (int step in steps)
            {
                if (step < scaledRawStep) continue;
                scaledStep = step * scale;
                bestMin = scaledStep * Math.Floor(vMin / scaledStep);
                bestMax = bestMin + scaledStep * nbins;
                if (bestMax >= vMax) break;
            }
            var extraBins = (int)Math.Floor((bestMax - vMax) / scaledStep);
            nbins -= extraBins;

            var labels = new List<Tuple<double, string>>();
            var option = options.DefaultAxis();

            //compute actual labels
            for (var i = 0; i <= nbins; i++)
            {
                var labelVal = bestMin + i * scaledStep + offset;
                labels.Add(Tuple.Create(labelVal, labelVal.ToString(CultureInfo.InvariantCulture)));
            }
            option.Labels = labels;
            option.Score = 1;

            option.VisibleRange = new Range(bestMin + offset, bestMin + nbins * scaledStep + offset);

            return option;
        }

        Tuple<double, double> matPlotLibScaleRange(double min, double max, int bins, int threshold = 100)
        {
            var dv = Math.Abs(max - min);
            var maxabsv = Math.Max(Math.Abs(min), Math.Abs(max));
            var epsilon = Math.Pow(10, -12);
            if (maxabsv == 0 || dv / maxabsv < epsilon) return new Tuple<double, double>(1.0, 0.0);
            var meanv = 0.5 * (max + min);
            double offset;
            double exp;

            if (Math.Abs(meanv) / dv < threshold) offset = 0;
            else if (meanv > 0)
            {
                exp = Math.Floor(Math.Log10(meanv));
                offset = Math.Pow(10.0, exp);
            }
            else
            {
                exp = Math.Floor(Math.Log10(-1 * meanv));
                offset = Math.Pow(-10.0, exp);
            }
            exp = Math.Floor(Math.Log10(dv / bins));
            var scale = Math.Pow(10.0, exp);

            return new Tuple<double, double>(scale, offset);
        }
    }
}