using System;
using System.Collections.Generic;
using System.Globalization;
using HRC.Plotting.AxisLabeling.Language.Types;

namespace HRC.Plotting.AxisLabeling.Layout.AxisLabelers
{
    class HeckbertAxisLabeler : AxisLabeler
    {
        static double heckbertNiceNum(double x, bool round)
        {
            var exp = (int)Math.Log10(x);
            var f = x / (Math.Pow(10.0, exp));
            double nf;

            if (round)
                if (f < 1.5) nf = 1;
                else if (f < 3) nf = 2;
                else if (f < 7) nf = 5;
                else nf = 10;
            else if (f <= 1) nf = 1;
            else if (f <= 2) nf = 2;
            else if (f <= 5) nf = 5;
            else nf = 10;
            return nf * Math.Pow(10.0, exp);
        }

        public override Axis Generate(AxisLabelerOptions options, double density)
        {
            var m = (((options.AxisLocation == AxisLocation.Top || options.AxisLocation == AxisLocation.Bottom) ? options.Screen.Width : options.Screen.Height) * density);
            m = Math.Max(m, 2);

            var v = (Numeric)options.Symbol;

            //loose labeling
            var vMin = v.Range.Min;
            var vMax = v.Range.Max;
            var range = heckbertNiceNum(vMax - vMin, false);
            var d = heckbertNiceNum(range / (m - 1), true);
            var rMin = Math.Floor(vMin / d) * d;
            var rMax = Math.Ceiling(vMax / d) * d;

            var labels = new List<AxisLabel>();
            var option = options.DefaultAxis();

            var currX = rMin;
            while (currX <= rMax + 0.5 * d)
            {
                labels.Add(new AxisLabel(currX, currX.ToString(CultureInfo.InvariantCulture)));
                currX += d;
            }

            option.VisibleRange = new Range(rMin, currX - d);

            option.Labels = labels;
            option.Score = 1;

            return option;
        }
    }
}