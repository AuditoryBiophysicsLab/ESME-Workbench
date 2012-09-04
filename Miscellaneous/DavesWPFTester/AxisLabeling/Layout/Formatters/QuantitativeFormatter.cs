using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Media;
using DavesWPFTester.AxisLabeling.Layout.AxisLabelers;

namespace DavesWPFTester.AxisLabeling.Layout.Formatters
{
    class QuantitativeFormatter : Formatter
    {
        static readonly double[] fontSizes = { 7, 8, 9, 10, 12, 14, 18, 20, 24 }; // Latex default font sizes
        readonly Dictionary<double, double> ems;
        // In the paper we had a minimum font size of 5, but that's pretty stinking tiny. 7 is probably a better minimum size.

        public QuantitativeFormatter()
        {
            var fontFamily = new FontFamily("Verdana");
            ems = (from x in fontSizes select new { x, size = fontFamily.LineSpacing * x }).ToDictionary(a => a.x, a => a.size);
        }

        public override Axis Format(List<Axis> list, List<Format> formats, AxisLabelerOptions options, Func<Axis, double> ScoreAxis, double bestScore = double.NegativeInfinity)
        {
            var result = options.DefaultAxis();
            foreach (var data in list)
                foreach (var format in formats)
                {
                    var f = data.Clone();
                    f._formatStyle = format;
                    f.Legibility = legibilityScoreMax(f, options);

                    if (ScoreAxis(f) < bestScore) continue;
                    var labels = f._formatStyle.FormalLabels(f.Labels.Select(x => (object)x.Item1).ToList());
                    f.Labels = f.Labels.Select(x => x.Item1).Zip(labels.Item1, (a, b) => new Tuple<double, string>(a, b)).ToList();
                    f.AxisTitleExtension = labels.Item2;
                    f.Legibility = legibilityScore(f, options);
                    f.Score = ScoreAxis(f);
                    if (f.Score < bestScore) continue;
                    bestScore = f.Score;
                    result = f;
                }
            return result;
        }

        protected double legibility_format(Axis data, AxisLabelerOptions options)
        {
            var format = data._formatStyle.Score(data.Labels.Select(x => (object)x.Item1).ToList());
            return format;
        }

        protected double legibility_fontSize(Axis data, AxisLabelerOptions options)
        {
            var fsmin = fontSizes.Min();
            return (data.FontSize > options.FontSize || data.FontSize < fsmin)
                       ? double.NegativeInfinity
                       : ((data.FontSize == options.FontSize)
                              ? 1
                              : 0.2 * ((data.FontSize - fsmin + 1) / (options.FontSize - fsmin)));
        }

        protected double legibility_orientation(Axis data, AxisLabelerOptions options) { return data.LabelDirection == AxisDirection.Horizontal ? 1.0 : -0.5; }

        protected double legibility_overlap(Axis data, AxisLabelerOptions options)
        {
            // compute overlap score
            var em = ems[data.FontSize];
            var rects = data.Labels.Select(s => options.ComputeLabelRect(s.Item2, s.Item1, data)).ToList();
            // takes adjacent pairs of rectangles
            var overlap = rects.Take(rects.Count() - 1).Zip(rects.Skip(1),
                                                            (a, b) =>
                                                            {
                                                                var dist = (options.AxisDirection == AxisDirection.Horizontal) ? b.Left - a.Right : a.Top - b.Bottom;
                                                                return Math.Min(1, 2 - (1.5 * em) / Math.Max(0, dist));
                                                            }).Min();
            return overlap;
        }

        protected double legibilityScoreMax(Axis data, AxisLabelerOptions options)
        {
            return (legibility_format(data, options) +
                    legibility_fontSize(data, options) +
                    legibility_orientation(data, options) +
                    1) / 4;
        }

        protected double legibilityScore(Axis data, AxisLabelerOptions options)
        {
            return (legibility_format(data, options) +
                    legibility_fontSize(data, options) +
                    legibility_orientation(data, options) +
                    legibility_overlap(data, options)) / 4;
        }

        public List<Axis> varyFontSize(List<Axis> list, AxisLabelerOptions options)
        {
            var possibilities = new List<Axis>();
            // Reverse to produce the font sizes in decreasing order of goodness
            foreach (var size in fontSizes.Where(s => s <= options.FontSize).Reverse())
                foreach (var data in list)
                {
                    var option = data.Clone();
                    option.FontSize = size;
                    possibilities.Add(option);
                }
            return possibilities;
        }

        public List<Axis> varyOrientation(List<Axis> list)
        {
            var possibilities = new List<Axis>();
            foreach (var data in list)
            {
                var option = data.Clone();
                option.LabelDirection = AxisDirection.Horizontal;
                possibilities.Add(option);
            }

            foreach (var data in list)
            {
                var option = data.Clone();
                option.LabelDirection = AxisDirection.Vertical;
                possibilities.Add(option);
            }
            return possibilities;
        }
    }
}