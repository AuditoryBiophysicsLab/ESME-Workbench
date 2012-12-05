using System;
using System.Collections.Generic;
using HRC.Plotting.AxisLabeling.Language.Expressions;
using HRC.Plotting.AxisLabeling.Layout.Formatters;

namespace HRC.Plotting.AxisLabeling.Layout
{
    public enum AxisDirection
    {
        Horizontal,
        Vertical
    };

    public class Axis
    {
        public Value Symbol { get; set; }
        public AxisLocation AxisLocation { get; set; }

        // Formatting results
        public Range VisibleRange { get; set; }

        public double FontSize { get; set; }
        public AxisDirection LabelDirection { get; set; }
        
        //tick placement, label text
        public List<AxisLabel> Labels { get; set; }
        public string AxisTitleExtension { get; set; }

        // Statistics and scoring
        public double Score { get; set; }

        public double Simplicity { get; set; }
        public double Coverage { get; set; }
        public double Granularity { get; set; }
        public double Legibility { get; set; }

        //testing purposes
        public Format FormatStyle;

        public Axis()
        {
            Labels = new List<AxisLabel>();
            AxisLocation = AxisLocation.Bottom;
            Score = -10000000;
            FontSize = 12;
            VisibleRange = new Range(0, 0);
            LabelDirection = AxisDirection.Horizontal;

            Simplicity = -100000000;
            Coverage = -1000000000;
            Granularity = -100000000;
            Legibility = -10000000;
            FormatStyle = null;
        }

        public Axis Clone()
        {
            return new Axis
            {
                Symbol = Symbol,
                FontSize = FontSize,
                AxisLocation = AxisLocation,
                Labels = new List<AxisLabel>(Labels),
                Score = Score,
                VisibleRange = VisibleRange,
                LabelDirection = LabelDirection,
                Simplicity = Simplicity,
                Coverage = Coverage,
                Granularity = Granularity,
                Legibility = Legibility,
                FormatStyle = FormatStyle
            };
        }
    }

    public class AxisLabel
    {
        public AxisLabel(double value, string label)
        {
            Value = value;
            Label = label;
        }

        public double Value { get; private set; }
        public string Label { get; private set; }
        public override string ToString() { return string.Format("Value = {0}, Label = {1}", Value, Label); }
    }

    public class AxisLabelEqualityComparer : EqualityComparer<AxisLabel>
    {
        public override bool Equals(AxisLabel x, AxisLabel y) { return Math.Abs(x.Value - y.Value) < 0.0000000001; }
        public override int GetHashCode(AxisLabel obj)
        {
            if (obj == null) return 0;
            var bytes = BitConverter.GetBytes(obj.Value);
            return BitConverter.ToInt32(bytes, 4);
        }
    }
}