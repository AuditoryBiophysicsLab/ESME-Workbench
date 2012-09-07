using System;
using System.Collections.Generic;
using DavesWPFTester.AxisLabeling.Language;
using DavesWPFTester.AxisLabeling.Language.Expressions;
using DavesWPFTester.AxisLabeling.Layout.Formatters;
using ESME.Views.Controls;

namespace DavesWPFTester.AxisLabeling.Layout
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
        public List<Tuple<double, string>> Labels { get; set; }
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
            Labels = new List<Tuple<double, string>>();
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
                Labels = new List<Tuple<double, string>>(Labels),
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
}