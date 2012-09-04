using System;
using System.Windows;
using System.Windows.Media;
using DavesWPFTester.AxisLabeling.Language;
using Vector = DavesWPFTester.AxisLabeling.Language.Types.Vector;

namespace DavesWPFTester.AxisLabeling.Layout.AxisLabelers
{
    public class AxisLabelerOptions
    {
        public AxisDirection AxisDirection { get; set; }
        public double FontSize { get; set; }
        public FontFamily FontFamily { get; set; }
        public Vector Symbol { get; set; }
        public Range DataRange { get; set; }
        public Range VisibleRange { get; set; }
        public Rect Screen { get; set; }
        public Func<string, double, Axis, Rect> ComputeLabelRect;

        public Axis DefaultAxis() { return new Axis { FontSize = FontSize, FontFamily = FontFamily, AxisDirection = AxisDirection, Symbol = Symbol, VisibleRange = VisibleRange }; }
    }
    public abstract class AxisLabeler
    {
        public abstract Axis Generate(AxisLabelerOptions labeler, double m);
    }

    public class NoOpAxisLabeler : AxisLabeler
    {
        public override Axis Generate(AxisLabelerOptions options, double m) { return options.DefaultAxis(); }
    }
}