using System;
using System.Windows;
using System.Windows.Media;
using Vector = HRC.Plotting.AxisLabeling.Language.Types.Vector;

namespace HRC.Plotting.AxisLabeling.Layout.AxisLabelers
{
    public class AxisLabelerOptions
    {
        public AxisLocation AxisLocation { get; set; }
        public double FontSize { get; set; }
        public Typeface Typeface { get; set; }
        public Vector Symbol { get; set; }
        public Range DataRange { get; set; }
        public Range VisibleRange { get; set; }
        public Rect Screen { get; set; }
        public GeneralTransform AxisTransform { get; set; }
        public Func<string, double, Axis, AxisLabelerOptions, Rect> ComputeLabelRect;

        public Axis DefaultAxis() { return new Axis { FontSize = FontSize, AxisLocation = AxisLocation, Symbol = Symbol, VisibleRange = VisibleRange }; }
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