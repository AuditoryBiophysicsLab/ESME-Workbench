using System;
using System.Windows;
using HRC.Plotting.AxisLabeling.Language.Types;
using HRC.Plotting.AxisLabeling.Layout.AxisLabelers;
using Vector = HRC.Plotting.AxisLabeling.Language.Types.Vector;

namespace HRC.Plotting.AxisLabeling.Layout
{
    public class AxisLayout
    {
        public enum Algorithm
        {
            ExtendedWilkinson,
            Wilkinson,
            Heckbert,
            MatPlotLib
        };

        public static Algorithm algorithm = Algorithm.ExtendedWilkinson;

        public static double AxisDensity = 1.0 / 150;
        public static double AxisFontSize = 12.0;
        readonly AxisLabelerOptions _options;
        public AxisLayout(AxisLocation axisLocation, Vector symbol, Range dataRange, Range visibleRange, Func<string, double, Axis, AxisLabelerOptions, Rect> computeLabelRect, Rect screen)
        {
            _options = new AxisLabelerOptions
            {
                AxisLocation = axisLocation,
                Symbol = symbol,
                DataRange = dataRange,
                VisibleRange = visibleRange,
                FontSize = AxisFontSize,
                ComputeLabelRect = computeLabelRect,
                Screen = screen
            };
        }

        public Axis layoutAxis()
        {
            AxisLabeler labeler = new NoOpAxisLabeler();

            if (_options.Symbol is Numeric)
                switch (algorithm)
                {
                    case Algorithm.ExtendedWilkinson:
                        labeler = new ExtendedAxisLabeler();
                        break;
                    case Algorithm.Heckbert:
                        labeler = new HeckbertAxisLabeler();
                        break;
                    case Algorithm.MatPlotLib:
                        labeler = new MatplotlibAxisLabeler();
                        break;
                    case Algorithm.Wilkinson:
                        labeler = new WilkinsonAxisLabeler();
                        break;
                }
            else if (_options.Symbol is Factor) labeler = new CategoricalAxisLabeler();

            return labeler.Generate(_options, AxisDensity);
        }
    }

    public enum AxisLayoutAlgorithm
    {
        ExtendedWilkinson,
        Wilkinson,
        Heckbert,
        MatPlotLib
    };

}