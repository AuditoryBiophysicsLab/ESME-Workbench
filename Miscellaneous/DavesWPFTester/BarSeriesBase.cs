using System;
using System.Linq;
using System.Windows;
using System.Windows.Shapes;
using HRC;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;

namespace DavesWPFTester
{
    public abstract class BarSeriesBase : SeriesViewModelBase
    {
        [UsedImplicitly] PropertyObserver<BarSeriesBase> _propertyObserver;
        [UsedImplicitly] PropertyObserver<DataAxisViewModel> _xAxisObserver;
        [UsedImplicitly] PropertyObserver<DataAxisViewModel> _yAxisObserver;

        protected BarSeriesBase()
        {
            _propertyObserver = new PropertyObserver<BarSeriesBase>(this)
                .RegisterHandler(d => d.XAxis,
                                 () =>
                                 {
                                     if (XAxis == null) return;
                                     _xAxisObserver = new PropertyObserver<DataAxisViewModel>(XAxis)
                                         .RegisterHandler(d => d.ValueToPosition, RenderShapes);
                                     RenderShapes();
                                 })
                .RegisterHandler(d => d.YAxis,
                                 () =>
                                 {
                                     if (YAxis == null) return;
                                     _yAxisObserver = new PropertyObserver<DataAxisViewModel>(YAxis)
                                         .RegisterHandler(d => d.ValueToPosition, RenderShapes);
                                     RenderShapes();
                                 });
        }

        /// <summary>
        /// The precision used to group X values when creating bars.  Default is 1 (round to tenths).  See documentation of Math.Round for more detail
        /// </summary>
        [Initialize(1)] public int XRoundingPrecision { get; set; }
        /// <summary>
        /// Width of a bar, as a fraction of the minimum x-axis separation seen in the series to be plotted.
        /// A value of 1 means the bar is as wide as possible, with no space between adjacent bars
        /// The default is 0.85
        /// </summary>
        [Initialize(0.85)] public double BarWidth { get; set; }

        internal double MinimumXPlotSpacing { get; set; }
        internal double PlotOriginY { get; set; }

        protected static Rect CreateBarRect(double x, double y, double width, double yOrigin, double xOffset, double yOffset)
        {
            var rect = new Rect(RectLocation(x, y, width, yOrigin), RectSize(width, y, yOrigin));
            rect.Offset(xOffset, yOffset);
            return rect;
        }

        protected static Point RectLocation(double x, double y, double width, double yOrigin)
        {
            var left = x - (width / 2);
            var top = Math.Min(y, yOrigin);
            return new Point(left, top);
        }

        protected static Size RectSize(double width, double height, double yOrigin)
        {
            var actualHeight = Math.Abs(height - yOrigin);
            return new Size(width, actualHeight);
        }

        public override void RenderShapes()
        {
            if (Points == null || Points.Count == 0 || XAxis == null || XAxis.ValueToPosition == null || YAxis == null || YAxis.ValueToPosition == null) return;
            MinimumXPlotSpacing = (from point in Points.Select(point => new Point(XAxis.ValueToPosition(Math.Round((double)point.X, XRoundingPrecision)), YAxis.ValueToPosition(point.Y))).ToList()
                                   select point.X).ToList().AdjacentDifferences().Min();
            PlotOriginY = YAxis.ValueToPosition(Math.Max(YAxis.VisibleRange.Min, 0));
            foreach (var point in Points)
            {
                var rect = CreateBarRect(XAxis.ValueToPosition(Math.Round(point.X, XRoundingPrecision)), YAxis.ValueToPosition(point.Y), MinimumXPlotSpacing * BarWidth, PlotOriginY, 0, 0);
                var shape = RectToShape(rect);
                shape.ToolTip = string.Format("{0:0.###}, {1:0.###}", point.X, point.Y);
                if (!PointShapeMap.ContainsKey(point))
                {
                    PointShapeMap.Add(point, shape);
                    Shapes.Add(shape);
                }
                else
                {
                    var shapeIndex = Shapes.IndexOf(PointShapeMap[point]);
                    PointShapeMap[point] = shape;
                    if (shapeIndex == -1) Shapes.Add(shape);
                    else Shapes[shapeIndex] = shape;
                }
            }
        }
        internal abstract Shape RectToShape(Rect rect);
    }
}