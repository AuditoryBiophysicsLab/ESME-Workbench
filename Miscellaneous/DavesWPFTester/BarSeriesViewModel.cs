using System;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using HRC;
using HRC.Aspects;
using HRC.ViewModels;
using HRC.Utility;

namespace DavesWPFTester
{
    public class BarSeriesViewModel : BarSeriesBase
    {
        [UsedImplicitly] PropertyObserver<BarSeriesViewModel> _propertyObserver;
        [UsedImplicitly] CollectionObserver _pointsObserver;
        public BarSeriesViewModel()
        {
            _propertyObserver = new PropertyObserver<BarSeriesViewModel>(this)
                .RegisterHandler(d => d.StrokeThickness, RenderPropertiesChanged)
                .RegisterHandler(d => d.Stroke, RenderPropertiesChanged)
                .RegisterHandler(d => d.Fill, RenderPropertiesChanged);
        }

        void RenderPropertiesChanged()
        {
            RenderSample();
            RenderShapes();
        }

        protected override void RenderSample()
        {
            var canvas = new Canvas { Width = 10, Height = 10, SnapsToDevicePixels = true, Background = Brushes.Transparent };
            canvas.Children.Add(new Path
            {
                Stroke = Stroke,
                StrokeThickness = StrokeThickness,
                Fill = Fill,
                Data = new RectangleGeometry(new Rect(0, 0, 10, 10)),
            });
            canvas.Measure(new Size(canvas.Width, canvas.Height));
            canvas.Arrange(new Rect(0, 0, canvas.Width, canvas.Height));
            var dpiX = 96.0;
            var dpiY = 96.0;
            var source = PresentationSource.FromVisual(Application.Current.MainWindow);
            if (source != null && source.CompositionTarget != null)
            {
                var matrix = source.CompositionTarget.TransformToDevice;
                dpiX *= matrix.M11;
                dpiY *= matrix.M22;
            }
            var rtb = new RenderTargetBitmap((int)Math.Round(canvas.Width), (int)Math.Round(canvas.Height), dpiX, dpiY, PixelFormats.Pbgra32);
            rtb.Render(canvas);
            SampleImageSource = rtb;
        }

        /// <summary>
        /// Brush used to stroke the outline of the bar
        /// </summary>
        public Brush Stroke { get; set; }
        /// <summary>
        /// Brush used to fill the interior of the bar
        /// </summary>
        public Brush Fill { get; set; }
        /// <summary>
        /// Thickness of the stroke used to draw the outline of a bar
        /// </summary>
        public double StrokeThickness { get; set; }

        protected override void RenderShapesOverride()
        {
            if (Points.Count == 0 || YAxis == null || XAxis == null || XAxis.ValueToPosition == null || YAxis.ValueToPosition == null) return;

            foreach (var dataPoint in Points)
            {
                var plotPoint = new Point(XAxis.ValueToPosition(dataPoint.X), YAxis.ValueToPosition(dataPoint.Y));
                //Debug.WriteLine("Drawing bar from: (left: {0:0.#}, bottom: {1:0.#}) to (right: {2:0.#}, top: {3:0.#})", left, bottom, right, top);
                var bar = new Path
                {
                    Stroke = Stroke,
                    StrokeThickness = StrokeThickness,
                    Fill = Fill,
                    Data = new RectangleGeometry(CreateBarRect(plotPoint.X, plotPoint.Y, 0, 0)),
                    ToolTip = string.Format("{0:0.###}, {1:0.###}", dataPoint.X, dataPoint.Y),
                };
                if (!PointShapeMap.ContainsKey(dataPoint))
                {
                    PointShapeMap.Add(dataPoint, bar);
                    Shapes.Add(bar);
                }
                else
                {
                    var shapeIndex = Shapes.IndexOf(PointShapeMap[dataPoint]);
                    PointShapeMap[dataPoint] = bar;
                    if (shapeIndex == -1) Shapes.Add(bar);
                    else Shapes[shapeIndex] = bar;
                }
            }
            //Debug.WriteLine("Finished rendering bars");
        }

        protected override void RemovePoint(Point oldPoint)
        {
            if (!PointShapeMap.ContainsKey(oldPoint)) return;
            Shapes.Remove(PointShapeMap[oldPoint]);
        }
        protected override void AddPoint(Point newPoint) { RenderShapes(); }
    }

    public abstract class BarSeriesBase : SeriesViewModelBase
    {
        [UsedImplicitly] PropertyObserver<SeriesViewModelBase> _propertyObserver;

        protected BarSeriesBase()
        {
            _propertyObserver = new PropertyObserver<SeriesViewModelBase>(this)
                .RegisterHandler(d => d.XAxis, XAxisChanged)
                .RegisterHandler(d => d.YAxis, YAxisChanged);
        }

        /// <summary>
        /// The precision used to group X values when creating bars.  Default is 1 (round to tenths).  See documentation of Math.Round for more detail
        /// </summary>
        [Initialize(1)]
        public int XRoundingPrecision { get; set; }
        /// <summary>
        /// Width of a bar, as a fraction of the minimum x-axis separation seen in the series to be plotted.
        /// A value of 1 means the bar is as wide as possible, with no space between adjacent bars
        /// The default is 0.85
        /// </summary>
        [Initialize(0.85)]
        public double BarWidth { get; set; }
        protected double MinimumXPlotSpacing;
        protected double PlotOriginY;

        [UsedImplicitly] PropertyObserver<DataAxisViewModel> _xAxisObserver;
        void XAxisChanged()
        {
            if (XAxis == null) return;
            _xAxisObserver = new PropertyObserver<DataAxisViewModel>(XAxis)
                .RegisterHandler(d => d.ValueToPosition, RenderShapes);
            RenderShapes();
        }
        [UsedImplicitly] PropertyObserver<DataAxisViewModel> _yAxisObserver;
        void YAxisChanged()
        {
            if (YAxis == null) return;
            _yAxisObserver = new PropertyObserver<DataAxisViewModel>(YAxis)
                .RegisterHandler(d => d.ValueToPosition, RenderShapes);
        }

        protected Rect CreateBarRect(double x, double y, double xOffset, double yOffset)
        {
            var rect = new Rect(RectLocation(x, y), RectSize(y));
            rect.Offset(xOffset, yOffset);
            return rect;
        }

        protected Point RectLocation(double x, double y)
        {
            var left = x - ((MinimumXPlotSpacing * BarWidth) / 2);
            var yOrigin = YAxis.ValueToPosition(Math.Max(YAxis.VisibleRange.Min, 0));
            var top = Math.Min(y, yOrigin);
            return new Point(left, top);
        }

        protected Size RectSize(double height)        
        {
            var yOrigin = YAxis.ValueToPosition(Math.Max(YAxis.VisibleRange.Min, 0));
            var actualHeight = Math.Abs(height - yOrigin);
            return new Size(MinimumXPlotSpacing * BarWidth, actualHeight);
        }

        public override void RenderShapes()
        {
            if (Points == null || Points.Count == 0 || XAxis == null || XAxis.ValueToPosition == null || YAxis == null || YAxis.ValueToPosition == null) return;
            // var xAxisWidth = Math.Abs(XAxis.ValueToPosition(XAxis.VisibleRange.Max) - XAxis.ValueToPosition(XAxis.VisibleRange.Min));
            // var yAxisHeight = Math.Abs(YAxis.ValueToPosition(YAxis.VisibleRange.Max) - YAxis.ValueToPosition(YAxis.VisibleRange.Min));
            MinimumXPlotSpacing = (from point in Points.Select(point => new Point(XAxis.ValueToPosition(point.X), YAxis.ValueToPosition(point.Y))).ToList()
                                   orderby point.X ascending
                                   select Math.Round(point.X, XRoundingPrecision)).ToList().AdjacentDifferences().Min();
            PlotOriginY = YAxis.ValueToPosition(Math.Max(YAxis.VisibleRange.Min, 0));
            RenderShapesOverride();
        }

        protected abstract void RenderShapesOverride();
    }

    public class StackedBarSeriesViewModel : BarSeriesViewModel
    {
        
    }
}