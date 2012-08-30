using System;
using System.Collections.Specialized;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using HRC;
using HRC.Aspects;
using HRC.ViewModels;

namespace DavesWPFTester
{
    public class BarSeriesViewModel : SeriesViewModelBase
    {
        [UsedImplicitly] PropertyObserver<BarSeriesViewModel> _propertyObserver;
        [UsedImplicitly] CollectionObserver _pointsObserver;
        public BarSeriesViewModel()
        {
            _propertyObserver = new PropertyObserver<BarSeriesViewModel>(this)
                .RegisterHandler(d => d.StrokeThickness, RenderPropertiesChanged)
                .RegisterHandler(d => d.Stroke, RenderPropertiesChanged)
                .RegisterHandler(d => d.Fill, RenderPropertiesChanged);
            _pointsObserver = new CollectionObserver(Points).RegisterHandler(PointsCollectionChanged);
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

        void PointsCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
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
        public override void RenderShapes()
        {
            if (XAxisMappingFunction == null || YAxisMappingFunction == null || Points.Count == 0) return;
            //Debug.WriteLine("Starting rendering bars");
            var xDataCoords = (from point in Points
                               orderby point.X ascending
                               select Math.Round(point.X, XRoundingPrecision)).ToList();
            var minXDataDelta = double.MaxValue;
            for (var i = 0; i < xDataCoords.Count - 1; i++) minXDataDelta = Math.Min(minXDataDelta, xDataCoords[i + 1] - xDataCoords[i]);
            XMin = xDataCoords.First() - (minXDataDelta / 2);
            XMax = xDataCoords.Last() + (minXDataDelta / 2);

            var plotPoints = Points.Select(point => new Point(XAxisMappingFunction(point.X), YAxisMappingFunction(point.Y))).ToList();
            var xPlotCoords = (from point in plotPoints
                           orderby point.X ascending
                           select Math.Round(point.X, XRoundingPrecision)).ToList();
            var minXPlotDelta = double.MaxValue;
            for (var i = 0; i < xPlotCoords.Count - 1; i++) minXPlotDelta = Math.Min(minXPlotDelta, xPlotCoords[i + 1] - xPlotCoords[i]);
            var barHalfWidth = (minXPlotDelta * BarWidth) / 2;
            var yMinValue = Math.Min(YMin, Points.Select(p => p.Y).Min());
            var yZeroCoordinate = YMin < 0 ? YAxisMappingFunction(0.0) : YAxisMappingFunction(yMinValue);
            foreach (var plotPoint in plotPoints)
            {
                var dataPoint = Points[plotPoints.IndexOf(plotPoint)];
                var left = plotPoint.X - barHalfWidth;
                var right = plotPoint.X + barHalfWidth;
                double top, bottom;
                if (dataPoint.Y <= 0)
                {
                    top = yZeroCoordinate;
                    bottom = plotPoint.Y;
                }
                else
                {
                    top = plotPoint.Y;
                    bottom = yZeroCoordinate;
                }
                //Debug.WriteLine("Drawing bar from: (left: {0:0.#}, bottom: {1:0.#}) to (right: {2:0.#}, top: {3:0.#})", left, bottom, right, top);
                var bar = new Path
                {
                    Stroke = Stroke,
                    StrokeThickness = StrokeThickness,
                    Fill = Fill,
                    Data = new RectangleGeometry(new Rect(new Point(left, bottom), new Point(right, top))),
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
        protected override void AddPoint(Point newPoint)
        {
            UpdateMinMax(newPoint);
            RenderShapes();
        }
    }
}