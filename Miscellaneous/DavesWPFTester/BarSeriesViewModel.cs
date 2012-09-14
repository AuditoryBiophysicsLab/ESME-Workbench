using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Diagnostics;
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

        internal override Shape RectToShape(Rect rect)
        {
            //Debug.WriteLine("Drawing bar from: (left: {0:0.#}, bottom: {1:0.#}) to (right: {2:0.#}, top: {3:0.#})", left, bottom, right, top);
            var bar = new Path
            {
                Stroke = Stroke,
                StrokeThickness = StrokeThickness,
                Fill = Fill,
                Data = new RectangleGeometry(rect),
            };
            return bar;
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
            MinimumXPlotSpacing = (from point in Points.Select(point => new Point(XAxis.ValueToPosition(Math.Round(point.X, XRoundingPrecision)), YAxis.ValueToPosition(point.Y))).ToList()
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

    public class StackedBarSeriesViewModel : BarSeriesViewModel
    {
        [UsedImplicitly] PropertyObserver<SeriesViewModelBase> _propertyObserver;
        [UsedImplicitly] PropertyObserver<DataAxisViewModel> _xAxisObserver;
        [UsedImplicitly] PropertyObserver<DataAxisViewModel> _yAxisObserver;
        [UsedImplicitly]
        CollectionObserver _seriesObserver;

        public StackedBarSeriesViewModel()
        {
            _propertyObserver = new PropertyObserver<SeriesViewModelBase>(this)
                .RegisterHandler(d => d.XAxis,
                                 () =>
                                 {
                                     if (XAxis == null) return;
                                     _xAxisObserver = new PropertyObserver<DataAxisViewModel>(XAxis)
                                         .RegisterHandler(d => d.ValueToPosition, RenderShapes);
                                     foreach (var barSeries in BarSeriesCollection) barSeries.XAxis = XAxis; 
                                     RenderShapes();
                                 })
                .RegisterHandler(d => d.YAxis,
                                 () =>
                                 {
                                     if (YAxis == null) return;
                                     _yAxisObserver = new PropertyObserver<DataAxisViewModel>(YAxis)
                                         .RegisterHandler(d => d.ValueToPosition, RenderShapes);
                                     foreach (var barSeries in BarSeriesCollection) barSeries.YAxis = YAxis;
                                     RenderShapes();
                                 });
            _seriesObserver = new CollectionObserver(BarSeriesCollection)
                .RegisterHandler(BarSeriesCollectionChanged);
        }

        [Initialize, UsedImplicitly] public ObservableCollection<BarSeriesBase> BarSeriesCollection { get; private set; }
        void BarSeriesCollectionChanged(INotifyCollectionChanged sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (BarSeriesBase series in args.NewItems)
                    {
                        series.XAxis = XAxis;
                        series.YAxis = YAxis;
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (BarSeriesBase series in args.OldItems)
                    {
                        series.XAxis = null;
                        series.YAxis = null;
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    foreach (BarSeriesBase series in args.OldItems)
                    {
                        series.XAxis = null;
                        series.YAxis = null;
                    }
                    foreach (BarSeriesBase series in args.NewItems)
                    {
                        series.XAxis = XAxis;
                        series.YAxis = YAxis;
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                case NotifyCollectionChangedAction.Move:
                    break;
            }
            RenderShapes();
        }


        public override void RenderShapes()
        {
            if (BarSeriesCollection == null || BarSeriesCollection.Count == 0 || XAxis == null || XAxis.ValueToPosition == null || YAxis == null || YAxis.ValueToPosition == null) return;
            var xCoordinates = new List<double>();
            foreach (var series in BarSeriesCollection) 
            {
                if (!_seriesPlotPointCache.ContainsKey(series)) _seriesPlotPointCache.Add(series, new Dictionary<double, Tuple<Point, double>>());
                _seriesPlotPointCache[series].Clear();
                foreach (var point in series.Points)
                {
                    var plotPoint = new Point(XAxis.ValueToPosition(Math.Round(point.X, XRoundingPrecision)), YAxis.ValueToPosition(point.Y));
                    xCoordinates.Add(plotPoint.X);
                    _seriesPlotPointCache[series][plotPoint.X] = Tuple.Create(point, plotPoint.Y);
                }
                XRange.Add(series.XRange);
            }
            var xPlotCoordinates = xCoordinates.Distinct().ToList();
            foreach (var x in xPlotCoordinates)
                YRange.Add((from series in BarSeriesCollection
                            where _seriesPlotPointCache.ContainsKey(series)
                            select _seriesPlotPointCache[series][x].Item1.Y).Sum());
            MinimumXPlotSpacing = xPlotCoordinates.AdjacentDifferences().Min();
            var width = MinimumXPlotSpacing * BarWidth;
            PlotOriginY = YAxis.ValueToPosition(Math.Max(YAxis.VisibleRange.Min, 0));
            Shapes.Clear();
            //Debug.WriteLine("");
            //Debug.WriteLine("Re-rendering StackedBarSeries");
            foreach (var plotX in xPlotCoordinates)
            {
                var lastDataY = Math.Max(YAxis.VisibleRange.Min, 0);
                //Debug.WriteLine(string.Format("Rendering stacked bars at plotX = {0}", plotX));
                foreach (var series in BarSeriesCollection)
                {
                    if (!_seriesPlotPointCache[series].ContainsKey(plotX)) continue;
                    // This series contains a Y value for the current X, turn it into a rect
                    var value = _seriesPlotPointCache[series][plotX];
                    var curDataY = value.Item1.Y;
                    var curPlotY = YAxis.ValueToPosition(curDataY);
                    var height = Math.Abs(PlotOriginY - curPlotY);
                    if (Math.Abs(height - 0) < double.Epsilon) continue;
                    var lastPlotY = YAxis.ValueToPosition(lastDataY);
                    var topShift = PlotOriginY - lastPlotY;
                    var rect = new Rect(plotX - width / 2, curPlotY - topShift, width, height);
                    //Debug.WriteLine(string.Format("Created rect for series {0}. Data: x = {1}, y = {2}  Rect: left = {3}, top = {4}, width = {5}, height = {6}, right = {7}, bottom = {8}", series.SeriesName, value.Item1.X, value.Item1.Y, rect.Left, rect.Top, rect.Width, rect.Height, rect.Right, rect.Bottom));
                    Shapes.Add(series.RectToShape(rect));
                    lastDataY = curDataY;
                }
            }
        }

        readonly Dictionary<BarSeriesBase, Dictionary<double, Tuple<Point, double>>> _seriesPlotPointCache = new Dictionary<BarSeriesBase, Dictionary<double, Tuple<Point, double>>>();
    }
}