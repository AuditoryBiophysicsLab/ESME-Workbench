using System;
using System.Collections.Generic;
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
            if (Points.Count == 0 || YAxis == null || XAxis == null || XAxis.ValueToPosition == null || YAxis.ValueToPosition == null) return;

            var plotPoints = Points.Select(point => new Point(XAxis.ValueToPosition(point.X), YAxis.ValueToPosition(point.Y))).ToList();
            var minXPlotDelta = (from point in plotPoints
                                                orderby point.X ascending
                                                select Math.Round(point.X, XRoundingPrecision)).ToList().AdjacentDifferences().Min();

            BarPlotWidth = minXPlotDelta * BarWidth;
            foreach (var plotPoint in plotPoints)
            {
                var dataPoint = Points[plotPoints.IndexOf(plotPoint)];
                //Debug.WriteLine("Drawing bar from: (left: {0:0.#}, bottom: {1:0.#}) to (right: {2:0.#}, top: {3:0.#})", left, bottom, right, top);
                var bar = new Path
                {
                    Stroke = Stroke,
                    StrokeThickness = StrokeThickness,
                    Fill = Fill,
                    Data = new RectangleGeometry(PointToRect(plotPoint)),
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
        [UsedImplicitly] CollectionObserver _pointsObserver;
        [UsedImplicitly] PropertyObserver<SeriesViewModelBase> _propertyObserver;

        protected BarSeriesBase()
        {
            _propertyObserver = new PropertyObserver<SeriesViewModelBase>(this)
                .RegisterHandler(d => d.XAxis, ProcessPoints)
                .RegisterHandler(d => d.YAxis, ProcessPoints);

            _pointsObserver = new CollectionObserver(Points).RegisterHandler(PointsCollectionChanged);
        }

        void PointsCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            if (XAxis == null || XAxis.ValueToPosition == null || YAxis == null || YAxis.ValueToPosition == null) return;
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (var point in args.NewItems.Cast<Point>().Where(point => !DataToPlotPointDictionary.ContainsKey(point))) DataToPlotPointDictionary.Add(point, new Point(XAxis.ValueToPosition(point.X), YAxis.ValueToPosition(point.Y)));
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (Point point in args.OldItems) DataToPlotPointDictionary.Remove(point);
                    break;
                case NotifyCollectionChangedAction.Replace:
                    foreach (Point point in args.OldItems) DataToPlotPointDictionary.Remove(point);
                    foreach (var point in args.NewItems.Cast<Point>().Where(point => !DataToPlotPointDictionary.ContainsKey(point))) DataToPlotPointDictionary.Add(point, new Point(XAxis.ValueToPosition(point.X), YAxis.ValueToPosition(point.Y)));
                    break;
                case NotifyCollectionChangedAction.Reset:
                    DataToPlotPointDictionary.Clear();
                    break;
                case NotifyCollectionChangedAction.Move:
                    throw new NotImplementedException("Move not implemented for BarSeriesBase data point collection");
            }
        }

        protected Rect PointToRect(Point plotPoint) { return new Rect(PointToRectLocation(plotPoint), PointToRectSize(plotPoint)); }

        protected Point PointToRectLocation(Point plotPoint)
        {
            var left = plotPoint.X - (BarPlotWidth / 2);
            var yOrigin = YAxis.ValueToPosition(Math.Max(YAxis.VisibleRange.Min, 0));
            var top = Math.Min(plotPoint.Y, yOrigin);
            return new Point(left, top);
        }

        protected Size PointToRectSize(Point plotPoint)
        {
            var yOrigin = YAxis.ValueToPosition(Math.Max(YAxis.VisibleRange.Min, 0));
            var height = Math.Abs(plotPoint.Y - yOrigin);
            return new Size(BarPlotWidth, height);
        }

        void ProcessPoints()
        {
            if (Points == null || Points.Count == 0 || XAxis == null || XAxis.ValueToPosition == null || YAxis == null || YAxis.ValueToPosition == null) return;
            foreach (var point in Points.Where(point => !DataToPlotPointDictionary.ContainsKey(point))) DataToPlotPointDictionary.Add(point, new Point(XAxis.ValueToPosition(point.X), YAxis.ValueToPosition(point.Y)));
            RenderShapes();
        }

        readonly protected Dictionary<Point, Point> DataToPlotPointDictionary = new Dictionary<Point, Point>();

        internal double BarPlotWidth { get; set; }

    }
}