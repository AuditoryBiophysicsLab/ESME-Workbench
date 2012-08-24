using System;
using System.Collections;
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
using ESME.Views.Controls;
using HRC;
using HRC.Aspects;
using HRC.ViewModels;
using Brush = System.Windows.Media.Brush;
using Path = System.Windows.Shapes.Path;
using Point = System.Windows.Point;

namespace DavesWPFTester
{
    public class DataSeriesViewModel : ViewModelBase, ISeries
    {
        [UsedImplicitly] PropertyObserver<DataSeriesViewModel> _propertyObserver;
        [UsedImplicitly] CollectionObserver _pointsObserver;
        public DataSeriesViewModel()
        {
            _propertyObserver = new PropertyObserver<DataSeriesViewModel>(this)
                .RegisterHandler(d => d.MarkerType, MarkerPropertiesChanged)
                .RegisterHandler(d => d.MarkerStrokeThickness, MarkerPropertiesChanged)
                .RegisterHandler(d => d.MarkerSize, MarkerPropertiesChanged)
                .RegisterHandler(d => d.MarkerStroke, MarkerPropertiesChanged)
                .RegisterHandler(d => d.LineStroke, LinePropertiesChanged)
                .RegisterHandler(d => d.LineStrokeDashArray, LinePropertiesChanged)
                .RegisterHandler(d => d.LineStrokeThickness, LinePropertiesChanged)
                .RegisterHandler(d => d.SeriesData, SeriesDataChanged)
                .RegisterHandler(d => d.ItemToPoint, ProcessSeriesData);
            _pointsObserver = new CollectionObserver(Points).RegisterHandler(PointsCollectionChanged);
        }

        void LinePropertiesChanged()
        {
            DrawLine = LineStrokeThickness > 0 && LineStroke != null;
            RenderSample();
            RenderLine();
        }

        void MarkerPropertiesChanged()
        {
            DrawMarker = MarkerType != null && MarkerStrokeThickness > 0 && MarkerSize > 0 && MarkerStroke != null;
            RenderSample();
            RenderMarkers();
        }

        bool DrawMarker { get; set; }
        bool DrawLine { get; set; }

        //string _randomFileName;
        void RenderSample()
        {
            StreamGeometry geometry;
            var canvas = new Canvas { Width = Math.Max(MarkerSize + 10, 10), Height = Math.Max(MarkerSize + 2, 10), SnapsToDevicePixels = true, Background = Brushes.Transparent };
            if (DrawLine)
            {
                geometry = new StreamGeometry();
                using (var ctx = geometry.Open())
                {
                    ctx.BeginFigure(new Point(0, canvas.Height / 2), false, false);
                    ctx.LineTo(new Point(canvas.Width, canvas.Height / 2), true, false);
                }
                canvas.Children.Add(new Path
                {
                    Stroke = LineStroke,
                    StrokeThickness = LineStrokeThickness,
                    StrokeDashArray = LineStrokeDashArray,
                    Data = geometry,
                });
            }
            if (DrawMarker)
            {
                geometry = new StreamGeometry();
                using (var ctx = geometry.Open())
                {
                    MarkerType(ctx, new Point(canvas.Width / 2, canvas.Height / 2), MarkerSize);
                }
                canvas.Children.Add(new Path
                {
                    Stroke = MarkerStroke,
                    StrokeThickness = MarkerStrokeThickness,
                    Data = geometry,
                });
            }
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
#if false
            if (_randomFileName == null) _randomFileName = System.IO.Path.GetFileNameWithoutExtension(System.IO.Path.GetRandomFileName()) + ".bmp";
            var stream = new FileStream(_randomFileName, FileMode.Create);
            var encoder = new BmpBitmapEncoder();
            encoder.Frames.Add(BitmapFrame.Create(rtb));
            encoder.Save(stream);
            stream.Close();
#endif
            SampleImageSource = rtb;
        }

        public void RenderShapes()
        {
            RenderLine();
            RenderMarkers();
        }

        Shape _lineShape;
        readonly Dictionary<Point, Shape> _pointShapeMap = new Dictionary<Point, Shape>();

        void RenderLine()
        {
            if (!DrawLine)
            {
                if (_lineShape != null)
                {
                    Shapes.Remove(_lineShape);
                    _lineShape = null;
                }
                return;
            }
            if (_lineShape != null) Shapes.Remove(_lineShape);
            var lineGeometry = new StreamGeometry();
            var lineContext = lineGeometry.Open();
            var isFirst = true;
            foreach (var plotPoint in Points.Select(point => new Point(XAxis.MappingFunction(point.X), YAxis.MappingFunction(point.Y)))) 
            {
                if (isFirst)
                {
                    lineContext.BeginFigure(plotPoint, false, false);
                    isFirst = false;
                }
                else lineContext.LineTo(plotPoint, true, true);
            }
            lineContext.Close();
            _lineShape = new Path
            {
                Stroke = LineStroke,
                StrokeDashArray = LineStrokeDashArray,
                StrokeThickness = LineStrokeThickness,
                Data = lineGeometry,
            };
            Shapes.Insert(0, _lineShape);
        }

        void RenderMarkers()
        {
            if (DrawMarker) foreach (var point in Points) RenderMarker(point);
            else
            {
                if (_lineShape == null)
                {
                    Shapes.Clear();
                    _pointShapeMap.Clear();
                }
                else foreach (var shape in Shapes.Where(shape => shape != _lineShape).ToList()) Shapes.Remove(shape);
            }
        }

        void RenderMarker(Point point)
        {
            if (XAxis == null || YAxis == null) return;
            var plotPoint = new Point(XAxis.MappingFunction(point.X), YAxis.MappingFunction(point.Y));
            var geometry = new StreamGeometry();
            var context = geometry.Open();
            MarkerType(context, plotPoint, MarkerSize);
            context.Close();
            var marker = new Path
            {
                Stroke = MarkerStroke,
                StrokeThickness = MarkerStrokeThickness,
                Data = geometry,
                Fill = Brushes.Transparent,
                ToolTip = string.Format("{0:0.###}, {1:0.###}", point.X, point.Y),
            };
            if (!_pointShapeMap.ContainsKey(point))
            {
                _pointShapeMap.Add(point, marker);
                Shapes.Add(marker);
            }
            else
            {
                var shapeIndex = Shapes.IndexOf(_pointShapeMap[point]);
                _pointShapeMap[point] = marker;
                if (shapeIndex == -1) Shapes.Add(marker);
                else Shapes[shapeIndex] = marker;
            }

        }

        void RemoveMarker(Point point)
        {
            if (!_pointShapeMap.ContainsKey(point)) return;
            Shapes.Remove(_pointShapeMap[point]);
        }

        [Initialize] public ObservableCollection<Shape> Shapes { get; set; }
        CollectionObserver _seriesDataObserver;
        public ICollection SeriesData { get; set; }
        void SeriesDataChanged()
        {
            Points.Clear();
            ProcessSeriesData();
            if (SeriesData == null || !(SeriesData is INotifyCollectionChanged)) return;
            var series = (INotifyCollectionChanged)SeriesData;
            if (_seriesDataObserver == null) _seriesDataObserver = new CollectionObserver(series).RegisterHandler(SeriesDataCollectionChanged);
            else
            {
                _seriesDataObserver.UnregisterHandler(SeriesDataCollectionChanged);
                _seriesDataObserver.RegisterHandler(SeriesDataCollectionChanged);
            }
        }

        void SeriesDataCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            if (ItemToPoint == null) return;
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    for (var i = 0; i < args.NewItems.Count; i++) Points.Insert(args.NewStartingIndex + i, ItemToPoint(args.NewItems[i]));
                    break;
                case NotifyCollectionChangedAction.Remove:
                    for (var i = 0; i < args.OldItems.Count; i++) Points.RemoveAt(args.OldStartingIndex);
                    break;
                case NotifyCollectionChangedAction.Replace:
                    for (var i = 0; i < args.NewItems.Count; i++) Points[args.NewStartingIndex + i] = ItemToPoint(args.NewItems[i]);
                    break;
                case NotifyCollectionChangedAction.Reset:
                    Points.Clear();
                    break;
                case NotifyCollectionChangedAction.Move:
                    throw new NotImplementedException("Move not implemented for SeriesData collection");
            }
        }

        void PointsCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (Point point in args.NewItems)
                    {
                        if (DrawMarker) RenderMarker(point);
                        UpdateMinMax(point);
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (Point point in args.OldItems) RemoveMarker(point);
                    break;
                case NotifyCollectionChangedAction.Replace:
                    for (var i = 0; i < args.NewItems.Count; i++)
                    {
                        var oldPoint = (Point)args.OldItems[i];
                        var newPoint = (Point)args.NewItems[i];
                        RemoveMarker(oldPoint);
                        if (DrawMarker) RenderMarker(newPoint);
                        UpdateMinMax(newPoint);
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    Shapes.Clear();
                    _pointShapeMap.Clear();
                    break;
                case NotifyCollectionChangedAction.Move:
                    throw new NotImplementedException("Move not implemented for Points collection");
            }
            RenderLine();
        }

        [Initialize]
        ObservableCollection<Point> Points { get; [UsedImplicitly] set; }
        void ProcessSeriesData()
        {
            if (ItemToPoint == null || SeriesData == null || SeriesData.Count == 0) return;
            foreach(var item in SeriesData) Points.Add(ItemToPoint(item));
            MaxX = Points.Max(p => p.X);
            MinX = Points.Min(p => p.X);
            MaxY = Points.Max(p => p.Y);
            MinY = Points.Min(p => p.Y);
            RenderShapes();
        }

        void UpdateMinMax(Point point)
        {
            MaxX = Math.Max(MaxX, point.X);
            MinX = Math.Min(MinX, point.X);
            MaxY = Math.Max(MaxY, point.Y);
            MinY = Math.Min(MinY, point.Y);
        }

        public double MinX { get; set; }

        public double MaxX { get; set; }

        public double MinY { get; set; }

        public double MaxY { get; set; }

        public Func<object, Point> ItemToPoint { get; set; }

        public ImageSource SampleImageSource { get; set; }

        /// <summary>
        /// An action that adds a Point to a StreamGeometryContext using a given size
        /// </summary>
        public Action<StreamGeometryContext, Point, double> MarkerType { get; set; }
        /// <summary>
        /// Thickness of the stroke used to draw the outline of a marker
        /// </summary>
        public double MarkerStrokeThickness { get; set; }
        /// <summary>
        /// Size of the marker
        /// </summary>
        public double MarkerSize { get; set; }
        /// <summary>
        /// Brush used to stroke the outline of the marker
        /// </summary>
        public Brush MarkerStroke { get; set; }
        /// <summary>
        /// Thickness of the line between series points
        /// </summary>
        public double LineStrokeThickness { get; set; }
        /// <summary>
        /// Brush used to stroke the line between series points.  If null, no line will be drawn
        /// </summary>
        public Brush LineStroke { get; set; }
        /// <summary>
        /// Each Double in the collection specifies the length of a dash or gap relative to the 
        /// Thickness of the pen. For example, a value of 1 creates a dash or gap that has the 
        /// same length as the thickness of the pen (a square).
        /// The first item in the collection, which is located at index 0, specifies the length 
        /// of a dash; the second item, which is located at index 1, specifies the length of a gap
        /// Objects with an even index value specify dashes; objects with an odd index value specify gaps.
        /// </summary>
        public DoubleCollection LineStrokeDashArray { get; set; }

        public DataAxis XAxis { get; set; }

        public DataAxis YAxis { get; set; }

        public string SeriesName { get; set; }
    }
}