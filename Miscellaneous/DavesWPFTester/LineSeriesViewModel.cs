using System;
using System.Collections.Specialized;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using HRC;
using Brush = System.Windows.Media.Brush;
using Path = System.Windows.Shapes.Path;
using Point = System.Windows.Point;

namespace DavesWPFTester
{
    public class LineSeriesViewModel : SeriesViewModelBase
    {
        [UsedImplicitly] PropertyObserver<LineSeriesViewModel> _propertyObserver;
        [UsedImplicitly] CollectionObserver _pointsObserver;
        public LineSeriesViewModel()
        {
            _propertyObserver = new PropertyObserver<LineSeriesViewModel>(this)
                .RegisterHandler(d => d.MarkerType, MarkerPropertiesChanged)
                .RegisterHandler(d => d.MarkerStrokeThickness, MarkerPropertiesChanged)
                .RegisterHandler(d => d.MarkerSize, MarkerPropertiesChanged)
                .RegisterHandler(d => d.MarkerStroke, MarkerPropertiesChanged)
                .RegisterHandler(d => d.LineStroke, LinePropertiesChanged)
                .RegisterHandler(d => d.LineStrokeDashArray, LinePropertiesChanged)
                .RegisterHandler(d => d.LineStrokeThickness, LinePropertiesChanged);
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

        protected override void RenderSample()
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
            SampleImageSource = rtb;
        }

        public override void RenderShapes()
        {
            RenderLine();
            RenderMarkers();
        }

        Shape _lineShape;

        void RenderLine()
        {
            if (XAxisMappingFunction == null || YAxisMappingFunction == null) return;
            if (!DrawLine)
            {
                if (_lineShape != null)
                {
                    Shapes.Remove(_lineShape);
                    _lineShape = null;
                }
                return;
            }
            if (_lineShape != null)
            {
                //Debug.WriteLine(string.Format("Re-rendering line for series {0}", SeriesName));
                Shapes.Remove(_lineShape);
            }
            //else Debug.WriteLine(string.Format("Rendering line for series {0}", SeriesName));
            var lineGeometry = new StreamGeometry();
            var lineContext = lineGeometry.Open();
            var isFirst = true;
            foreach (var plotPoint in Points.Select(point => new Point(XAxisMappingFunction(point.X), YAxisMappingFunction(point.Y)))) 
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
                    PointShapeMap.Clear();
                }
                else foreach (var shape in Shapes.Where(shape => shape != _lineShape).ToList()) Shapes.Remove(shape);
            }
        }

        void RenderMarker(Point point)
        {
            if (XAxisMappingFunction == null || YAxisMappingFunction == null) return;
            var plotPoint = new Point(XAxisMappingFunction(point.X), YAxisMappingFunction(point.Y));
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
            if (!PointShapeMap.ContainsKey(point))
            {
                PointShapeMap.Add(point, marker);
                Shapes.Add(marker);
            }
            else
            {
                var shapeIndex = Shapes.IndexOf(PointShapeMap[point]);
                PointShapeMap[point] = marker;
                if (shapeIndex == -1) Shapes.Add(marker);
                else Shapes[shapeIndex] = marker;
            }
        }

        void RemoveMarker(Point point)
        {
            if (!PointShapeMap.ContainsKey(point)) return;
            Shapes.Remove(PointShapeMap[point]);
        }


        void PointsCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            RenderLine();
        }

        protected override void AddPoint(Point newPoint)
        {
            base.AddPoint(newPoint);
            if (DrawMarker) RenderMarker(newPoint);
        }

        protected override void RemovePoint(Point oldPoint)
        {
            RemoveMarker(oldPoint);
        }

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
    }
}