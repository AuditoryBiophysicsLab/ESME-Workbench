using System;
using System.Collections;
using System.Collections.ObjectModel;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using ESME.Views.Controls;
using HRC.Aspects;
using HRC.ViewModels;
using Brush = System.Windows.Media.Brush;
using Path = System.Windows.Shapes.Path;
using Point = System.Windows.Point;

namespace DavesWPFTester
{
    public class DataSeriesViewModel : ViewModelBase, ISeries
    {
        PropertyObserver<DataSeriesViewModel> _observer;
        public DataSeriesViewModel()
        {
            _observer = new PropertyObserver<DataSeriesViewModel>(this)
                .RegisterHandler(d => d.MarkerType, RenderSample)
                .RegisterHandler(d => d.MarkerStrokeThickness, RenderSample)
                .RegisterHandler(d => d.MarkerSize, RenderSample)
                .RegisterHandler(d => d.MarkerStroke, RenderSample)
                .RegisterHandler(d => d.MarkerFill, RenderSample)
                .RegisterHandler(d => d.LineStroke, RenderSample)
                .RegisterHandler(d => d.LineStrokeDashArray, RenderSample)
                .RegisterHandler(d => d.LineStrokeThickness, RenderSample);
        }

        //string _randomFileName;
        void RenderSample(DataSeriesViewModel vm)
        {
            StreamGeometry geometry;
            var canvas = new Canvas { Width = Math.Max(MarkerSize + 10, 10), Height = Math.Max(MarkerSize + 2, 10), SnapsToDevicePixels = true, Background = Brushes.Transparent };
            if (LineStrokeThickness > 0 && LineStroke != null)
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
            if (MarkerType != null && MarkerStrokeThickness > 0 && MarkerSize > 0 && (MarkerStroke != null || MarkerFill != null))
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
                    Fill = MarkerFill,
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
            // If we need to render a line for this series, do so
            if ((LineStrokeThickness > 0 && LineStroke != null))
            {
                var lineGeometry = (LineStroke == null) ? null : new StreamGeometry();
                var lineContext = lineGeometry == null ? null : lineGeometry.Open();
                var isFirst = true;
                Shapes.Clear();
                foreach (var item in SeriesData)
                {
                    var dataPoint = ItemToPoint(item);
                    var plotPoint = new Point(XAxis.MappingFunction(dataPoint.X), YAxis.MappingFunction(dataPoint.Y));
                    if (lineContext == null) continue;
                    if (isFirst)
                    {
                        lineContext.BeginFigure(plotPoint, false, false);
                        isFirst = false;
                    }
                    else lineContext.LineTo(plotPoint, true, true);
                }
                if (lineContext != null)
                {
                    lineContext.Close();
                    Shapes.Add(new Path
                    {
                        Stroke = LineStroke,
                        StrokeDashArray = LineStrokeDashArray,
                        StrokeThickness = LineStrokeThickness,
                        Data = lineGeometry,
                    });
                }
            }
            // If we need to render a marker for this series, do so
            if (MarkerSize > 0 || (MarkerStrokeThickness > 0 && MarkerStroke != null) || MarkerFill != null) 
                foreach (var item in SeriesData) Shapes.Add(RenderMarker(item));
        }

        Shape RenderMarker(object dataItem)
        {
            var dataPoint = ItemToPoint(dataItem);
            var plotPoint = new Point(XAxis.MappingFunction(dataPoint.X), YAxis.MappingFunction(dataPoint.Y));
            var geometry = new StreamGeometry();
            var context = geometry.Open();
            MarkerType(context, plotPoint, MarkerSize);
            context.Close();
            return new Path
            {
                Stroke = MarkerStroke,
                StrokeThickness = MarkerStrokeThickness,
                Fill = MarkerFill,
                Data = geometry,
            };
        }

        [Initialize] public ObservableCollection<Shape> Shapes { get; set; }

        public ICollection SeriesData { get; set; }

        public Func<object, Point> ItemToPoint { get; set; }

        public Action<StreamGeometryContext, Point, double> MarkerType { get; set; }

        public ImageSource SampleImageSource { get; set; }

        public double MarkerStrokeThickness { get; set; }

        public double MarkerSize { get; set; }

        public Brush MarkerStroke { get; set; }

        public Brush MarkerFill { get; set; }

        public double LineStrokeThickness { get; set; }

        public Brush LineStroke { get; set; }

        public DoubleCollection LineStrokeDashArray { get; set; }

        public DataAxis XAxis { get; set; }

        public DataAxis YAxis { get; set; }

        public string SeriesName { get; set; }
    }
}