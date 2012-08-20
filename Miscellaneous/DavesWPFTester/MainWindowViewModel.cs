using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using System.Windows.Shapes;
using ESME.Views.Controls;
using HRC.Aspects;
using HRC.Services;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;
using MEFedMVVM.ViewModelLocator;
using System.ComponentModel.Composition;

namespace DavesWPFTester
{
    [ExportViewModel("MainWindowViewModel")]
    public class MainWindowViewModel : ViewModelBase
    {
        readonly IViewAwareStatus _viewAwareStatus;

        [ImportingConstructor]
        public MainWindowViewModel(IViewAwareStatus viewAwareStatus)
        {
            _viewAwareStatus = viewAwareStatus;
            _viewAwareStatus.ViewLoaded += () =>
            {
                ShapeCanvas = ((MainWindow)_viewAwareStatus.View).ShapeCanvas;
                ShapeCanvas.SizeChanged += (s, e) => DrawGridShapes();
                DrawGridShapes();
                PlotSamplePoints();
            };
            PropertyChanged += (s, e) =>
            {
                Debug.WriteLine(string.Format("PropertyChanged: {0}", e.PropertyName));
                switch (e.PropertyName)
                {
                    case "XAxisMajorTicks":
                        if (XAxisMajorTicks != null) XAxisMajorTicks.CollectionChanged += TickCollectionChanged;
                        break;
                    case "XAxisMinorTicks":
                        if (XAxisMinorTicks != null) XAxisMinorTicks.CollectionChanged += TickCollectionChanged;
                        break;
                    case "YAxisMajorTicks":
                        if (YAxisMajorTicks != null) YAxisMajorTicks.CollectionChanged += TickCollectionChanged;
                        break;
                    case "YAxisMinorTicks":
                        if (YAxisMinorTicks != null) YAxisMinorTicks.CollectionChanged += TickCollectionChanged;
                        break;
                    case "XAxis":
                    case "YAxis":
                        PlotSamplePoints();
                        break;
                    case "XTestFunc":
                        if (XTestFunc != null) Debug.WriteLine(string.Format("XTestFunc(1) result: {0}", XTestFunc(1)));
                        break;
                }
            };
        }

        #region ViewClosingCommand

        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand { get { return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(vcArgs => Properties.Settings.Default.Save())); } }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;

        #endregion

        public ShapeCanvas ShapeCanvas { get; private set; }
        public DataAxis XAxis { get; set; }
        public DataAxis YAxis { get; set; }
        public ObservableList<AxisTick> XAxisMajorTicks { get; set; }
        public ObservableList<AxisTick> XAxisMinorTicks { get; set; }
        public ObservableList<AxisTick> YAxisMajorTicks { get; set; }
        public ObservableList<AxisTick> YAxisMinorTicks { get; set; }
        public Func<double, double> XTestFunc { get; set; }

        public DrawingImage DisplayImage { get; set; }

        void TickCollectionChanged(object sender, NotifyCollectionChangedEventArgs args) { DrawGridShapes(); }

        void DrawGridDrawingContext()
        {
            if (XAxisMajorTicks == null || XAxisMinorTicks == null || YAxisMajorTicks == null || YAxisMinorTicks == null) return;
            if (XAxisMajorTicks.Count == 0 || XAxisMinorTicks.Count == 0 || YAxisMajorTicks.Count == 0 || YAxisMinorTicks.Count == 0) return;
            var grayPen = new Pen(Brushes.LightGray, 1);
            var blackPen = new Pen(Brushes.Black, 1);
            var drawing = new DrawingGroup();
            var endX = ShapeCanvas.ActualWidth;
            var endY = ShapeCanvas.ActualHeight;
            using (var dc = drawing.Open())
            {
                foreach (var xMinor in XAxisMinorTicks) dc.DrawLine(grayPen, new Point(xMinor.Location, 0), new Point(xMinor.Location, endY));
                foreach (var yMinor in YAxisMinorTicks) dc.DrawLine(grayPen, new Point(0, yMinor.Location), new Point(endX, yMinor.Location));
                foreach (var xMajor in XAxisMajorTicks.Skip(1).Take(XAxisMajorTicks.Count - 2)) dc.DrawLine(blackPen, new Point(xMajor.Location, 0), new Point(xMajor.Location, endY));
                foreach (var yMajor in YAxisMajorTicks.Skip(1).Take(YAxisMajorTicks.Count - 2)) dc.DrawLine(blackPen, new Point(0, yMajor.Location), new Point(endX, yMajor.Location));
            }
            DisplayImage = new DrawingImage(drawing);
        }

        void PlotSamplePoints()
        {
            if (XAxis == null || YAxis == null) return;
            var xMin = Math.Min(XAxis.StartValue, XAxis.EndValue);
            var xMax = Math.Max(XAxis.StartValue, XAxis.EndValue);
            var xRange = xMax - xMin;
            var yMin = Math.Min(YAxis.StartValue, YAxis.EndValue);
            var yMax = Math.Max(YAxis.StartValue, YAxis.EndValue);
            var yRange = yMax - yMin;
            var random = new Random();
            for (var xCount = 0; xCount < 10; xCount++)
                for (var yCount = 0; yCount < 10; yCount++)
                {
                    var xValue = (random.NextDouble() * xRange) + xMin;
                    var yValue = (random.NextDouble() * yRange) + yMin;
                    var geometry = new StreamGeometry();
                    using (var geometryContext = geometry.Open()) CreateDataPoint(geometryContext, XAxis, YAxis, xValue, yValue, 5);
                    geometry.Freeze();
                    Shapes.Add(new Path
                    {
                        Stroke = Brushes.Red,
                        Fill = Brushes.Blue,
                        SnapsToDevicePixels = true,
                        Data = geometry,
                        ToolTip = string.Format("({0}, {1})", xValue, yValue)
                    });
                }
        }

        static void CreateDataPoint(StreamGeometryContext geometryContext, DataAxis xAxis, DataAxis yAxis, double xValue, double yValue, double size)
        {
            var halfSize = size / 2;
            var xCoordinate = xAxis.MappingFunction(xValue);
            var yCoordinate = yAxis.MappingFunction(yValue);
            geometryContext.BeginFigure(new Point(xCoordinate - halfSize, yCoordinate), true, true);
            geometryContext.ArcTo(new Point(xCoordinate + halfSize, yCoordinate), new Size(halfSize, halfSize), 180, false, SweepDirection.Clockwise, true, false);
            geometryContext.ArcTo(new Point(xCoordinate - halfSize, yCoordinate), new Size(halfSize, halfSize), 180, false, SweepDirection.Clockwise, true, false);
        }

        void DrawGridShapes()
        {
            if (ShapeCanvas == null) return;
            if (XAxisMajorTicks == null || XAxisMinorTicks == null || YAxisMajorTicks == null || YAxisMinorTicks == null) return;
            if (XAxisMajorTicks.Count == 0 || XAxisMinorTicks.Count == 0 || YAxisMajorTicks.Count == 0 || YAxisMinorTicks.Count == 0) return;
            var endX = ShapeCanvas.ActualWidth;
            var endY = ShapeCanvas.ActualHeight;

            Shapes.Clear();
            Shapes.Add(CreateAxisLines(XAxisMinorTicks, endY, true, Brushes.LightGray, 1));
            Shapes.Add(CreateAxisLines(YAxisMinorTicks, endX, false, Brushes.LightGray, 1));
            Shapes.Add(CreateAxisLines(XAxisMajorTicks.Skip(1).Take(XAxisMajorTicks.Count - 2), endY, true, Brushes.Black, 1));
            Shapes.Add(CreateAxisLines(YAxisMajorTicks.Skip(1).Take(YAxisMajorTicks.Count - 2), endX, false, Brushes.Black, 1));
            PlotSamplePoints();
        }

        static Path CreateAxisLines(IEnumerable<AxisTick> ticks, double length, bool isVertical, Brush brush, double strokeThickness)
        {
            var geometry = new StreamGeometry();
            using (var geometryContext = geometry.Open()) 
                foreach (var coordinate in ticks) 
                    CreateAxisLine(geometryContext, coordinate.Location, length, isVertical);
            geometry.Freeze();
            return new Path
            {
                Stroke = brush,
                StrokeThickness = strokeThickness,
                SnapsToDevicePixels = true,
                Data = geometry,
            };
        }

        static void CreateAxisLine(StreamGeometryContext geometryContext, double location, double length, bool isVertical)
        {
            if (isVertical)
            {
                geometryContext.BeginFigure(new Point(location, 0), false, false);
                geometryContext.LineTo(new Point(location, length), true, false);
            }
            else
            {
                geometryContext.BeginFigure(new Point(0, location), false, false);
                geometryContext.LineTo(new Point(length, location), true, false);
            }
        }

        [Initialize] public ObservableCollection<Shape> Shapes { get; set; }
    }

}