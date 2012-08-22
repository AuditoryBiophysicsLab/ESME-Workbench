using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using ESME.NEMO;
using ESME.Views.Controls;
using HRC.Aspects;
using HRC.Navigation;
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
                var xAxis = ((MainWindow)_viewAwareStatus.View).BottomLinearAxis;
                var yAxis = ((MainWindow)_viewAwareStatus.View).LeftLinearAxis;
                const double rangeStart = 0.0;
                var rangeEnd = MoreMath.TwoPi;
                var rangeStep = MoreMath.TwoPi / 100;
                const int pointSize = 10;
                var wrapper = new SeriesWrapper<ObservableList<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 12)).ToObservableList(),
                    MarkerType = SeriesMarkerType.Plus,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Red,
                    MarkerSize = pointSize,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 12",
                    LineStroke = Brushes.DarkViolet,
                    LineStrokeThickness = 2,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<ObservableList<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 11)).ToObservableList(),
                    MarkerType = SeriesMarkerType.Circle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Green,
                    MarkerSize = pointSize,
                    MarkerFill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 11",
                    LineStroke = Brushes.Red,
                    LineStrokeThickness = 2,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<ObservableList<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 10)).ToObservableList(),
                    MarkerType = SeriesMarkerType.Asterisk,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Blue,
                    MarkerSize = pointSize,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 10",
                    LineStroke = Brushes.Green,
                    LineStrokeThickness = 2,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<ObservableList<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 9)).ToObservableList(),
                    MarkerType = SeriesMarkerType.Cross,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Cyan,
                    MarkerSize = pointSize,
                    MarkerFill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 9",
                    LineStroke = Brushes.Blue,
                    LineStrokeThickness = 2,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);

                wrapper = new SeriesWrapper<ObservableList<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 8)).ToObservableList(),
                    MarkerType = SeriesMarkerType.Square,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Magenta,
                    MarkerSize = pointSize,
                    MarkerFill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 8",
                    LineStroke = Brushes.Cyan,
                    LineStrokeThickness = 2,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<ObservableList<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 7)).ToObservableList(),
                    MarkerType = SeriesMarkerType.Diamond,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Cyan,
                    MarkerSize = pointSize,
                    MarkerFill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 7",
                    LineStroke = Brushes.Magenta,
                    LineStrokeThickness = 2,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<ObservableList<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 6)).ToObservableList(),
                    MarkerType = SeriesMarkerType.UpTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Orange,
                    MarkerSize = pointSize,
                    MarkerFill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 6",
                    LineStroke = Brushes.Cyan,
                    LineStrokeThickness = 2,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<ObservableList<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 5)).ToObservableList(),
                    MarkerType = SeriesMarkerType.DownTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.DarkCyan,
                    MarkerSize = pointSize,
                    MarkerFill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 5",
                    LineStroke = Brushes.Orange,
                    LineStrokeThickness = 2,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);

                wrapper = new SeriesWrapper<ObservableList<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 4)).ToObservableList(),
                    MarkerType = SeriesMarkerType.RightTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.DarkRed,
                    MarkerSize = pointSize,
                    MarkerFill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 4",
                    LineStroke = Brushes.DarkCyan,
                    LineStrokeThickness = 2,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);

                wrapper = new SeriesWrapper<ObservableList<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 3)).ToObservableList(),
                    MarkerType = SeriesMarkerType.LeftTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.DarkSeaGreen,
                    MarkerSize = pointSize,
                    MarkerFill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 3",
                    LineStroke = Brushes.DarkRed,
                    LineStrokeThickness = 2,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<ObservableList<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 2)).ToObservableList(),
                    MarkerType = SeriesMarkerType.Pentagram,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.DodgerBlue,
                    MarkerSize = pointSize,
                    MarkerFill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 2",
                    LineStroke = Brushes.DarkSeaGreen,
                    LineStrokeThickness = 2,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<ObservableList<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 1)).ToObservableList(),
                    MarkerType = SeriesMarkerType.Hexagram,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.DarkViolet,
                    MarkerSize = pointSize,
                    MarkerFill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 1",
                    LineStroke = Brushes.DodgerBlue,
                    LineStrokeThickness = 2,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
            };
        }

        static IEnumerable<double> Range(double start, double end, double step)
        {
            if (start == end) throw new ParameterOutOfRangeException("Start and End cannot be equal");
            if (step == 0) throw new ParameterOutOfRangeException("Step cannot be zero");
            if (start < end && step < 0) throw new ParameterOutOfRangeException("Step value cannot be negative when Start < End");
            if (start > end && step > 0) throw new ParameterOutOfRangeException("Step value cannot be positive when Start > End");
            if (start < end) for (var value = start; value <= end; value += step) yield return value;
            else for (var value = end; value >= start; value += step) yield return value;
        }
        #region ViewClosingCommand

        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand { get { return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(vcArgs => Properties.Settings.Default.Save())); } }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;

        #endregion

        [Initialize] public ObservableCollection<ISeries> SeriesSource { get; set; }
    }

    public class SeriesWrapper<T> : SeriesWrapper
    {
        public T SeriesData { get; set; }
    }

    public class SeriesWrapper : ViewModelBase, ISeries
    {
        public SeriesWrapper()
        {
            PropertyChanged += (s, e) =>
            {
                switch (e.PropertyName)
                {
                    case "MarkerType":
                    case "MarkerStrokeThickness":
                    case "MarkerSize":
                    case "MarkerStroke":
                    case "MarkerFill":
                        RenderSampleMarker();
                        break;
                    case "LineStroke":
                    case "LineStrokeDashArray":
                    case "LineStrokeThickness":
                        RenderSampleLine();
                        break;
                }
            };
        }

        void RenderSampleMarker()
        {
            if (MarkerType == null || MarkerStrokeThickness == 0 || MarkerSize == 0 || (MarkerStroke == null && MarkerFill == null)) return;
            var geometry = new StreamGeometry();
            using (var ctx = geometry.Open())
            {
                MarkerType(ctx, new Point(MarkerSize / 2, MarkerSize), MarkerSize);
            }
            SampleMarker = geometry;
        }

        void RenderSampleLine()
        {
            if (LineStrokeThickness == 0 || LineStroke == null) return;
            var geometry = new StreamGeometry();
            using (var ctx = geometry.Open())
            {
                ctx.BeginFigure(new Point(-5, MarkerSize), false, false);
                ctx.LineTo(new Point(MarkerSize + 5, MarkerSize), true, false);
            }
            SampleLine = geometry;
        }

        public Func<object, Point> ItemToPoint { get; set; }

        public IEnumerable<object> DataPoints { get; set; }

        public Action<StreamGeometryContext, Point, double> MarkerType { get; set; }

        public Geometry SampleMarker { get; private set; }

        public Geometry SampleLine { get; private set; }

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

    public static class SeriesMarkerType
    {
        public static Action<StreamGeometryContext, Point, double> Circle
        {
            get
            {
                return (ctx, point, size) =>
                {
                    var halfSize = size / 2;
                    ctx.BeginFigure(new Point(point.X - halfSize, point.Y), true, true);
                    ctx.ArcTo(new Point(point.X + halfSize, point.Y), new Size(halfSize, halfSize), 180, false, SweepDirection.Clockwise, true, true);
                    ctx.ArcTo(new Point(point.X - halfSize, point.Y), new Size(halfSize, halfSize), 180, false, SweepDirection.Clockwise, true, true);
                };
            }
        }

        public static Action<StreamGeometryContext, Point, double> Square
        {
            get
            {
                return (ctx, point, size) =>
                {
                    var halfSize = size / 2;
                    var top = point.Y - halfSize;
                    var bottom = point.Y + halfSize;
                    var left = point.X - halfSize;
                    var right = point.X + halfSize;
                    ctx.BeginFigure(new Point(left, top), true, true);
                    ctx.LineTo(new Point(right, top), true, true);
                    ctx.LineTo(new Point(right, bottom), true, true);
                    ctx.LineTo(new Point(left, bottom), true, true);
                };
            }
        }
        public static Action<StreamGeometryContext, Point, double> Diamond
        {
            get
            {
                return (ctx, point, size) =>
                {
                    var halfSize = size / 2;
                    var top = point.Y - halfSize;
                    var bottom = point.Y + halfSize;
                    var left = point.X - halfSize;
                    var right = point.X + halfSize;
                    ctx.BeginFigure(new Point(point.X, top), true, true);
                    ctx.LineTo(new Point(right, point.Y), true, true);
                    ctx.LineTo(new Point(point.X, bottom), true, true);
                    ctx.LineTo(new Point(left, point.Y), true, true);
                };
            }
        }

        public static Action<StreamGeometryContext, Point, double> Plus
        {
            get
            {
                return (ctx, point, size) =>
                {
                    var halfSize = size / 2;
                    var top = point.Y - halfSize;
                    var bottom = point.Y + halfSize;
                    var left = point.X - halfSize;
                    var right = point.X + halfSize;
                    ctx.BeginFigure(new Point(point.X, top), false, false);
                    ctx.LineTo(new Point(point.X, bottom), true, false);
                    ctx.BeginFigure(new Point(left, point.Y), false, false);
                    ctx.LineTo(new Point(right, point.Y), true, false);
                };
            }
        }

        public static Action<StreamGeometryContext, Point, double> Cross
        {
            get
            {
                return (ctx, point, size) =>
                {
                    var halfSize = size / 2;
                    var top = point.Y - halfSize;
                    var bottom = point.Y + halfSize;
                    var left = point.X - halfSize;
                    var right = point.X + halfSize;
                    ctx.BeginFigure(new Point(left, top), false, false);
                    ctx.LineTo(new Point(right, bottom), true, false);
                    ctx.BeginFigure(new Point(right, top), false, false);
                    ctx.LineTo(new Point(left, bottom), true, false);
                };
            }
        }

        public static Action<StreamGeometryContext, Point, double> Asterisk
        {
            get
            {
                return (ctx, point, size) =>
                {
                    var halfSize = size / 2;
                    for (double angle = 0; angle < MoreMath.TwoPi; angle += Math.PI / 3)
                    {
                        ctx.BeginFigure(new Point(point.X, point.Y), false, false);
                        ctx.LineTo(new Point(point.X + (halfSize * Math.Sin(angle)), point.Y + (halfSize * Math.Cos(angle))), true, false);
                    }
                };
            }
        }

        public static Action<StreamGeometryContext, Point, double> Pentagram
        {
            get
            {
                return (ctx, point, size) =>
                {
                    var halfSize = size / 2;
                    var quarterSize = halfSize / 2;
                    const int steps = 5;
                    ctx.BeginFigure(new Point(point.X, point.Y - halfSize), true, true);
                    for (var angle = Math.PI / steps; angle < MoreMath.TwoPi; angle += Math.PI / steps)
                    {
                        ctx.LineTo(new Point(point.X + (quarterSize * Math.Sin(angle)), point.Y - (quarterSize * Math.Cos(angle))), true, true);
                        angle += Math.PI / steps;
                        ctx.LineTo(new Point(point.X + (halfSize * Math.Sin(angle)), point.Y - (halfSize * Math.Cos(angle))), true, true);
                    }
                };
            }
        }

        public static Action<StreamGeometryContext, Point, double> Hexagram
        {
            get
            {
                return (ctx, point, size) =>
                {
                    var halfSize = size / 2;
                    var quarterSize = halfSize / 2;
                    const int steps = 6;
                    ctx.BeginFigure(new Point(point.X, point.Y - halfSize), true, true);
                    for (var angle = Math.PI / steps; angle < MoreMath.TwoPi; angle += Math.PI / steps)
                    {
                        ctx.LineTo(new Point(point.X + (quarterSize * Math.Sin(angle)), point.Y - (quarterSize * Math.Cos(angle))), true, true);
                        angle += Math.PI / steps;
                        ctx.LineTo(new Point(point.X + (halfSize * Math.Sin(angle)), point.Y - (halfSize * Math.Cos(angle))), true, true);
                    }
                };
            }
        }

        public static Action<StreamGeometryContext, Point, double> UpTriangle
        {
            get
            {
                return (ctx, point, size) =>
                {
                    var halfSize = size / 2;
                    var top = point.Y - halfSize;
                    var bottom = point.Y + halfSize;
                    var left = point.X - halfSize;
                    var right = point.X + halfSize;
                    ctx.BeginFigure(new Point(point.X, top), true, true);
                    ctx.LineTo(new Point(right, bottom), true, true);
                    ctx.LineTo(new Point(left, bottom), true, true);
                };
            }
        }

        public static Action<StreamGeometryContext, Point, double> DownTriangle
        {
            get
            {
                return (ctx, point, size) =>
                {
                    var halfSize = size / 2;
                    var top = point.Y - halfSize;
                    var bottom = point.Y + halfSize;
                    var left = point.X - halfSize;
                    var right = point.X + halfSize;
                    ctx.BeginFigure(new Point(point.X, bottom), true, true);
                    ctx.LineTo(new Point(left, top), true, true);
                    ctx.LineTo(new Point(right, top), true, true);
                };
            }
        }

        public static Action<StreamGeometryContext, Point, double> LeftTriangle
        {
            get
            {
                return (ctx, point, size) =>
                {
                    var halfSize = size / 2;
                    var top = point.Y - halfSize;
                    var bottom = point.Y + halfSize;
                    var left = point.X - halfSize;
                    var right = point.X + halfSize;
                    ctx.BeginFigure(new Point(left, point.Y), true, true);
                    ctx.LineTo(new Point(right, top), true, true);
                    ctx.LineTo(new Point(right, bottom), true, true);
                };
            }
        }

        public static Action<StreamGeometryContext, Point, double> RightTriangle
        {
            get
            {
                return (ctx, point, size) =>
                {
                    var halfSize = size / 2;
                    var top = point.Y - halfSize;
                    var bottom = point.Y + halfSize;
                    var left = point.X - halfSize;
                    var right = point.X + halfSize;
                    ctx.BeginFigure(new Point(right, point.Y), true, true);
                    ctx.LineTo(new Point(left, top), true, true);
                    ctx.LineTo(new Point(left, bottom), true, true);
                };
            }
        }
    }

    public interface ISeries
    {
        /// <summary>
        /// Converts an item in the series to a Point.  X and Y should be whatever natural values should be plotted for that point
        /// </summary>
        Func<object, Point> ItemToPoint { get; }
        /// <summary>
        /// An enumerable that returns the data points in the series, in the order they should be plotted
        /// </summary>
        IEnumerable<object> DataPoints { get; }
        /// <summary>
        /// An action that adds a Point to a StreamGeometryContext using a given size
        /// </summary>
        Action<StreamGeometryContext, Point, double> MarkerType { get; }
        /// <summary>
        /// The geometry for a glyph in this series, usually used to draw the legend
        /// </summary>
        Geometry SampleMarker { get; }
        /// <summary>
        /// The geometry for a glyph in this series, usually used to draw the legend
        /// </summary>
        Geometry SampleLine { get; }
        /// <summary>
        /// Width of the stroke
        /// </summary>
        double MarkerStrokeThickness { get; }
        /// <summary>
        /// Size of the point, passed to the AddToGeometry action
        /// </summary>
        double MarkerSize { get; }
        /// <summary>
        /// Stroke brush
        /// </summary>
        Brush MarkerStroke { get; }
        /// <summary>
        /// Fill brush
        /// </summary>
        Brush MarkerFill { get; }
        /// <summary>
        /// Stroke thickness for the line between series points
        /// </summary>
        double LineStrokeThickness { get; }
        /// <summary>
        /// Brush used to stroke the line between series points.  If null, no line will be drawn
        /// </summary>
        Brush LineStroke { get; }
        /// <summary>
        /// Each Double in the collection specifies the length of a dash or gap relative to the 
        /// Thickness of the pen. For example, a value of 1 creates a dash or gap that has the 
        /// same length as the thickness of the pen (a square).
        /// The first item in the collection, which is located at index 0, specifies the length 
        /// of a dash; the second item, which is located at index 1, specifies the length of a gap
        /// Objects with an even index value specify dashes; objects with an odd index value specify gaps.
        /// </summary>
        DoubleCollection LineStrokeDashArray { get; }
        /// <summary>
        /// The X Axis control to plot the DataPoints against (used for mapping X values to screen coordinates)
        /// </summary>
        DataAxis XAxis { get; set; }
        /// <summary>
        /// The Y Axis control to plot the DataPoints against (used for mapping Y values to screen coordinates)        
        /// </summary>
        DataAxis YAxis { get; set; }
        /// <summary>
        /// The name of the series, used to create a legend
        /// </summary>
        string SeriesName { get; }
    }
}