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
            _viewAwareStatus.ViewActivated += () =>
            {
                var xAxis = ((MainWindow)_viewAwareStatus.View).BottomLinearAxis;
                var yAxis = ((MainWindow)_viewAwareStatus.View).LeftLinearAxis;
                const double rangeStart = 0.0;
                var rangeEnd = MoreMath.TwoPi;
                var rangeStep = MoreMath.TwoPi / 100;
                const int pointSize = 10;
                var wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 12)).ToList(),
                    MarkerType = SeriesMarkerType.Plus,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.Red,
                    PointSize = pointSize,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 12"
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 10)).ToList(),
                    MarkerType = SeriesMarkerType.Circle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.Green,
                    PointSize = pointSize,
                    Fill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 10"
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 8)).ToList(),
                    MarkerType = SeriesMarkerType.Asterisk,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.Blue,
                    PointSize = pointSize,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 8"
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 6)).ToList(),
                    MarkerType = SeriesMarkerType.Cross,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.Cyan,
                    PointSize = pointSize,
                    Fill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 6"
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);

                wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 4)).ToList(),
                    MarkerType = SeriesMarkerType.Square,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.Magenta,
                    PointSize = pointSize,
                    Fill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 4"
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 2)).ToList(),
                    MarkerType = SeriesMarkerType.Diamond,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.Cyan,
                    PointSize = pointSize,
                    Fill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 2"
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x))).ToList(),
                    MarkerType = SeriesMarkerType.UpTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.Orange,
                    PointSize = pointSize,
                    Fill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x)"
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) - 2)).ToList(),
                    MarkerType = SeriesMarkerType.DownTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.Cyan,
                    PointSize = pointSize,
                    Fill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) - 2"
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);

                wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) - 4)).ToList(),
                    MarkerType = SeriesMarkerType.RightTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.DarkRed,
                    PointSize = pointSize,
                    Fill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) - 4"
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);

                wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) - 6)).ToList(),
                    MarkerType = SeriesMarkerType.LeftTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.DarkSeaGreen,
                    PointSize = pointSize,
                    Fill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) - 6"
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) - 8)).ToList(),
                    MarkerType = SeriesMarkerType.Pentagram,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.DodgerBlue,
                    PointSize = pointSize,
                    Fill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) - 8"
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) - 10)).ToList(),
                    MarkerType = SeriesMarkerType.Hexagram,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.DarkViolet,
                    PointSize = pointSize,
                    Fill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) - 10"
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

    public class SeriesWrapper : ISeries
    {
        public Func<object, Point> ItemToPoint { get; set; }

        public IEnumerable<object> DataPoints { get; set; }

        public Action<StreamGeometryContext, Point, double> MarkerType { get; set; }

        public double StrokeWidth { get; set; }

        public double PointSize { get; set; }

        public Brush Stroke { get; set; }

        public Brush Fill { get; set; }

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
                    ctx.ArcTo(new Point(point.X + halfSize, point.Y), new Size(halfSize, halfSize), 180, false, SweepDirection.Clockwise, true, false);
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
                    ctx.LineTo(new Point(right, top), true, false);
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
                    ctx.LineTo(new Point(right, point.Y), true, false);
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
        /// Width of the stroke
        /// </summary>
        double StrokeWidth { get; }
        /// <summary>
        /// Size of the point, passed to the AddToGeometry action
        /// </summary>
        double PointSize { get; }
        /// <summary>
        /// Stroke brush
        /// </summary>
        Brush Stroke { get; }
        /// <summary>
        /// Fill brush
        /// </summary>
        Brush Fill { get; }
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