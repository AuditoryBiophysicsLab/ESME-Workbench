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
            _viewAwareStatus.ViewActivated += () =>
            {
                var xAxis = ((MainWindow)_viewAwareStatus.View).BottomLinearAxis;
                var yAxis = ((MainWindow)_viewAwareStatus.View).LeftLinearAxis;
                var wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(0, MoreMath.TwoPi, MoreMath.TwoPi / 100).Select(v => Tuple.Create(v, Math.Sin(v))).ToList(),
                    MarkerType = SeriesWrapper.Square,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.Blue,
                    PointSize = 5,
                    Fill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(MoreMath.TwoPi, 2 * MoreMath.TwoPi, MoreMath.TwoPi / 100).Select(v => Tuple.Create(v, Math.Sin(v))).ToList(),
                    MarkerType = SeriesWrapper.UpTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.Red,
                    PointSize = 5,
                    Fill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(2 * MoreMath.TwoPi, 4 * MoreMath.TwoPi, MoreMath.TwoPi / 100).Select(v => Tuple.Create(v, Math.Sin(v))).ToList(),
                    MarkerType = SeriesWrapper.Circle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.Green,
                    PointSize = 5,
                    Fill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
                wrapper = new SeriesWrapper<List<Tuple<double, double>>>
                {
                    SeriesData = Range(4 * MoreMath.TwoPi, 6 * MoreMath.TwoPi, MoreMath.TwoPi / 100).Select(v => Tuple.Create(v, Math.Sin(v))).ToList(),
                    MarkerType = SeriesWrapper.DownTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    StrokeWidth = 1,
                    Stroke = Brushes.OrangeRed,
                    PointSize = 5,
                    Fill = null,
                    XAxis = xAxis,
                    YAxis = yAxis,
                };
                wrapper.DataPoints = wrapper.SeriesData;
                SeriesSource.Add(wrapper);
            };
        }

        IEnumerable<double> Range(double start, double end, double step)
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

        SeriesWrapper CreateSeries()
        {
            var wrapper = new SeriesWrapper<ObservableList<Tuple<double, double>>>
            {
                SeriesData = new ObservableList<Tuple<double, double>>(),
                MarkerType = SeriesWrapper.Square,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeWidth = 1,
                Stroke = Brushes.Blue,
                PointSize = 5,
                Fill = null,
            };
            wrapper.DataPoints = wrapper.SeriesData;
            for (double x = 0; x <= MoreMath.TwoPi; x += (MoreMath.TwoPi / 100))
                wrapper.SeriesData.Add(new Tuple<double, double>(x, Math.Sin(x)));
            return wrapper;
        }
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

        public double StrokeWidth { get; set; }

        public double PointSize { get; set; }

        public Brush Stroke { get; set; }

        public Brush Fill { get; set; }

        public DataAxis XAxis { get; set; }

        public DataAxis YAxis { get; set; }
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
    }
}