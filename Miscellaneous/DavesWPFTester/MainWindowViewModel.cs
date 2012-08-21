using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Media;
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
            CreateSeries();
            _viewAwareStatus.ViewActivated += () =>
            {
                TestSeries.XAxis = ((MainWindow)_viewAwareStatus.View).BottomLinearAxis;
                TestSeries.YAxis = ((MainWindow)_viewAwareStatus.View).LeftLinearAxis;
            };
        }

        #region ViewClosingCommand

        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand { get { return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(vcArgs => Properties.Settings.Default.Save())); } }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;

        #endregion

        [Initialize] public List<ISeries> SeriesSource { get; set; }

        void CreateSeries()
        {
            SeriesSource.Clear();
            for (double x = 0; x <= MoreMath.TwoPi; x += (MoreMath.TwoPi / 100))
                TestSeries.Add(new Tuple<double, double>(x, Math.Sin(x)));
            SeriesSource.Add(TestSeries);
        }

        [Initialize] public TestSeries TestSeries { get; set; }
    }

    public class TestSeries : ObservableList<Tuple<double, double>>, ISeries
    {
        public Func<object, Point> ItemToPoint { get { return p => new Point(((Tuple<double, double>)p).Item1, ((Tuple<double, double>)p).Item2); } }

        public IEnumerable<object> DataPoints { get { return this; } }

        public Action<StreamGeometryContext, Point, double> AddToGeometry
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

        public double StrokeWidth { get { return 1; } }

        public double PointSize { get { return 5; } }

        public Brush Stroke { get { return Brushes.Blue; } }

        public Brush Fill { get { return null; } }

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
        Action<StreamGeometryContext, Point, double> AddToGeometry { get; }
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