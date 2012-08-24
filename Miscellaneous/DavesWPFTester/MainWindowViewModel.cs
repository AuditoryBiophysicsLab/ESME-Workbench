using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Threading;
using System.Windows;
using System.Windows.Media;
using System.Windows.Threading;
using ESME.NEMO;
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
        Timer _timer;
        readonly Random _random = new Random();
        Dispatcher _dispatcher;
        [ImportingConstructor]
        public MainWindowViewModel(IViewAwareStatus viewAwareStatus)
        {
            _viewAwareStatus = viewAwareStatus;
            _viewAwareStatus.ViewLoaded += () =>
            {
                _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
                var xAxis = ((MainWindow)_viewAwareStatus.View).BottomLinearAxis;
                var yAxis = ((MainWindow)_viewAwareStatus.View).LeftLinearAxis;
                const double rangeStart = 0.0;
                var rangeEnd = MoreMath.TwoPi;
                var rangeStep = MoreMath.TwoPi / 100;
                const int pointSize = 10;

                SeriesSource.Add(new DataSeriesViewModel
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
                });
                SeriesSource.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 11)).ToObservableList(),
                    MarkerType = SeriesMarkerType.Circle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Green,
                    MarkerSize = pointSize,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 11",
                    LineStroke = Brushes.Red,
                    LineStrokeThickness = 2,
                });
                SeriesSource.Add(new DataSeriesViewModel
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
                });
                SeriesSource.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 9)).ToObservableList(),
                    MarkerType = SeriesMarkerType.Cross,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Cyan,
                    MarkerSize = pointSize,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 9",
                    LineStroke = Brushes.Blue,
                    LineStrokeThickness = 2,
                });
                SeriesSource.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 8)).ToObservableList(),
                    MarkerType = SeriesMarkerType.Square,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Magenta,
                    MarkerSize = pointSize,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 8",
                    LineStroke = Brushes.Cyan,
                    LineStrokeThickness = 2,
                });
                SeriesSource.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 7)).ToObservableList(),
                    MarkerType = SeriesMarkerType.Diamond,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Cyan,
                    MarkerSize = pointSize,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 7",
                    LineStroke = Brushes.Magenta,
                    LineStrokeThickness = 2,
                });
                SeriesSource.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 6)).ToObservableList(),
                    MarkerType = SeriesMarkerType.UpTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Orange,
                    MarkerSize = pointSize,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 6",
                    LineStroke = Brushes.Cyan,
                    LineStrokeThickness = 2,
                });
                SeriesSource.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 5)).ToObservableList(),
                    MarkerType = SeriesMarkerType.DownTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.DarkCyan,
                    MarkerSize = pointSize,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 5",
                    LineStroke = Brushes.Orange,
                    LineStrokeThickness = 2,
                });
                SeriesSource.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 4)).ToObservableList(),
                    MarkerType = SeriesMarkerType.RightTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.DarkRed,
                    MarkerSize = pointSize,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 4",
                    LineStroke = Brushes.DarkCyan,
                    LineStrokeThickness = 2,
                });
                SeriesSource.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 3)).ToObservableList(),
                    MarkerType = SeriesMarkerType.LeftTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.DarkSeaGreen,
                    MarkerSize = pointSize,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 3",
                    LineStroke = Brushes.DarkRed,
                    LineStrokeThickness = 2,
                });
                SeriesSource.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 2)).ToObservableList(),
                    MarkerType = SeriesMarkerType.Pentagram,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.DodgerBlue,
                    MarkerSize = pointSize,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 2",
                    LineStroke = Brushes.DarkSeaGreen,
                    LineStrokeThickness = 2,
                });
                SeriesSource.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 1)).ToObservableList(),
                    MarkerType = SeriesMarkerType.Hexagram,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.DarkViolet,
                    MarkerSize = pointSize,
                    XAxis = xAxis,
                    YAxis = yAxis,
                    SeriesName = "y = sin(x) + 1",
                    LineStroke = Brushes.DodgerBlue,
                    LineStrokeThickness = 2,
                });
#if true
                _timer = new Timer(state =>
                {
                    var selectedSeriesIndex = _random.Next(SeriesSource.Count);
                    var selectedSeries = (DataSeriesViewModel)SeriesSource[selectedSeriesIndex];
                    var seriesData = (ObservableList<Tuple<double, double>>)selectedSeries.SeriesData;
                    for (var i = 0; i < seriesData.Count; i++ )
                        seriesData[i] = Tuple.Create(seriesData[i].Item1, -seriesData[i].Item2);
                    _dispatcher.InvokeInBackgroundIfRequired(selectedSeries.RenderShapes);
                }, null, 100, 100);
#endif
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
}