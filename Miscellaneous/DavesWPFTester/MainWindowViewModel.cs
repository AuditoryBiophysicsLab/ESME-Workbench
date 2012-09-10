using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using System.Windows.Threading;
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
        Dispatcher _dispatcher;
        [Initialize] public FourAxisSeriesViewModel UpperLeft { get; set; }
        [Initialize] public FourAxisSeriesViewModel UpperRight { get; set; }
        [Initialize] public FourAxisSeriesViewModel LowerLeft { get; set; }
        [Initialize] public FourAxisSeriesViewModel LowerRight { get; set; }
        [Initialize] public FourAxisSeriesViewModel BottomLeft { get; set; }
        public bool AnimateUpperLeft { get; set; }
        public bool AnimateLowerLeft { get; set; }
        [ImportingConstructor]
        public MainWindowViewModel(IViewAwareStatus viewAwareStatus)
        {
            _viewAwareStatus = viewAwareStatus;
            _viewAwareStatus.ViewLoaded += () =>
            {
                _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
                CreateUpperLeftSeries();
                CreateUpperRightSeries();
                CreateLowerLeftSeries();
                CreateLowerRightSeries();
                BottomLeft.LeftAxis.AxisType = AxisType.Logarithmic;
                CreateBottomLeftSeries();
#if true
                var timer = new DispatcherTimer(DispatcherPriority.ApplicationIdle, _dispatcher) { Interval = TimeSpan.FromMilliseconds(5) };
                timer.Start();
                timer.Tick += (s, e) =>
                {
                    if (AnimateLowerLeft)
                    {
                        var selectedSeries = (BarSeriesViewModel)LowerLeft.DataSeriesCollection[0];
                        var seriesData = (ObservableList<Tuple<double, double>>)selectedSeries.SeriesData;
                        selectedSeries.SeriesName = string.Format("y = {0:0.0} * x", _lowerLeftAmplitude);
                        for (var i = 0; i < seriesData.Count; i++) seriesData[i] = Tuple.Create(seriesData[i].Item1, _lowerLeftAmplitude * seriesData[i].Item1);
                        _lowerLeftAmplitude += _lowerLeftAmplitudeDelta;
                        if (_lowerLeftAmplitude > 10) _lowerLeftAmplitudeDelta = -1;
                        if (_lowerLeftAmplitude < -10) _lowerLeftAmplitudeDelta = 1;
                    }
                    if (!AnimateUpperLeft) return;
                    for (var seriesIndex = 0; seriesIndex < UpperLeft.DataSeriesCollection.Count; seriesIndex++)
                    {
                        var selectedSeries = (LineSeriesViewModel)UpperLeft.DataSeriesCollection[seriesIndex];
                        var seriesData = (ObservableList<Tuple<double, double>>)selectedSeries.SeriesData;
                        selectedSeries.SeriesName = string.Format("y = ({0:0.0} * sin(x)) + {1}", _upperLeftAmplitude, 11 - seriesIndex);
                        for (var i = 0; i < seriesData.Count; i++) seriesData[i] = Tuple.Create(seriesData[i].Item1, (_upperLeftAmplitude * Math.Sin(seriesData[i].Item1)) + (11 - seriesIndex));
                    }
                    _upperLeftAmplitude += _upperLeftAmplitudeDelta;
                    if (_upperLeftAmplitude > 10) _upperLeftAmplitudeDelta = -1;
                    if (_upperLeftAmplitude < -10) _upperLeftAmplitudeDelta = 1;
                };
#endif
            };
        }

        double _upperLeftAmplitude = 1;
        double _upperLeftAmplitudeDelta = 1;
        double _lowerLeftAmplitude = 1;
        double _lowerLeftAmplitudeDelta = 1;
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

        void CreateBottomLeftSeries()
        {
            const double rangeStart = 1;
            const int rangeEnd = 10;
            const double rangeStep = .5;
            BottomLeft.MajorTickLineColor = Colors.Black;
            BottomLeft.MinorTickLineColor = Colors.AliceBlue;
            BottomLeft.DataSeriesCollection.Add(new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = x",
                Fill = Brushes.Blue,
            });
            BottomLeft.BottomAxis.Range.Update(-0.5, 10.5);
        }

        void CreateLowerRightSeries()
        {
            const double rangeStart = 1;
            const int rangeEnd = 10;
            const double rangeStep = 1;
            LowerRight.MajorTickLineColor = Colors.Black;
            LowerRight.MinorTickLineColor = Colors.AliceBlue;
            LowerRight.YAxis.AxisType = AxisType.Logarithmic;
            LowerRight.DataSeriesCollection.Add(new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x * x)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = x^2",
                Fill = Brushes.Blue,
            });
            ((BarSeriesViewModel)LowerRight.DataSeriesCollection[0]).YRange.Min = 0.1;
        }

        void CreateLowerLeftSeries()
        {
            const double rangeStart = -10;
            const int rangeEnd = 10;
            const double rangeStep = 1;
            LowerLeft.MajorTickLineColor = Colors.Black;
            LowerLeft.MinorTickLineColor = Colors.AliceBlue;
            LowerLeft.DataSeriesCollection.Add(new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = x",
                Stroke = Brushes.DarkViolet,
                Fill = Brushes.Red,
            });
        }

        void CreateUpperRightSeries()
        {
            const double rangeStart = -9;
            const int rangeEnd = 9;
            const double rangeStep = .1;
            UpperRight.MajorTickLineColor = Colors.Black;
            UpperRight.MinorTickLineColor = Colors.AliceBlue;
            UpperRight.YAxis.AxisType = AxisType.Logarithmic;
            UpperRight.YAxis.TickValueFormat = "m";
            UpperRight.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Cosh(x) + 1)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                SeriesName = "y = cosh(x) + 1",
                LineStroke = Brushes.DarkViolet,
                LineStrokeThickness = 1,
            });
            UpperRight.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, (x * x) + 1)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                SeriesName = "y = x^2 + 1",
                LineStroke = Brushes.Red,
                LineStrokeThickness = 1,
            });
            UpperRight.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Exp(x))).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                SeriesName = "y = e^x",
                LineStroke = Brushes.Green,
                LineStrokeThickness = 1,
            });
        }
        void CreateUpperLeftSeries()
        {
            const double rangeStart = 0.0;
            var rangeEnd = MoreMath.TwoPi + 0.01;
            var rangeStep = MoreMath.TwoPi / 16;
            const int pointSize = 10;
            UpperLeft.MajorTickLineColor = Colors.Black;
            UpperLeft.MinorTickLineColor = Colors.AliceBlue;
            UpperLeft.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 11)).ToObservableList(),
                MarkerType = SeriesMarkerType.Plus,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.Red,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 11",
                LineStroke = Brushes.DarkViolet,
                LineStrokeThickness = 1,
            });
            UpperLeft.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 10)).ToObservableList(),
                MarkerType = SeriesMarkerType.Circle,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.Green,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 10",
                LineStroke = Brushes.Red,
                LineStrokeThickness = 1,
            });
            UpperLeft.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 9)).ToObservableList(),
                MarkerType = SeriesMarkerType.Asterisk,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.Blue,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 9",
                LineStroke = Brushes.Green,
                LineStrokeThickness = 1,
            });
            UpperLeft.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 8)).ToObservableList(),
                MarkerType = SeriesMarkerType.Cross,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.Cyan,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 8",
                LineStroke = Brushes.Blue,
                LineStrokeThickness = 1,
            });
            UpperLeft.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 7)).ToObservableList(),
                MarkerType = SeriesMarkerType.Square,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.Magenta,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 7",
                LineStroke = Brushes.Cyan,
                LineStrokeThickness = 1,
            });
            UpperLeft.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 6)).ToObservableList(),
                MarkerType = SeriesMarkerType.Diamond,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.Cyan,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 6",
                LineStroke = Brushes.Magenta,
                LineStrokeThickness = 1,
            });
            UpperLeft.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 5)).ToObservableList(),
                MarkerType = SeriesMarkerType.UpTriangle,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.Orange,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 5",
                LineStroke = Brushes.Cyan,
                LineStrokeThickness = 1,
            });
            UpperLeft.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 4)).ToObservableList(),
                MarkerType = SeriesMarkerType.DownTriangle,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.DarkCyan,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 4",
                LineStroke = Brushes.Orange,
                LineStrokeThickness = 1,
            });
            UpperLeft.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 3)).ToObservableList(),
                MarkerType = SeriesMarkerType.RightTriangle,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.DarkRed,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 3",
                LineStroke = Brushes.DarkCyan,
                LineStrokeThickness = 1,
            });
            UpperLeft.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 2)).ToObservableList(),
                MarkerType = SeriesMarkerType.LeftTriangle,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.DarkSeaGreen,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 2",
                LineStroke = Brushes.DarkRed,
                LineStrokeThickness = 1,
            });
            UpperLeft.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 1)).ToObservableList(),
                MarkerType = SeriesMarkerType.Pentagram,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.DodgerBlue,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 1",
                LineStroke = Brushes.DarkSeaGreen,
                LineStrokeThickness = 1,
            });
            UpperLeft.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x))).ToObservableList(),
                MarkerType = SeriesMarkerType.Hexagram,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.DarkViolet,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x)",
                LineStroke = Brushes.DodgerBlue,
                LineStrokeThickness = 1,
            });
        }
    }
}