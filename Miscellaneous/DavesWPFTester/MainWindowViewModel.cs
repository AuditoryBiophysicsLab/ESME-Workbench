using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Threading;
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
                var mainWindowView = (MainWindow)_viewAwareStatus.View;
                //CreateTopLeftSeriesSource(mainWindowView.TopLeftHorizontalAxis, mainWindowView.TopLeftVerticalAxis);
                const double rangeStart = 0.0;
                var rangeEnd = MoreMath.TwoPi;
                var rangeStep = MoreMath.TwoPi / 100;
                const int pointSize = 10;
                LowerRight.Dispatcher = _dispatcher;
                LowerRight.DataSeriesCollection.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 11)).ToObservableList(),
                    //MarkerType = SeriesMarkerType.Plus,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Red,
                    MarkerSize = pointSize,
                    SeriesName = "y = (1.0 * sin(x)) + 11",
                    LineStroke = Brushes.DarkViolet,
                    LineStrokeThickness = 1,
                });
                LowerRight.DataSeriesCollection.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 10)).ToObservableList(),
                    //MarkerType = SeriesMarkerType.Circle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Green,
                    MarkerSize = pointSize,
                    SeriesName = "y = sin(x) + 10",
                    LineStroke = Brushes.Red,
                    LineStrokeThickness = 1,
                });
#if true
                LowerRight.DataSeriesCollection.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 9)).ToObservableList(),
                    //MarkerType = SeriesMarkerType.Asterisk,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Blue,
                    MarkerSize = pointSize,
                    SeriesName = "y = sin(x) + 9",
                    LineStroke = Brushes.Green,
                    LineStrokeThickness = 1,
                });
                LowerRight.DataSeriesCollection.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 8)).ToObservableList(),
                    //MarkerType = SeriesMarkerType.Cross,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Cyan,
                    MarkerSize = pointSize,
                    SeriesName = "y = sin(x) + 8",
                    LineStroke = Brushes.Blue,
                    LineStrokeThickness = 1,
                });
                LowerRight.DataSeriesCollection.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 7)).ToObservableList(),
                    //MarkerType = SeriesMarkerType.Square,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Magenta,
                    MarkerSize = pointSize,
                    SeriesName = "y = sin(x) + 7",
                    LineStroke = Brushes.Cyan,
                    LineStrokeThickness = 1,
                });
                LowerRight.DataSeriesCollection.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 6)).ToObservableList(),
                    //MarkerType = SeriesMarkerType.Diamond,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Cyan,
                    MarkerSize = pointSize,
                    SeriesName = "y = sin(x) + 6",
                    LineStroke = Brushes.Magenta,
                    LineStrokeThickness = 1,
                });
                LowerRight.DataSeriesCollection.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 5)).ToObservableList(),
                    //MarkerType = SeriesMarkerType.UpTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.Orange,
                    MarkerSize = pointSize,
                    SeriesName = "y = sin(x) + 5",
                    LineStroke = Brushes.Cyan,
                    LineStrokeThickness = 1,
                });
                LowerRight.DataSeriesCollection.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 4)).ToObservableList(),
                    //MarkerType = SeriesMarkerType.DownTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.DarkCyan,
                    MarkerSize = pointSize,
                    SeriesName = "y = sin(x) + 4",
                    LineStroke = Brushes.Orange,
                    LineStrokeThickness = 1,
                });
                LowerRight.DataSeriesCollection.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 3)).ToObservableList(),
                    //MarkerType = SeriesMarkerType.RightTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.DarkRed,
                    MarkerSize = pointSize,
                    SeriesName = "y = sin(x) + 3",
                    LineStroke = Brushes.DarkCyan,
                    LineStrokeThickness = 1,
                });
                LowerRight.DataSeriesCollection.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 2)).ToObservableList(),
                    //MarkerType = SeriesMarkerType.LeftTriangle,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.DarkSeaGreen,
                    MarkerSize = pointSize,
                    SeriesName = "y = sin(x) + 2",
                    LineStroke = Brushes.DarkRed,
                    LineStrokeThickness = 1,
                });
                LowerRight.DataSeriesCollection.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 1)).ToObservableList(),
                    //MarkerType = SeriesMarkerType.Pentagram,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.DodgerBlue,
                    MarkerSize = pointSize,
                    SeriesName = "y = sin(x) + 1",
                    LineStroke = Brushes.DarkSeaGreen,
                    LineStrokeThickness = 1,
                });
                LowerRight.DataSeriesCollection.Add(new DataSeriesViewModel
                {
                    SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x))).ToObservableList(),
                    //MarkerType = SeriesMarkerType.Hexagram,
                    ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                    MarkerStrokeThickness = 1,
                    MarkerStroke = Brushes.DarkViolet,
                    MarkerSize = pointSize,
                    SeriesName = "y = sin(x)",
                    LineStroke = Brushes.DodgerBlue,
                    LineStrokeThickness = 1,
                });
#endif
                _timer = new Timer(state => _dispatcher.InvokeInBackgroundIfRequired(() =>
                {
                    var selectedSeries = (DataSeriesViewModel)LowerRight.DataSeriesCollection.First();
                    var seriesData = (ObservableList<Tuple<double, double>>)selectedSeries.SeriesData;
                    _amplitude += _amplitudeDelta;
                    selectedSeries.SeriesName = string.Format("y = ({0:0.0} * sin(x)) + 11", _amplitude);
                    using (var d = _dispatcher.DisableProcessing())
                    {
                        for (var i = 0; i < seriesData.Count; i++) seriesData[i] = Tuple.Create(seriesData[i].Item1, (_amplitude * Math.Sin(seriesData[i].Item1)) + 11);
                    }
                    if (_amplitude > 10) _amplitudeDelta = -1;
                    if (_amplitude < -10) _amplitudeDelta = 1;
                }), null, 20, 20);
            };
        }

        double _amplitude = 1;
        double _amplitudeDelta = 1;

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

        [Initialize] public ObservableCollection<ISeries> TopLeftSeriesSource { get; set; }
        [Initialize] public TwoAxisSeriesViewModel LowerRight { get; set; }

        void CreateTopLeftSeriesSource(DataAxis xAxis, DataAxis yAxis)
        {
            const double rangeStart = 0.0;
            var rangeEnd = MoreMath.TwoPi;
            var rangeStep = MoreMath.TwoPi / 100;
            const int pointSize = 10;

            TopLeftSeriesSource.Add(new DataSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 12)).ToObservableList(),
                MarkerType = SeriesMarkerType.Plus,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.Red,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 12",
                LineStroke = Brushes.DarkViolet,
                LineStrokeThickness = 2,
            });
            TopLeftSeriesSource.Add(new DataSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 11)).ToObservableList(),
                MarkerType = SeriesMarkerType.Circle,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.Green,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 11",
                LineStroke = Brushes.Red,
                LineStrokeThickness = 2,
            });
            TopLeftSeriesSource.Add(new DataSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 10)).ToObservableList(),
                MarkerType = SeriesMarkerType.Asterisk,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.Blue,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 10",
                LineStroke = Brushes.Green,
                LineStrokeThickness = 2,
            });
            TopLeftSeriesSource.Add(new DataSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 9)).ToObservableList(),
                MarkerType = SeriesMarkerType.Cross,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.Cyan,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 9",
                LineStroke = Brushes.Blue,
                LineStrokeThickness = 2,
            });
            TopLeftSeriesSource.Add(new DataSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 8)).ToObservableList(),
                MarkerType = SeriesMarkerType.Square,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.Magenta,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 8",
                LineStroke = Brushes.Cyan,
                LineStrokeThickness = 2,
            });
            TopLeftSeriesSource.Add(new DataSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 7)).ToObservableList(),
                MarkerType = SeriesMarkerType.Diamond,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.Cyan,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 7",
                LineStroke = Brushes.Magenta,
                LineStrokeThickness = 2,
            });
            TopLeftSeriesSource.Add(new DataSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 6)).ToObservableList(),
                MarkerType = SeriesMarkerType.UpTriangle,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.Orange,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 6",
                LineStroke = Brushes.Cyan,
                LineStrokeThickness = 2,
            });
            TopLeftSeriesSource.Add(new DataSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 5)).ToObservableList(),
                MarkerType = SeriesMarkerType.DownTriangle,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.DarkCyan,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 5",
                LineStroke = Brushes.Orange,
                LineStrokeThickness = 2,
            });
            TopLeftSeriesSource.Add(new DataSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 4)).ToObservableList(),
                MarkerType = SeriesMarkerType.RightTriangle,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.DarkRed,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 4",
                LineStroke = Brushes.DarkCyan,
                LineStrokeThickness = 2,
            });
            TopLeftSeriesSource.Add(new DataSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 3)).ToObservableList(),
                MarkerType = SeriesMarkerType.LeftTriangle,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.DarkSeaGreen,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 3",
                LineStroke = Brushes.DarkRed,
                LineStrokeThickness = 2,
            });
            TopLeftSeriesSource.Add(new DataSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 2)).ToObservableList(),
                MarkerType = SeriesMarkerType.Pentagram,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.DodgerBlue,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 2",
                LineStroke = Brushes.DarkSeaGreen,
                LineStrokeThickness = 2,
            });
            TopLeftSeriesSource.Add(new DataSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Sin(x) + 1)).ToObservableList(),
                MarkerType = SeriesMarkerType.Hexagram,
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.DarkViolet,
                MarkerSize = pointSize,
                SeriesName = "y = sin(x) + 1",
                LineStroke = Brushes.DodgerBlue,
                LineStrokeThickness = 2,
            });
        }
    }
}