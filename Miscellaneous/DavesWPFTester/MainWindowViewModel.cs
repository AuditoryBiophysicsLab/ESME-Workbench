using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Threading;
using ESME.NEMO;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Plotting;
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
        [Initialize] public FourAxisSeriesViewModel TopLeft { get; set; }
        [Initialize] public FourAxisSeriesViewModel TopRight { get; set; }
        [Initialize] public FourAxisSeriesViewModel MiddleLeft { get; set; }
        [Initialize] public FourAxisSeriesViewModel MiddleRight { get; set; }
        [Initialize] public FourAxisSeriesViewModel BottomLeft { get; set; }
        [Initialize] public FourAxisSeriesViewModel BottomRight { get; set; }

        public bool AnimateTopLeft { get; set; }
        public bool AnimateMiddleLeft { get; set; }
        [ImportingConstructor]
        public MainWindowViewModel(IViewAwareStatus viewAwareStatus)
        {
            _viewAwareStatus = viewAwareStatus;
            _viewAwareStatus.ViewLoaded += () =>
            {
                _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
                CreateTopLeftSeries();
                CreateTopRightSeries();
                CreateMiddleLeftSeries();
                CreateMiddleRightSeries();
                CreateBottomLeftSeries();
                CreateBottomRightSeries();
#if true
                var timer = new DispatcherTimer(DispatcherPriority.Background, _dispatcher) { Interval = TimeSpan.FromMilliseconds(5) };
                timer.Start();
                timer.Tick += (s, e) =>
                {
                    if (AnimateMiddleLeft)
                    {
                        var selectedSeries = (BarSeriesViewModel)MiddleLeft.DataSeriesCollection[1];
                        var seriesData = (ObservableList<Tuple<double, double>>)selectedSeries.SeriesData;
                        selectedSeries.SeriesName = string.Format("y = {0:0.0} * x", _middleLeftAmplitude);
                        for (var i = 0; i < seriesData.Count; i++) seriesData[i] = Tuple.Create(seriesData[i].Item1, _middleLeftAmplitude * seriesData[i].Item1);
                        selectedSeries = (BarSeriesViewModel)MiddleLeft.DataSeriesCollection[2];
                        seriesData = (ObservableList<Tuple<double, double>>)selectedSeries.SeriesData;
                        selectedSeries.SeriesName = string.Format("y = {0:0.0} * x", -_middleLeftAmplitude);
                        for (var i = 0; i < seriesData.Count; i++) seriesData[i] = Tuple.Create(seriesData[i].Item1, -_middleLeftAmplitude * seriesData[i].Item1);
                        _middleLeftAmplitude += _middleLeftAmplitudeDelta;
                        if (_middleLeftAmplitude > 10) _middleLeftAmplitudeDelta = -1;
                        if (_middleLeftAmplitude < -10) _middleLeftAmplitudeDelta = 1;

                    }
                    if (!AnimateTopLeft) return;
                    for (var seriesIndex = 0; seriesIndex < TopLeft.DataSeriesCollection.Count; seriesIndex++)
                    {
                        var selectedSeries = (LineSeriesViewModel)TopLeft.DataSeriesCollection[seriesIndex];
                        var seriesData = (ObservableList<Tuple<double, double>>)selectedSeries.SeriesData;
                        selectedSeries.SeriesName = string.Format("y = ({0:0.0} * sin(x)) + {1}", _topLeftAmplitude, 11 - seriesIndex);
                        for (var i = 0; i < seriesData.Count; i++) seriesData[i] = Tuple.Create(seriesData[i].Item1, (_topLeftAmplitude * Math.Sin(seriesData[i].Item1)) + (11 - seriesIndex));
                    }
                    _topLeftAmplitude += _topLeftAmplitudeDelta;
                    if (_topLeftAmplitude > 10) _topLeftAmplitudeDelta = -1;
                    if (_topLeftAmplitude < -10) _topLeftAmplitudeDelta = 1;
                };
#endif
            };
        }

        double _topLeftAmplitude = 1;
        double _topLeftAmplitudeDelta = 1;
        double _middleLeftAmplitude = 1;
        double _middleLeftAmplitudeDelta = 1;
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

        #region AddSeriesBottomLeftCommand
        public SimpleCommand<object, EventToCommandArgs> AddSeriesBottomLeftCommand { get { return _addSeriesBottomLeft ?? (_addSeriesBottomLeft = new SimpleCommand<object, EventToCommandArgs>(AddSeriesBottomLeftHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _addSeriesBottomLeft;

        void AddSeriesBottomLeftHandler(EventToCommandArgs args)
        {
            const double rangeStart = 1;
            const int rangeEnd = 10;
            const double rangeStep = 1;
            var blackSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x * 3.5)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = 3.5x",
                Fill = Brushes.Black,
            };
            ((StackedBarSeriesViewModel)BottomLeft.DataSeriesCollection.First()).BarSeriesCollection.Add(blackSeries);
        }
        #endregion

        #region RemoveSeriesBottomLeftCommand
        public SimpleCommand<object, EventToCommandArgs> RemoveSeriesBottomLeftCommand { get { return _removeSeriesBottomLeft ?? (_removeSeriesBottomLeft = new SimpleCommand<object, EventToCommandArgs>(RemoveSeriesBottomLeftHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _removeSeriesBottomLeft;

        void RemoveSeriesBottomLeftHandler(EventToCommandArgs args)
        {
            var lastSeries = ((StackedBarSeriesViewModel)BottomLeft.DataSeriesCollection.First()).BarSeriesCollection.Last();
            ((StackedBarSeriesViewModel)BottomLeft.DataSeriesCollection.First()).BarSeriesCollection.Remove(lastSeries);
        }
        #endregion

        #region AddSeriesBottomRightCommand
        public SimpleCommand<object, EventToCommandArgs> AddSeriesBottomRightCommand { get { return _addSeriesBottomRight ?? (_addSeriesBottomRight = new SimpleCommand<object, EventToCommandArgs>(AddSeriesBottomRightHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _addSeriesBottomRight;

        void AddSeriesBottomRightHandler(EventToCommandArgs args)
        {
            const double rangeStart = 1;
            const int rangeEnd = 10;
            const double rangeStep = 1;
            var blackSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x * 3.5)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = 3.5x",
                Fill = Brushes.Black,
            };
            ((GroupedBarSeriesViewModel)BottomRight.DataSeriesCollection.First()).BarSeriesCollection.Add(blackSeries);
        }
        #endregion

        #region RemoveSeriesBottomRightCommand
        public SimpleCommand<object, EventToCommandArgs> RemoveSeriesBottomRightCommand { get { return _removeSeriesBottomRight ?? (_removeSeriesBottomRight = new SimpleCommand<object, EventToCommandArgs>(RemoveSeriesBottomRightHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _removeSeriesBottomRight;

        void RemoveSeriesBottomRightHandler(EventToCommandArgs args)
        {
            var lastSeries = ((GroupedBarSeriesViewModel)BottomRight.DataSeriesCollection.First()).BarSeriesCollection.Last();
            ((GroupedBarSeriesViewModel)BottomRight.DataSeriesCollection.First()).BarSeriesCollection.Remove(lastSeries);
        }
        #endregion
        void CreateBottomRightSeries()
        {
            const double rangeStart = 1;
            const int rangeEnd = 10;
            const double rangeStep = 1;
            BottomRight.XAxis.AxisType = AxisType.Enumerated;
            BottomRight.YAxis.AxisType = AxisType.Logarithmic;
            BottomRight.XAxis.AxisTicks = new ObservableCollection<NewDataAxisTick>
            {
                new NewDataAxisTick(-1, null, false),
                new NewDataAxisTick(0, "Zero", false),
                new NewDataAxisTick(1, "One", false),
                new NewDataAxisTick(2, "Two", false),
                new NewDataAxisTick(3, "Three", false),
                new NewDataAxisTick(4, "Four", false),
                new NewDataAxisTick(5, "Five", false),
                new NewDataAxisTick(6, "Six", false),
                new NewDataAxisTick(7, "Seven", false),
                new NewDataAxisTick(8, "Eight", false),
                new NewDataAxisTick(9, "Nine", false),
                new NewDataAxisTick(10, "Ten", false),
                new NewDataAxisTick(11, null, false),
            };
            BottomRight.YAxis.DataRange.Update(0.1, 10);
            BottomRight.XAxis.DataRange.Update(-1, 11);
            var redSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x / 2.0)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = x / 2",
                Fill = Brushes.Red,
            };
            var greenSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = x",
                Fill = Brushes.Green,
            };
            var blueSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x * 1.5)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = 1.5x",
                Fill = Brushes.Blue,
            };
            var cyanSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x * 2)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = 2x",
                Fill = Brushes.Cyan,
            };
            var magentaSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x * 2.5)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = 2.5x",
                Fill = Brushes.Magenta,
            };
            var orangeSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x * 3)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = 3x",
                Fill = Brushes.Orange,
            };
            var blackSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x * 3.5)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = 3.5x",
                Fill = Brushes.Black,
            };
            var groupedSeries = new GroupedBarSeriesViewModel();
            groupedSeries.BarSeriesCollection.Add(redSeries);
            groupedSeries.BarSeriesCollection.Add(greenSeries);
            groupedSeries.BarSeriesCollection.Add(blueSeries);
            groupedSeries.BarSeriesCollection.Add(cyanSeries);
            groupedSeries.BarSeriesCollection.Add(magentaSeries);
            groupedSeries.BarSeriesCollection.Add(orangeSeries);
            groupedSeries.BarSeriesCollection.Add(blackSeries);
            BottomRight.DataSeriesCollection.Add(groupedSeries);
            //BottomRight.DataSeriesCollection.Add(redSeries);
            //BottomRight.DataSeriesCollection.Add(blueSeries);
            //BottomRight.YAxis.VisibleRange.Add(0, 10);
            BottomRight.XAxisTicks = null;
        }

        void CreateBottomLeftSeries()
        {
            const double rangeStart = 1;
            const int rangeEnd = 10;
            const double rangeStep = 1;
            BottomLeft.XAxis.AxisType = AxisType.Enumerated;
            BottomLeft.YAxis.AxisType = AxisType.Logarithmic;
            BottomLeft.XAxis.AxisTicks = new ObservableCollection<NewDataAxisTick>
            {
                new NewDataAxisTick(-1, null, false),
                new NewDataAxisTick(0, "Zero", false),
                new NewDataAxisTick(1, "One", false),
                new NewDataAxisTick(2, "Two", false),
                new NewDataAxisTick(3, "Three", false),
                new NewDataAxisTick(4, "Four", false),
                new NewDataAxisTick(5, "Five", false),
                new NewDataAxisTick(6, "Six", false),
                new NewDataAxisTick(7, "Seven", false),
                new NewDataAxisTick(8, "Eight", false),
                new NewDataAxisTick(9, "Nine", false),
                new NewDataAxisTick(10, "Ten", false),
                new NewDataAxisTick(11, null, false),
            };
            //BottomLeft.YAxis.DataRange.Update(0, 2);
            BottomLeft.XAxis.DataRange.Update(-1, 11);
            BottomLeft.YAxis.DataRange.Update(0.1, 10);
            var redSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x / 2.0)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = x / 2",
                Fill = new SolidColorBrush(Color.FromArgb(128, 255, 0, 0)),
            };
            var greenSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = x",
                Fill = new SolidColorBrush(Color.FromArgb(128, 0, 255, 0)),
            };
            var blueSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x * 1.5)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = 1.5x",
                Fill = new SolidColorBrush(Color.FromArgb(128, 0, 0, 255)),
            };
            var cyanSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x * 2)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = 2x",
                Fill = new SolidColorBrush(Color.FromArgb(128, 0, 255, 255)),
            };
            var magentaSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x * 2.5)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = 2.5x",
                Fill = new SolidColorBrush(Color.FromArgb(128, 255, 0, 255)),
            };
            var orangeSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x * 3)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = 3x",
                Fill = new SolidColorBrush(Color.FromArgb(128, 255, 165, 0)),
            };
            var blackSeries = new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x * 3.5)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = 3.5x",
                Fill = new SolidColorBrush(Color.FromArgb(128, 0, 0, 0)),
            };
            var stackedSeries = new StackedBarSeriesViewModel();
            stackedSeries.BarSeriesCollection.Add(greenSeries);
            stackedSeries.BarSeriesCollection.Add(redSeries);
            stackedSeries.BarSeriesCollection.Add(blueSeries);
            stackedSeries.BarSeriesCollection.Add(cyanSeries);
            stackedSeries.BarSeriesCollection.Add(magentaSeries);
            stackedSeries.BarSeriesCollection.Add(orangeSeries);
            stackedSeries.BarSeriesCollection.Add(blackSeries);
            BottomLeft.DataSeriesCollection.Add(stackedSeries);
            //BottomLeft.DataSeriesCollection.Add(redSeries);
            //BottomLeft.DataSeriesCollection.Add(blueSeries);
            //BottomLeft.YAxis.VisibleRange.Add(0, 10);
            BottomLeft.XAxisTicks = null;
        }

        void CreateMiddleRightSeries()
        {
            const double rangeStart = 1;
            const int rangeEnd = 10;
            const double rangeStep = 1;
            MiddleRight.YAxis.AxisType = AxisType.Logarithmic;
            MiddleRight.DataSeriesCollection.Add(new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x * x)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = x^2",
                Fill = Brushes.Blue,
            });
        }

        void CreateMiddleLeftSeries()
        {
            const double rangeStart = -10;
            const int rangeEnd = 10;
            const double rangeStep = 1;
            MiddleLeft.DataSeriesCollection.Add(new ImageSeriesViewModel
            {
                Top = 10,
                Left = -10,
                Bottom = -10,
                Right = 10,
                ImageSource = new BitmapImage(new Uri(@"C:\Users\Dave Anderson\Desktop\onr-logo.png")),
            });
            MiddleLeft.DataSeriesCollection.Add(new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, x)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = x",
                Stroke = Brushes.Blue,
                Fill = Brushes.Red,
            });
            MiddleLeft.DataSeriesCollection.Add(new BarSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, -x)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                StrokeThickness = 1,
                SeriesName = "y = -x",
                Fill = Brushes.Blue,
                Stroke = Brushes.Red,
            });
            MiddleLeft.XAxisTicks = null;
            MiddleLeft.XAxis.VisibleRange.Update(rangeStart - 0.5, rangeEnd + 0.5);
        }

        void CreateTopRightSeries()
        {
            const double rangeStart = -9;
            const int rangeEnd = 9;
            const double rangeStep = .1;
            TopRight.YAxis.AxisType = AxisType.Logarithmic;
            TopRight.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Cosh(x) + 1)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                SeriesName = "y = cosh(x) + 1",
                LineStroke = Brushes.DarkViolet,
                LineStrokeThickness = 1,
            });
            TopRight.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, (x * x) + 1)).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                SeriesName = "y = x^2 + 1",
                LineStroke = Brushes.Red,
                LineStrokeThickness = 1,
            });
            TopRight.DataSeriesCollection.Add(new LineSeriesViewModel
            {
                SeriesData = Range(rangeStart, rangeEnd, rangeStep).Select(x => Tuple.Create(x, Math.Exp(x))).ToObservableList(),
                ItemToPoint = i => new Point(((Tuple<double, double>)i).Item1, ((Tuple<double, double>)i).Item2),
                MarkerStrokeThickness = 1,
                SeriesName = "y = e^x",
                LineStroke = Brushes.Green,
                LineStrokeThickness = 1,
            });
        }
        void CreateTopLeftSeries()
        {
            const double rangeStart = 0.0;
            var rangeEnd = MoreMath.TwoPi + 0.01;
            var rangeStep = MoreMath.TwoPi / 16;
            const int pointSize = 10;
            TopLeft.XAxis.VisibleRange.Update(Math.Floor(rangeStart), Math.Ceiling(rangeEnd));
            TopLeft.DataSeriesCollection.Add(new LineSeriesViewModel
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
            TopLeft.DataSeriesCollection.Add(new LineSeriesViewModel
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
            TopLeft.DataSeriesCollection.Add(new LineSeriesViewModel
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
            TopLeft.DataSeriesCollection.Add(new LineSeriesViewModel
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
            TopLeft.DataSeriesCollection.Add(new LineSeriesViewModel
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
            TopLeft.DataSeriesCollection.Add(new LineSeriesViewModel
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
            TopLeft.DataSeriesCollection.Add(new LineSeriesViewModel
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
            TopLeft.DataSeriesCollection.Add(new LineSeriesViewModel
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
            TopLeft.DataSeriesCollection.Add(new LineSeriesViewModel
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
            TopLeft.DataSeriesCollection.Add(new LineSeriesViewModel
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
            TopLeft.DataSeriesCollection.Add(new LineSeriesViewModel
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
            TopLeft.DataSeriesCollection.Add(new LineSeriesViewModel
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