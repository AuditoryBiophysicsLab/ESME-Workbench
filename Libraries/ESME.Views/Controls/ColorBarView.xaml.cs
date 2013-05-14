using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media.Animation;
using System.Windows.Media.Imaging;
using HRC.Plotting;

namespace ESME.Views.Controls
{
    /// <summary>
    /// Interaction logic for ColorBarView.xaml
    /// </summary>
    public partial class ColorBarView
    {
        double _curRange;
        double _fullRange;
        Point _previousPoint;
        StepFunction _steps;
        Image _colorBarImage;
        IDisposable _currentRangeObserver, _fullRangeObserver, _statisticalRangeObserver;

        public ColorBarView() { InitializeComponent(); }

        #region Dependency Properties

        #region public WriteableBitmap ColorBarImage {get; set;}

        public static readonly DependencyProperty ColorBarImageProperty = DependencyProperty.Register("ColorBarImage", typeof (WriteableBitmap), typeof (ColorBarView), new FrameworkPropertyMetadata(ColorBarImagePropertyChanged));

        public WriteableBitmap ColorBarImage
        {
            get { return (WriteableBitmap) GetValue(ColorBarImageProperty); }
            set { SetCurrentValue(ColorBarImageProperty, value); }
        }

        static void ColorBarImagePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarView) obj).ColorBarImagePropertyChanged(args); }

        void ColorBarImagePropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            _colorBarImage = args.NewValue as Image;
            InvalidateVisual();
        }

        #endregion

        #region dependency property Range CurrentRange

        public static DependencyProperty CurrentRangeProperty = DependencyProperty.Register("CurrentRange",
                                                                                 typeof(Range),
                                                                                 typeof(ColorBarView),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, CurrentRangePropertyChanged));

        public Range CurrentRange { get { return (Range)GetValue(CurrentRangeProperty); } set { SetValue(CurrentRangeProperty, value); } }

        static void CurrentRangePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarView)obj).CurrentRangePropertyChanged(); }
        void CurrentRangePropertyChanged()
        {
            if (_currentRangeObserver != null) _currentRangeObserver.Dispose();
            if (CurrentRange == null) return;
            _currentRangeObserver = CurrentRange.Subscribe(e => CurrentRangeChanged());
        }
        void CurrentRangeChanged()
        {
            if (CurrentRange == null) return;
            if (_steps != null)
            {
                CurrentRange.Max = Math.Min(CurrentRange.Max, FullRange.Max);
                CurrentRange.Min = Math.Max(CurrentRange.Min, FullRange.Min);
                if ((CurrentRange.Min >= (CurrentRange.Max - _steps.Last().Y)) &&
                    ((CurrentRange.Max <= (CurrentRange.Min + _steps.Last().Y)))) return;
            }
            _curRange = CurrentRange.Max - CurrentRange.Min;
            topMargin.Height = Math.Max(0, ActualHeight * (FullRange.Max - CurrentRange.Max) / _fullRange);
            botMargin.Height = Math.Max(0, ActualHeight * (CurrentRange.Min - FullRange.Min) / _fullRange);
        }
        #endregion

        #region dependency property Range FullRange

        public static DependencyProperty FullRangeProperty = DependencyProperty.Register("FullRange",
                                                                                 typeof(Range),
                                                                                 typeof(ColorBarView),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, FullRangePropertyChanged));

        public Range FullRange { get { return (Range)GetValue(FullRangeProperty); } set { SetValue(FullRangeProperty, value); } }

        static void FullRangePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarView)obj).FullRangePropertyChanged(); }
        void FullRangePropertyChanged()
        {
            if (_fullRangeObserver != null) _fullRangeObserver.Dispose();
            if (FullRange == null) return;
            _fullRangeObserver = FullRange.Subscribe(e => FullRangeChanged());
            FullRangeChanged();
        }
        void FullRangeChanged()
        {
            if (FullRange == null) return;
            _fullRange = FullRange.Value;
            if (Math.Abs(_fullRange) < double.Epsilon) _fullRange = 1.0;
            _steps = new StepFunction(0, 95, 95, x => _fullRange * Math.Exp(-0.047 * x));
            NewResetColorbarRange(0.2);
        }
        #endregion

        #region dependency property Range StatisticalRange

        public static DependencyProperty StatisticalRangeProperty = DependencyProperty.Register("StatisticalRange",
                                                                                 typeof(Range),
                                                                                 typeof(ColorBarView),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, StatisticalRangePropertyChanged));

        public Range StatisticalRange { get { return (Range)GetValue(StatisticalRangeProperty); } set { SetValue(StatisticalRangeProperty, value); } }

        static void StatisticalRangePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarView)obj).StatisticalRangePropertyChanged(); }
        void StatisticalRangePropertyChanged()
        {
            if (_statisticalRangeObserver != null) _statisticalRangeObserver.Dispose();
            if (StatisticalRange == null) return;
            _statisticalRangeObserver = StatisticalRange.Subscribe(e => StatisticalRangeChanged());
            StatisticalRangeChanged();
        }
        void StatisticalRangeChanged() { }
        #endregion
#if false
        #region public double Maximum {get; set;}

        public static readonly DependencyProperty MaximumProperty = DependencyProperty.Register("Maximum", typeof (double), typeof (ColorBarView), new FrameworkPropertyMetadata(100.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, MinMaxPropertiesChanged));

        public double Maximum
        {
            get { return (double) GetValue(MaximumProperty); }
            set { SetCurrentValue(MaximumProperty, value); }
        }

        static void MinMaxPropertiesChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarView) obj).MinMaxPropertiesChanged(); }

        void MinMaxPropertiesChanged()
        {
            _fullRange = (Maximum - Minimum);
            if (Math.Abs(_fullRange) < double.Epsilon) _fullRange = 1.0;
            _steps = new StepFunction(0, 95, 95, x => _fullRange*Math.Exp(-0.047*x));
            ResetColorbarRange(0.2);
        }

        #endregion

        #region public double Minimum {get; set;}

        public static readonly DependencyProperty MinimumProperty = DependencyProperty.Register("Minimum", typeof (double), typeof (ColorBarView), new FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, MinMaxPropertiesChanged));

        public double Minimum
        {
            get { return (double) GetValue(MinimumProperty); }
            set { SetCurrentValue(MinimumProperty, value); }
        }

        #endregion

        #region dependency property double StatisticalMaximum

        public static DependencyProperty StatisticalMaximumProperty = DependencyProperty.Register("StatisticalMaximum", typeof(double), typeof(ColorBarView), new FrameworkPropertyMetadata(double.NaN, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public double StatisticalMaximum { get { return (double)GetValue(StatisticalMaximumProperty); } set { SetValue(StatisticalMaximumProperty, value); } }

        #endregion

        #region dependency property double StatisticalMinimum

        public static DependencyProperty StatisticalMinimumProperty = DependencyProperty.Register("StatisticalMinimum", typeof(double), typeof(ColorBarView), new FrameworkPropertyMetadata(double.NaN, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public double StatisticalMinimum { get { return (double)GetValue(StatisticalMinimumProperty); } set { SetValue(StatisticalMinimumProperty, value); } }

        #endregion

        #region public double CurrentMaximum {get; set;}

        public static readonly DependencyProperty CurrentMaximumProperty = DependencyProperty.Register("CurrentMaximum", typeof (double), typeof (ColorBarView), new FrameworkPropertyMetadata(100.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, CurMaximumPropertyChanged));

        public double CurrentMaximum
        {
            get { return (double) GetValue(CurrentMaximumProperty); }
            set { SetCurrentValue(CurrentMaximumProperty, value); }
        }

        static void CurMaximumPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarView) obj).CurMaximumPropertyChanged(args); }

        void CurMaximumPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if (_steps == null) return;
            if ((double)args.NewValue > Maximum) CurrentMaximum = Maximum;
            else if (CurrentMinimum < ((double)args.NewValue - _steps.Last().Y)) CurRangePropertiesChanged();
        }

        void CurRangePropertiesChanged()
        {
            _curRange = CurrentMaximum - CurrentMinimum;
            topMargin.Height = Math.Max(0, ActualHeight * (Maximum - CurrentMaximum) / _fullRange);
            botMargin.Height = Math.Max(0, ActualHeight * (CurrentMinimum - Minimum) / _fullRange);
        }

        #endregion

        #region public double CurrentMinimum {get; set;}

        public static readonly DependencyProperty CurrentMinimumProperty = DependencyProperty.Register("CurrentMinimum", typeof (double), typeof (ColorBarView), new FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, CurMinimumPropertyChanged));

        public double CurrentMinimum
        {
            get { return (double) GetValue(CurrentMinimumProperty); }
            set { SetCurrentValue(CurrentMinimumProperty, value); }
        }

        static void CurMinimumPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarView) obj).CurMinimumPropertyChanged(args); }

        void CurMinimumPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if (_steps == null) return;
            if ((double)args.NewValue < Minimum) CurrentMinimum = Minimum;
            else if (CurrentMaximum > ((double)args.NewValue + _steps.Last().Y)) CurRangePropertiesChanged();
        }

        #endregion
#endif

        #endregion

        #region Colorbar Animation
#if false
        // Animates the colorbar returning to its full range over a specified number of seconds
        void ResetColorbarRange(double transitionTimeSeconds)
        {
            var maxTarget = Maximum;
            var minTarget = Minimum;
            if (CurrentMaximum == Maximum && CurrentMinimum == Minimum && !double.IsNaN(StatisticalMaximum) && !double.IsNaN(StatisticalMinimum))
            {
                maxTarget = StatisticalMaximum;
                minTarget = StatisticalMinimum;
            }
            if (transitionTimeSeconds <= 0)
            {
                CurrentMaximum = maxTarget;
                CurrentMinimum = minTarget;
            }
            else
            {
                var duration = new Duration(TimeSpan.FromSeconds(transitionTimeSeconds));
                var curMaxAnimation = new DoubleAnimation(maxTarget, duration, FillBehavior.Stop);
                var curMinAnimation = new DoubleAnimation(minTarget, duration, FillBehavior.Stop);
                curMaxAnimation.Completed += delegate
                                             {
                                                 BeginAnimation(CurrentMaximumProperty, null);
                                                 CurrentMaximum = maxTarget;
                                             };
                curMinAnimation.Completed += delegate
                                             {
                                                 BeginAnimation(CurrentMinimumProperty, null);
                                                 CurrentMinimum = minTarget;
                                             };
                BeginAnimation(CurrentMaximumProperty, curMaxAnimation);
                BeginAnimation(CurrentMinimumProperty, curMinAnimation);
            }
        }
#endif

        // Animates the colorbar returning to its full range over a specified number of seconds
        void NewResetColorbarRange(double transitionTimeSeconds)
        {
            //var maxTarget = FullRange.Max;
            //var minTarget = FullRange.Min;
            _animationTarget = FullRange;
            if (CurrentRange == FullRange && !StatisticalRange.IsEmpty)
            {
                //maxTarget = StatisticalRange.Max;
                //minTarget = StatisticalRange.Min;
                _animationTarget = StatisticalRange;
            }
            if (transitionTimeSeconds <= 0)
            {
                //CurrentRange.Update(maxTarget, minTarget);
                CurrentRange.Update(_animationTarget);
            }
            else
            {
                var duration = new Duration(TimeSpan.FromSeconds(transitionTimeSeconds));
                //var curMaxAnimation = new DoubleAnimation(maxTarget, duration, FillBehavior.Stop);
                //var curMinAnimation = new DoubleAnimation(minTarget, duration, FillBehavior.Stop);
                _rangeAnimation = new RangeAnimation(CurrentRange, _animationTarget, duration);
                _rangeAnimation.Completed += RangeAnimationCompleted;
                //curMaxAnimation.Completed += () => 
                //{
                //    BeginAnimation(CurrentMaximumProperty, null);
                //    CurrentMaximum = maxTarget;
                //};
                //curMinAnimation.Completed += () =>
                //{
                //    BeginAnimation(CurrentMinimumProperty, null);
                //    CurrentMinimum = minTarget;
                //};
                //BeginAnimation(CurrentMaximumProperty, curMaxAnimation);
                //BeginAnimation(CurrentMinimumProperty, curMinAnimation);
                //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} BeginAnimation From: {1} To: {2}", DateTime.Now, CurrentRange, _animationTarget));
                BeginAnimation(CurrentRangeProperty, _rangeAnimation);
            }
        }

        Range _animationTarget;
        RangeAnimation _rangeAnimation;

        void RangeAnimationCompleted(object sender, EventArgs args)
        {
            //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} AnimationCompleted", DateTime.Now));
            BeginAnimation(CurrentRangeProperty, null);
            CurrentRange.Update(_animationTarget);
            _rangeAnimation.Completed -= RangeAnimationCompleted;
        }
        #endregion

        void Image_MouseWheel(object sender, MouseWheelEventArgs e)
        {
            var newRange = e.Delta < 0 ? _steps.StepForward(_curRange) : _steps.StepBack(_curRange);

            var newDelta = (_curRange - newRange)/2;
            //CurrentMaximum -= newDelta;
            //CurrentMinimum += newDelta;
            CurrentRange.Update(CurrentRange.Min - newDelta, CurrentRange.Max + newDelta);
        }

        void Image_MouseMove(object sender, MouseEventArgs e)
        {
            if (e.LeftButton == MouseButtonState.Pressed)
            {
                //Detect if moving up or down, left or right:
                //Up Positive, Down Negative, Left Negative, Right Positive.
                //FullRange / ActualHeightInPixels -> DeltaValuePP
                //XMove changes min, max by DeltaValuePP /2
                //YMove changes min/max by +/-DeltaValuePP
                var deltaValuePerPixel = _fullRange/ActualHeight;
                var yDeltaValue = (_previousPoint.Y - e.GetPosition(this).Y)*deltaValuePerPixel;
                var xDeltaValue = (e.GetPosition(this).X - _previousPoint.X)*deltaValuePerPixel;
                var netMouseMove = (xDeltaValue / 2) - yDeltaValue;
                //CurrentMaximum = CurrentMaximum - netMouseMove;
                //CurrentMinimum = CurrentMinimum - netMouseMove;
                CurrentRange.Update(CurrentRange.Min - netMouseMove, CurrentRange.Max - netMouseMove);
            }
            _previousPoint = e.GetPosition(this);
        }

        void ColorBarImageMouseUp(object sender, MouseButtonEventArgs e) { Mouse.Capture(null); }

        void DockPanel_MouseLeftButtonDown(object sender, MouseButtonEventArgs e)
        {
            switch (e.ClickCount)
            {
                case 1:
                    Mouse.Capture(_colorBarImage, CaptureMode.Element);
                    break;
                case 2:
                    NewResetColorbarRange(0.2);
                    break;
                default:
                    break;
            }
        }

        private void Image_MouseDown(object sender, MouseButtonEventArgs e)
        {
            if (e.LeftButton == MouseButtonState.Pressed) _previousPoint = e.GetPosition(this);
            else
            {
                //ColorMapViewModel.Default.Reverse();
                InvalidateVisual();
            }
        }
    }

    #region StepFunction class

    internal class StepFunction : List<Point>
    {
        public StepFunction(double startX, double endX, int numSteps, Func<double, double> mappingFunction) 
        { 
            for (var x = startX; x <= endX; x += (endX - startX)/numSteps) 
                Add(new Point(x, mappingFunction(x))); 
        }

        Point? FindY(double currentY)
        {
            for (var i = 0; i < this.Count() - 1; i++) 
                if ((this[i].Y <= currentY) && (currentY > this[i + 1].Y)) 
                    return this[i];
            return null;
        }

        public double StepForward(double currentY)
        {
            var curStep = FindY(currentY);

            if (curStep == null) return this[Count - 1].Y;

            var nextIndex = IndexOf(curStep.Value) + 1;

            if (nextIndex >= Count) return this[Count - 1].Y;
            return currentY + this[nextIndex].Y - curStep.Value.Y;
        }

        public double StepBack(double currentY)
        {
            var curStep = FindY(currentY);

            if (curStep == null) return this[0].Y;

            var prevIndex = IndexOf(curStep.Value) - 1;

            if (prevIndex < 0) return this[0].Y;
            return currentY + this[prevIndex].Y - curStep.Value.Y;
        }
    }

    #endregion
}