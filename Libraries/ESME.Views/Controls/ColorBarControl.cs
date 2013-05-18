using System;
using System.Diagnostics;
using System.Linq;
using System.Reactive.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media.Imaging;
using HRC.Plotting;

namespace ESME.Views.Controls
{
    [TemplatePart(Name = "PART_Image", Type = typeof(Image))]
    public class ColorBarControl : Control
    {
        static ColorBarControl() { DefaultStyleKeyProperty.OverrideMetadata(typeof(ColorBarControl), new FrameworkPropertyMetadata(typeof(ColorBarControl))); }

        public ColorBarControl() { ColorbarAdjustmentMargins = _colorbarAdjustmentMargins; }
        double _curRange;
        double _fullRange;
        Point _previousPoint;
        StepFunction _steps;
        IDisposable _currentRangeObserver, _fullRangeObserver, _statisticalRangeObserver;

        #region Dependency Properties

        #region dependency property ColorMapViewModel ColorMapViewModel

        public static DependencyProperty ColorMapViewModelProperty = DependencyProperty.Register("ColorMapViewModel",
                                                                                                 typeof(ColorMapViewModel),
                                                                                                 typeof(ColorBarControl),
                                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, ColorMapViewModelPropertyChanged));

        public ColorMapViewModel ColorMapViewModel { get { return (ColorMapViewModel)GetValue(ColorMapViewModelProperty); } set { SetValue(ColorMapViewModelProperty, value); } }

        static void ColorMapViewModelPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarControl)obj).ColorMapViewModelPropertyChanged(); }
        void ColorMapViewModelPropertyChanged() { }
        #endregion

        #region public WriteableBitmap ColorBarImage {get; set;}

        public static readonly DependencyProperty ColorBarImageProperty = DependencyProperty.Register("ColorBarImage", typeof (WriteableBitmap), typeof (ColorBarControl), new FrameworkPropertyMetadata(ColorBarImagePropertyChanged));

        public WriteableBitmap ColorBarImage
        {
            get { return (WriteableBitmap) GetValue(ColorBarImageProperty); }
            set { SetCurrentValue(ColorBarImageProperty, value); }
        }

        static void ColorBarImagePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarControl) obj).ColorBarImagePropertyChanged(); }

        void ColorBarImagePropertyChanged() { InvalidateVisual(); }

        #endregion

        #region dependency property Range CurrentRange

        public static DependencyProperty CurrentRangeProperty = DependencyProperty.Register("CurrentRange",
                                                                                            typeof(Range),
                                                                                            typeof(ColorBarControl),
                                                                                            new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, CurrentRangePropertyChanged));

        public Range CurrentRange { get { return (Range)GetValue(CurrentRangeProperty); } set { SetValue(CurrentRangeProperty, value); } }

        static void CurrentRangePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarControl)obj).CurrentRangePropertyChanged(); }
        void CurrentRangePropertyChanged()
        {
            if (_currentRangeObserver != null) _currentRangeObserver.Dispose();
            if (CurrentRange == null) return;
            _currentRangeObserver = CurrentRange.ObserveOnDispatcher().Subscribe(e => CurrentRangeChanged());
        }
        void CurrentRangeChanged()
        {
            if (CurrentRange == null) return;
            //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: CurrentRange changed to {1}", DateTime.Now, CurrentRange));
            if (_steps != null)
            {
                CurrentRange.Max = Math.Min(CurrentRange.Max, FullRange.Max);
                CurrentRange.Min = Math.Max(CurrentRange.Min, FullRange.Min);
                if ((CurrentRange.Min >= (CurrentRange.Max - _steps.Last().Y)) &&
                    ((CurrentRange.Max <= (CurrentRange.Min + _steps.Last().Y)))) return;
            }
            _curRange = CurrentRange.Max - CurrentRange.Min;
            _colorbarAdjustmentMargins.Top = Math.Max(0, ActualHeight * (FullRange.Max - CurrentRange.Max) / _fullRange);
            _colorbarAdjustmentMargins.Bottom = Math.Max(0, ActualHeight * (CurrentRange.Min - FullRange.Min) / _fullRange);
            ColorbarAdjustmentMargins = _colorbarAdjustmentMargins;
        }
        #endregion

        #region dependency property Range FullRange

        public static DependencyProperty FullRangeProperty = DependencyProperty.Register("FullRange",
                                                                                         typeof(Range),
                                                                                         typeof(ColorBarControl),
                                                                                         new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, FullRangePropertyChanged));

        public Range FullRange { get { return (Range)GetValue(FullRangeProperty); } set { SetValue(FullRangeProperty, value); } }

        static void FullRangePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarControl)obj).FullRangePropertyChanged(); }
        void FullRangePropertyChanged()
        {
            if (_fullRangeObserver != null) _fullRangeObserver.Dispose();
            if (FullRange == null) return;
            _fullRangeObserver = FullRange.ObserveOnDispatcher().Subscribe(e => FullRangeChanged(true));
            FullRangeChanged(false);
        }
        void FullRangeChanged(bool animate)
        {
            if (FullRange == null) return;
            _fullRange = FullRange.Value;
            //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: FullRange changed to {1}", DateTime.Now, FullRange));
            if (Math.Abs(_fullRange) < double.Epsilon) _fullRange = 1.0;
            _steps = new StepFunction(0, 95, 95, x => _fullRange * Math.Exp(-0.047 * x));
            if (animate) ResetColorbarRange(0.0);
        }
        #endregion

        #region dependency property Range StatisticalRange

        public static DependencyProperty StatisticalRangeProperty = DependencyProperty.Register("StatisticalRange",
                                                                                                typeof(Range),
                                                                                                typeof(ColorBarControl),
                                                                                                new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, StatisticalRangePropertyChanged));

        public Range StatisticalRange { get { return (Range)GetValue(StatisticalRangeProperty); } set { SetValue(StatisticalRangeProperty, value); } }

        static void StatisticalRangePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarControl)obj).StatisticalRangePropertyChanged(); }
        void StatisticalRangePropertyChanged()
        {
            if (_statisticalRangeObserver != null) _statisticalRangeObserver.Dispose();
            if (StatisticalRange == null) return;
            _statisticalRangeObserver = StatisticalRange.ObserveOnDispatcher().Subscribe(e => StatisticalRangeChanged());
            StatisticalRangeChanged();
        }
        void StatisticalRangeChanged()
        {
            //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: StatisticalRange changed to {1}", DateTime.Now, StatisticalRange));
        }
        #endregion

        #region dependency property string AxisTitle

        public static DependencyProperty AxisTitleProperty = DependencyProperty.Register("AxisTitle",
                                                                                 typeof(string),
                                                                                 typeof(ColorBarControl),
                                                                                 new FrameworkPropertyMetadata("ColorBarControl", FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, AxisTitlePropertyChanged));

        public string AxisTitle { get { return (string)GetValue(AxisTitleProperty); } set { SetValue(AxisTitleProperty, value); } }

        static void AxisTitlePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarControl)obj).AxisTitlePropertyChanged(); }
        void AxisTitlePropertyChanged() { }
        #endregion


        #region dependency property Thickness ColorbarAdjustmentMargins

        static readonly DependencyPropertyKey ColorbarAdjustmentMarginsPropertyKey = DependencyProperty.RegisterReadOnly("ColorbarAdjustmentMargins",
                                                                                                                         typeof(Thickness),
                                                                                                                         typeof(ColorBarControl),
                                                                                                                         new FrameworkPropertyMetadata(new Thickness(0),
                                                                                                                                                       FrameworkPropertyMetadataOptions.
                                                                                                                                                           BindsTwoWayByDefault,
                                                                                                                                                       ColorbarAdjustmentMarginsPropertyChanged));
        public static readonly DependencyProperty ColorbarAdjustmentMarginsProperty = ColorbarAdjustmentMarginsPropertyKey.DependencyProperty;
        public Thickness ColorbarAdjustmentMargins { get { return (Thickness)GetValue(ColorbarAdjustmentMarginsProperty); } protected set { SetValue(ColorbarAdjustmentMarginsPropertyKey, value); } }

        static void ColorbarAdjustmentMarginsPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarControl)obj).ColorbarAdjustmentMarginsPropertyChanged(); }
        void ColorbarAdjustmentMarginsPropertyChanged() { }
        Thickness _colorbarAdjustmentMargins = new Thickness(0);
        #endregion


        #endregion

        protected override void OnTemplateChanged(ControlTemplate oldTemplate, ControlTemplate newTemplate)
        {
            base.OnTemplateChanged(oldTemplate, newTemplate);
            var imageInTemplate = (Image)newTemplate.FindName("PART_Image", this);
            if (imageInTemplate == null)
            {
                if (_mouseClicks != null) _mouseClicks.Dispose();
                if (_mouseMoves != null) _mouseMoves.Dispose();
                if (_mouseWheels != null) _mouseWheels.Dispose();
                return;
            }
            _mouseClicks = Observable.FromEventPattern<MouseButtonEventArgs>(imageInTemplate, "MouseDown")
                .Where(e => e.EventArgs.ChangedButton == MouseButton.Left)
                .Select(e => new { IsDown = true, e.EventArgs.ClickCount, Location = e.EventArgs.GetPosition(imageInTemplate) })
                .Merge(Observable.FromEventPattern<MouseButtonEventArgs>(imageInTemplate, "MouseUp")
                           .Where(e => e.EventArgs.ChangedButton == MouseButton.Left)
                           .Select(e => new { IsDown = false, e.EventArgs.ClickCount, Location = e.EventArgs.GetPosition(imageInTemplate) }))
                .Subscribe(leftButton =>
                {
                    if (leftButton.IsDown)
                    {
                        _previousPoint = leftButton.Location;
                        switch (leftButton.ClickCount)
                        {
                            case 1:
                                Mouse.Capture(imageInTemplate, CaptureMode.Element);
                                break;
                            case 2:
                                ResetColorbarRange(0.2);
                                break;
                        }
                    }
                    else
                    {
                        Mouse.Capture(null);
                        //InvalidateVisual();
                    }
                    _isLeftButtonDown = leftButton.IsDown;
                });
            _mouseMoves = Observable.FromEventPattern<MouseEventArgs>(imageInTemplate, "MouseMove")
                .Select(e => e.EventArgs.GetPosition(imageInTemplate))
                .Subscribe(mouseLocation =>
                {
                    if (_isLeftButtonDown)
                    {
                        //Detect if moving up or down, left or right:
                        //Up Positive, Down Negative, Left Negative, Right Positive.
                        //FullRange / ActualHeightInPixels -> DeltaValuePP
                        //XMove changes min, max by DeltaValuePP /2
                        //YMove changes min/max by +/-DeltaValuePP
                        var deltaValuePerPixel = _fullRange / imageInTemplate.ActualHeight;
                        var yDeltaValue = (_previousPoint.Y - mouseLocation.Y) * deltaValuePerPixel;
                        var xDeltaValue = (mouseLocation.X - _previousPoint.X) * deltaValuePerPixel;
                        var netMouseMove = (xDeltaValue / 2) - yDeltaValue;
                        CurrentRange.Update(CurrentRange.Min - netMouseMove, CurrentRange.Max - netMouseMove);
                    }
                    _previousPoint = mouseLocation;

                });
            _mouseWheels = Observable.FromEventPattern<MouseWheelEventArgs>(imageInTemplate, "MouseWheel")
                .Select(e => e.EventArgs.Delta)
                .Subscribe(delta =>
                {
                    var newRange = delta < 0 ? _steps.StepForward(_curRange) : _steps.StepBack(_curRange);

                    var newDelta = (_curRange - newRange) / 2;
                    CurrentRange.Update(CurrentRange.Min - newDelta, CurrentRange.Max + newDelta);
                });
        }

        IDisposable _mouseClicks, _mouseMoves, _mouseWheels;
        bool _isLeftButtonDown;
        #region Colorbar Animation
        // Animates the colorbar returning to its full range over a specified number of seconds
        void ResetColorbarRange(double transitionTimeSeconds)
        {
            _animationTarget = FullRange;
            if (CurrentRange == FullRange && !StatisticalRange.IsEmpty) _animationTarget = StatisticalRange;
            if (transitionTimeSeconds <= 0) CurrentRange.Update(_animationTarget);
            else
            {
                Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: Animating CurrentRange from {1} to {2}", DateTime.Now, CurrentRange, _animationTarget));
                var duration = new Duration(TimeSpan.FromSeconds(transitionTimeSeconds));
                _rangeAnimation = new RangeAnimation(CurrentRange, _animationTarget, duration);
                _rangeAnimation.Completed += RangeAnimationCompleted;
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
#if false
        void Image_MouseWheel(object sender, MouseWheelEventArgs e)
        {
            var newRange = e.Delta < 0 ? _steps.StepForward(_curRange) : _steps.StepBack(_curRange);

            var newDelta = (_curRange - newRange)/2;
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
                    ResetColorbarRange(0.2);
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
#endif
    }
}