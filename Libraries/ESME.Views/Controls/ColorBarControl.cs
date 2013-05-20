using System;
using System.Collections.Generic;
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
    [TemplatePart(Name = "PART_DataAxis", Type = typeof(DataAxis))]
    public class ColorBarControl : Control
    {
        static ColorBarControl() { DefaultStyleKeyProperty.OverrideMetadata(typeof(ColorBarControl), new FrameworkPropertyMetadata(typeof(ColorBarControl))); }

        double _axisRange;
        double _previousY;
        StepFunction _steps;
        IDisposable _currentRangeObserver, _axisRangeObserver, _animationTargetRangeObserver;

        #region Dependency Properties

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
            CurrentRangeChanged();
        }

        void CurrentRangeChanged()
        {
            if (CurrentRange == null || CurrentRange.IsEmpty || AxisRange == null || AxisRange.IsEmpty) return;
            Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: CurrentRange changed to {1}", DateTime.Now, CurrentRange));
            //if (_steps != null)
            //{
            //    if ((CurrentRange.Min < axisVisibleRange.Min) || (CurrentRange.Max > axisVisibleRange.Max)) CurrentRange.Update(Math.Max(CurrentRange.Min, axisVisibleRange.Min), Math.Min(CurrentRange.Max, axisVisibleRange.Max));
            //    if ((CurrentRange.Min >= (CurrentRange.Max - _steps.Last().Y)) && 
            //        ((CurrentRange.Max <= (CurrentRange.Min + _steps.Last().Y)))) return;
            //}
            var top = Math.Max(0, ActualHeight * (AxisRange.Max - CurrentRange.Max) / _axisRange);
            var bottom = Math.Max(0, ActualHeight * (CurrentRange.Min - AxisRange.Min) / _axisRange);
            ColorbarAdjustmentMargins = new Thickness(0, top, 0, bottom);
        }

        void UpdateCurrentRange(double minDelta, double maxDelta = double.NaN)
        {
            if (double.IsNaN(minDelta)) throw new ArgumentException(@"Cannot be NaN", "minDelta");
            if (double.IsNaN(maxDelta)) maxDelta = minDelta;

            Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: UpdateCurrentRange by [{1:0.##}, {2:0.##}]", DateTime.Now, minDelta, maxDelta));
            CurrentRange.Update(Math.Max(FullRange.Min, CurrentRange.Min + minDelta), Math.Min(FullRange.Max, CurrentRange.Max + maxDelta));
        }
        #endregion

        #region dependency property Range FullRange

        public static DependencyProperty FullRangeProperty = DependencyProperty.Register("FullRange",
                                                                                 typeof(Range),
                                                                                 typeof(ColorBarControl),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, FullRangePropertyChanged));

        public Range FullRange { get { return (Range)GetValue(FullRangeProperty); } set { SetValue(FullRangeProperty, value); } }

        static void FullRangePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarControl)obj).FullRangePropertyChanged(); }
        void FullRangePropertyChanged() { }
        #endregion

        #region dependency property Range AnimationTargetRange

        public static DependencyProperty AnimationTargetRangeProperty = DependencyProperty.Register("AnimationTargetRange",
                                                                                                typeof(Range),
                                                                                                typeof(ColorBarControl),
                                                                                                new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, AnimationTargetRangePropertyChanged));

        public Range AnimationTargetRange { get { return (Range)GetValue(AnimationTargetRangeProperty); } set { SetValue(AnimationTargetRangeProperty, value); } }

        static void AnimationTargetRangePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarControl)obj).AnimationTargetRangePropertyChanged(); }
        void AnimationTargetRangePropertyChanged()
        {
            if (_animationTargetRangeObserver != null) _animationTargetRangeObserver.Dispose();
            if (AnimationTargetRange == null) return;
            _animationTargetRangeObserver = AnimationTargetRange.ObserveOnDispatcher().Subscribe(e => AnimationTargetRangeChanged());
            AnimationTargetRangeChanged();
        }
        void AnimationTargetRangeChanged()
        {
            //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: AnimationTargetRange changed to {1}", DateTime.Now, AnimationTargetRange));
            if (AnimationTargetRange == null || AnimationTargetRange.IsEmpty || CurrentRange == null) return;
            if (AnimationTime.Ticks <= 0) CurrentRange.Update(AnimationTargetRange);
            else
            {
                //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: Animating CurrentRange from {1} to {2}", DateTime.Now, CurrentRange, _animationTarget));
                var duration = new Duration(AnimationTime);
                var rangeAnimation = new RangeAnimation(CurrentRange, AnimationTargetRange, duration);
                Observable.FromEventPattern<EventArgs>(rangeAnimation, "Completed").Subscribe(e =>
                {
                    //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} AnimationCompleted", DateTime.Now));
                    BeginAnimation(CurrentRangeProperty, null);
                    CurrentRange.Update(AnimationTargetRange);
                });
                BeginAnimation(CurrentRangeProperty, rangeAnimation);
            }
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

        #region dependency property TimeSpan AnimationTime

        public static DependencyProperty AnimationTimeProperty = DependencyProperty.Register("AnimationTime",
                                                                                 typeof(TimeSpan),
                                                                                 typeof(ColorBarControl),
                                                                                 new FrameworkPropertyMetadata(TimeSpan.FromSeconds(0), FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, AnimationTimePropertyChanged));

        public TimeSpan AnimationTime { get { return (TimeSpan)GetValue(AnimationTimeProperty); } set { SetValue(AnimationTimeProperty, value); } }

        static void AnimationTimePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarControl)obj).AnimationTimePropertyChanged(); }
        void AnimationTimePropertyChanged() { }
        #endregion
    
        #region Read-only Dependency Properties

        #region dependency property Thickness ColorbarAdjustmentMargins

        static readonly DependencyPropertyKey ColorbarAdjustmentMarginsPropertyKey = DependencyProperty.RegisterReadOnly("ColorbarAdjustmentMargins",
                                                                                                                         typeof(Thickness),
                                                                                                                         typeof(ColorBarControl),
                                                                                                                         new FrameworkPropertyMetadata(new Thickness(0),
                                                                                                                                                       FrameworkPropertyMetadataOptions.BindsTwoWayByDefault,
                                                                                                                                                       ColorbarAdjustmentMarginsPropertyChanged));
        public static readonly DependencyProperty ColorbarAdjustmentMarginsProperty = ColorbarAdjustmentMarginsPropertyKey.DependencyProperty;
        public Thickness ColorbarAdjustmentMargins { get { return (Thickness)GetValue(ColorbarAdjustmentMarginsProperty); } protected set { SetValue(ColorbarAdjustmentMarginsPropertyKey, value); } }

        static void ColorbarAdjustmentMarginsPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarControl)obj).ColorbarAdjustmentMarginsPropertyChanged(); }
        void ColorbarAdjustmentMarginsPropertyChanged() { }
        #endregion

        #region dependency property Range AxisRange

        static readonly DependencyPropertyKey AxisRangePropertyKey = DependencyProperty.RegisterReadOnly("AxisRange",
                                                                                                         typeof(Range),
                                                                                                         typeof(ColorBarControl),
                                                                                                         new FrameworkPropertyMetadata(null,
                                                                                                                                       FrameworkPropertyMetadataOptions.BindsTwoWayByDefault,
                                                                                                                                       AxisRangePropertyChanged));
        public static readonly DependencyProperty AxisRangeProperty = AxisRangePropertyKey.DependencyProperty;
        public Range AxisRange { get { return (Range)GetValue(AxisRangeProperty); } protected set { SetValue(AxisRangePropertyKey, value); } }

        static void AxisRangePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarControl)obj).AxisRangePropertyChanged(); }
        void AxisRangePropertyChanged()
        {
            if (_axisRangeObserver != null) _axisRangeObserver.Dispose();
            if (AxisRange == null) return;
            _axisRangeObserver = AxisRange.ObserveOnDispatcher().Subscribe(e => AxisRangeChanged());
            AxisRangeChanged();

        }
        void AxisRangeChanged()
        {
            if (AxisRange == null) return;
            _axisRange = AxisRange.Size;
            Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: AxisRange changed to {1}", DateTime.Now, AxisRange));
            if (Math.Abs(_axisRange) < double.Epsilon) _axisRange = 1.0;
            _steps = new StepFunction(0, 95, 95, x => _axisRange * Math.Exp(-0.047 * x));
            CurrentRangeChanged();
            //ResetColorbarRange();
        }
        #endregion

        #endregion

        #endregion

        public override void OnApplyTemplate()
        {
            base.OnApplyTemplate();
            if (_mouseClicks != null) _mouseClicks.Dispose();
            if (_mouseMoves != null) _mouseMoves.Dispose();
            if (_mouseWheels != null) _mouseWheels.Dispose();
            if (_axisRangeObserver != null) _axisRangeObserver.Dispose();
            var imageInTemplate = (Image)GetTemplateChild("PART_Image");
            var dataAxisInTemplate = (DataAxis)GetTemplateChild("PART_DataAxis");
            if (imageInTemplate == null) throw new NullReferenceException("PART_Image required in template for ColorBarControl");
            if (dataAxisInTemplate == null) throw new NullReferenceException("PART_DataAxis required in template for ColorBarControl");
            AxisRange = dataAxisInTemplate.VisibleRange;
            if (imageInTemplate.ToolTip == null) imageInTemplate.ToolTip = new ToolTip { Content = new TextBlock { Text = "Left-click and drag or use the mouse wheel to change colorbar range" } };
            Observable.FromEventPattern<MouseEventArgs>(imageInTemplate, "MouseMove")
                .Select(e => new { IsDown = (bool?)null, Location = e.EventArgs.GetPosition(this) });
            _mouseClicks = Observable.FromEventPattern<MouseButtonEventArgs>(imageInTemplate, "MouseDown")
                .Where(e => e.EventArgs.ChangedButton == MouseButton.Left)
                .Select(e => new { IsDown = true, e.EventArgs.ClickCount, Location = e.EventArgs.GetPosition(this), e.EventArgs.MouseDevice, e.EventArgs.Timestamp })
                .Merge(Observable.FromEventPattern<MouseButtonEventArgs>(imageInTemplate, "MouseUp")
                           .Where(e => e.EventArgs.ChangedButton == MouseButton.Left)
                           .Select(e => new { IsDown = false, e.EventArgs.ClickCount, Location = e.EventArgs.GetPosition(this), e.EventArgs.MouseDevice, e.EventArgs.Timestamp }))
                .Subscribe(leftButton =>
                {
                    if (leftButton.IsDown)
                    {
                        switch (leftButton.ClickCount)
                        {
                            case 1:
                                //Mouse.Capture(this, CaptureMode.Element);
                                //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: Mouse captured", DateTime.Now));
                                break;
                            case 2:
                                RaiseEvent(new RoutedEventArgs(MouseDoubleClickEvent));
                                break;
                        }
                    }
                    else
                    {
                        //Mouse.Capture(null);
                        //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: Mouse released", DateTime.Now));
                        //InvalidateVisual();
                    }
                    _isLeftButtonDown = leftButton.IsDown;
                });
            _mouseMoves = Observable.FromEventPattern<MouseEventArgs>(imageInTemplate, "MouseMove")
                .Select(e => e.EventArgs.GetPosition(this))
                .Subscribe(mouseLocation =>
                {
                    if (_isLeftButtonDown)
                    {
                        //Detect if moving up or down, left or right:
                        //Up Positive, Down Negative, Left Negative, Right Positive.
                        //AxisRange / ActualHeightInPixels -> DeltaValuePP
                        //XMove changes min, max by DeltaValuePP /2
                        //YMove changes min/max by +/-DeltaValuePP
                        var deltaValuePerPixel = _axisRange / ActualHeight;
                        Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: deltaValuePerPixel is now {1}", DateTime.Now, deltaValuePerPixel));
                        Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: mouseDeltaY is {1}", DateTime.Now, _previousY - mouseLocation.Y));
                        var yDeltaValue = (_previousY - mouseLocation.Y) * deltaValuePerPixel;
                        Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: yDeltaValue is now {1}", DateTime.Now, yDeltaValue));
                        //var xDeltaValue = (mouseLocation.X - _previousPoint.X) * deltaValuePerPixel;
                        //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: xDeltaValue is now {1}", DateTime.Now, xDeltaValue));
                        //var netMouseMove = (xDeltaValue / 2) - yDeltaValue;
                        //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: netMouseMove is now {1}", DateTime.Now, netMouseMove));
                        //CurrentRange.Update(CurrentRange.Min + yDeltaValue, CurrentRange.Max + yDeltaValue);
                        UpdateCurrentRange(yDeltaValue);
                        Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: CurrentRange is now {1}", DateTime.Now, CurrentRange));
                    }
                    _previousY = mouseLocation.Y;
                    Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: _previousY is now {1}", DateTime.Now, _previousY));
                });
            _mouseWheels = Observable.FromEventPattern<MouseWheelEventArgs>(imageInTemplate, "MouseWheel")
                .Select(e => e.EventArgs.Delta)
                .Subscribe(delta =>
                {
                    var curRange = CurrentRange.Size;
                    var newRange = delta < 0 ? _steps.StepForward(curRange) : _steps.StepBack(curRange);

                    var newDelta = (curRange - newRange) / 2;
                    CurrentRange.Update(CurrentRange.Min - newDelta, CurrentRange.Max + newDelta);
                    UpdateCurrentRange(-newDelta, newDelta);
                    Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: Expanding current range by {1}", DateTime.Now, newDelta));
                });
        }

        IDisposable _mouseClicks, _mouseMoves, _mouseWheels;
        bool _isLeftButtonDown;
#if false
        #region Colorbar Animation
        // Animates the colorbar returning to its full range over a specified number of seconds
        void ResetColorbarRange()
        {
            _animationTarget = AxisRange;
            if (CurrentRange == AxisRange && !AnimationTargetRange.IsEmpty) _animationTarget = AnimationTargetRange;
            if (AnimationTime.Ticks <= 0) CurrentRange.Update(_animationTarget);
            else
            {
                //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} ColorBarControl: Animating CurrentRange from {1} to {2}", DateTime.Now, CurrentRange, _animationTarget));
                var duration = new Duration(AnimationTime);
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
                //AxisRange / ActualHeightInPixels -> DeltaValuePP
                //XMove changes min, max by DeltaValuePP /2
                //YMove changes min/max by +/-DeltaValuePP
                var deltaValuePerPixel = _AxisRange/ActualHeight;
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

    internal class StepFunction : List<Point>
    {
        public StepFunction(double startX, double endX, int numSteps, Func<double, double> mappingFunction)
        {
            for (var x = startX; x <= endX; x += (endX - startX) / numSteps)
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
}