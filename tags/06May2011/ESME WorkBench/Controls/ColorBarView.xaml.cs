#if false
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media.Animation;
using System.Windows.Media.Imaging;

namespace ESMEWorkBench.Controls
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

        public ColorBarView() { InitializeComponent(); }

        #region Dependency Properties

        #region public WriteableBitmap ColorBarImage {get; set;}

        public static readonly DependencyProperty ColorBarImageProperty = DependencyProperty.Register("ColorBarImage", typeof (WriteableBitmap), typeof (ColorBarView), new FrameworkPropertyMetadata(new PropertyChangedCallback(ColorBarImagePropertyChanged)));

        public WriteableBitmap ColorBarImage
        {
            get { return (WriteableBitmap) GetValue(ColorBarImageProperty); }
            set { SetValue(ColorBarImageProperty, value); }
        }

        static void ColorBarImagePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarView) obj).ColorBarImagePropertyChanged(args); }

        void ColorBarImagePropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            _colorBarImage = args.NewValue as Image;
            InvalidateVisual();
        }

        #endregion

        #region public double Maximum {get; set;}

        public static readonly DependencyProperty MaximumProperty = DependencyProperty.Register("Maximum", typeof (double), typeof (ColorBarView), new FrameworkPropertyMetadata(100.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, MinMaxPropertiesChanged));

        public double Maximum
        {
            get { return (double) GetValue(MaximumProperty); }
            set { SetValue(MaximumProperty, value); }
        }

        static void MinMaxPropertiesChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarView) obj).MinMaxPropertiesChanged(); }

        void MinMaxPropertiesChanged()
        {
            _fullRange = (Maximum - Minimum);
            _steps = new StepFunction(0, 95, 95, x => _fullRange*Math.Exp(-0.047*x));
            ResetColorbarRange(0.2);
        }

        #endregion

        #region public double Minimum {get; set;}

        public static readonly DependencyProperty MinimumProperty = DependencyProperty.Register("Minimum", typeof (double), typeof (ColorBarView), new FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, MinMaxPropertiesChanged));

        public double Minimum
        {
            get { return (double) GetValue(MinimumProperty); }
            set { SetValue(MinimumProperty, value); }
        }

        #endregion

        #region public double CurrentMaximum {get; set;}

        public static readonly DependencyProperty CurrentMaximumProperty = DependencyProperty.Register("CurrentMaximum", typeof (double), typeof (ColorBarView), new FrameworkPropertyMetadata(100.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, CurMaximumPropertyChanged));

        public double CurrentMaximum
        {
            get { return (double) GetValue(CurrentMaximumProperty); }
            set { SetValue(CurrentMaximumProperty, value); }
        }

        static void CurMaximumPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarView) obj).CurMaximumPropertyChanged(args); }

        void CurMaximumPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if ((double)args.NewValue > Maximum) CurrentMaximum = Maximum;
            // else if (CurrentMinimum >= ((double) args.NewValue - _steps.Last().Y)) CurrentMinimum = (double) args.NewValue - _steps.Last().Y;
            else if (CurrentMinimum >= ((double)args.NewValue - _steps.Last().Y)) return;
            else CurRangePropertiesChanged();
        }

        void CurRangePropertiesChanged()
        {
            _curRange = CurrentMaximum - CurrentMinimum;
            double topHeight = ActualHeight*(Maximum - CurrentMaximum)/_fullRange;
            double botHeight = ActualHeight*(CurrentMinimum - Minimum)/_fullRange;
            if (topHeight >= 0) topMargin.Height = ActualHeight*(Maximum - CurrentMaximum)/_fullRange;
            else topMargin.Height = 0;
            if (botHeight >= 0) botMargin.Height = ActualHeight*(CurrentMinimum - Minimum)/_fullRange;
            else botMargin.Height = 0;
            //botMargin.Height = 0;
        }

        #endregion

        #region public double CurrentMinimum {get; set;}

        public static readonly DependencyProperty CurrentMinimumProperty = DependencyProperty.Register("CurrentMinimum", typeof (double), typeof (ColorBarView), new FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, CurMinimumPropertyChanged));

        public double CurrentMinimum
        {
            get { return (double) GetValue(CurrentMinimumProperty); }
            set { SetValue(CurrentMinimumProperty, value); }
        }

        static void CurMinimumPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ColorBarView) obj).CurMinimumPropertyChanged(args); }

        void CurMinimumPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if ((double)args.NewValue < Minimum) CurrentMinimum = Minimum;
            //else if (CurrentMaximum <= ((double) args.NewValue + _steps.Last().Y)) CurrentMaximum = (double) args.NewValue + _steps.Last().Y;
            else if (CurrentMaximum <= ((double)args.NewValue + _steps.Last().Y)) return;
            else CurRangePropertiesChanged();
        }

        #endregion

        #endregion

        #region Colorbar Animation

        // Animates the colorbar returning to its full range over a specified number of seconds
        void ResetColorbarRange(double transitionTimeSeconds)
        {
            if (transitionTimeSeconds <= 0)
            {
                CurrentMaximum = Maximum;
                CurrentMinimum = Minimum;
            }
            else
            {
                var duration = new Duration(TimeSpan.FromSeconds(transitionTimeSeconds));
                var curMaxAnimation = new DoubleAnimation(Maximum, duration, FillBehavior.Stop);
                var curMinAnimation = new DoubleAnimation(Minimum, duration, FillBehavior.Stop);
                curMaxAnimation.Completed += delegate
                                             {
                                                 BeginAnimation(CurrentMaximumProperty, null);
                                                 CurrentMaximum = Maximum;
                                             };
                curMinAnimation.Completed += delegate
                                             {
                                                 BeginAnimation(CurrentMinimumProperty, null);
                                                 CurrentMinimum = Minimum;
                                             };
                BeginAnimation(CurrentMaximumProperty, curMaxAnimation);
                BeginAnimation(CurrentMinimumProperty, curMinAnimation);
            }
        }

        #endregion

        void Image_MouseWheel(object sender, MouseWheelEventArgs e)
        {
            var newRange = e.Delta < 0 ? _steps.StepForward(_curRange) : _steps.StepBack(_curRange);

            double newDelta = (_curRange - newRange)/2;
            CurrentMaximum -= newDelta;
            CurrentMinimum += newDelta;
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
                CurrentMaximum = CurrentMaximum - netMouseMove;
                CurrentMinimum = CurrentMinimum - netMouseMove;
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
#endif