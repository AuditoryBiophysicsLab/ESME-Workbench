using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Media.Animation;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.ComponentModel;
using ESME.GUI.ViewModels;

namespace ESME.GUI.Controls
{
    /// <summary>
    /// Interaction logic for ColorBarView.xaml
    /// </summary>
    public partial class ColorBarView : UserControl
    {
        public ColorBarView()
        {
            InitializeComponent();
        }

        #region Dependency Properties
        #region public WriteableBitmap ColorBarImage {get; set;}
#if false
        public static readonly DependencyProperty ColorBarImageProperty = DependencyProperty.Register("ColorBarImage",
            typeof(WriteableBitmap), typeof(ColorBarView));
        public WriteableBitmap ColorBarImage
        {
            get { return (WriteableBitmap)GetValue(ColorBarView.ColorBarImageProperty); }
            set { SetValue(ColorBarView.ColorBarImageProperty, value); _colorBarImage.Source = value; }
        }
#endif
        public static readonly DependencyProperty ColorBarImageProperty = DependencyProperty.Register("ColorBarImage",
            typeof(WriteableBitmap), typeof(ColorBarView),
            new FrameworkPropertyMetadata(new PropertyChangedCallback(ColorBarImagePropertyChanged)));
        public WriteableBitmap ColorBarImage
        {
            get { return (WriteableBitmap)GetValue(ColorBarView.ColorBarImageProperty); }
            set { SetValue(ColorBarView.ColorBarImageProperty, value); }
        }
        static void ColorBarImagePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            (obj as ColorBarView).ColorBarImagePropertyChanged(args);
        }
        void ColorBarImagePropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            _colorBarImage = args.NewValue as Image;
            InvalidateVisual();
        }

        #endregion

        #region public double Maximum {get; set;}
        public static readonly DependencyProperty MaximumProperty = DependencyProperty.Register("Maximum", 
            typeof(double), typeof(ColorBarView), 
            new FrameworkPropertyMetadata(100.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault,
                new PropertyChangedCallback(MinMaxPropertiesChanged)));
        public double Maximum
        {
            get { return (double)GetValue(ColorBarView.MaximumProperty); }
            set { SetValue(ColorBarView.MaximumProperty, value); }
        }
        static void MinMaxPropertiesChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            (obj as ColorBarView).MinMaxPropertiesChanged(args);
        }
        void MinMaxPropertiesChanged(DependencyPropertyChangedEventArgs args)
        {
            _fullRange = (Maximum - Minimum);
            _steps = new StepFunction(0, 95, 95, x => _fullRange * Math.Exp(-0.047 * x));
            ResetColorbarRange(0.2);
        }
        #endregion

        #region public double Minimum {get; set;}
        public static readonly DependencyProperty MinimumProperty = DependencyProperty.Register("Minimum", 
            typeof(double), typeof(ColorBarView),
            new FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault,
                new PropertyChangedCallback(MinMaxPropertiesChanged)));
        public double Minimum
        {
            get { return (double)GetValue(ColorBarView.MinimumProperty); }
            set { SetValue(ColorBarView.MinimumProperty, value); }
        }
        #endregion

        #region public double CurrentMaximum {get; set;}
        public static readonly DependencyProperty CurrentMaximumProperty = DependencyProperty.Register("CurrentMaximum", 
            typeof(double), typeof(ColorBarView),
            new FrameworkPropertyMetadata(100.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault,
                new PropertyChangedCallback(CurMaximumPropertyChanged)));
        public double CurrentMaximum
        {
            get { return (double)GetValue(ColorBarView.CurrentMaximumProperty); }
            set { SetValue(ColorBarView.CurrentMaximumProperty, value); }
        }
        static void CurMaximumPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            (obj as ColorBarView).CurMaximumPropertyChanged(args);
        }
        void CurMaximumPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if ((double)args.NewValue > Maximum)
                CurrentMaximum = Maximum;
            else if (CurrentMinimum >= ((double)args.NewValue - _steps.Last().Y))
                CurrentMinimum = (double)args.NewValue - _steps.Last().Y;
            else
                CurRangePropertiesChanged(args);
           
        }
        void CurRangePropertiesChanged(DependencyPropertyChangedEventArgs args)
        {   
            _curRange = CurrentMaximum - CurrentMinimum;
            double topHeight, botHeight;
            topHeight = this.ActualHeight * (Maximum - CurrentMaximum) / _fullRange;
            botHeight = this.ActualHeight * (CurrentMinimum - Minimum) / _fullRange;
            if (topHeight >= 0)
                topMargin.Height = this.ActualHeight * (Maximum - CurrentMaximum) / _fullRange;
            else topMargin.Height = 0;
            if (botHeight >= 0)
                botMargin.Height = this.ActualHeight * (CurrentMinimum - Minimum) / _fullRange;
            else botMargin.Height = 0;
        }
        #endregion

        #region public double CurrentMinimum {get; set;}
        public static readonly DependencyProperty CurrentMinimumProperty = DependencyProperty.Register("CurrentMinimum", 
            typeof(double), typeof(ColorBarView),
            new FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault,
                new PropertyChangedCallback(CurMinimumPropertyChanged)));
        public double CurrentMinimum
        {
            get { return (double)GetValue(ColorBarView.CurrentMinimumProperty); }
            set { SetValue(ColorBarView.CurrentMinimumProperty, value); }
        }
        static void CurMinimumPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            (obj as ColorBarView).CurMinimumPropertyChanged(args);
        }
        void CurMinimumPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if ((double)args.NewValue < Minimum)
                CurrentMinimum = Minimum;
            else if (CurrentMaximum <= ((double)args.NewValue + _steps.Last().Y))
                CurrentMaximum = (double)args.NewValue + _steps.Last().Y;
            else
                CurRangePropertiesChanged(args);
        }
        #endregion

        #endregion

        #region Colorbar Animation
        // Animates the colorbar returning to its full range over a specified number of seconds
        private void ResetColorbarRange(double TransitionTime_seconds)
        {
            if (TransitionTime_seconds <= 0)
            {
                CurrentMaximum = Maximum;
                CurrentMinimum = Minimum;
            }
            else
            {
                var Duration = new Duration(TimeSpan.FromSeconds(TransitionTime_seconds));
                var CurMaxAnimation = new DoubleAnimation(Maximum, Duration, FillBehavior.Stop);
                var CurMinAnimation = new DoubleAnimation(Minimum, Duration, FillBehavior.Stop);
                CurMaxAnimation.Completed += delegate(object o, EventArgs e)
                {
                    this.BeginAnimation(CurrentMaximumProperty, null);
                    CurrentMaximum = Maximum;
                };
                CurMinAnimation.Completed += delegate(object o, EventArgs e)
                {
                    this.BeginAnimation(CurrentMinimumProperty, null);
                    CurrentMinimum = Minimum;
                };
                this.BeginAnimation(CurrentMaximumProperty, CurMaxAnimation);
                this.BeginAnimation(CurrentMinimumProperty, CurMinAnimation);
            }
        }
        #endregion


        private void Image_MouseWheel(object sender, MouseWheelEventArgs e)
        {
            double newRange;

            if (e.Delta < 0)
                newRange = _steps.StepForward(_curRange);
            else
                newRange = _steps.StepBack(_curRange);

            double newDelta = (_curRange - newRange) / 2;
            CurrentMaximum -= newDelta;
            CurrentMinimum += newDelta;
        }

        protected override void  OnInitialized(EventArgs e)
        {
            base.OnInitialized(e);
            Binding binding = new Binding("RadialDataViewModel.ColorMapViewModel.ColorBitmap");
            binding.Mode = BindingMode.OneWay;
            binding.Source = ColorBarImage;
        }

      
        private void Image_MouseMove(object sender, MouseEventArgs e)
        {
            
            if (e.LeftButton == MouseButtonState.Pressed)
            {
                double YDeltaValue = 0;
                double XDeltaValue = 0;
                double DeltaValuePerPixel;
                //Detect if moving up or down, left or right:
                //Up Positive, Down Negative, Left Negative, Right Positive.
                //FullRange / ActualHeightInPixels -> DeltaValuePP
                //XMove changes min, max by DeltaValuePP /2
                //YMove changes min/max by +/-DeltaValuePP
                DeltaValuePerPixel = _fullRange / this.ActualHeight;
                YDeltaValue = (_previousPoint.Y - e.GetPosition(this).Y) * DeltaValuePerPixel;
                XDeltaValue = (e.GetPosition(this).X - _previousPoint.X) * DeltaValuePerPixel;
                CurrentMaximum = CurrentMaximum + XDeltaValue / 2 - YDeltaValue;
                CurrentMinimum = CurrentMinimum - XDeltaValue / 2 - YDeltaValue;
            }
            _previousPoint = e.GetPosition(this);
        }

        void Image_MouseDown(object sender, MouseEventArgs e)
        {
            if(e.LeftButton == MouseButtonState.Pressed)
                _previousPoint = e.GetPosition(this);
            else
            {
                ColorMapViewModel.Default.Reverse();
                this.InvalidateVisual();
            }
        }

        void _colorBarImage_MouseUp(object sender, MouseButtonEventArgs e)
        {
            Mouse.Capture(null);
        }

        private void DockPanel_MouseLeftButtonDown(object sender, MouseButtonEventArgs e)
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

        private StepFunction _steps;
        private double _fullRange, _curRange;
        private Point _previousPoint = new Point();
    }

    #region StepFunction class
    internal class StepFunction : List<Point>
    {
        public StepFunction(double StartX, double EndX, int NumSteps, Func<double, double> MappingFunction)
        {
            for (double x = StartX; x <= EndX; x += (EndX - StartX) / NumSteps)
                Add(new Point(x, MappingFunction(x)));
        }

        private Point? FindY(double CurrentY)
        {
            for (int i = 0; i < this.Count() - 1; i++)
                if ((this[i].Y <= CurrentY) && (CurrentY > this[i + 1].Y))
                    return this[i];
            return null;
        }

        public double StepForward(double CurrentY)
        {
            Point? CurStep = FindY(CurrentY);
            
            if (CurStep == null)
                return this[Count - 1].Y; 
            
            double CurY = CurStep.Value.Y;
            int NextIndex = IndexOf(CurStep.Value) + 1;

            if (NextIndex >= Count)
                return this[Count - 1].Y;
            else
            {
                double DeltaStep = this[NextIndex].Y - CurStep.Value.Y;
                return CurrentY + DeltaStep;
            }
        }

        public double StepBack(double CurrentY)
        {
            Point? CurStep = FindY(CurrentY);
            
            if (CurStep == null)
                return this[0].Y;
            
            double CurY = CurStep.Value.Y;
            int PrevIndex = IndexOf(CurStep.Value) - 1;

            if (PrevIndex < 0)
                return this[0].Y;
            else
            {
                double DeltaStep = this[PrevIndex].Y - CurStep.Value.Y;
                return CurrentY + DeltaStep;
            }
        }
    }
    #endregion
}
