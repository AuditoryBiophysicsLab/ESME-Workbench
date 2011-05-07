#if false


using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;

namespace ESMEWorkBench.Controls
{
    /// <summary>
    /// Follow steps 1a or 1b and then 2 to use this custom control in a XAML file.
    ///
    /// Step 1a) Using this custom control in a XAML file that exists in the current project.
    /// Add this XmlNamespace attribute to the root element of the markup file where it is 
    /// to be used:
    ///
    ///     xmlns:MyNamespace="clr-namespace:GeoCanvasDevelopment.Controls"
    ///
    ///
    /// Step 1b) Using this custom control in a XAML file that exists in a different project.
    /// Add this XmlNamespace attribute to the root element of the markup file where it is 
    /// to be used:
    ///
    ///     xmlns:MyNamespace="clr-namespace:GeoCanvasDevelopment.Controls;assembly=GeoCanvasDevelopment.Controls"
    ///
    /// You will also need to add a project reference from the project where the XAML file lives
    /// to this project and Rebuild to avoid compilation errors:
    ///
    ///     Right click on the target project in the Solution Explorer and
    ///     "Add Reference"->"Projects"->[Browse to and select this project]
    ///
    ///
    /// Step 2)
    /// Go ahead and use your control in the XAML file.
    ///
    ///     <MyNamespace:GeoCanvas/>
    ///
    /// </summary>
    public class DataAxis : Panel
    {
        #region AxisLocationEnum enum

        public enum AxisLocationEnum
        {
            Top,
            Bottom,
            Left,
            Right
        }

        #endregion

        static DataAxis() { DefaultStyleKeyProperty.OverrideMetadata(typeof (DataAxis), new FrameworkPropertyMetadata(typeof (DataAxis))); }

        public DataAxis()
        {
            _lineThickness = 1;
            _majorTickLength = 6;
            _minorTickLength = 2;
            _majorTickSpacing = 100;
            _minorTickSpacing = 10;
            AxisLocation = AxisLocationEnum.Right;
        }

        #region Layout and drawing code

        void CreateChildren(Size newSize)
        {
            _length = _isVertical ? newSize.Height : newSize.Width;

            _startLocation = 0;
            _endLocation = _length;

            for (_majorTickCount = 2; _majorTickCount < 20; _majorTickCount++) if ((_endLocation/(_majorTickCount + 1)) < 100) break;

            _majorTickSpacing = _endLocation/_majorTickCount;
            _minorTickSpacing = _majorTickSpacing/5;
            Children.Clear();
            _minWidths.Clear();
            _minHeights.Clear();
            _axis = CreateAxis();
            Children.Add(_axis);
            _ticks.AddLabels(this);

            if (!string.IsNullOrEmpty(AxisLabel))
            {
                _axisLabel.Text = AxisLabel;
                Children.Add(_axisLabel);
            }
        }

        protected override Size MeasureOverride(Size availableSize)
        {
            CreateChildren(availableSize);
            var sizes = new List<double>();
            var sizeToContent = new Size(Double.PositiveInfinity, Double.PositiveInfinity);
            _axis.Measure(sizeToContent);
            foreach (AxisTick tick in _ticks)
            {
                if (tick.Label != null)
                {
                    tick.Label.Measure(sizeToContent);
                    sizes.Add(_isVertical ? tick.Label.DesiredSize.Width : tick.Label.DesiredSize.Height);
                }
            }

            double axisLabelSize = 0;
            if (_axisLabel != null)
            {
                _axisLabel.Measure(sizeToContent);
                axisLabelSize = _axisLabel.DesiredSize.Height;
            }

            Size desiredSize = _isVertical ? new Size(sizes.Max() + _majorTickLength + TickLabelSpacing + axisLabelSize + TickLabelSpacing, _endLocation) : new Size(_endLocation, sizes.Max() + TickLabelSpacing + _majorTickLength + TickLabelSpacing + axisLabelSize);

            // desiredSize = ... computed sum of children's DesiredSize ...;
            // IMPORTANT: do not allow PositiveInfinity to be returned, that will raise an exception in the caller!
            // PositiveInfinity might be an availableSize input; this means that the parent does not care about sizing
            return desiredSize;
        }

        protected override Size ArrangeOverride(Size arrangeSize)
        {
            switch (AxisLocation)
            {
                case AxisLocationEnum.Top:
                    if (_axisLabel != null)
                    {
                        _axisLabel.Arrange(new Rect((DesiredSize.Width - _axisLabel.DesiredSize.Width)/2, DesiredSize.Height - _axisLabel.DesiredSize.Height, _axisLabel.DesiredSize.Width, _axisLabel.DesiredSize.Height));
                        _axis.Arrange(new Rect(0, 0, _axis.DesiredSize.Width, _axis.DesiredSize.Height));
                    }
                    break;
                case AxisLocationEnum.Bottom:
                    if (_axisLabel != null)
                    {
                        _axisLabel.Arrange(new Rect((DesiredSize.Width - _axisLabel.DesiredSize.Width)/2, 0, _axisLabel.DesiredSize.Width, _axisLabel.DesiredSize.Height));
                        _axis.Arrange(new Rect(0, arrangeSize.Height - _majorTickLength, _axis.DesiredSize.Width, _axis.DesiredSize.Height));
                    }
                    break;
                case AxisLocationEnum.Left:
                    if (_axisLabel != null)
                    {
                        _axisLabel.RenderTransformOrigin = new Point(0.5, 0.5);
                        _axisLabel.Arrange(new Rect(DesiredSize.Width - (_axisLabel.DesiredSize.Width/2) - (_axisLabel.DesiredSize.Height/2), (DesiredSize.Height - _axisLabel.DesiredSize.Height)/2, _axisLabel.DesiredSize.Width, _axisLabel.DesiredSize.Height));
                        _axis.Arrange(new Rect(0, 0, _axis.DesiredSize.Width, _axis.DesiredSize.Height));
                        _axisLabel.RenderTransform = new RotateTransform(90);
                    }
                    break;
                case AxisLocationEnum.Right:
                    if (_axisLabel != null)
                    {
                        _axisLabel.RenderTransformOrigin = new Point(0.5, 0.5);
                        _axisLabel.Arrange(new Rect((_axisLabel.DesiredSize.Height/2) - (_axisLabel.DesiredSize.Width/2), (DesiredSize.Height - _axisLabel.DesiredSize.Height)/2, _axisLabel.DesiredSize.Width, _axisLabel.DesiredSize.Height));
                        _axis.Arrange(new Rect(arrangeSize.Width, 0, _axis.DesiredSize.Width, _axis.DesiredSize.Height));
                        _axisLabel.RenderTransform = new RotateTransform(-90);
                    }
                    break;
                default:
                    throw new ApplicationException("DataAxis: Unknown AxisLocation value.");
            }
            _axis.SnapsToDevicePixels = true;

            var location = new Point();
            var rect = new Rect();
            foreach (var tick in _ticks)
                if (tick.Label != null)
                {
                    var size = tick.Label.DesiredSize;
                    double width = tick.Label.DesiredSize.Width;
                    double height = tick.Label.DesiredSize.Height;
                    double left;
                    double top;
                    switch (AxisLocation)
                    {
                        case AxisLocationEnum.Top:
                            left = Math.Max(0, tick.Location - (width/2));
                            if (DesiredSize.Width < (left + width)) left -= (left + width) - DesiredSize.Width;
                            top = tick.Length + 1;
                            break;
                        case AxisLocationEnum.Bottom:
                            left = Math.Max(0, tick.Location - (width/2));
                            if (DesiredSize.Width < (left + width)) left -= (left + width) - DesiredSize.Width;
                            top = tick.Length - height - 1;
                            break;
                        case AxisLocationEnum.Left:
                            top = Math.Max(0, tick.Location - (height/2));
                            if (DesiredSize.Height < (top + height)) top -= (top + height) - DesiredSize.Height;
                            left = tick.Length + 1;
                            break;
                        case AxisLocationEnum.Right:
                            top = Math.Max(0, tick.Location - (height/2));
                            if (DesiredSize.Height < (top + height)) top -= (top + height) - DesiredSize.Height;
                            left = Math.Max(0, DesiredSize.Width - tick.Length - width - 1);
                            break;
                        default:
                            throw new ApplicationException("DataAxis: Unknown AxisLocation value.");
                    }
                    location.X = left;
                    location.Y = top;
                    rect.Location = location;
                    rect.Size = size;
                    tick.Label.Arrange(new Rect(location, size));
                }
            //this.Background = Brushes.LightBlue;
            return arrangeSize;
        }

        Path CreateAxis()
        {
            double x;
            double valueStep = (EndValue - StartValue)/(_endLocation - _startLocation);
            var newTicks = new AxisTicks();
            string format = string.Format("{0}:{1}{2}", "{0", TickValueFormat, "}");

            switch (AxisLocation)
            {
                case AxisLocationEnum.Top:
                    break;
                case AxisLocationEnum.Bottom:
                    //axisTransform.Children.Add(new RotateTransform(180));
                    //axisTransform.Children.Add(new TranslateTransform(_endLocation, _majorTickLength));
                    break;
                case AxisLocationEnum.Left:
                    //axisTransform.Children.Add(new RotateTransform(-90));
                    //axisTransform.Children.Add(new TranslateTransform(0, _majorTickLength));
                    break;
                case AxisLocationEnum.Right:
                    //axisTransform.Children.Add(new RotateTransform(90));
                    //axisTransform.Children.Add(new TranslateTransform(_endLocation, 0));
                    break;
                default:
                    throw new ApplicationException("DataAxis: Unknown AxisLocation value.");
            }

            // Add minor ticks
            for (x = _startLocation + _minorTickSpacing; x < _endLocation - 1; x += _minorTickSpacing) newTicks.Add(new AxisTick(x, _minorTickLength, null, null));

            // Add a major at the start
            newTicks.Add(new AxisTick(_startLocation + (_lineThickness/2), _majorTickLength, StartValue, format));

            // Add the interior major ticks
            double value = StartValue + (valueStep*_majorTickSpacing);
            for (x = _startLocation + _majorTickSpacing; x < _endLocation - 1; x += _majorTickSpacing, value += (valueStep*_majorTickSpacing)) newTicks.Add(new AxisTick(x, _majorTickLength, value, format));

            // Add a major tick at the end
            newTicks.Add(new AxisTick(_endLocation - (_lineThickness/2), _majorTickLength, EndValue, format));

            // Make sure all the ticks are in order
            newTicks.Sort();

            // Clear the tick cache
            _ticks.Clear();
            // Populate the tick cache with unique ticks (discard any superfluous minor ticks)
            _ticks.AddRange(newTicks.Distinct().ToArray());

            // Create a StreamGeometry to use to specify _axis.
            var geometry = new StreamGeometry
                           {
                               FillRule = FillRule.EvenOdd
                           };

            // Open a StreamGeometryContext that can be used to describe this StreamGeometry 
            // object's contents.
            using (StreamGeometryContext ctx = geometry.Open())
            {
                // Begin the triangle at the point specified. Notice that the shape is set to 
                // be closed so only two lines need to be specified below to make the triangle.
                ctx.BeginFigure(TransformedPoint(_startLocation + (_lineThickness/2), _lineThickness/2), false, false);

                foreach (AxisTick tick in _ticks)
                {
                    ctx.LineTo(TransformedPoint(tick.Location, _lineThickness/2), true, true);
                    ctx.LineTo(TransformedPoint(tick.Location, tick.Length + _lineThickness), true, true);
                    ctx.LineTo(TransformedPoint(tick.Location, _lineThickness/2), true, true);
                }
                // Draw a line to the next specified point.
                ctx.LineTo(TransformedPoint(_endLocation - (_lineThickness/2), _lineThickness/2), true, true);
            }
            // Freeze the geometry (make it unmodifiable)
            // for additional performance benefits.
            geometry.Freeze();
            var axis = new Path
                       {
                           Stroke = Brushes.Black,
                           StrokeThickness = _lineThickness
                       };
            axis.Stroke = Brushes.Black;
            axis.StrokeThickness = _lineThickness;
            axis.StrokeMiterLimit = 1;
            axis.StrokeStartLineCap = PenLineCap.Flat;
            axis.StrokeEndLineCap = PenLineCap.Flat;
            axis.SnapsToDevicePixels = true;
            axis.Data = geometry;

            return axis;
        }

        Point TransformedPoint(double location, double offset)
        {
            switch (AxisLocation)
            {
                case AxisLocationEnum.Top:
                    return new Point(location, offset);
                case AxisLocationEnum.Bottom:
                    return new Point(location, -offset);
                case AxisLocationEnum.Left:
                    return new Point(offset, location);
                case AxisLocationEnum.Right:
                    return new Point(-offset, location);
                default:
                    throw new ApplicationException("DataAxis: Unknown AxisLocation value.");
            }
        }

        #endregion

        #region Private data members

        readonly TextBlock _axisLabel = new TextBlock();
        readonly double _lineThickness;

        readonly double _majorTickLength;
        readonly List<double> _minHeights = new List<double>();
        readonly List<double> _minWidths = new List<double>();
        readonly double _minorTickLength;

        readonly AxisTicks _ticks = new AxisTicks();
        Path _axis;
        double _endLocation;
        bool _isVertical;
        double _length;
        int _majorTickCount;

        double _majorTickSpacing,
               _minorTickSpacing;

        double _startLocation;
        const double TickLabelSpacing = 3;

        #endregion

        #region public AxisLocationEnum AxisLocation {get; set;}

        public static readonly DependencyProperty AxisLocationProperty = DependencyProperty.Register("AxisLocation", typeof (AxisLocationEnum), typeof (DataAxis), new FrameworkPropertyMetadata((AxisLocationEnum.Top), FrameworkPropertyMetadataOptions.AffectsArrange | FrameworkPropertyMetadataOptions.AffectsMeasure, AxisLocationPropertyChanged));

        public AxisLocationEnum AxisLocation
        {
            get { return (AxisLocationEnum) GetValue(AxisLocationProperty); }
            set { SetValue(AxisLocationProperty, value); }
        }

        static void AxisLocationPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis) obj).AxisLocationPropertyChanged(); }

        void AxisLocationPropertyChanged()
        {
            switch (AxisLocation)
            {
                case AxisLocationEnum.Top:
                    _isVertical = false;
                    break;
                case AxisLocationEnum.Bottom:
                    _isVertical = false;
                    break;
                case AxisLocationEnum.Left:
                    _isVertical = true;
                    break;
                case AxisLocationEnum.Right:
                    _isVertical = true;
                    break;
                default:
                    throw new ApplicationException("DataAxis: Unknown AxisLocation value.");
            }
            InvalidateVisual();
        }

        #endregion

        #region public string AxisLabel { get; set; }

        public static readonly DependencyProperty AxisLabelProperty = DependencyProperty.RegisterAttached("AxisLabel", typeof (string), typeof (DataAxis), new FrameworkPropertyMetadata("", FrameworkPropertyMetadataOptions.AffectsArrange | FrameworkPropertyMetadataOptions.AffectsMeasure, AxisLabelPropertyChanged));

        public string AxisLabel
        {
            get { return (string) GetValue(AxisLabelProperty); }
            set { SetValue(AxisLabelProperty, value); }
        }

        static void AxisLabelPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis) obj).AxisLabelPropertyChanged(); }
        void AxisLabelPropertyChanged() { InvalidateVisual(); }

        #endregion

        #region public double StartValue { get; set; }

        public static readonly DependencyProperty StartValueProperty = DependencyProperty.RegisterAttached("StartValue", typeof (double), typeof (DataAxis), new FrameworkPropertyMetadata(-10.0, FrameworkPropertyMetadataOptions.AffectsArrange | FrameworkPropertyMetadataOptions.AffectsMeasure, StartValuePropertyChanged));

        public double StartValue
        {
            get { return (double) GetValue(StartValueProperty); }
            set { SetValue(StartValueProperty, value); }
        }

        static void StartValuePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis) obj).StartValuePropertyChanged(); }
        void StartValuePropertyChanged() { InvalidateVisual(); }

        #endregion

        #region public double EndValue { get; set; }

        public static readonly DependencyProperty EndValueProperty = DependencyProperty.RegisterAttached("EndValue", typeof (double), typeof (DataAxis), new FrameworkPropertyMetadata(10.0, FrameworkPropertyMetadataOptions.AffectsArrange | FrameworkPropertyMetadataOptions.AffectsMeasure, EndValuePropertyChanged));

        public double EndValue
        {
            get { return (double) GetValue(EndValueProperty); }
            set { SetValue(EndValueProperty, value); }
        }

        static void EndValuePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis) obj).EndValuePropertyChanged(); }
        void EndValuePropertyChanged() { InvalidateVisual(); }

        #endregion

        #region public string TickValueFormat { get; set; }

        public static readonly DependencyProperty TickValueFormatProperty = DependencyProperty.RegisterAttached("TickValueFormat", typeof (string), typeof (DataAxis), new FrameworkPropertyMetadata("0.###", FrameworkPropertyMetadataOptions.AffectsArrange | FrameworkPropertyMetadataOptions.AffectsMeasure, TickValueFormatPropertyChanged));

        public string TickValueFormat
        {
            get { return (string) GetValue(TickValueFormatProperty); }
            set { SetValue(TickValueFormatProperty, value); }
        }

        static void TickValueFormatPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis) obj).TickValueFormatPropertyChanged(); }
        void TickValueFormatPropertyChanged() { InvalidateVisual(); }

        #endregion
    }

    #region Axis utility classes, delegates and enumerations

    internal delegate double TickLabelLayoutDelegate(AxisTick tick);

    internal delegate double AxisLabelLayoutDelegate(double position);

    internal class AxisTicks : List<AxisTick>
    {
        public void AddLabels(DataAxis parent)
        {
            foreach (AxisTick tick in this)
                if (tick.Label != null)
                {
                    parent.Children.Add(tick.Label);
                }
        }
    }

    internal class AxisTick : IComparable<AxisTick>
    {
        public AxisTick(double location, double height, string label)
        {
            Value = null;
            Location = location;
            Length = height;
            if (label != null)
                Label = new TextBlock
                             {
                                 Text = label
                             };
        }

        public AxisTick(double location, double height, double? value, string format)
        {
            Location = location;
            Length = height;
            Value = value;
            if (value != null)
                Label = new TextBlock
                        {
                            Text = string.Format(format, value)
                        };
        }

        public double Location { get; internal set; }
        public double Length { get; internal set; }
        public double? Value { get; internal set; }
        public TextBlock Label { get; set; }

        #region IComparable<AxisTick> Members

        int IComparable<AxisTick>.CompareTo(AxisTick that)
        {
            if ((that == null) || (Location > that.Location)) return 1;
            if (Location < that.Location) return -1;
            return 0;
        }

        #endregion

        ~AxisTick() { Label = null; }
    }

    #endregion
}
#endif