﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Media;
using System.Windows.Shapes;
using ESME.NEMO;
using HRC.Utility;

namespace ESME.Views.Controls
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

        public enum AxisTypeEnum
        {
            Linear,
            Logarithmic
        }

        static DataAxis() { DefaultStyleKeyProperty.OverrideMetadata(typeof (DataAxis), new FrameworkPropertyMetadata(typeof (DataAxis))); }

        #region dependency property ObservableList<double> MajorTicks

        public static DependencyProperty MajorTicksProperty = DependencyProperty.Register("MajorTicks",
                                                                                 typeof (ObservableList<double>),
                                                                                 typeof (DataAxis),
                                                                                 new FrameworkPropertyMetadata(null));

        public ObservableList<double> MajorTicks
        {
            get { return (ObservableList<double>)GetValue(MajorTicksProperty); }
            set { SetCurrentValue(MajorTicksProperty, value); }
        }

        #endregion

        #region dependency property ObservableList<double> MinorTicks

        public static DependencyProperty MinorTicksProperty = DependencyProperty.Register("MinorTicks",
                                                                                 typeof (ObservableList<double>),
                                                                                 typeof (DataAxis),
                                                                                 new FrameworkPropertyMetadata(null));

        public ObservableList<double> MinorTicks
        {
            get { return (ObservableList<double>)GetValue(MinorTicksProperty); }
            set { SetCurrentValue(MinorTicksProperty, value); }
        }

        #endregion

        #region public AxisLocationEnum AxisLocation {get; set;}

        public static readonly DependencyProperty AxisLocationProperty = DependencyProperty.Register("AxisLocation", typeof(AxisLocationEnum), typeof(DataAxis), new FrameworkPropertyMetadata((AxisLocationEnum.Top), FrameworkPropertyMetadataOptions.AffectsArrange | FrameworkPropertyMetadataOptions.AffectsMeasure, AxisLocationPropertyChanged));

        public AxisLocationEnum AxisLocation
        {
            get { return (AxisLocationEnum)GetValue(AxisLocationProperty); }
            set { SetCurrentValue(AxisLocationProperty, value); }
        }

        static void AxisLocationPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).AxisLocationPropertyChanged(); }

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

        #region dependency property AxisTypeEnum AxisType

        public static DependencyProperty AxisTypeProperty = DependencyProperty.Register("AxisType",
                                                                                 typeof(AxisTypeEnum),
                                                                                 typeof(DataAxis),
                                                                                 new FrameworkPropertyMetadata(AxisTypeEnum.Linear, FrameworkPropertyMetadataOptions.AffectsArrange, AxisTypePropertyChanged));

        public AxisTypeEnum AxisType { get { return (AxisTypeEnum)GetValue(AxisTypeProperty); } set { SetValue(AxisTypeProperty, value); } }

        static void AxisTypePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).AxisTypePropertyChanged(); }

        void AxisTypePropertyChanged() { InvalidateVisual(); }
        #endregion

        #region public string AxisLabel { get; set; }

        public static readonly DependencyProperty AxisLabelProperty = DependencyProperty.RegisterAttached("AxisLabel", typeof(string), typeof(DataAxis), new FrameworkPropertyMetadata("", FrameworkPropertyMetadataOptions.AffectsArrange | FrameworkPropertyMetadataOptions.AffectsMeasure, AxisLabelPropertyChanged));

        public string AxisLabel
        {
            get { return (string)GetValue(AxisLabelProperty); }
            set { SetCurrentValue(AxisLabelProperty, value); }
        }

        static void AxisLabelPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).AxisLabelPropertyChanged(); }
        void AxisLabelPropertyChanged() { InvalidateVisual(); }

        #endregion

        #region public double StartValue { get; set; }

        public static readonly DependencyProperty StartValueProperty = DependencyProperty.RegisterAttached("StartValue", typeof(double), typeof(DataAxis), new FrameworkPropertyMetadata(0.1, FrameworkPropertyMetadataOptions.AffectsArrange | FrameworkPropertyMetadataOptions.AffectsMeasure, StartValuePropertyChanged));

        public double StartValue
        {
            get { return (double)GetValue(StartValueProperty); }
            set { SetCurrentValue(StartValueProperty, value); }
        }

        static void StartValuePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).StartValuePropertyChanged(); }
        void StartValuePropertyChanged() { InvalidateVisual(); }

        #endregion

        #region public double EndValue { get; set; }

        public static readonly DependencyProperty EndValueProperty = DependencyProperty.RegisterAttached("EndValue", typeof(double), typeof(DataAxis), new FrameworkPropertyMetadata(10.0, FrameworkPropertyMetadataOptions.AffectsArrange | FrameworkPropertyMetadataOptions.AffectsMeasure, EndValuePropertyChanged));

        public double EndValue
        {
            get { return (double)GetValue(EndValueProperty); }
            set { SetCurrentValue(EndValueProperty, value); }
        }

        static void EndValuePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).EndValuePropertyChanged(); }
        void EndValuePropertyChanged() { InvalidateVisual(); }

        #endregion

        #region public string TickValueFormat { get; set; }

        public static readonly DependencyProperty TickValueFormatProperty = DependencyProperty.RegisterAttached("TickValueFormat", typeof(string), typeof(DataAxis), new FrameworkPropertyMetadata("0.###", FrameworkPropertyMetadataOptions.AffectsArrange | FrameworkPropertyMetadataOptions.AffectsMeasure, TickValueFormatPropertyChanged));

        public string TickValueFormat
        {
            get { return (string)GetValue(TickValueFormatProperty); }
            set { SetCurrentValue(TickValueFormatProperty, value); }
        }

        static void TickValueFormatPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).TickValueFormatPropertyChanged(); }
        void TickValueFormatPropertyChanged() { InvalidateVisual(); }

        #endregion

        public DataAxis()
        {
            _lineThickness = 1;
            _majorTickLength = 6;
            _minorTickLength = 2;
            _majorTickSpacing = 100;
            _minorTickSpacing = 10;
            AxisLocation = AxisLocationEnum.Right;
            MajorTicks = new ObservableList<double>();
            MinorTicks = new ObservableList<double>();
        }

        #region Layout and drawing code

        void CreateChildren(Size newSize)
        {
            if (_isVertical)
            {
                _length = newSize.Height;
                _startLocation = _length;
                _endLocation = 0;
            }
            else
            {
                _length = newSize.Width;
                _startLocation = 0;
                _endLocation = _length;
            }
            if (AxisType == AxisTypeEnum.Logarithmic)
            {
                var startTickValue = Math.Floor(Math.Log10(StartValue));
                var endTickValue = Math.Ceiling(Math.Log10(EndValue));
                _majorTickCount = (int)(endTickValue - startTickValue);
            }
            else for (_majorTickCount = 2; _majorTickCount < 20; _majorTickCount++) if ((_length / (_majorTickCount + 1)) < 100) break;

            _majorTickSpacing = _length / _majorTickCount;
            _minorTickSpacing = _majorTickSpacing / 5;

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
            if (AxisType == AxisTypeEnum.Logarithmic)
            {
                if (StartValue <= 0) throw new ParameterOutOfRangeException("StartValue cannot be zero or negative on a Logarithmic axis");
                if (EndValue <= 0) throw new ParameterOutOfRangeException("EndValue cannot be zero or negative on a Logarithmic axis");
                if (EndValue <= StartValue) throw new ParameterOutOfRangeException("EndValue cannot be less than or equal to StartValue on a Logarithmic axis");
            }
            CreateChildren(availableSize);
            var labelSizes = new List<double>();

            if (double.IsNaN(availableSize.Width) || double.IsInfinity(availableSize.Width)) availableSize.Width = SystemParameters.VirtualScreenWidth;
            if (double.IsNaN(availableSize.Height) || double.IsInfinity(availableSize.Height)) availableSize.Height = SystemParameters.VirtualScreenHeight;
            var sizeToContent = new Size(SystemParameters.VirtualScreenWidth, SystemParameters.VirtualScreenHeight);
            _axis.Measure(sizeToContent);
            foreach (var tick in _ticks)
            {
                if (tick.Label != null)
                {
                    tick.Label.Measure(sizeToContent);
                    labelSizes.Add(_isVertical ? tick.Label.DesiredSize.Width : tick.Label.DesiredSize.Height);
                }
            }

            double axisLabelSize = 0;
            if (_axisLabel != null)
            {
                _axisLabel.Measure(sizeToContent);
                axisLabelSize = _axisLabel.DesiredSize.Height;
            }
            var maxLabelSize = labelSizes.Max();
            var shortSize = maxLabelSize + _majorTickLength + TickLabelSpacing + axisLabelSize + TickLabelSpacing;
            var longSize = _length;
            var desiredSize = _isVertical ? new Size(shortSize, longSize) : new Size(longSize, shortSize);

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
                        _axis.Arrange(new Rect(0, arrangeSize.Height, _axis.DesiredSize.Width, _axis.DesiredSize.Height));
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
                    var width = tick.Label.DesiredSize.Width;
                    var height = tick.Label.DesiredSize.Height;
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
                            top = Math.Max(0, DesiredSize.Height - tick.Length - height - 1);
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
            var valueStep = (EndValue - StartValue)/Math.Abs(_endLocation - _startLocation);
            var format = TickValueFormat == "m" ? "m" : string.Format("{{0:{0}}}", TickValueFormat);
            if (MajorTicks == null) MajorTicks = new ObservableList<double>();
            if (MinorTicks == null) MinorTicks = new ObservableList<double>();
            MajorTicks.Clear();
            MinorTicks.Clear();
            // Clear the tick cache
            _ticks.Clear();

            var majorTickValue = AxisType == AxisTypeEnum.Linear ? StartValue : Math.Pow(10, Math.Floor(Math.Log10(StartValue)));
            var direction = _isVertical ? -1 : 1;
            var conditionLambda = !_isVertical
                                      ? new Func<double, double, bool>((tickLocation, endLocation) => tickLocation < endLocation - 1)
                                      : ((tickLocation, endLocation) => tickLocation > endLocation + 1);
            for (var majorTickLocation = _startLocation; conditionLambda(majorTickLocation, _endLocation); majorTickLocation += direction * _majorTickSpacing)
            {
                var majorTick = new AxisTick(majorTickLocation, _majorTickLength, majorTickValue, format);
                _ticks.Add(majorTick);
                MajorTicks.Add(majorTick.Location);
                Debug.WriteLine(string.Format("Added major tick at location {0}", majorTick.Location));
                if (AxisType == AxisTypeEnum.Linear)
                {
                    for (var minorTickCount = 1; minorTickCount < 5; minorTickCount++)
                    {
                        var minorTick = new AxisTick(majorTickLocation + (direction * minorTickCount * _minorTickSpacing), _minorTickLength, null, null);
                        _ticks.Add(minorTick);
                        MinorTicks.Add(minorTick.Location);
                        Debug.WriteLine(string.Format("Linear: Added minor tick at location {0}", minorTick.Location));
                    }
                    majorTickValue += (valueStep * _majorTickSpacing);
                }
                else
                {
                    for (var minorTickCount = 2; minorTickCount < 10; minorTickCount++)
                    {
                        var minorTick = new AxisTick(majorTickLocation + (direction * Math.Log10(minorTickCount) * _majorTickSpacing), _minorTickLength, null, null);
                        _ticks.Add(minorTick);
                        MinorTicks.Add(minorTick.Location);
                        Debug.WriteLine(string.Format("Log: Added minor tick at location {0}", minorTick.Location));
                    }
                    majorTickValue = Math.Pow(10, Math.Log10(majorTickValue) + 1);
                }
            }

            // Add a major tick at the end
            majorTickValue = EndValue;
            var endTick = new AxisTick(_endLocation, _majorTickLength, majorTickValue, format);
            _ticks.Add(endTick);
            MajorTicks.Add(endTick.Location);
            Debug.WriteLine(string.Format("Added last major tick at location {0}", endTick.Location));

            // Create a StreamGeometry to use to specify _axis.
            var geometry = new StreamGeometry
                           {
                               FillRule = FillRule.EvenOdd
                           };

            // Open a StreamGeometryContext that can be used to describe this StreamGeometry 
            // object's contents.
            using (var ctx = geometry.Open())
            {
                ctx.BeginFigure(TransformedPoint(_startLocation, 0), false, false);
                ctx.LineTo(TransformedPoint(_endLocation, 0), true, false);

                foreach (var tick in _ticks)
                {
                    ctx.BeginFigure(TransformedPoint(tick.Location, 0), false, false);
                    ctx.LineTo(TransformedPoint(tick.Location, tick.Length), true, false);
                }
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

    }

    #region Axis utility classes, delegates and enumerations

    internal delegate double TickLabelLayoutDelegate(AxisTick tick);

    internal delegate double AxisLabelLayoutDelegate(double position);

    internal class AxisTicks : List<AxisTick>
    {
        public void AddLabels(DataAxis parent)
        {
            foreach (var tick in this)
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
            {
                if (format == "m")
                {
                    Label = new TextBlock { Text = "10" };
                    var superscript = new TextBlock { Text = ((int)Math.Floor(Math.Log10(value.Value))).ToString(CultureInfo.InvariantCulture), FontSize = 10 };
                    var inline = new InlineUIContainer(superscript) { BaselineAlignment = BaselineAlignment.Superscript };
                    Label.Inlines.Add(inline);
                }
                else
                {
                    Label = new TextBlock
                    {
                        Text = string.Format(format, value)
                    };
                }
            }
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