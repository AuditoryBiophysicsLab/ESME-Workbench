using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Media;
using ESME.NEMO;
using HRC;
using HRC.ViewModels;
using Path = System.Windows.Shapes.Path;

namespace ESME.Views.Controls
{
    public class DataAxis : Panel
    {
        static DataAxis() { DefaultStyleKeyProperty.OverrideMetadata(typeof(DataAxis), new FrameworkPropertyMetadata(typeof(DataAxis))); }

        #region dependency property ObservableCollection<AxisTick> MajorTicks
        public static DependencyProperty MajorTicksProperty = DependencyProperty.Register("MajorTicks",
                                                                                          typeof(ObservableCollection<AxisTick>),
                                                                                          typeof(DataAxis),
                                                                                          new FrameworkPropertyMetadata(null));

        public ObservableCollection<AxisTick> MajorTicks { get { return (ObservableCollection<AxisTick>)GetValue(MajorTicksProperty); } set { SetCurrentValue(MajorTicksProperty, value); } }
        #endregion

        #region dependency property ObservableCollection<AxisTick> MinorTicks
        public static DependencyProperty MinorTicksProperty = DependencyProperty.Register("MinorTicks",
                                                                                          typeof(ObservableCollection<AxisTick>),
                                                                                          typeof(DataAxis),
                                                                                          new FrameworkPropertyMetadata(null));

        public ObservableCollection<AxisTick> MinorTicks { get { return (ObservableCollection<AxisTick>)GetValue(MinorTicksProperty); } set { SetCurrentValue(MinorTicksProperty, value); } }
        #endregion

        #region dependency property ObservableCollection<AxisTick> TickValues

        public static DependencyProperty TickValuesProperty = DependencyProperty.Register("TickValues",
                                                                                          typeof(ObservableCollection<AxisTick>),
                                                                                          typeof(DataAxis),
                                                                                          new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsMeasure, TickValuesPropertyChanged));

        public ObservableCollection<AxisTick> TickValues { get { return (ObservableCollection<AxisTick>)GetValue(TickValuesProperty); } set { SetValue(TickValuesProperty, value); } }
        static void TickValuesPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).TickValuesPropertyChanged(); }
        [UsedImplicitly] CollectionObserver _tickValuesObserver;
        void TickValuesPropertyChanged()
        {
            if (AxisType == AxisType.Logarithmic) throw new InvalidOperationException("Cannot set TickValues on a Logarithmic axis");
            if (_tickValuesObserver != null)
            {
                _tickValuesObserver.UnregisterHandler(TickValuesCollectionChanged);
                _tickValuesObserver = null;
            }
            if (TickValues == null) return;
            _tickValuesObserver = new CollectionObserver(TickValues);
            _tickValuesObserver.RegisterHandler(TickValuesCollectionChanged);
        }

        void TickValuesCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            InvalidateMeasure();
        }
        #endregion

        #region dependency property bool ShowMajorTicks

        public static DependencyProperty ShowMajorTicksProperty = DependencyProperty.Register("ShowMajorTicks",
                                                                                 typeof(bool),
                                                                                 typeof(DataAxis),
                                                                                 new FrameworkPropertyMetadata(true, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, ShowMajorTicksPropertyChanged));

        public bool ShowMajorTicks { get { return (bool)GetValue(ShowMajorTicksProperty); } set { SetValue(ShowMajorTicksProperty, value); } }

        static void ShowMajorTicksPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).ShowMajorTicksPropertyChanged(); }
        void ShowMajorTicksPropertyChanged() { }
        #endregion

        #region dependency property bool ShowMinorTicks

        public static DependencyProperty ShowMinorTicksProperty = DependencyProperty.Register("ShowMinorTicks",
                                                                                 typeof(bool),
                                                                                 typeof(DataAxis),
                                                                                 new FrameworkPropertyMetadata(true, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, ShowMinorTicksPropertyChanged));

        public bool ShowMinorTicks { get { return (bool)GetValue(ShowMinorTicksProperty); } set { SetValue(ShowMinorTicksProperty, value); } }

        static void ShowMinorTicksPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).ShowMinorTicksPropertyChanged(); }
        void ShowMinorTicksPropertyChanged() { }
        #endregion

        #region dependency property AxisLocation AxisLocation {get; set;}
        public static readonly DependencyProperty AxisLocationProperty = DependencyProperty.Register("AxisLocation",
                                                                                                     typeof(AxisLocation),
                                                                                                     typeof(DataAxis),
                                                                                                     new FrameworkPropertyMetadata((AxisLocation.Top),
                                                                                                                                   FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                                   FrameworkPropertyMetadataOptions.AffectsMeasure |
                                                                                                                                   FrameworkPropertyMetadataOptions.AffectsRender,
                                                                                                                                   AxisLocationPropertyChanged));

        public AxisLocation AxisLocation { get { return (AxisLocation)GetValue(AxisLocationProperty); } set { SetCurrentValue(AxisLocationProperty, value); } }

        static void AxisLocationPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).AxisLocationPropertyChanged(); }

        void AxisLocationPropertyChanged()
        {
            switch (AxisLocation)
            {
                case AxisLocation.Top:
                    _isVertical = false;
                    break;
                case AxisLocation.Bottom:
                    _isVertical = false;
                    break;
                case AxisLocation.Left:
                    _isVertical = true;
                    break;
                case AxisLocation.Right:
                    _isVertical = true;
                    break;
                default:
                    throw new ApplicationException("DataAxis: Unknown AxisLocation value.");
            }
            InvalidateVisual();
        }
        #endregion

        #region dependency property AxisType AxisType
        public static DependencyProperty AxisTypeProperty = DependencyProperty.Register("AxisType",
                                                                                        typeof(AxisType),
                                                                                        typeof(DataAxis),
                                                                                        new FrameworkPropertyMetadata(AxisType.Linear,
                                                                                                                      FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                      FrameworkPropertyMetadataOptions.AffectsRender,
                                                                                                                      AxisTypePropertyChanged));

        public AxisType AxisType { get { return (AxisType)GetValue(AxisTypeProperty); } set { SetValue(AxisTypeProperty, value); } }

        static void AxisTypePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).AxisTypePropertyChanged(); }

        void AxisTypePropertyChanged()
        {
            if (AxisType == AxisType.Logarithmic && (TickValues != null || TickValues != null)) throw new InvalidOperationException("Cannot set an axis to Logarithmic that is using MajorTickValues or MinorTickValues");
            InvalidateVisual();
        }
        #endregion

        #region dependency property string AxisLabel { get; set; }
        public static readonly DependencyProperty AxisLabelProperty = DependencyProperty.RegisterAttached("AxisLabel",
                                                                                                          typeof(string),
                                                                                                          typeof(DataAxis),
                                                                                                          new FrameworkPropertyMetadata("",
                                                                                                                                        FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                                        FrameworkPropertyMetadataOptions.AffectsMeasure |
                                                                                                                                        FrameworkPropertyMetadataOptions.AffectsRender,
                                                                                                                                        AxisLabelPropertyChanged));

        public string AxisLabel { get { return (string)GetValue(AxisLabelProperty); } set { SetCurrentValue(AxisLabelProperty, value); } }

        static void AxisLabelPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).AxisLabelPropertyChanged(); }
        void AxisLabelPropertyChanged() { InvalidateVisual(); }
        #endregion

        #region dependency property double StartValue { get; set; }
        public static readonly DependencyProperty StartValueProperty = DependencyProperty.RegisterAttached("StartValue",
                                                                                                           typeof(double),
                                                                                                           typeof(DataAxis),
                                                                                                           new FrameworkPropertyMetadata(0.1,
                                                                                                                                         FrameworkPropertyMetadataOptions.AffectsMeasure,
                                                                                                                                         StartValuePropertyChanged));

        public double StartValue { get { return (double)GetValue(StartValueProperty); } set { SetCurrentValue(StartValueProperty, value); } }

        static void StartValuePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).StartValuePropertyChanged(); }
        void StartValuePropertyChanged()
        {
            MappingFunction = PrivateMappingFunction;
            InvalidateMeasure();
        }
        #endregion

        #region dependency property double EndValue { get; set; }
        public static readonly DependencyProperty EndValueProperty = DependencyProperty.RegisterAttached("EndValue",
                                                                                                         typeof(double),
                                                                                                         typeof(DataAxis),
                                                                                                         new FrameworkPropertyMetadata(10.0,
                                                                                                                                       FrameworkPropertyMetadataOptions.AffectsMeasure,
                                                                                                                                       EndValuePropertyChanged));

        public double EndValue { get { return (double)GetValue(EndValueProperty); } set { SetCurrentValue(EndValueProperty, value); } }

        static void EndValuePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).EndValuePropertyChanged(); }
        void EndValuePropertyChanged()
        {
            MappingFunction = PrivateMappingFunction;
            InvalidateMeasure();
        }
        #endregion

        #region dependency property string TickValueFormat { get; set; }
        public static readonly DependencyProperty TickValueFormatProperty = DependencyProperty.RegisterAttached("TickValueFormat",
                                                                                                                typeof(string),
                                                                                                                typeof(DataAxis),
                                                                                                                new FrameworkPropertyMetadata("0.###",
                                                                                                                                              FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                                              FrameworkPropertyMetadataOptions.AffectsMeasure,
                                                                                                                                              TickValueFormatPropertyChanged));

        public string TickValueFormat { get { return (string)GetValue(TickValueFormatProperty); } set { SetCurrentValue(TickValueFormatProperty, value); } }

        static void TickValueFormatPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).TickValueFormatPropertyChanged(); }
        void TickValueFormatPropertyChanged() { InvalidateVisual(); }
        #endregion

        #region dependency property Func<double, double> MappingFunction

        public static DependencyProperty MappingFunctionProperty = DependencyProperty.Register("MappingFunction",
                                                                                               typeof(Func<double, double>),
                                                                                               typeof(DataAxis),
                                                                                               new FrameworkPropertyMetadata(null,
                                                                                                                             FrameworkPropertyMetadataOptions.BindsTwoWayByDefault,
                                                                                                                             MappingFunctionPropertyChanged));

        public Func<double, double> MappingFunction { get { return (Func<double, double>)GetValue(MappingFunctionProperty); } set { SetValue(MappingFunctionProperty, value); } }

        static void MappingFunctionPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataAxis)obj).MappingFunctionPropertyChanged(); }
        void MappingFunctionPropertyChanged() { }
        #endregion

        #region dependency property DataAxis Axis

        public static DependencyProperty AxisProperty = DependencyProperty.Register("Axis",
                                                                                 typeof(DataAxis),
                                                                                 typeof(DataAxis),
                                                                                 new FrameworkPropertyMetadata(null));

        public DataAxis Axis { get { return (DataAxis)GetValue(AxisProperty); } set { SetValue(AxisProperty, value); } }
        #endregion

        double PrivateMappingFunction(double value)
        {
            var startValue = AxisType == AxisType.Linear ? StartValue : Math.Floor(Math.Log10(StartValue));
            var endValue = AxisType == AxisType.Linear ? EndValue : Math.Ceiling(Math.Log10(EndValue));
            value = AxisType == AxisType.Linear ? value : Math.Log10(value);
            var lowValue = Math.Min(startValue, endValue);
            var highValue = Math.Max(startValue, endValue);
            if (highValue == lowValue) return highValue;
            //if (value < lowValue || value > highValue) throw new ParameterOutOfRangeException("value is out of range for this axis");
            var axisDelta = highValue - lowValue;
            var valueDelta = value - lowValue;
            var valueRatio = valueDelta / axisDelta;
            var offsetFromLength = _isVertical;
            if (endValue < startValue) offsetFromLength = !offsetFromLength;
            var lengthOffset = offsetFromLength ? _length - (_length * valueRatio) : _length * valueRatio;
            return lengthOffset;
        }

        public DataAxis()
        {
            _lineThickness = 1;
            _majorTickLength = 6;
            _minorTickLength = 3;
            _majorTickSpacing = 100;
            _minorTickSpacing = 10;
            AxisLocation = AxisLocation.Right;
            MajorTicks = new ObservableCollection<AxisTick>();
            MinorTicks = new ObservableCollection<AxisTick>();
            SnapsToDevicePixels = true;
            UseLayoutRounding = true;
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
            if (AxisType == AxisType.Logarithmic)
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

            if (!String.IsNullOrEmpty(AxisLabel))
            {
                _axisLabel.Text = AxisLabel;
                Children.Add(_axisLabel);
            }
        }

        protected override Size MeasureOverride(Size availableSize)
        {
            if (AxisType == AxisType.Logarithmic)
            {
                if (StartValue <= 0) throw new ParameterOutOfRangeException("StartValue cannot be zero or negative on a Logarithmic axis");
                if (EndValue <= 0) throw new ParameterOutOfRangeException("EndValue cannot be zero or negative on a Logarithmic axis");
                if (EndValue <= StartValue) throw new ParameterOutOfRangeException("EndValue cannot be less than or equal to StartValue on a Logarithmic axis");
            }
            CreateChildren(availableSize);
            Axis = this;
            MappingFunction = PrivateMappingFunction;
            var labelSizes = new List<double>();

            if (Double.IsNaN(availableSize.Width) || Double.IsInfinity(availableSize.Width)) availableSize.Width = SystemParameters.VirtualScreenWidth;
            if (Double.IsNaN(availableSize.Height) || Double.IsInfinity(availableSize.Height)) availableSize.Height = SystemParameters.VirtualScreenHeight;
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
            //Debug.WriteLine(string.Format("DataAxis: MeasureOverride for {0} returning desired width {1} and height {2}", AxisLabel, desiredSize.Width, desiredSize.Height));
            return desiredSize;
        }

        protected override Size ArrangeOverride(Size arrangeSize)
        {
            switch (AxisLocation)
            {
                case AxisLocation.Top:
                    if (_axisLabel != null)
                    {
                        _axisLabel.Arrange(new Rect((DesiredSize.Width - _axisLabel.DesiredSize.Width) / 2,
                                                    DesiredSize.Height - _axisLabel.DesiredSize.Height,
                                                    _axisLabel.DesiredSize.Width,
                                                    _axisLabel.DesiredSize.Height));
                        _axis.Arrange(new Rect(0, 0, _axis.DesiredSize.Width, _axis.DesiredSize.Height));
                    }
                    break;
                case AxisLocation.Bottom:
                    if (_axisLabel != null)
                    {
                        _axisLabel.Arrange(new Rect((DesiredSize.Width - _axisLabel.DesiredSize.Width) / 2, 0, _axisLabel.DesiredSize.Width, _axisLabel.DesiredSize.Height));
                        _axis.Arrange(new Rect(0, arrangeSize.Height, _axis.DesiredSize.Width, _axis.DesiredSize.Height));
                    }
                    break;
                case AxisLocation.Left:
                    if (_axisLabel != null)
                    {
                        _axisLabel.RenderTransformOrigin = new Point(0.5, 0.5);
                        _axisLabel.Arrange(new Rect(DesiredSize.Width - (_axisLabel.DesiredSize.Width / 2) - (_axisLabel.DesiredSize.Height / 2),
                                                    (DesiredSize.Height - _axisLabel.DesiredSize.Height) / 2,
                                                    _axisLabel.DesiredSize.Width,
                                                    _axisLabel.DesiredSize.Height));
                        _axis.Arrange(new Rect(0, 0, _axis.DesiredSize.Width, _axis.DesiredSize.Height));
                        _axisLabel.RenderTransform = new RotateTransform(90);
                    }
                    break;
                case AxisLocation.Right:
                    if (_axisLabel != null)
                    {
                        _axisLabel.RenderTransformOrigin = new Point(0.5, 0.5);
                        _axisLabel.Arrange(new Rect((_axisLabel.DesiredSize.Height / 2) - (_axisLabel.DesiredSize.Width / 2),
                                                    (DesiredSize.Height - _axisLabel.DesiredSize.Height) / 2,
                                                    _axisLabel.DesiredSize.Width,
                                                    _axisLabel.DesiredSize.Height));
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
                        case AxisLocation.Top:
                            left = Math.Max(0, tick.Location - (width / 2));
                            if (DesiredSize.Width < (left + width)) left -= (left + width) - DesiredSize.Width;
                            top = tick.Length + 1;
                            break;
                        case AxisLocation.Bottom:
                            left = Math.Max(0, tick.Location - (width / 2));
                            if (DesiredSize.Width < (left + width)) left -= (left + width) - DesiredSize.Width;
                            top = Math.Max(0, DesiredSize.Height - tick.Length - height - 1);
                            break;
                        case AxisLocation.Left:
                            top = Math.Max(0, tick.Location - (height / 2));
                            if (DesiredSize.Height < (top + height)) top -= (top + height) - DesiredSize.Height;
                            left = tick.Length + 1;
                            break;
                        case AxisLocation.Right:
                            top = Math.Max(0, tick.Location - (height / 2));
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
            //Debug.WriteLine(string.Format("DataAxis: ArrangeOverride for {0} returning desired width {1} and height {2}", AxisLabel, arrangeSize.Width, arrangeSize.Height));
            return arrangeSize;
        }

        Path CreateAxis()
        {
            var valueStep = (EndValue - StartValue) / Math.Abs(_endLocation - _startLocation);
            var format = TickValueFormat == "m" ? "m" : String.Format("{{0:{0}}}", TickValueFormat);
            if (MajorTicks == null) MajorTicks = new ObservableCollection<AxisTick>();
            if (MinorTicks == null) MinorTicks = new ObservableCollection<AxisTick>();
            MajorTicks.Clear();
            MinorTicks.Clear();
            // Clear the tick cache
            _ticks.Clear();

            var majorTickValue = AxisType == AxisType.Linear ? StartValue : Math.Pow(10, Math.Floor(Math.Log10(StartValue)));
            var direction = _isVertical ? -1 : 1;
            var conditionLambda = !_isVertical
                                      ? new Func<double, double, bool>((tickLocation, endLocation) => tickLocation < endLocation - 1)
                                      : ((tickLocation, endLocation) => tickLocation > endLocation + 1);
            for (var majorTickLocation = _startLocation + (direction * (_lineThickness / 2)); conditionLambda(majorTickLocation, _endLocation); majorTickLocation += direction * _majorTickSpacing)
            {
                var majorTick = new AxisTickInternal(majorTickLocation, _majorTickLength, majorTickValue, true, format);
                _ticks.Add(majorTick);
                MajorTicks.Add(new AxisTick { Location = majorTick.Location, Value = majorTickValue });
                //Debug.WriteLine(String.Format("Added major tick at location {0}", majorTick.Location));
                if (AxisType == AxisType.Linear)
                {
                    for (var minorTickCount = 1; minorTickCount < 5; minorTickCount++)
                    {
                        var minorTick = new AxisTickInternal(majorTickLocation + (direction * minorTickCount * _minorTickSpacing), _minorTickLength, double.NaN, false, null);
                        _ticks.Add(minorTick);
                        MinorTicks.Add(new AxisTick { Location = minorTick.Location, Value = double.NaN });
                        //Debug.WriteLine(String.Format("Linear: Added minor tick at location {0}", minorTick.Location));
                    }
                    majorTickValue += (valueStep * _majorTickSpacing);
                }
                else
                {
                    for (var minorTickCount = 2; minorTickCount < 10; minorTickCount++)
                    {
                        var minorTick = new AxisTickInternal(majorTickLocation + (direction * Math.Log10(minorTickCount) * _majorTickSpacing), _minorTickLength, double.NaN, false, null);
                        _ticks.Add(minorTick);
                        MinorTicks.Add(new AxisTick { Location = minorTick.Location, Value = double.NaN });
                        //Debug.WriteLine(String.Format("Log: Added minor tick at location {0}", minorTick.Location));
                    }
                    majorTickValue = Math.Pow(10, Math.Log10(majorTickValue) + 1);
                }
            }

            // Add a major tick at the end
            majorTickValue = AxisType == AxisType.Linear ? EndValue : Math.Pow(10, Math.Ceiling(Math.Log10(EndValue)));
            var endTick = new AxisTickInternal(_endLocation - (direction * (_lineThickness / 2)), _majorTickLength, majorTickValue, true, format);
            _ticks.Add(endTick);
            MajorTicks.Add(new AxisTick { Location = endTick.Location, Value = majorTickValue });
            //Debug.WriteLine(String.Format("Added last major tick at location {0}", endTick.Location));

            // Create a StreamGeometry to use to specify _axis.
            var geometry = new StreamGeometry { FillRule = FillRule.EvenOdd };

            // Open a StreamGeometryContext that can be used to describe this StreamGeometry 
            // object's contents.
            using (var ctx = geometry.Open())
            {
                ctx.BeginFigure(TransformedPoint(_startLocation, _lineThickness / 2), false, false);
                ctx.LineTo(TransformedPoint(_endLocation, _lineThickness / 2), true, false);

                foreach (var tick in _ticks)
                {
                    ctx.BeginFigure(TransformedPoint(tick.Location, _lineThickness / 2), false, false);
                    ctx.LineTo(TransformedPoint(tick.Location, tick.Length + (_lineThickness / 2)), true, true);
                }
            }
            // Freeze the geometry (make it unmodifiable)
            // for additional performance benefits.
            geometry.Freeze();
            var axis = new Path
            {
                Stroke = Brushes.Black,
                StrokeThickness = _lineThickness,
                StrokeMiterLimit = 1,
                StrokeStartLineCap = PenLineCap.Flat,
                StrokeEndLineCap = PenLineCap.Flat,
                SnapsToDevicePixels = true,
                Data = geometry
            };

            return axis;
        }

        Point TransformedPoint(double location, double offset)
        {
            switch (AxisLocation)
            {
                case AxisLocation.Top:
                    return new Point(location, offset);
                case AxisLocation.Bottom:
                    return new Point(location, -offset);
                case AxisLocation.Left:
                    return new Point(offset, location);
                case AxisLocation.Right:
                    return new Point(-offset, location);
                default:
                    throw new ApplicationException("DataAxis: Unknown AxisLocation value.");
            }
        }
        #endregion

        public static string GetGrid(ICollection<AxisTick> horizontalTicks, ICollection<AxisTick> verticalTicks, int skipFactor, double height, double width)
        {
            var sb = new StringBuilder();
            foreach (var tick in horizontalTicks.Skip(skipFactor))
                sb.Append(String.Format("M 0,{0} H {1}", tick.Location, width));
            foreach (var tick in verticalTicks.Skip(skipFactor))
                sb.Append(String.Format("M {0},0 V {1}", tick.Location, height));
            return sb.ToString();
        }

        #region Private data members
        readonly TextBlock _axisLabel = new TextBlock();
        readonly double _lineThickness;

        readonly double _majorTickLength;
        readonly List<double> _minHeights = new List<double>();
        readonly List<double> _minWidths = new List<double>();
        readonly double _minorTickLength;

        readonly AxisTicksInternal _ticks = new AxisTicksInternal();
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
        #region Axis utility classes
        class AxisTicksInternal : List<AxisTickInternal>
        {
            public void AddLabels(Panel parent) { foreach (var tick in this.Where(tick => tick.Label != null)) parent.Children.Add(tick.Label); }
        }

        class AxisTickInternal : IComparable<AxisTickInternal>
        {
            public AxisTickInternal(double location, double height, double value, bool isMajorTick, string format)
            {
                Location = location;
                Length = height;
                if (!isMajorTick) return;
                if (format == "m")
                {
                    Label = new TextBlock { Text = "10" };
                    var superscript = new TextBlock { Text = ((int)Math.Floor(Math.Log10(value))).ToString(CultureInfo.InvariantCulture), FontSize = 10 };
                    var inline = new InlineUIContainer(superscript) { BaselineAlignment = BaselineAlignment.Superscript };
                    Label.Inlines.Add(inline);
                }
                else
                {
                    Label = new TextBlock
                    {
                        Text = String.Format(format, value)
                    };
                }
            }

            public double Location { get; private set; }
            public double Length { get; private set; }
            public TextBlock Label { get; private set; }

            #region IComparable<AxisTickInternal> Members
            int IComparable<AxisTickInternal>.CompareTo(AxisTickInternal that)
            {
                if ((that == null) || (Location > that.Location)) return 1;
                if (Location < that.Location) return -1;
                return 0;
            }
            #endregion

            ~AxisTickInternal() { Label = null; }
        }
        #endregion
    }

    public class AxisTick
    {
        public double Location { get; set; }
        public double Value { get; set; }
        public bool IsMajorTick { get; set; }
    }

    public enum AxisLocation
    {
        Top,
        Bottom,
        Left,
        Right
    }

    public enum AxisType
    {
        Linear,
        Logarithmic,
        Enumerated,
    }

}