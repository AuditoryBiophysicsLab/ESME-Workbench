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
using System.Windows.Shapes;
using DavesWPFTester.AxisLabeling.Language;
using DavesWPFTester.AxisLabeling.Layout;
using DavesWPFTester.AxisLabeling.Layout.AxisLabelers;
using DavesWPFTester.Transforms;
using ESME.NEMO;
using ESME.Views.Controls;
using HRC;
using HRC.ViewModels;

namespace DavesWPFTester
{
    public class NewDataAxis : Panel
    {
        static NewDataAxis() { DefaultStyleKeyProperty.OverrideMetadata(typeof(NewDataAxis), new FrameworkPropertyMetadata(typeof(NewDataAxis))); }

        #region dependency property ObservableCollection<AxisTick> AxisTicks

        public static DependencyProperty AxisTicksProperty = DependencyProperty.Register("AxisTicks",
                                                                                          typeof(ObservableCollection<AxisTick>),
                                                                                          typeof(NewDataAxis),
                                                                                          new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsMeasure, AxisTicksPropertyChanged));

        public ObservableCollection<AxisTick> AxisTicks { get { return (ObservableCollection<AxisTick>)GetValue(AxisTicksProperty); } set { SetValue(AxisTicksProperty, value); } }
        static void AxisTicksPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).AxisTicksPropertyChanged(args); }
        [UsedImplicitly] CollectionObserver _AxisTicksObserver;
        void AxisTicksPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if (AxisType == AxisType.Logarithmic) throw new InvalidOperationException("Cannot set AxisTicks on a Logarithmic axis");
            if (_AxisTicksObserver != null)
            {
                _AxisTicksObserver.UnregisterHandler(AxisTicksCollectionChanged);
                _AxisTicksObserver = null;
            }
            if (AxisTicks == null) return;
            _AxisTicksObserver = new CollectionObserver(AxisTicks);
            _AxisTicksObserver.RegisterHandler(AxisTicksCollectionChanged);
        }

        void AxisTicksCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            InvalidateMeasure();
        }
        #endregion

        #region dependency property AxisLocation AxisLocation {get; set;}
        public static readonly DependencyProperty AxisLocationProperty = DependencyProperty.Register("AxisLocation",
                                                                                                     typeof(AxisLocation),
                                                                                                     typeof(NewDataAxis),
                                                                                                     new FrameworkPropertyMetadata((AxisLocation.Bottom),
                                                                                                                                   FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                                   FrameworkPropertyMetadataOptions.AffectsMeasure |
                                                                                                                                   FrameworkPropertyMetadataOptions.AffectsRender,
                                                                                                                                   AxisLocationPropertyChanged));

        public AxisLocation AxisLocation { get { return (AxisLocation)GetValue(AxisLocationProperty); } set { SetCurrentValue(AxisLocationProperty, value); } }

        static void AxisLocationPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).OnDependencyPropertyChanged(); }
        #endregion

        #region dependency property AxisType AxisType
        public static DependencyProperty AxisTypeProperty = DependencyProperty.Register("AxisType",
                                                                                        typeof(AxisType),
                                                                                        typeof(NewDataAxis),
                                                                                        new FrameworkPropertyMetadata(AxisType.Linear,
                                                                                                                      FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                      FrameworkPropertyMetadataOptions.AffectsRender,
                                                                                                                      AxisTypePropertyChanged));

        public AxisType AxisType { get { return (AxisType)GetValue(AxisTypeProperty); } set { SetValue(AxisTypeProperty, value); } }

        static void AxisTypePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).OnDependencyPropertyChanged(); }
        #endregion

        #region dependency property string AxisLabel { get; set; }
        public static readonly DependencyProperty AxisLabelProperty = DependencyProperty.RegisterAttached("AxisLabel",
                                                                                                          typeof(string),
                                                                                                          typeof(NewDataAxis),
                                                                                                          new FrameworkPropertyMetadata("",
                                                                                                                                        FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                                        FrameworkPropertyMetadataOptions.AffectsMeasure |
                                                                                                                                        FrameworkPropertyMetadataOptions.AffectsRender,
                                                                                                                                        AxisLabelPropertyChanged));

        public string AxisLabel { get { return (string)GetValue(AxisLabelProperty); } set { SetCurrentValue(AxisLabelProperty, value); } }

        static void AxisLabelPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).OnDependencyPropertyChanged(); }
        #endregion

        #region dependency property double StartValue { get; set; }
        public static readonly DependencyProperty StartValueProperty = DependencyProperty.RegisterAttached("StartValue",
                                                                                                           typeof(double),
                                                                                                           typeof(NewDataAxis),
                                                                                                           new FrameworkPropertyMetadata(0.1,
                                                                                                                                         FrameworkPropertyMetadataOptions.AffectsMeasure,
                                                                                                                                         StartValuePropertyChanged));

        public double StartValue { get { return (double)GetValue(StartValueProperty); } set { SetCurrentValue(StartValueProperty, value); } }

        static void StartValuePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).OnDependencyPropertyChanged(); }
        #endregion

        #region dependency property double EndValue { get; set; }
        public static readonly DependencyProperty EndValueProperty = DependencyProperty.RegisterAttached("EndValue",
                                                                                                         typeof(double),
                                                                                                         typeof(NewDataAxis),
                                                                                                         new FrameworkPropertyMetadata(10.0,
                                                                                                                                       FrameworkPropertyMetadataOptions.AffectsMeasure,
                                                                                                                                       EndValuePropertyChanged));

        public double EndValue { get { return (double)GetValue(EndValueProperty); } set { SetCurrentValue(EndValueProperty, value); } }

        static void EndValuePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).OnDependencyPropertyChanged(); }
        #endregion

        #region dependency property string TickValueFormat { get; set; }
        public static readonly DependencyProperty TickValueFormatProperty = DependencyProperty.RegisterAttached("TickValueFormat",
                                                                                                                typeof(string),
                                                                                                                typeof(NewDataAxis),
                                                                                                                new FrameworkPropertyMetadata("0.###",
                                                                                                                                              FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                                              FrameworkPropertyMetadataOptions.AffectsMeasure,
                                                                                                                                              TickValueFormatPropertyChanged));

        public string TickValueFormat { get { return (string)GetValue(TickValueFormatProperty); } set { SetCurrentValue(TickValueFormatProperty, value); } }

        static void TickValueFormatPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).OnDependencyPropertyChanged(); }
        #endregion

        #region dependency property AxisLayoutAlgorithm AxisLayoutAlgorithm

        public static DependencyProperty AxisLayoutAlgorithmProperty = DependencyProperty.Register("AxisLayoutAlgorithm",
                                                                                 typeof(AxisLayoutAlgorithm),
                                                                                 typeof(NewDataAxis),
                                                                                 new FrameworkPropertyMetadata(AxisLayoutAlgorithm.ExtendedWilkinson, FrameworkPropertyMetadataOptions.None, AxisLayoutAlgorithmPropertyChanged));

        public AxisLayoutAlgorithm AxisLayoutAlgorithm { get { return (AxisLayoutAlgorithm)GetValue(AxisLayoutAlgorithmProperty); } set { SetValue(AxisLayoutAlgorithmProperty, value); } }
        static void AxisLayoutAlgorithmPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).OnDependencyPropertyChanged(); }
        #endregion

        #region dependency property Func<double, double> MappingFunction

        public static DependencyProperty MappingFunctionProperty = DependencyProperty.Register("MappingFunction",
                                                                                               typeof(Func<double, double>),
                                                                                               typeof(NewDataAxis),
                                                                                               new FrameworkPropertyMetadata(null,
                                                                                                                             FrameworkPropertyMetadataOptions.BindsTwoWayByDefault,
                                                                                                                             MappingFunctionPropertyChanged));

        public Func<double, double> MappingFunction { get { return (Func<double, double>)GetValue(MappingFunctionProperty); } set { SetValue(MappingFunctionProperty, value); } }

        static void MappingFunctionPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).MappingFunctionPropertyChanged(); }
        void MappingFunctionPropertyChanged() { }
        #endregion

        #region dependency property NewDataAxis Axis

        public static DependencyProperty AxisProperty = DependencyProperty.Register("Axis",
                                                                                 typeof(NewDataAxis),
                                                                                 typeof(NewDataAxis),
                                                                                 new FrameworkPropertyMetadata(null));

        public NewDataAxis Axis { get { return (NewDataAxis)GetValue(AxisProperty); } set { SetValue(AxisProperty, value); } }
        #endregion

        #region dependency property Range DataRange

        public static DependencyProperty DataRangeProperty = DependencyProperty.Register("DataRange",
                                                                                 typeof(Range),
                                                                                 typeof(NewDataAxis),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.None, DataRangePropertyChanged));

        public Range DataRange { get { return (Range)GetValue(DataRangeProperty); } set { SetValue(DataRangeProperty, value); } }
        static void DataRangePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).OnDependencyPropertyChanged(); }
        #endregion

        #region dependency property Range VisibleRange

        public static DependencyProperty VisibleRangeProperty = DependencyProperty.Register("VisibleRange",
                                                                                 typeof(Range),
                                                                                 typeof(NewDataAxis),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.None, VisibleRangePropertyChanged));

        public Range VisibleRange { get { return (Range)GetValue(VisibleRangeProperty); } set { SetValue(VisibleRangeProperty, value); } }
        static void VisibleRangePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).OnDependencyPropertyChanged(); }
        #endregion

        #region dependency property double MajorTickLength

        public static DependencyProperty MajorTickLengthProperty = DependencyProperty.Register("MajorTickLength",
                                                                                 typeof(double),
                                                                                 typeof(NewDataAxis),
                                                                                 new FrameworkPropertyMetadata(6.0, FrameworkPropertyMetadataOptions.None, MajorTickLengthPropertyChanged));

        public double MajorTickLength { get { return (double)GetValue(MajorTickLengthProperty); } set { SetValue(MajorTickLengthProperty, value); } }
        static void MajorTickLengthPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).OnDependencyPropertyChanged(); }
        #endregion

        #region dependency property double MinorTickLength

        public static DependencyProperty MinorTickLengthProperty = DependencyProperty.Register("MinorTickLength",
                                                                                 typeof(double),
                                                                                 typeof(NewDataAxis),
                                                                                 new FrameworkPropertyMetadata(3.0, FrameworkPropertyMetadataOptions.None, MinorTickLengthPropertyChanged));

        public double MinorTickLength { get { return (double)GetValue(MinorTickLengthProperty); } set { SetValue(MinorTickLengthProperty, value); } }
        static void MinorTickLengthPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).OnDependencyPropertyChanged(); }
        #endregion
    
        double PrivateMappingFunction(double value)
        {
            var startValue = AxisType == AxisType.Linear ? StartValue : Math.Floor(Math.Log10(StartValue));
            var endValue = AxisType == AxisType.Linear ? EndValue : Math.Ceiling(Math.Log10(EndValue));
            value = AxisType == AxisType.Linear ? value : Math.Log10(value);
            var lowValue = Math.Min(startValue, endValue);
            var highValue = Math.Max(startValue, endValue);
            if (highValue == lowValue) return highValue;
            if (value < lowValue || value > highValue) throw new ParameterOutOfRangeException("value is out of range for this axis");
            var axisDelta = highValue - lowValue;
            var valueDelta = value - lowValue;
            var valueRatio = valueDelta / axisDelta;
            var offsetFromLength = _axisOptions.AxisLocation == AxisLocation.Left || _axisOptions.AxisLocation == AxisLocation.Right;
            if (endValue < startValue) offsetFromLength = !offsetFromLength;
            var lengthOffset = offsetFromLength ? _length - (_length * valueRatio) : _length * valueRatio;
            return lengthOffset;
        }

        public NewDataAxis()
        {
            SnapsToDevicePixels = true;
            UseLayoutRounding = true;
            AxisTicks = new ObservableCollection<AxisTick>();
            SizeChanged += (s, e) => CreateAxisTransform(new Size(ActualWidth, ActualHeight));
        }

        void OnDependencyPropertyChanged()
        {
            // Catchall property changed function, called when any of the dependency properties that might affect layout, arrange or render change
            if (_axisOptions == null) _axisOptions = new AxisLabelerOptions();
            _axisOptions.DataRange = DataRange;
            _axisOptions.VisibleRange = VisibleRange;
            _axisOptions.FontSize = TextBlock.GetFontSize(this);
            _axisOptions.FontFamily = TextBlock.GetFontFamily(this);
            _axisOptions.ComputeLabelRect = ComputeLabelRect;
            _axisOptions.DataRange = new Range(StartValue, EndValue);
            _axisOptions.VisibleRange = new Range(StartValue, EndValue);
            _lineThickness = 1;
            _majorTickSpacing = 100;
            _minorTickSpacing = 10;
            _axisOptions.AxisLocation = AxisLocation;
            MappingFunction = PrivateMappingFunction;
            Axis = this;
            switch (AxisLayoutAlgorithm)
            {
                case AxisLayoutAlgorithm.Wilkinson:
                    _axisLabeler = new WilkinsonAxisLabeler();
                    break;
                case AxisLayoutAlgorithm.ExtendedWilkinson:
                    _axisLabeler = new ExtendedAxisLabeler();
                    break;
                case AxisLayoutAlgorithm.MatPlotLib:
                    _axisLabeler = new MatplotlibAxisLabeler();
                    break;
                case AxisLayoutAlgorithm.Heckbert:
                    _axisLabeler = new HeckbertAxisLabeler();
                    break;
            }
            _typeface = new Typeface(TextBlock.GetFontFamily(this), TextBlock.GetFontStyle(this), TextBlock.GetFontWeight(this), TextBlock.GetFontStretch(this));
            InvalidateVisual();
        }

        void CreateAxisTransform(Size newSize)
        {
            double tickDirectionScale;
            double originScale;
            double axisDirectionTranslation;
            double tickDirectionTranslation;
            _axisTransform.Children.Clear();
            // The intent of _axisTransform is to make every axis draw the same as a Bottom axis (i.e. the StartValue is at 
            // transformed-X of 0, and the axis line is drawn from top left to top right, axis ticks from top to tickLength)
            switch (AxisLocation)
            {
                case AxisLocation.Top:
                    tickDirectionScale = -1.0;
                    originScale = 1.0;
                    axisDirectionTranslation = 0.0;
                    tickDirectionTranslation = newSize.Height;
                    break;
                case AxisLocation.Bottom:
                    tickDirectionScale = 1.0;
                    originScale = 1.0;
                    axisDirectionTranslation = 0.0;
                    tickDirectionTranslation = 0.0;
                    break;
                case AxisLocation.Left:
                    tickDirectionScale = -1.0;
                    originScale = -1.0;
                    axisDirectionTranslation = newSize.Height;
                    tickDirectionTranslation = newSize.Width;
                    //_axisTransform.Children.Add(new SwapTransform());
                    break;
                case AxisLocation.Right:
                    tickDirectionScale = 1.0;
                    originScale = -1.0;
                    axisDirectionTranslation = newSize.Height;
                    tickDirectionTranslation = 0.0;
                    //_axisTransform.Children.Add(new SwapTransform());
                    break;
                default:
                    throw new ApplicationException("NewDataAxis: Unknown AxisLocation value.");
            }
            var startValue = StartValue;
            var endValue = EndValue;
            if (AxisType == AxisType.Logarithmic)
            {
                startValue = Math.Log10(startValue);
                endValue = Math.Log10(endValue);
                _axisTransform.Children.Add(new LogTransform(10));
            }
            var lowValue = Math.Min(startValue, endValue);
            var highValue = Math.Max(startValue, endValue);
            var range = highValue - lowValue;
            _axisTransform.Children.Add(new TranslateTransform(-startValue, 0));
            if (startValue > endValue) originScale *= -1;
            _axisTransform.Children.Add(new ScaleTransform(originScale * (newSize.Width / range), tickDirectionScale));
            _axisTransform.Children.Add(new TranslateTransform(axisDirectionTranslation, tickDirectionTranslation));
            if (AxisLocation == AxisLocation.Left || AxisLocation == AxisLocation.Right) _axisTransform.Children.Add(new SwapTransform());
        }

        readonly GeneralTransformGroup _axisTransform = new GeneralTransformGroup();

        Typeface _typeface;
        Rect ComputeLabelRect(string label, double position, Axis axis)
        {
            var axisPosition = _axisTransform.Transform(new Point(position, MajorTickLength));
            var text = new FormattedText(label, CultureInfo.CurrentCulture, FlowDirection.LeftToRight, _typeface, TextBlock.GetFontSize(this), Brushes.Black);
            var left = axisPosition.X;
            var top = axisPosition.Y;
            switch (axis.AxisLocation)
            {
                case AxisLocation.Top:
                    left -= text.Width / 2;
                    top -= text.Height;
                    break;
                case AxisLocation.Bottom:
                    left -= text.Width / 2;
                    break;
                case AxisLocation.Left:
                    top -= text.Height / 2;
                    left -= text.Width;
                    break;
                case AxisLocation.Right:
                    top -= text.Height / 2;
                    break;
                default:
                    throw new ApplicationException("NewDataAxis: Unknown AxisLocation value.");
            }
            return new Rect(new Point(top, left), new Size(text.Width, text.Height));
        }

        #region Layout and drawing code
        void CreateChildren(Size newSize)
        {
            if (_axisOptions == null) OnDependencyPropertyChanged();
            if (_axisOptions == null) throw new ApplicationException();
            CreateAxisTransform(newSize);
            _axisOptions.Screen = new Rect(newSize);
            var axis = _axisLabeler.Generate(_axisOptions, 1.0 / 96.0);

            if (_axisOptions.AxisLocation == AxisLocation.Left || _axisOptions.AxisLocation == AxisLocation.Right)
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
            _axis.Measure(availableSize);
            foreach (var tick in _ticks)
            {
                if (tick.Label != null)
                {
                    tick.Label.Measure(availableSize);
                    labelSizes.Add(_axisOptions.AxisLocation == AxisLocation.Left || _axisOptions.AxisLocation == AxisLocation.Right ? tick.Label.DesiredSize.Width : tick.Label.DesiredSize.Height);
                }
            }

            double axisLabelSize = 0;
            if (_axisLabel != null)
            {
                _axisLabel.Measure(availableSize);
                axisLabelSize = _axisLabel.DesiredSize.Height;
            }
            var maxLabelSize = labelSizes.Max();
            var shortSize = maxLabelSize + MajorTickLength + TickLabelSpacing + axisLabelSize + TickLabelSpacing;
            var longSize = _length;
            var desiredSize = _axisOptions.AxisLocation == AxisLocation.Left || _axisOptions.AxisLocation == AxisLocation.Right ? new Size(shortSize, longSize) : new Size(longSize, shortSize);

            // desiredSize = ... computed sum of children's DesiredSize ...;
            // IMPORTANT: do not allow PositiveInfinity to be returned, that will raise an exception in the caller!
            // PositiveInfinity might be an availableSize input; this means that the parent does not care about sizing
            return desiredSize;
        }

        protected override Size ArrangeOverride(Size arrangeSize)
        {
            if (_axisOptions == null) throw new ApplicationException();
            _axisOptions.Screen = new Rect(arrangeSize);
            //var axis = _axisLabeler.Generate(_axisOptions, 1.0 / 150);
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
                    throw new ApplicationException("NewDataAxis: Unknown AxisLocation value.");
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
                            throw new ApplicationException("NewDataAxis: Unknown AxisLocation value.");
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
            var valueStep = (EndValue - StartValue) / Math.Abs(_endLocation - _startLocation);
            var format = TickValueFormat == "m" ? "m" : String.Format("{{0:{0}}}", TickValueFormat);
            if (AxisTicks == null) AxisTicks = new ObservableCollection<AxisTick>();
            AxisTicks.Clear();
            // Clear the tick cache
            _ticks.Clear();

            var majorTickValue = AxisType == AxisType.Linear ? StartValue : Math.Pow(10, Math.Floor(Math.Log10(StartValue)));
            var direction = _axisOptions.AxisLocation == AxisLocation.Left || _axisOptions.AxisLocation == AxisLocation.Right ? -1 : 1;
            var conditionLambda = _axisOptions.AxisLocation == AxisLocation.Left || _axisOptions.AxisLocation == AxisLocation.Right
                                      ? new Func<double, double, bool>((tickLocation, endLocation) => tickLocation < endLocation - 1)
                                      : ((tickLocation, endLocation) => tickLocation > endLocation + 1);
            for (var majorTickLocation = _startLocation + (direction * (_lineThickness / 2)); conditionLambda(majorTickLocation, _endLocation); majorTickLocation += direction * _majorTickSpacing)
            {
                var majorTick = new AxisTickInternal(majorTickLocation, MajorTickLength, majorTickValue, true, format);
                _ticks.Add(majorTick);
                AxisTicks.Add(new AxisTick { Location = majorTick.Location, Value = majorTickValue, IsMajorTick = true });
                //Debug.WriteLine(String.Format("Added major tick at location {0}", majorTick.Location));
                if (AxisType == AxisType.Linear)
                {
                    for (var minorTickCount = 1; minorTickCount < 5; minorTickCount++)
                    {
                        var minorTick = new AxisTickInternal(majorTickLocation + (direction * minorTickCount * _minorTickSpacing), MinorTickLength, double.NaN, false, null);
                        _ticks.Add(minorTick);
                        AxisTicks.Add(new AxisTick { Location = minorTick.Location, Value = double.NaN, IsMajorTick = false });
                        //Debug.WriteLine(String.Format("Linear: Added minor tick at location {0}", minorTick.Location));
                    }
                    majorTickValue += (valueStep * _majorTickSpacing);
                }
                else
                {
                    for (var minorTickCount = 2; minorTickCount < 10; minorTickCount++)
                    {
                        var minorTick = new AxisTickInternal(majorTickLocation + (direction * Math.Log10(minorTickCount) * _majorTickSpacing), MinorTickLength, double.NaN, false, null);
                        _ticks.Add(minorTick);
                        AxisTicks.Add(new AxisTick { Location = minorTick.Location, Value = double.NaN, IsMajorTick = false });
                        //Debug.WriteLine(String.Format("Log: Added minor tick at location {0}", minorTick.Location));
                    }
                    majorTickValue = Math.Pow(10, Math.Log10(majorTickValue) + 1);
                }
            }

            // Add a major tick at the end
            majorTickValue = AxisType == AxisType.Linear ? EndValue : Math.Pow(10, Math.Ceiling(Math.Log10(EndValue)));
            var endTick = new AxisTickInternal(_endLocation - (direction * (_lineThickness / 2)), MajorTickLength, majorTickValue, true, format);
            _ticks.Add(endTick);
            AxisTicks.Add(new AxisTick { Location = endTick.Location, Value = majorTickValue, IsMajorTick = true });
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
                    throw new ApplicationException("NewDataAxis: Unknown AxisLocation value.");
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
        double _lineThickness;

        readonly AxisTicksInternal _ticks = new AxisTicksInternal();
        Path _axis;
        double _endLocation;
        double _length;
        int _majorTickCount;

        double _majorTickSpacing,
               _minorTickSpacing;

        AxisLabeler _axisLabeler;
        AxisLabelerOptions _axisOptions;
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
}