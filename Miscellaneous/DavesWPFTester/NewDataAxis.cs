using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Diagnostics;
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
                                                                                          new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.None, AxisTicksPropertyChanged));

        public ObservableCollection<AxisTick> AxisTicks { get { return (ObservableCollection<AxisTick>)GetValue(AxisTicksProperty); } set { SetValue(AxisTicksProperty, value); } }
        static void AxisTicksPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).AxisTicksPropertyChanged(); }
        [UsedImplicitly] CollectionObserver _axisTicksObserver;
        void AxisTicksPropertyChanged()
        {
            if (AxisType == AxisType.Logarithmic) throw new InvalidOperationException("Cannot set AxisTicks on a Logarithmic axis");
            if (_axisTicksObserver != null)
            {
                _axisTicksObserver.UnregisterHandler(AxisTicksCollectionChanged);
                _axisTicksObserver = null;
            }
            if (AxisTicks == null) return;
            _axisTicksObserver = new CollectionObserver(AxisTicks);
            _axisTicksObserver.RegisterHandler(AxisTicksCollectionChanged);
        }

        static void AxisTicksCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
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

        static void AxisLocationPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).AxisLocationPropertyChanged(); }
        void AxisLocationPropertyChanged() { _axisOptions.AxisLocation = AxisLocation; }
        #endregion

        #region dependency property AxisType AxisType
        public static DependencyProperty AxisTypeProperty = DependencyProperty.Register("AxisType",
                                                                                        typeof(AxisType),
                                                                                        typeof(NewDataAxis),
                                                                                        new FrameworkPropertyMetadata(AxisType.Linear,
                                                                                                                      FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                      FrameworkPropertyMetadataOptions.AffectsMeasure |
                                                                                                                      FrameworkPropertyMetadataOptions.AffectsRender,
                                                                                                                      AxisTypePropertyChanged));

        public AxisType AxisType { get { return (AxisType)GetValue(AxisTypeProperty); } set { SetValue(AxisTypeProperty, value); } }

        static void AxisTypePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).OnSizeOrVisibleRangeChanged(); }
        #endregion

        #region dependency property string AxisLabel { get; set; }
        public static readonly DependencyProperty AxisLabelProperty = DependencyProperty.RegisterAttached("AxisLabel",
                                                                                                          typeof(string),
                                                                                                          typeof(NewDataAxis),
                                                                                                          new FrameworkPropertyMetadata("",
                                                                                                                                        FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                                        FrameworkPropertyMetadataOptions.AffectsMeasure |
                                                                                                                                        FrameworkPropertyMetadataOptions.AffectsRender));

        public string AxisLabel { get { return (string)GetValue(AxisLabelProperty); } set { SetCurrentValue(AxisLabelProperty, value); } }
        #endregion

        #region dependency property bool IsInverted

        public static DependencyProperty IsInvertedProperty = DependencyProperty.Register("IsInverted",
                                                                                 typeof(bool),
                                                                                 typeof(NewDataAxis),
                                                                                 new FrameworkPropertyMetadata(false, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, IsInvertedPropertyChanged));

        public bool IsInverted { get { return (bool)GetValue(IsInvertedProperty); } set { SetValue(IsInvertedProperty, value); } }

        static void IsInvertedPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).IsInvertedPropertyChanged(); }
        void IsInvertedPropertyChanged() { }
        #endregion

        #region dependency property string TickValueFormat { get; set; }
        public static readonly DependencyProperty TickValueFormatProperty = DependencyProperty.RegisterAttached("TickValueFormat",
                                                                                                                typeof(string),
                                                                                                                typeof(NewDataAxis),
                                                                                                                new FrameworkPropertyMetadata("0.###",
                                                                                                                                              FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                                              FrameworkPropertyMetadataOptions.AffectsMeasure));

        public string TickValueFormat { get { return (string)GetValue(TickValueFormatProperty); } set { SetCurrentValue(TickValueFormatProperty, value); } }
        #endregion

        #region dependency property AxisLayoutAlgorithm AxisLayoutAlgorithm

        public static DependencyProperty AxisLayoutAlgorithmProperty = DependencyProperty.Register("AxisLayoutAlgorithm",
                                                                                 typeof(AxisLayoutAlgorithm),
                                                                                 typeof(NewDataAxis),
                                                                                 new FrameworkPropertyMetadata(AxisLayoutAlgorithm.ExtendedWilkinson, FrameworkPropertyMetadataOptions.None, AxisLayoutAlgorithmPropertyChanged));

        public AxisLayoutAlgorithm AxisLayoutAlgorithm { get { return (AxisLayoutAlgorithm)GetValue(AxisLayoutAlgorithmProperty); } set { SetValue(AxisLayoutAlgorithmProperty, value); } }
        static void AxisLayoutAlgorithmPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).AxisLayoutAlgorithmPropertyChanged(); }
        void AxisLayoutAlgorithmPropertyChanged()
        {             // Catchall property changed function, called when any of the dependency properties that might affect layout, arrange or render change
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
        }
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
        static void DataRangePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).DataRangePropertyChanged(args); }
        void DataRangePropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if (args.OldValue != null) ((Range)args.OldValue).RangeChanged -= DataRangeChanged;
            if (args.NewValue != null) ((Range)args.NewValue).RangeChanged += DataRangeChanged;
            DataRangeChanged(null, null);
        }
        void DataRangeChanged(object sender, EventArgs args)
        {
            if (DataRange == null)
            {
                _axisOptions.DataRange = null;
                VisibleRange = null;
                return;
            }
            _dataRange = DataRange.Expand(0);
            if (AxisType == AxisType.Logarithmic)
            {
                if (_dataRange.Min <= 0) throw new InvalidOperationException("Cannot plot negative or zero values on a log scale");
                _dataRange.Min = Math.Log10(_dataRange.Min);
                _dataRange.Max = Math.Log10(_dataRange.Max);
            }
            _axisOptions.DataRange = _dataRange;
            VisibleRange = DataRange.Size > 0 ? DataRange.Expand(DataRange.Size * 0.05) : new Range(DataRange.Min - 0.5, DataRange.Max + 0.5);
        }

        Range _dataRange;
        #endregion

        #region dependency property Range VisibleRange

        public static DependencyProperty VisibleRangeProperty = DependencyProperty.Register("VisibleRange",
                                                                                            typeof(Range),
                                                                                            typeof(NewDataAxis),
                                                                                            new FrameworkPropertyMetadata(null,
                                                                                                                          FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                          FrameworkPropertyMetadataOptions.AffectsRender |
                                                                                                                          FrameworkPropertyMetadataOptions.AffectsMeasure,
                                                                                                                          VisibleRangePropertyChanged));

        public Range VisibleRange { get { return (Range)GetValue(VisibleRangeProperty); } set { SetValue(VisibleRangeProperty, value); } }
        static void VisibleRangePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).OnSizeOrVisibleRangeChanged(); }
        void OnSizeOrVisibleRangeChanged()
        {
            if (ActualWidth <= 0 || ActualHeight <= 0 || VisibleRange == null || VisibleRange.Size <= 0)
            {
                MappingFunction = null;
                return;
            }
            _visibleRange = VisibleRange.Expand(0);
            if (AxisType == AxisType.Logarithmic)
            {
                if (VisibleRange.Min <= 0) VisibleRange.Min = _visibleRange.Min = DataRange.Min;
                _visibleRange.Min = Math.Log10(_visibleRange.Min);
                _visibleRange.Max = Math.Log10(_visibleRange.Max);
            }
            _mappingFunctionTransform = CreateAxisTransform(_visibleRange, new Size(ActualWidth, ActualHeight), false);
            MappingFunction = v => _mappingFunctionTransform.Transform(new Point(v, 0)).X;
            _axisOptions.VisibleRange = _visibleRange.Expand(0);
        }

        Range _visibleRange;

        #endregion

        #region dependency property double MajorTickLength

        public static DependencyProperty MajorTickLengthProperty = DependencyProperty.Register("MajorTickLength",
                                                                                               typeof(double),
                                                                                               typeof(NewDataAxis),
                                                                                               new FrameworkPropertyMetadata(6.0,
                                                                                                                             FrameworkPropertyMetadataOptions.AffectsMeasure |
                                                                                                                             FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                             FrameworkPropertyMetadataOptions.AffectsRender));

        public double MajorTickLength { get { return (double)GetValue(MajorTickLengthProperty); } set { SetValue(MajorTickLengthProperty, value); } }
        #endregion

        #region dependency property double MinorTickLength

        public static DependencyProperty MinorTickLengthProperty = DependencyProperty.Register("MinorTickLength",
                                                                                               typeof(double),
                                                                                               typeof(NewDataAxis),
                                                                                               new FrameworkPropertyMetadata(3.0,
                                                                                                                             FrameworkPropertyMetadataOptions.AffectsMeasure |
                                                                                                                             FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                             FrameworkPropertyMetadataOptions.AffectsRender));

        public double MinorTickLength { get { return (double)GetValue(MinorTickLengthProperty); } set { SetValue(MinorTickLengthProperty, value); } }
        #endregion
    
        public NewDataAxis()
        {
            SnapsToDevicePixels = true;
            UseLayoutRounding = true;
            AxisTicks = new ObservableCollection<AxisTick>();
            Axis = this;
            SizeChanged += (s, e) => OnSizeOrVisibleRangeChanged();
            _axisOptions = new AxisLabelerOptions
            {
                ComputeLabelRect = ComputeLabelRect,
                FontSize = TextBlock.GetFontSize(this),
                Typeface = new Typeface(TextBlock.GetFontFamily(this), TextBlock.GetFontStyle(this), TextBlock.GetFontWeight(this), TextBlock.GetFontStretch(this))
            };
            _axisLabeler = new ExtendedAxisLabeler();
        }

        GeneralTransform _mappingFunctionTransform;

        GeneralTransform CreateAxisTransform(Range visbleRange, Size newSize, bool includeSwapTransform)
        {
            double tickDirectionScale;
            double originScale;
            double axisDirectionTranslation;
            double tickDirectionTranslation;
            double axisLength;
            switch (AxisLocation)
            {
                case AxisLocation.Top:
                    tickDirectionScale = -1.0;
                    originScale = 1.0;
                    axisDirectionTranslation = 0.0;
                    tickDirectionTranslation = newSize.Height;
                    axisLength = newSize.Width;
                    break;
                case AxisLocation.Bottom:
                    tickDirectionScale = 1.0;
                    originScale = 1.0;
                    axisDirectionTranslation = 0.0;
                    tickDirectionTranslation = 0.0;
                    axisLength = newSize.Width;
                    break;
                case AxisLocation.Left:
                    tickDirectionScale = -1.0;
                    originScale = -1.0;
                    axisDirectionTranslation = newSize.Height;
                    tickDirectionTranslation = newSize.Width;
                    axisLength = newSize.Height;
                    break;
                case AxisLocation.Right:
                    tickDirectionScale = 1.0;
                    originScale = -1.0;
                    axisDirectionTranslation = newSize.Height;
                    tickDirectionTranslation = 0.0;
                    axisLength = newSize.Height;
                    break;
                default:
                    throw new ApplicationException("NewDataAxis: Unknown AxisLocation value.");
            }
            if (IsInverted) originScale *= -1;
            var result = new GeneralTransformGroup();
            // The intent of _axisTransform is to make every axis draw the same as a Bottom axis (i.e. the StartValue is at 
            // transformed-X of 0, and the axis line is drawn from top left to top right, axis ticks from top to tickLength)
            result.Children.Add(new TranslateTransform(-visbleRange.Min, 0)); // might be -lowValue
            result.Children.Add(new ScaleTransform(originScale * (axisLength / visbleRange.Size), tickDirectionScale));
            result.Children.Add(new TranslateTransform(axisDirectionTranslation, tickDirectionTranslation));
            if (includeSwapTransform && AxisLocation == AxisLocation.Left || AxisLocation == AxisLocation.Right) result.Children.Add(new SwapTransform());
            return result;
        }

        static Rect ComputeLabelRect(string label, double position, Axis axis, AxisLabelerOptions options)
        {
            var axisPosition = options.AxisTransform.Transform(new Point(position, axis.TickSize));
            var text = new FormattedText(label, CultureInfo.CurrentCulture, FlowDirection.LeftToRight, options.Typeface, options.FontSize, Brushes.Black);
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
            return new Rect(new Point(left, top), new Size(text.Width, text.Height));
        }

        #region Layout and drawing code

        Axis CalculateAxisLabels(Size newSize)
        {
            _axisOptions.AxisTransform = CreateAxisTransform(_visibleRange, newSize, true);
            _axisOptions.Screen = new Rect(newSize);
            return _axisLabeler.Generate(_axisOptions, 1.0 / 96.0);
        }

        double _tickLabelDimension;
        double _axisLabelDimension;
        Axis _tickLabels;
        protected override Size MeasureOverride(Size availableSize)
        {
            if (_visibleRange == null) return AxisLocation == AxisLocation.Top || AxisLocation == AxisLocation.Bottom ? new Size(availableSize.Width, 22) : new Size(availableSize.Height, 22);
            if (Double.IsNaN(availableSize.Width) || Double.IsInfinity(availableSize.Width)) availableSize.Width = SystemParameters.VirtualScreenWidth;
            if (Double.IsNaN(availableSize.Height) || Double.IsInfinity(availableSize.Height)) availableSize.Height = SystemParameters.VirtualScreenHeight;
            _tickLabels = CalculateAxisLabels(availableSize);
            var axisTransform = CreateAxisTransform(_visibleRange, availableSize, true);
            Children.Clear();
            _axis = CreateAxis(_tickLabels, axisTransform);
            Children.Add(_axis);
            _ticks.AddLabels(this);
            var longestTickLabel = (from label in _tickLabels.Labels
                                    orderby label.Item2.Length
                                    select label.Item2).Last();
            var longestTickLabelText = new FormattedText(longestTickLabel, CultureInfo.CurrentCulture, FlowDirection.LeftToRight, _axisOptions.Typeface, _axisOptions.FontSize, Brushes.Black);
            var axisLabel = string.IsNullOrEmpty(_tickLabels.AxisTitleExtension) ? AxisLabel : string.Format("{0} ({1})", AxisLabel, _tickLabels.AxisTitleExtension);
            var axisLabelText = new FormattedText(axisLabel, CultureInfo.CurrentCulture, FlowDirection.LeftToRight, _axisOptions.Typeface, _axisOptions.FontSize + 2, Brushes.Black);
            var desiredSize = new Size(availableSize.Width, availableSize.Height);
            Children.Add(_axisLabel);
            switch (AxisLocation)
            {
                case AxisLocation.Top:
                case AxisLocation.Bottom:
                    _tickLabelDimension = longestTickLabelText.Height;
                    _axisLabelDimension = axisLabelText.Height;
                    desiredSize.Height = _tickLabels.TickSize + 2 + _tickLabelDimension + 2 + _axisLabelDimension;
                    break;
                case AxisLocation.Left:
                case AxisLocation.Right:
                    _tickLabelDimension = longestTickLabelText.Width;
                    _axisLabelDimension = axisLabelText.Width;
                    desiredSize.Width = _tickLabels.TickSize + 2 + _tickLabelDimension + 2 + _axisLabelDimension;
                    break;
                default:
                    throw new ApplicationException("NewDataAxis: Unknown AxisLocation value.");
            }
            // desiredSize = ... computed sum of children's DesiredSize ...;
            // IMPORTANT: do not allow PositiveInfinity to be returned, that will raise an exception in the caller!
            // PositiveInfinity might be an availableSize input; this means that the parent does not care about sizing
            Debug.WriteLine(string.Format("NewDataAxis: MeasureOverride for {0} returning desired width {1} and height {2}. Count: {3}", AxisLabel, desiredSize.Width, desiredSize.Height, _measureOverrideCount++));
            return desiredSize;
        }
        int _measureOverrideCount;

        protected override Size ArrangeOverride(Size arrangeSize)
        {
            if (_visibleRange == null) return AxisLocation == AxisLocation.Top || AxisLocation == AxisLocation.Bottom ? new Size(arrangeSize.Width, 22) : new Size(arrangeSize.Height, 22);
            switch (AxisLocation)
            {
                case AxisLocation.Top:
                case AxisLocation.Bottom:
                    arrangeSize.Height = _tickLabels.TickSize + 2 + _tickLabelDimension + 2 + _axisLabelDimension;
                    break;
                case AxisLocation.Left:
                case AxisLocation.Right:
                    arrangeSize.Width = _tickLabels.TickSize + 2 + _tickLabelDimension + 2 + _axisLabelDimension;
                    break;
                default:
                    throw new ApplicationException("NewDataAxis: Unknown AxisLocation value.");
            }
            var axisTransform = CreateAxisTransform(_visibleRange, arrangeSize, true);

            _axis.Arrange(axisTransform.TransformBounds(new Rect(_visibleRange.Min, 0, _visibleRange.Size, _tickLabels.TickSize)));
            var longestTickLabel = (from label in _tickLabels.Labels
                                    orderby label.Item2.Length
                                    select label.Item2).Last();
            var longestTickLabelText = new FormattedText(longestTickLabel, CultureInfo.CurrentCulture, FlowDirection.LeftToRight, _axisOptions.Typeface, _axisOptions.FontSize, Brushes.Black);
            var axisLabel = string.IsNullOrEmpty(_tickLabels.AxisTitleExtension) ? AxisLabel : string.Format("{0} ({1})", AxisLabel, _tickLabels.AxisTitleExtension);
            var axisLabelText = new FormattedText(axisLabel, CultureInfo.CurrentCulture, FlowDirection.LeftToRight, _axisOptions.Typeface, _axisOptions.FontSize + 2, Brushes.Black);
            _axisLabel.Text = axisLabel;
            _axisLabel.FontSize = _tickLabels.FontSize + 2;
            Point axisLabelTopLeft;
            switch (AxisLocation)
            {
                case AxisLocation.Top:
                case AxisLocation.Bottom:
                    axisLabelTopLeft = axisTransform.Transform(new Point(_visibleRange.Size / 2, _tickLabels.TickSize + 2 + longestTickLabelText.Height + 2 + axisLabelText.Height));
                    axisLabelTopLeft.X -= axisLabelText.Width / 2;
                    _axisLabel.Arrange(new Rect(axisLabelTopLeft, new Size(axisLabelText.Width, axisLabelText.Height)));
                    break;
                case AxisLocation.Left:
                case AxisLocation.Right:
                    axisLabelTopLeft = axisTransform.Transform(new Point(_visibleRange.Size / 2, _tickLabels.TickSize + 2 + longestTickLabelText.Width + 2 + axisLabelText.Height));
                    axisLabelTopLeft.Y -= axisLabelText.Width / 2;
                    _axisLabel.Arrange(new Rect(axisLabelTopLeft, new Size(axisLabelText.Width, axisLabelText.Height)));
#if false
                    _axisLabel.RenderTransformOrigin = new Point(0.5, 0.5);
                    _axisLabel.Arrange(new Rect(DesiredSize.Width - (_axisLabel.DesiredSize.Width / 2) - (_axisLabel.DesiredSize.Height / 2),
                                                (DesiredSize.Height - _axisLabel.DesiredSize.Height) / 2,
                                                _axisLabel.DesiredSize.Width,
                                                _axisLabel.DesiredSize.Height));
                    _axis.Arrange(new Rect(0, 0, _axis.DesiredSize.Width, _axis.DesiredSize.Height));
                    _axisLabel.RenderTransform = new RotateTransform(90);
#endif
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
            Debug.WriteLine(string.Format("NewDataAxis: ArrangeOverride for {0} returning desired width {1} and height {2}", AxisLabel, arrangeSize.Width, arrangeSize.Height));
            return arrangeSize;
        }


        Path CreateAxis(Axis axis, GeneralTransform axisTransform)
        {
            var format = TickValueFormat == "m" ? "m" : String.Format("{{0:{0}}}", TickValueFormat);
            if (AxisTicks == null) AxisTicks = new ObservableCollection<AxisTick>();
            AxisTicks.Clear();
            // Clear the tick cache
            _ticks.Clear();
            var axisGeometry = new StreamGeometry { FillRule = FillRule.EvenOdd };
            using (var ctx = axisGeometry.Open())
            {
                ctx.BeginFigure(axisTransform.Transform(new Point(_visibleRange.Min, 0)), false, false);
                ctx.LineTo(axisTransform.Transform(new Point(_visibleRange.Max, 0)), true, false);
                foreach (var label in axis.Labels)
                {
                    var tickStart = axisTransform.Transform(new Point(label.Item1, 0));
                    var tickEnd = axisTransform.Transform(new Point(label.Item1, axis.TickSize));
                    ctx.BeginFigure(tickStart, false, false);
                    ctx.LineTo(tickEnd, true, false);
                    var tickLocation = AxisLocation == AxisLocation.Top || AxisLocation == AxisLocation.Bottom ? tickStart.X : tickStart.Y;
                    var tick = new AxisTickInternal(tickLocation, axis.TickSize, label.Item1, true, format);
                    _ticks.Add(tick);
                    AxisTicks.Add(new AxisTick { Location = tickLocation, IsMajorTick = true, Value = label.Item1 });
                }
            }
            axisGeometry.Freeze();
            return new Path
            {
                Stroke = Brushes.Black,
                StrokeThickness = _lineThickness,
                StrokeMiterLimit = 1,
                StrokeStartLineCap = PenLineCap.Flat,
                StrokeEndLineCap = PenLineCap.Flat,
                SnapsToDevicePixels = true,
                Data = axisGeometry
            };
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
        double _lineThickness = 1;

        readonly AxisTicksInternal _ticks = new AxisTicksInternal();
        Path _axis;

        AxisLabeler _axisLabeler;
        readonly AxisLabelerOptions _axisOptions;
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