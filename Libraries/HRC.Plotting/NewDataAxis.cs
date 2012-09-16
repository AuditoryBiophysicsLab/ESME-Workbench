using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Media;
using HRC.Plotting.AxisLabeling.Layout;
using HRC.Plotting.AxisLabeling.Layout.AxisLabelers;
using HRC.Plotting.Transforms;
using HRC.ViewModels;

namespace HRC.Plotting
{
    public class NewDataAxis : Panel
    {
        static NewDataAxis() { DefaultStyleKeyProperty.OverrideMetadata(typeof(NewDataAxis), new FrameworkPropertyMetadata(typeof(NewDataAxis))); }

        #region Dependency properties
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

        #region dependency property AxisLayoutAlgorithm AxisLayoutAlgorithm

        public static DependencyProperty AxisLayoutAlgorithmProperty = DependencyProperty.Register("AxisLayoutAlgorithm",
                                                                                                   typeof(AxisLayoutAlgorithm),
                                                                                                   typeof(NewDataAxis),
                                                                                                   new FrameworkPropertyMetadata(AxisLayoutAlgorithm.ExtendedWilkinson,
                                                                                                                                 FrameworkPropertyMetadataOptions.AffectsMeasure |
                                                                                                                                 FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                                 FrameworkPropertyMetadataOptions.AffectsRender,
                                                                                                                                 AxisLayoutAlgorithmPropertyChanged));

        public AxisLayoutAlgorithm AxisLayoutAlgorithm { get { return (AxisLayoutAlgorithm)GetValue(AxisLayoutAlgorithmProperty); } set { SetValue(AxisLayoutAlgorithmProperty, value); } }
        static void AxisLayoutAlgorithmPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).AxisLayoutAlgorithmPropertyChanged(); }
        void AxisLayoutAlgorithmPropertyChanged()
        {             
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
        void AxisLocationPropertyChanged()
        {
            switch (AxisLocation)
            {
                case AxisLocation.Top:
                    break;
                case AxisLocation.Bottom:
                    break;
                case AxisLocation.Left:
                    _axisLabel.LayoutTransform = new RotateTransform(-90);
                    break;
                case AxisLocation.Right:
                    _axisLabel.LayoutTransform = new RotateTransform(-90);
                    break;
                default:
                    throw new NotImplementedException(string.Format("AxisLocation of {0} is not implemented", AxisLocation));
            }
            _axisOptions.AxisLocation = AxisLocation;
        }
        #endregion

        #region dependency property ObservableCollection<AxisTick> AxisTicks

        public static DependencyProperty AxisTicksProperty = DependencyProperty.Register("AxisTicks",
                                                                                          typeof(ObservableCollection<NewDataAxisTick>),
                                                                                          typeof(NewDataAxis),
                                                                                          new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.None, AxisTicksPropertyChanged));

        public ObservableCollection<NewDataAxisTick> AxisTicks { get { return (ObservableCollection<NewDataAxisTick>)GetValue(AxisTicksProperty); } set { SetValue(AxisTicksProperty, value); } }
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

        static void AxisTypePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            ((NewDataAxis)obj).UpdateVisibleRange();
        }
        bool IsLogarithmic { get { return AxisType == AxisType.Logarithmic; } }
        #endregion

        #region dependency property RangeBase DataRange

        public static DependencyProperty DataRangeProperty = DependencyProperty.Register("DataRange",
                                                                                         typeof(RangeBase),
                                                                                         typeof(NewDataAxis),
                                                                                         new FrameworkPropertyMetadata(null,
                                                                                                                       FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                       FrameworkPropertyMetadataOptions.AffectsRender,
                                                                                                                       DataRangePropertyChanged));

        public RangeBase DataRange { get { return (RangeBase)GetValue(DataRangeProperty); } set { SetValue(DataRangeProperty, value); } }
        static void DataRangePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).DataRangePropertyChanged(args); }
        void DataRangePropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if (args.OldValue != null) ((RangeBase)args.OldValue).RangeChanged -= DataRangeChanged;
            if (args.NewValue != null) ((RangeBase)args.NewValue).RangeChanged += DataRangeChanged;
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
            if (AxisType == AxisType.Logarithmic && DataRange.Min <= 0) throw new InvalidOperationException("Cannot plot negative or zero values on a log scale");
            if (VisibleRange == null) VisibleRange = DataRange.Expand(0);
            else VisibleRange.Update(DataRange);
        }

        #endregion

        #region dependency property bool IsInverted

        public static DependencyProperty IsInvertedProperty = DependencyProperty.Register("IsInverted",
                                                                                          typeof(bool),
                                                                                          typeof(NewDataAxis),
                                                                                          new FrameworkPropertyMetadata(false,
                                                                                                                        FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                        FrameworkPropertyMetadataOptions.AffectsMeasure |
                                                                                                                        FrameworkPropertyMetadataOptions.AffectsRender,
                                                                                                                        IsInvertedPropertyChanged));

        public bool IsInverted { get { return (bool)GetValue(IsInvertedProperty); } set { SetValue(IsInvertedProperty, value); } }

        static void IsInvertedPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).IsInvertedPropertyChanged(); }
        void IsInvertedPropertyChanged() { OnTransformChanged(); }
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

        #region dependency property double MajorTicksPerInch

        public static DependencyProperty MajorTicksPerInchProperty = DependencyProperty.Register("MajorTicksPerInch",
                                                                                 typeof(double),
                                                                                 typeof(NewDataAxis),
                                                                                 new FrameworkPropertyMetadata(1.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, MajorTicksPerInchPropertyChanged));

        public double MajorTicksPerInch { get { return (double)GetValue(MajorTicksPerInchProperty); } set { SetValue(MajorTicksPerInchProperty, value); } }

        static void MajorTicksPerInchPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).MajorTicksPerInchPropertyChanged(); }
        void MajorTicksPerInchPropertyChanged() { if (MajorTicksPerInch <= 0) throw new ArgumentException("MajorTicksPerInch must be greater than zero"); }
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

        #region dependency property double MinorTicksPerInch

        public static DependencyProperty MinorTicksPerInchProperty = DependencyProperty.Register("MinorTicksPerInch",
                                                                                 typeof(double),
                                                                                 typeof(NewDataAxis),
                                                                                 new FrameworkPropertyMetadata(4.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, MinorTicksPerInchPropertyChanged));

        public double MinorTicksPerInch { get { return (double)GetValue(MinorTicksPerInchProperty); } set { SetValue(MinorTicksPerInchProperty, value); } }

        static void MinorTicksPerInchPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).MinorTicksPerInchPropertyChanged(); }
        void MinorTicksPerInchPropertyChanged() { }
        #endregion

        #region dependency property Func<double, double> PositionToValue

        public static DependencyProperty PositionToValueProperty = DependencyProperty.Register("PositionToValue",
                                                                                               typeof(Func<double, double>),
                                                                                               typeof(NewDataAxis),
                                                                                               new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.None));

        public Func<double, double> PositionToValue { get { return (Func<double, double>)GetValue(PositionToValueProperty); } set { SetValue(PositionToValueProperty, value); } }
        #endregion

        #region dependency property Func<double, double> ValueToPosition

        public static DependencyProperty ValueToPositionProperty = DependencyProperty.Register("ValueToPosition",
                                                                                               typeof(Func<double, double>),
                                                                                               typeof(NewDataAxis),
                                                                                               new FrameworkPropertyMetadata(null,
                                                                                                                             FrameworkPropertyMetadataOptions.None));

        public Func<double, double> ValueToPosition { get { return (Func<double, double>)GetValue(ValueToPositionProperty); } set { SetValue(ValueToPositionProperty, value); } }
        #endregion

        #region dependency property Range VisibleRange

        public static DependencyProperty VisibleRangeProperty = DependencyProperty.Register("VisibleRange",
                                                                                            typeof(Range),
                                                                                            typeof(NewDataAxis),
                                                                                            new FrameworkPropertyMetadata(null,
                                                                                                                          FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                          FrameworkPropertyMetadataOptions.AffectsRender |
                                                                                                                          FrameworkPropertyMetadataOptions.AffectsMeasure |
                                                                                                                          FrameworkPropertyMetadataOptions.BindsTwoWayByDefault,
                                                                                                                          VisibleRangePropertyChanged));

        public Range VisibleRange { get { return (Range)GetValue(VisibleRangeProperty); } set { SetValue(VisibleRangeProperty, value); } }
        static void VisibleRangePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            ((NewDataAxis)obj).VisibleRangePropertyChanged(args);
        }
        void VisibleRangePropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if (args.OldValue != null) ((Range)args.OldValue).RangeChanged -= VisibleRangeChanged;
            if (args.NewValue != null) ((Range)args.NewValue).RangeChanged += VisibleRangeChanged;
            UpdateVisibleRange();
            var bindingExpression = GetBindingExpression(VisibleRangeProperty);
            if (bindingExpression != null) bindingExpression.UpdateSource();
        }
        void VisibleRangeChanged(object sender, EventArgs args)
        {
            UpdateVisibleRange();
            var bindingExpression = GetBindingExpression(VisibleRangeProperty);
            if (bindingExpression != null) bindingExpression.UpdateSource();
            InvalidateMeasure();
        }

        void OnSizeChanged()
        {
            if (ActualWidth <= 0 || ActualHeight <= 0)
            {
                ValueToPosition = null;
                PositionToValue = null;
                return;
            }
            OnTransformChanged();
        }

        void UpdateVisibleRange()
        {
            if (VisibleRange == null || VisibleRange.IsEmpty)
            {
                ValueToPosition = null;
                PositionToValue = null;
                return;
            }
            Debug.WriteLine(string.Format("{0} Visible range changed to {1}", AxisLabel, VisibleRange));
            _visibleRange = VisibleRange.Expand(0);
            if (AxisType == AxisType.Logarithmic)
            {
                _visibleRange.Min = Math.Log10(_visibleRange.Min);
                _visibleRange.Max = Math.Log10(_visibleRange.Max);
            }
            _axisOptions.DataRange = _visibleRange.Expand(0);
            _axisOptions.VisibleRange = _visibleRange.Expand(0);
            OnTransformChanged();
        }

        void OnTransformChanged()
        {
            if (_visibleRange == null || _visibleRange.IsEmpty) UpdateVisibleRange();
            if (_visibleRange == null || _visibleRange.IsEmpty) return;
            _valueToPositionTransform = CreateAxisTransform(_visibleRange, new Size(ActualWidth, ActualHeight), false, IsInverted, IsLogarithmic);
            _positionToValueTransform = _valueToPositionTransform.Inverse;
            ValueToPosition = v => _valueToPositionTransform.Transform(new Point(v, 0)).X;
            PositionToValue = p => _positionToValueTransform.Transform(new Point(p, 0)).X;
        }

        Range _visibleRange;

        #endregion
        #endregion

        public NewDataAxis()
        {
            SnapsToDevicePixels = true;
            UseLayoutRounding = true;
            AxisTicks = new ObservableCollection<NewDataAxisTick>();
            SizeChanged += (s, e) => OnSizeChanged();
            _axisOptions = new AxisLabelerOptions
            {
                ComputeLabelRect = ComputeLabelRect,
                FontSize = TextBlock.GetFontSize(this),
                Typeface = new Typeface(TextBlock.GetFontFamily(this), TextBlock.GetFontStyle(this), TextBlock.GetFontWeight(this), TextBlock.GetFontStretch(this))
            };
            _axisLabeler = new ExtendedAxisLabeler();
            var presentationSource = PresentationSource.FromVisual(this);
            if (presentationSource == null || presentationSource.CompositionTarget == null) return;
            var matrix = presentationSource.CompositionTarget.TransformToDevice;
            _pixelsPerInch = Math.Max(matrix.M11, matrix.M22);
        }

#if false
        protected override void  OnMouseWheel(MouseWheelEventArgs e)
        {
            //Debug.WriteLine(string.Format("{0} OnMouseWheel: {1}", AxisLabel, e.Delta));
            e.Handled = true;
            var direction = Math.Sign(e.Delta);
            if (IsLogarithmic) VisibleRange.Update(Math.Pow(10, _visibleRange.Min + (0.1 * direction)), Math.Pow(10, _visibleRange.Max - (0.1 * direction)));
        }
#endif

        protected override HitTestResult HitTestCore(PointHitTestParameters hitTestParameters) { return new PointHitTestResult(this, hitTestParameters.HitPoint); }

        readonly double _pixelsPerInch = 96.0;
        GeneralTransform _valueToPositionTransform, _positionToValueTransform;

        GeneralTransform CreateAxisTransform(Range visbleRange, Size newSize, bool includeSwapTransform, bool isInverted, bool isLogarithmic)
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
                    axisDirectionTranslation = isInverted ? newSize.Width - (StrokeWeight / 2): StrokeWeight / 2;
                    tickDirectionTranslation = newSize.Height - (StrokeWeight / 2);
                    axisLength = newSize.Width;
                    break;
                case AxisLocation.Bottom:
                    tickDirectionScale = 1.0;
                    originScale = 1.0;
                    axisDirectionTranslation = isInverted ? newSize.Width - (StrokeWeight / 2): StrokeWeight / 2;
                    tickDirectionTranslation = StrokeWeight / 2;
                    axisLength = newSize.Width;
                    break;
                case AxisLocation.Left:
                    tickDirectionScale = -1.0;
                    originScale = -1.0;
                    axisDirectionTranslation = isInverted ? StrokeWeight / 2 : newSize.Height - (StrokeWeight / 2);
                    tickDirectionTranslation = newSize.Width - (StrokeWeight / 2);
                    axisLength = newSize.Height;
                    break;
                case AxisLocation.Right:
                    tickDirectionScale = 1.0;
                    originScale = -1.0;
                    axisDirectionTranslation = isInverted ? StrokeWeight / 2 : newSize.Height - (StrokeWeight / 2);
                    tickDirectionTranslation = StrokeWeight / 2;
                    axisLength = newSize.Height;
                    break;
                default:
                    throw new ApplicationException("NewDataAxis: Unknown AxisLocation value.");
            }
            if (isInverted) originScale *= -1;
            // The intent of this transform is to make every axis draw the same as a Bottom axis (i.e. the StartValue is at 
            // transformed-X of 0, and the axis line is drawn from top left to top right, axis ticks from top to tickLength)
            var result = new GeneralTransformGroup();
            // If this is a logarithmic axis, take the log of the data point.
            // If the data point is zero or negative, an exception will be thrown
            if (isLogarithmic) result.Children.Add(new LogTransform(10));
            // Reduce the data point by the minimum visible value, which normalizes the data with respect to the axis origin
            result.Children.Add(new TranslateTransform(-visbleRange.Min, 0));
            // Scale the data by an amount that will place the maximum visible value at the end of the axis
            // Takes axis direction (and partially axis inversion) into account with originScale, which is set according to those parameters
            result.Children.Add(new ScaleTransform(originScale * ((axisLength - StrokeWeight) / visbleRange.Size), tickDirectionScale));
            // Shift the result so that the data point is now located at the point along the axis that corresponds to the original data value
            result.Children.Add(new TranslateTransform(axisDirectionTranslation, tickDirectionTranslation));
            // if the includeSwapTransform parameter is true, then swap the X and Y values produced if this is a Left or Right axis
            if (includeSwapTransform && AxisLocation == AxisLocation.Left || AxisLocation == AxisLocation.Right) result.Children.Add(new SwapTransform());
            return result;
        }

        const double StrokeWeight = 1.0;
        Rect ComputeLabelRect(string label, double position, Axis axis, AxisLabelerOptions options)
        {
            var axisPosition = options.AxisTransform.Transform(new Point(position, MajorTickLength));
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

        double _tickLabelDimension;
        double _axisLabelDimension;
        Axis _axis;
        protected override Size MeasureOverride(Size availableSize) { return AxisType == AxisType.Enumerated ? MeasureEnumerated(availableSize) : MeasureNonEnumerated(availableSize); }

        Size MeasureEnumerated(Size availableSize)
        {
            if (AxisTicks == null) return AxisLocation == AxisLocation.Top || AxisLocation == AxisLocation.Bottom ? new Size(availableSize.Width, 22) : new Size(availableSize.Height, 22);
            if (AxisTicks.Count == 0) return AxisLocation == AxisLocation.Top || AxisLocation == AxisLocation.Bottom ? new Size(availableSize.Width, 22) : new Size(availableSize.Height, 22);
            Children.Clear();
            foreach (var tick in AxisTicks.Where(tick => tick.TextBlock != null))
            {
                Children.Add(tick.TextBlock);
                tick.TextBlock.Measure(availableSize);
            }
            var tickLength = AxisTicks.Select(tick => tick.IsMajorTick ? MajorTickLength : MinorTickLength).Max();
            var maxLabelWidth = AxisTicks.Select(tick => tick.TextBlock != null ? tick.TextBlock.DesiredSize.Width : 0).Max();
            var maxLabelHeight = AxisTicks.Select(tick => tick.TextBlock != null ? tick.TextBlock.DesiredSize.Height : 0).Max();
            var desiredSize = new Size(availableSize.Width, availableSize.Height);
            switch (AxisLocation)
            {
                case AxisLocation.Top:
                case AxisLocation.Bottom:
                    _tickLabelDimension = maxLabelHeight;
                    _axisLabelDimension = _axisLabel.DesiredSize.Height;
                    desiredSize.Height = tickLength + _tickLabelDimension + _axisLabelDimension;
                    break;
                case AxisLocation.Left:
                case AxisLocation.Right:
                    _tickLabelDimension = maxLabelWidth;
                    _axisLabelDimension = _axisLabel.DesiredSize.Width;
                    desiredSize.Width = MajorTickLength + 2 + _tickLabelDimension + _axisLabelDimension;
                    break;
                default:
                    throw new ApplicationException("NewDataAxis: Unknown AxisLocation value.");
            }
            //Debug.WriteLine(string.Format("NewDataAxis: MeasureEnumerated for {0} returning desired width {1} and height {2}", AxisLabel, desiredSize.Width, desiredSize.Height));
            return desiredSize;
        }

        Size MeasureNonEnumerated(Size availableSize)
        {
            if (_visibleRange == null || _visibleRange.IsEmpty) return AxisLocation == AxisLocation.Top || AxisLocation == AxisLocation.Bottom ? new Size(availableSize.Width, 22) : new Size(availableSize.Height, 22);
            if (Double.IsNaN(availableSize.Width) || Double.IsInfinity(availableSize.Width)) availableSize.Width = SystemParameters.VirtualScreenWidth;
            if (Double.IsNaN(availableSize.Height) || Double.IsInfinity(availableSize.Height)) availableSize.Height = SystemParameters.VirtualScreenHeight;
            // We need to fake out the layout code re: inversion and logarithmic mode so we NEVER set these two parameters to true
            _axisOptions.AxisTransform = CreateAxisTransform(_visibleRange, availableSize, true, false, false); 
            _axisOptions.Screen = new Rect(availableSize);
            _axis = _axisLabeler.Generate(_axisOptions, MajorTicksPerInch / _pixelsPerInch);
            if (_axis == null) return AxisLocation == AxisLocation.Top || AxisLocation == AxisLocation.Bottom ? new Size(availableSize.Width, 22) : new Size(availableSize.Height, 22);
            var majorTickLabels = _axis.Labels;
            if (!_visibleRange.Contains(_axis.VisibleRange)) _visibleRange = _axis.VisibleRange;
            Children.Clear();
            AxisTicks.Clear();
            foreach (var label in majorTickLabels)
            {
                // For logarithmic axes make sure we are only using integral label values
                if (IsLogarithmic && (Math.Floor(label.Value) != label.Value)) continue;
                var labelValue = IsLogarithmic ? Math.Pow(10, label.Value) : label.Value;
                var majorTick = new NewDataAxisTick(labelValue, label.Label, true, IsLogarithmic);
                AxisTicks.Add(majorTick);
                Children.Add(majorTick.TextBlock);
                majorTick.TextBlock.Measure(availableSize);
            }
            // If the minor tick frequency isn't at least twice the major tick frequency, don't do any minor ticks
            var minorTicksPerMajorTick = IsLogarithmic ? 10 : (int)(MinorTicksPerInch / MajorTicksPerInch);
            if (minorTicksPerMajorTick > 1)
            {
                if (IsLogarithmic)
                {
                    // Get the major tick values in descending order
                    var majorTickLogValues = AxisTicks.Select(t => Math.Log10(t.Value)).Reverse().ToList();
                    // Add minor ticks at whole-number log values if any are needed
                    for (var i = 0; i < majorTickLogValues.Count - 1; i++) 
                        for (var j = majorTickLogValues[i] - 1; j > majorTickLogValues[i + 1]; j--) 
                            AxisTicks.Add(new NewDataAxisTick(Math.Pow(10, j), null, false, IsLogarithmic));
                    // Add minor ticks at the usual places for a log scale (2, 3, 4, 5, 6, 7, 8, 9)
                    for (var baseValue = Math.Floor(_visibleRange.Min); baseValue < Math.Ceiling(_visibleRange.Max); baseValue++)
                    {
                        foreach (var logOffset in LogMinorTicks)
                        {
                            var logValue = baseValue + logOffset;
                            if (_visibleRange.Min <= logValue && logValue <= _visibleRange.Max) 
                                AxisTicks.Add(new NewDataAxisTick(Math.Pow(10, logValue), null, false, IsLogarithmic));
                        }
                    }
#if false
                    // Get the major tick values in descending order
                    var majorTickLogValues = AxisTicks.Select(t => Math.Log10(t.Value)).Reverse().ToList();
                    var virtualMajorTicks = new List<double>();
                    for (var i = 0; i < majorTickLogValues.Count - 1; i++)
                    {
                        for (var j = majorTickLogValues[i] - 1; j > majorTickLogValues[i + 1]; j--)
                        {
                            virtualMajorTicks.Add(j);
                            var minorTickValue = Math.Pow(10, j);
                            var minorTick = new NewDataAxisTick(minorTickValue, null, false, IsLogarithmic);
                            AxisTicks.Add(minorTick);
                        }
                    }
                    majorTickLogValues.AddRange(virtualMajorTicks);
                    // Add a phantom major tick at the beginning that's one greater than the actual last major tick
                    majorTickLogValues.Insert(0, majorTickLogValues[0] + 1);
                    var fullRange = new Range(Math.Pow(10, _visibleRange.Min), Math.Pow(10, _visibleRange.Max));
                    foreach (var majorTickLogValue in majorTickLogValues)
                    {
                        var majorTickValue = Math.Pow(10, majorTickLogValue);
                        for (var step = 0.9; step >= 0.2; step -= 0.1)
                        {
                            var minorTickValue = majorTickValue * step;
                            if (minorTickValue < fullRange.Min || minorTickValue > fullRange.Max) continue;
                            var minorTick = new NewDataAxisTick(minorTickValue, null, false, IsLogarithmic);
                            AxisTicks.Add(minorTick);
                        }
                    }
#endif
                }
                else
                {
                    var majorTickValueSpacing = majorTickLabels[1].Value - majorTickLabels[0].Value;
                    var stepSize = majorTickValueSpacing / minorTicksPerMajorTick;
                    var majorTickValues = AxisTicks.Select(t => t.Value).ToList();
                    // Add a phantom major tick to the beginning and end of the list so we can bracket
                    // the major ticks at each end of the range with more minor ticks if they fit within the
                    // visible range of the axis
                    majorTickValues.Insert(0, AxisTicks[0].Value - majorTickValueSpacing);
                    majorTickValues.Add(AxisTicks.Last().Value + majorTickValueSpacing);
                    foreach (var t in majorTickValues)
                        for (var j = 1; j < minorTicksPerMajorTick; j++)
                        {
                            var tickValue = t + (stepSize * j);
                            if (tickValue < _visibleRange.Min || tickValue > _visibleRange.Max) continue;
                            var minorTick = new NewDataAxisTick(tickValue, null, false, IsLogarithmic);
                            AxisTicks.Add(minorTick);
                        }
                }
            }
            _tickLabelMaxWidth = AxisTicks.Where(t => t.TextBlock != null).Max(t => t.TextBlock.DesiredSize.Width);
            _tickLabelMaxHeight = AxisTicks.Where(t => t.TextBlock != null).Max(t => t.TextBlock.DesiredSize.Height);
            var axisLabel = string.IsNullOrEmpty(_axis.AxisTitleExtension) ? AxisLabel : string.Format("{0} ({1})", AxisLabel, _axis.AxisTitleExtension);
            _axisLabel.Text = axisLabel;
            _axisLabel.FontSize = _axis.FontSize + 2;
            Children.Add(_axisLabel);
            _axisLabel.Measure(availableSize);
            var desiredSize = new Size(availableSize.Width, availableSize.Height);
            switch (AxisLocation)
            {
                case AxisLocation.Top:
                case AxisLocation.Bottom:
                    _tickLabelDimension = _tickLabelMaxHeight;
                    _axisLabelDimension = _axisLabel.DesiredSize.Height;
                    desiredSize.Height = MajorTickLength + _tickLabelDimension + _axisLabelDimension;
                    break;
                case AxisLocation.Left:
                case AxisLocation.Right:
                    _tickLabelDimension = _tickLabelMaxWidth;
                    _axisLabelDimension = _axisLabel.DesiredSize.Width;
                    desiredSize.Width = MajorTickLength + 2 + _tickLabelDimension + _axisLabelDimension;
                    break;
                default:
                    throw new ApplicationException("NewDataAxis: Unknown AxisLocation value.");
            }
            // desiredSize = ... computed sum of children's DesiredSize ...;
            // IMPORTANT: do not allow PositiveInfinity to be returned, that will raise an exception in the caller!
            // PositiveInfinity might be an availableSize input; this means that the parent does not care about sizing
            //Debug.WriteLine(string.Format("NewDataAxis: MeasureNonEnumerated for {0} returning desired width {1} and height {2}", AxisLabel, desiredSize.Width, desiredSize.Height));
            return desiredSize;
        }

        double _tickLabelMaxWidth, _tickLabelMaxHeight;

        protected override Size ArrangeOverride(Size arrangeSize)
        {
            if (AxisType == AxisType.Enumerated)
                return ArrangeNonEnumerated(arrangeSize);
            return ArrangeNonEnumerated(arrangeSize);
        }

        Size ArrangeNonEnumerated(Size arrangeSize)
        {
            if (_visibleRange == null) return AxisLocation == AxisLocation.Top || AxisLocation == AxisLocation.Bottom ? new Size(arrangeSize.Width, 22) : new Size(arrangeSize.Height, 22);
            var axisTransform = CreateAxisTransform(_visibleRange, arrangeSize, true, IsInverted, false);
            Point axisLabelPosition;
            var midpoint = _visibleRange.Min + (_visibleRange.Size / 2);
            switch (AxisLocation)
            {
                case AxisLocation.Top:
                    axisLabelPosition = axisTransform.Transform(new Point(midpoint, MajorTickLength + _tickLabelMaxHeight + _axisLabel.DesiredSize.Height));
                    axisLabelPosition.X -= _axisLabel.DesiredSize.Width / 2;
                    break;
                case AxisLocation.Bottom:
                    axisLabelPosition = axisTransform.Transform(new Point(midpoint, MajorTickLength + _tickLabelMaxHeight));
                    axisLabelPosition.X -= _axisLabel.DesiredSize.Width / 2;
                    break;
                case AxisLocation.Left:
                    axisLabelPosition = axisTransform.Transform(new Point(midpoint, MajorTickLength + 2 + _tickLabelMaxWidth + _axisLabel.DesiredSize.Width));
                    axisLabelPosition.Y -= _axisLabel.DesiredSize.Height / 2;
                    break;
                case AxisLocation.Right:
                    axisLabelPosition = axisTransform.Transform(new Point(midpoint, MajorTickLength + _tickLabelMaxWidth));
                    axisLabelPosition.Y -= _axisLabel.DesiredSize.Height / 2;
                    break;
                default:
                    throw new ApplicationException("NewDataAxis: Unknown AxisLocation value.");
            }
            _axisLabel.Arrange(new Rect(axisLabelPosition, _axisLabel.DesiredSize));
            axisTransform = CreateAxisTransform(_visibleRange, arrangeSize, true, IsInverted, IsLogarithmic);

            foreach (var tick in AxisTicks) 
            {
                Point tickLabelPosition;
                switch (AxisLocation)
                {
                    case AxisLocation.Top:
                    case AxisLocation.Bottom:
                        tick.Location = axisTransform.Transform(new Point(tick.Value, 0)).X;
                        if (tick.TextBlock != null)
                        {
                            var yPos = MajorTickLength;
                            if (AxisLocation == AxisLocation.Top) yPos += tick.TextBlock.DesiredSize.Height;
                            tickLabelPosition = axisTransform.Transform(new Point(tick.Value, yPos));
                            tickLabelPosition.X -= tick.TextBlock.DesiredSize.Width / 2;
                            tickLabelPosition.X = Math.Min(Math.Max(tickLabelPosition.X, 0), arrangeSize.Width - tick.TextBlock.DesiredSize.Width);
                            tick.TextBlock.Arrange(new Rect(tickLabelPosition, tick.TextBlock.DesiredSize));
                        }
                        break;
                    case AxisLocation.Left:
                    case AxisLocation.Right:
                        tick.Location = axisTransform.Transform(new Point(tick.Value, 0)).Y;
                        if (tick.TextBlock != null)
                        {
                            tickLabelPosition = axisTransform.Transform(new Point(tick.Value, MajorTickLength + 2));
                            if (AxisLocation == AxisLocation.Left) tickLabelPosition.X -= tick.TextBlock.DesiredSize.Width;
                            tickLabelPosition.Y -= tick.TextBlock.DesiredSize.Height / 2;
                            tickLabelPosition.Y = Math.Min(Math.Max(tickLabelPosition.Y, 0), arrangeSize.Height - tick.TextBlock.DesiredSize.Height);
                            tick.TextBlock.Arrange(new Rect(tickLabelPosition, tick.TextBlock.DesiredSize));
                        }
                        break;
                    default:
                        throw new ApplicationException("NewDataAxis: Unknown AxisLocation value.");
                }
            }
            //Debug.WriteLine(string.Format("NewDataAxis: ArrangeOverride for {0} returning desired width {1} and height {2}", AxisLabel, arrangeSize.Width, arrangeSize.Height));
            return arrangeSize;
        }

        protected override void OnRender(DrawingContext dc)
        {
            base.OnRender(dc);
            if (_visibleRange == null) return;
            var size = new Size(ActualWidth, ActualHeight);
            var axisTransform = CreateAxisTransform(_visibleRange, size, true, IsInverted, false);
            var pen = new Pen(Brushes.Black, 1) { StartLineCap = PenLineCap.Square, EndLineCap = PenLineCap.Square };
            dc.DrawLine(pen, axisTransform.Transform(new Point(_visibleRange.Min, 0)), axisTransform.Transform(new Point(_visibleRange.Max, 0)));
            axisTransform = CreateAxisTransform(_visibleRange, size, true, IsInverted, IsLogarithmic);
            foreach (var tick in AxisTicks) dc.DrawLine(pen, axisTransform.Transform(new Point(tick.Value, 0)), axisTransform.Transform(new Point(tick.Value, tick.IsMajorTick ? MajorTickLength : MinorTickLength)));
            // This draws a 50-pixel line in the center of the axis, used to check the centering of the axis label
            //dc.DrawLine(pen, axisTransform.Transform(new Point(_visibleRange.Min + (_visibleRange.Size / 2), 0)), axisTransform.Transform(new Point(_visibleRange.Min + (_visibleRange.Size / 2), 50)));
        }
        #endregion

        #region Private data members
        readonly TextBlock _axisLabel = new TextBlock();

        AxisLabeler _axisLabeler;
        readonly AxisLabelerOptions _axisOptions;
        static readonly double[] LogMinorTicks = { Math.Log10(2.0), Math.Log10(3.0), Math.Log10(4.0), Math.Log10(5.0), Math.Log10(6.0), Math.Log10(7.0), Math.Log10(8.0), Math.Log10(9.0) };
        #endregion
    }

    public class NewDataAxisTick
    {
        internal NewDataAxisTick(double value, string text, bool isMajorTick, bool isMagnitude)
        {
            Value = value;
            Text = text;
            IsMajorTick = isMajorTick;
            if (!isMajorTick) return;
            if (isMagnitude)
            {
                TextBlock = new TextBlock { Text = "10" };
                var superscript = new TextBlock { Text = Math.Round(Math.Log10(value), 1).ToString(CultureInfo.InvariantCulture), FontSize = 10 };
                var inline = new InlineUIContainer(superscript) { BaselineAlignment = BaselineAlignment.Superscript };
                TextBlock.Inlines.Add(inline);
            }
            else TextBlock = new TextBlock { Text = text };
        }

        public NewDataAxisTick(double value, string text, bool isMajorTick)
        {
            Value = value;
            Text = text;
            IsMajorTick = isMajorTick;
            if (text != null) TextBlock = new TextBlock { Text = text };
        }

        internal TextBlock TextBlock { get; private set; }
        public string Text { get; private set; }
        public double Value { get; private set; }
        public bool IsMajorTick { get; private set; }
        public double Location { get; internal set; }
        ~NewDataAxisTick() { TextBlock = null; }
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