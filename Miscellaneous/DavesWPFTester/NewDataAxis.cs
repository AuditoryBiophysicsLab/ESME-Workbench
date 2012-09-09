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
            ((NewDataAxis)obj).DataRangeChanged(null, null);
            ((NewDataAxis)obj).OnSizeOrVisibleRangeChanged();
        }
        bool IsLogarithmic { get { return AxisType == AxisType.Logarithmic; } }
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
                                                                                          new FrameworkPropertyMetadata(false,
                                                                                                                        FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                        FrameworkPropertyMetadataOptions.AffectsMeasure |
                                                                                                                        FrameworkPropertyMetadataOptions.AffectsRender,
                                                                                                                        IsInvertedPropertyChanged));

        public bool IsInverted { get { return (bool)GetValue(IsInvertedProperty); } set { SetValue(IsInvertedProperty, value); } }

        static void IsInvertedPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).IsInvertedPropertyChanged(); }
        void IsInvertedPropertyChanged() { OnSizeOrVisibleRangeChanged(); }
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
                                                                                         new FrameworkPropertyMetadata(null,
                                                                                                                       FrameworkPropertyMetadataOptions.AffectsArrange |
                                                                                                                       FrameworkPropertyMetadataOptions.AffectsRender,
                                                                                                                       DataRangePropertyChanged));

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
            VisibleRange = DataRange.Size > 0 ? DataRange.Expand(DataRange.Size * 0.05) : new Range(DataRange.Min, DataRange.Max + 10);
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
            _mappingFunctionTransform = CreateAxisTransform(_visibleRange, new Size(ActualWidth, ActualHeight), false, IsInverted, IsLogarithmic);
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

        #region dependency property double MajorTicksPerInch

        public static DependencyProperty MajorTicksPerInchProperty = DependencyProperty.Register("MajorTicksPerInch",
                                                                                 typeof(double),
                                                                                 typeof(NewDataAxis),
                                                                                 new FrameworkPropertyMetadata(1.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, MajorTicksPerInchPropertyChanged));

        public double MajorTicksPerInch { get { return (double)GetValue(MajorTicksPerInchProperty); } set { SetValue(MajorTicksPerInchProperty, value); } }

        static void MajorTicksPerInchPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((NewDataAxis)obj).MajorTicksPerInchPropertyChanged(); }
        void MajorTicksPerInchPropertyChanged() { if (MajorTicksPerInch <= 0) throw new ParameterOutOfRangeException("MajorTicksPerInch must be greater than zero"); }
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
            var presentationSource = PresentationSource.FromVisual(this);
            if (presentationSource == null || presentationSource.CompositionTarget == null) return;
            var matrix = presentationSource.CompositionTarget.TransformToDevice;
            _pixelsPerInch = Math.Max(matrix.M11, matrix.M22);
        }

        readonly double _pixelsPerInch = 96.0;
        GeneralTransform _mappingFunctionTransform;

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
        protected override Size MeasureOverride(Size availableSize)
        {
            if (_visibleRange == null) return AxisLocation == AxisLocation.Top || AxisLocation == AxisLocation.Bottom ? new Size(availableSize.Width, 22) : new Size(availableSize.Height, 22);
            if (Double.IsNaN(availableSize.Width) || Double.IsInfinity(availableSize.Width)) availableSize.Width = SystemParameters.VirtualScreenWidth;
            if (Double.IsNaN(availableSize.Height) || Double.IsInfinity(availableSize.Height)) availableSize.Height = SystemParameters.VirtualScreenHeight;
            // We need to fake out the layout code re: inversion and logarithmic mode so we NEVER set these two parameters to true
            _axisOptions.AxisTransform = CreateAxisTransform(_visibleRange, availableSize, true, false, false); 
            _axisOptions.Screen = new Rect(availableSize);
            _axis = _axisLabeler.Generate(_axisOptions, MajorTicksPerInch / _pixelsPerInch);
            if (_axis == null) return AxisLocation == AxisLocation.Top || AxisLocation == AxisLocation.Bottom ? new Size(availableSize.Width, 22) : new Size(availableSize.Height, 22);
            var majorTickLabels = _axis.Labels;
            if (!_visibleRange.Contains(_axis.VisibleRange)) _visibleRange = _axis.VisibleRange;
            // For purposes of creating this transform, we pretend that the axis is NOT logarithmic because the
            // labels are already the base magnitudes for the data. Also, we don't include the swap transform because
            // all we're interested in is the position along the axis, not the actual X and Y locations of the children
            var axisTransform = CreateAxisTransform(_visibleRange, availableSize, false, IsInverted, IsLogarithmic);
            Children.Clear();
            AxisTicks.Clear();
            // Clear the tick cache
            _ticks.Clear();
            foreach (var label in majorTickLabels)
            {
                // For logarithmic axes make sure we are only using integral label values
                if (IsLogarithmic && (Math.Floor(label.Value) != label.Value)) continue;
                var labelValue = IsLogarithmic ? Math.Pow(10, label.Value) : label.Value;
                var tickStart = axisTransform.Transform(new Point(labelValue, 0));
                var majorTick = new AxisTickInternal(labelValue, label.Label, true, IsLogarithmic);
                _ticks.Add(majorTick);
                Children.Add(majorTick.Label);
                majorTick.Label.Measure(availableSize);
                AxisTicks.Add(new AxisTick { Location = tickStart.X, IsMajorTick = true, Value = labelValue });
            }
            // If the minor tick frequency isn't at least twice the major tick frequency, don't do any minor ticks
            var minorTicksPerMajorTick = IsLogarithmic ? 10 : (int)(MinorTicksPerInch / MajorTicksPerInch);
            if (minorTicksPerMajorTick > 1)
            {
                if (IsLogarithmic)
                {
                    // Get the major tick values in descending order
                    var majorTickLogValues = _ticks.Select(t => t.Value).Reverse().ToList();
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
                            var tickStart = axisTransform.Transform(new Point(minorTickValue, 0));
                            var minorTick = new AxisTickInternal(minorTickValue, null, false, IsLogarithmic);
                            _ticks.Add(minorTick);
                            AxisTicks.Add(new AxisTick { Location = tickStart.X, IsMajorTick = false, Value = minorTickValue });
                        }
                    }
                }
                else
                {
                    var majorTickValueSpacing = majorTickLabels[1].Value - majorTickLabels[0].Value;
                    var stepSize = majorTickValueSpacing / minorTicksPerMajorTick;
                    var majorTickValues = _ticks.Select(t => t.Value).ToList();
                    // Add a phantom major tick to the beginning and end of the list so we can bracket
                    // the major ticks at each end of the range with more minor ticks if they fit within the
                    // visible range of the axis
                    majorTickValues.Insert(0, _ticks[0].Value - majorTickValueSpacing);
                    majorTickValues.Add(_ticks.Last().Value + majorTickValueSpacing);
                    for (var i = 0; i < majorTickValues.Count; i++)
                        for (var j = 1; j < minorTicksPerMajorTick; j++)
                        {
                            var tickValue = majorTickValues[i] + (stepSize * j);
                            if (tickValue < _visibleRange.Min || tickValue > _visibleRange.Max) continue;
                            var tickStart = axisTransform.Transform(new Point(tickValue, 0));
                            var minorTick = new AxisTickInternal(tickValue, null, false, IsLogarithmic);
                            _ticks.Add(minorTick);
                            AxisTicks.Add(new AxisTick { Location = tickStart.X, IsMajorTick = false, Value = tickValue });
                        }
                }
            }
            _tickLabelMaxWidth = _ticks.Where(t => t.Label != null).Max(t => t.Label.DesiredSize.Width);
            _tickLabelMaxHeight = _ticks.Where(t => t.Label != null).Max(t => t.Label.DesiredSize.Height);
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
            Debug.WriteLine(string.Format("NewDataAxis: MeasureOverride for {0} returning desired width {1} and height {2}", AxisLabel, desiredSize.Width, desiredSize.Height));
            return desiredSize;
        }

        double _tickLabelMaxWidth, _tickLabelMaxHeight;

        protected override Size ArrangeOverride(Size arrangeSize)
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

            foreach (var tick in _ticks.Where(tick => tick.IsMajorTick)) 
            {
                Point tickLabelPosition;
                switch (AxisLocation)
                {
                    case AxisLocation.Top:
                    case AxisLocation.Bottom:
                        var yPos = MajorTickLength;
                        if (AxisLocation == AxisLocation.Top) yPos += tick.Label.DesiredSize.Height;
                        tickLabelPosition = axisTransform.Transform(new Point(tick.Value, yPos));
                        tickLabelPosition.X -= tick.Label.DesiredSize.Width / 2;
                        tickLabelPosition.X = Math.Min(Math.Max(tickLabelPosition.X, 0), arrangeSize.Width - tick.Label.DesiredSize.Width);
                        break;
                    case AxisLocation.Left:
                    case AxisLocation.Right:
                        tickLabelPosition = axisTransform.Transform(new Point(tick.Value, MajorTickLength + 2));
                        if (AxisLocation == AxisLocation.Left) tickLabelPosition.X -= tick.Label.DesiredSize.Width;
                        tickLabelPosition.Y -= tick.Label.DesiredSize.Height / 2;
                        tickLabelPosition.Y = Math.Min(Math.Max(tickLabelPosition.Y, 0), arrangeSize.Height - tick.Label.DesiredSize.Height);
                        break;
                    default:
                        throw new ApplicationException("NewDataAxis: Unknown AxisLocation value.");
                }
                tick.Label.Arrange(new Rect(tickLabelPosition, tick.Label.DesiredSize));
            }
            Debug.WriteLine(string.Format("NewDataAxis: ArrangeOverride for {0} returning desired width {1} and height {2}", AxisLabel, arrangeSize.Width, arrangeSize.Height));
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
            foreach (var tick in _ticks) 
                dc.DrawLine(pen, axisTransform.Transform(new Point(tick.Value, 0)), axisTransform.Transform(new Point(tick.Value, tick.IsMajorTick ? MajorTickLength : MinorTickLength)));
            // This draws a 50-pixel line in the center of the axis, used to check the centering of the axis label
            //dc.DrawLine(pen, axisTransform.Transform(new Point(_visibleRange.Min + (_visibleRange.Size / 2), 0)), axisTransform.Transform(new Point(_visibleRange.Min + (_visibleRange.Size / 2), 50)));
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

        readonly List<AxisTickInternal> _ticks = new List<AxisTickInternal>();

        AxisLabeler _axisLabeler;
        readonly AxisLabelerOptions _axisOptions;
        #endregion
        #region Axis utility classes
        class AxisTickInternal
        {
            public AxisTickInternal(double value, string text, bool isMajorTick, bool isMagnitude)
            {
                Value = value;
                IsMajorTick = isMajorTick;
                if (!isMajorTick) return;
                Label = new TextBlock();
                if (isMagnitude)
                {
                    Label.Text = "10";
                    var superscript = new TextBlock { Text = Math.Round(Math.Log10(value), 1).ToString(CultureInfo.InvariantCulture), FontSize = 10 };
                    var inline = new InlineUIContainer(superscript) { BaselineAlignment = BaselineAlignment.Superscript };
                    Label.Inlines.Add(inline);
                }
                else Label.Text = text;
            }

            public TextBlock Label { get; private set; }
            public double Value { get; private set; }
            public bool IsMajorTick { get; private set; }
            ~AxisTickInternal() { Label = null; }
        }
        #endregion
    }
}