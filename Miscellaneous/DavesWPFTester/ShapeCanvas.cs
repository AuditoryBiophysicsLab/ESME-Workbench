using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;
using ESME.Views.Controls;
using HRC.Utility;

namespace DavesWPFTester
{
    public class ShapeCanvas : Canvas
    {
        static ShapeCanvas() { DefaultStyleKeyProperty.OverrideMetadata(typeof(ShapeCanvas), new FrameworkPropertyMetadata(typeof(ShapeCanvas))); }

        #region dependency property ObservableCollection<Shape> Shapes

        public static DependencyProperty ShapesProperty = DependencyProperty.Register("Shapes",
                                                                                 typeof(ObservableCollection<Shape>),
                                                                                 typeof(ShapeCanvas),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender, ShapesPropertyChanged));

        public ObservableCollection<Shape> Shapes { get { return (ObservableCollection<Shape>)GetValue(ShapesProperty); } set { SetValue(ShapesProperty, value); } }

        static void ShapesPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ShapeCanvas)obj).ShapesPropertyChanged(args); }
        void ShapesPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if (args.OldValue != null) ((ObservableCollection<Shape>)args.OldValue).CollectionChanged -= ShapesCollectionChanged;
            if (args.NewValue != null) ((ObservableCollection<Shape>)args.NewValue).CollectionChanged += ShapesCollectionChanged;
        }

        void ShapesCollectionChanged(object sender, NotifyCollectionChangedEventArgs args) { Redraw(); }
        #endregion
        #region dependency property ObservableList<AxisTick> XAxisMajorTicks
        public static DependencyProperty XAxisMajorTicksProperty = DependencyProperty.Register("XAxisMajorTicks",
                                                                                          typeof(ObservableList<AxisTick>),
                                                                                          typeof(ShapeCanvas),
                                                                                          new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender, XAxisMajorTicksPropertyChanged));

        static void XAxisMajorTicksPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ShapeCanvas)obj).XAxisMajorTicksPropertyChanged(); }
        void XAxisMajorTicksPropertyChanged() { Redraw(); }

        public ObservableList<AxisTick> XAxisMajorTicks { get { return (ObservableList<AxisTick>)GetValue(XAxisMajorTicksProperty); } set { SetCurrentValue(XAxisMajorTicksProperty, value); } }
        #endregion
        #region dependency property ObservableList<AxisTick> XAxisMinorTicks
        public static DependencyProperty XAxisMinorTicksProperty = DependencyProperty.Register("XAxisMinorTicks",
                                                                                          typeof(ObservableList<AxisTick>),
                                                                                          typeof(ShapeCanvas),
                                                                                          new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender, XAxisMinorTicksPropertyChanged));

        static void XAxisMinorTicksPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ShapeCanvas)obj).XAxisMinorTicksPropertyChanged(); }
        void XAxisMinorTicksPropertyChanged() { Redraw(); }

        public ObservableList<AxisTick> XAxisMinorTicks { get { return (ObservableList<AxisTick>)GetValue(XAxisMinorTicksProperty); } set { SetCurrentValue(XAxisMinorTicksProperty, value); } }
        #endregion
        #region dependency property ObservableList<AxisTick> YAxisMajorTicks
        public static DependencyProperty YAxisMajorTicksProperty = DependencyProperty.Register("YAxisMajorTicks",
                                                                                          typeof(ObservableList<AxisTick>),
                                                                                          typeof(ShapeCanvas),
                                                                                          new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender, YAxisMajorTicksPropertyChanged));

        static void YAxisMajorTicksPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ShapeCanvas)obj).YAxisMajorTicksPropertyChanged(); }
        void YAxisMajorTicksPropertyChanged() { Redraw(); }

        public ObservableList<AxisTick> YAxisMajorTicks { get { return (ObservableList<AxisTick>)GetValue(YAxisMajorTicksProperty); } set { SetCurrentValue(YAxisMajorTicksProperty, value); } }
        #endregion
        #region dependency property ObservableList<AxisTick> YAxisMinorTicks
        public static DependencyProperty YAxisMinorTicksProperty = DependencyProperty.Register("YAxisMinorTicks",
                                                                                          typeof(ObservableList<AxisTick>),
                                                                                          typeof(ShapeCanvas),
                                                                                          new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender, YAxisMinorTicksPropertyChanged));

        static void YAxisMinorTicksPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ShapeCanvas)obj).YAxisMinorTicksPropertyChanged(); }
        void YAxisMinorTicksPropertyChanged() { Redraw(); }

        public ObservableList<AxisTick> YAxisMinorTicks { get { return (ObservableList<AxisTick>)GetValue(YAxisMinorTicksProperty); } set { SetCurrentValue(YAxisMinorTicksProperty, value); } }
        #endregion

        #region dependency property Color MinorTickLineColor

        public static DependencyProperty MinorTickLineColorProperty = DependencyProperty.Register("MinorTickLineColor",
                                                                                 typeof(Color),
                                                                                 typeof(ShapeCanvas),
                                                                                 new FrameworkPropertyMetadata(Colors.LightGray, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, MinorTickLineColorPropertyChanged));

        public Color MinorTickLineColor { get { return (Color)GetValue(MinorTickLineColorProperty); } set { SetValue(MinorTickLineColorProperty, value); } }

        static void MinorTickLineColorPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ShapeCanvas)obj).MinorTickLineColorPropertyChanged(); }
        void MinorTickLineColorPropertyChanged()
        {
            _minorTickBrush = new SolidColorBrush(MinorTickLineColor);
            Redraw();
        }
        Brush _minorTickBrush = Brushes.LightGray;
        #endregion

        #region dependency property Color MajorTickLineColor

        public static DependencyProperty MajorTickLineColorProperty = DependencyProperty.Register("MajorTickLineColor",
                                                                                 typeof(Color),
                                                                                 typeof(ShapeCanvas),
                                                                                 new FrameworkPropertyMetadata(Colors.Black, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, MajorTickLineColorPropertyChanged));

        public Color MajorTickLineColor { get { return (Color)GetValue(MajorTickLineColorProperty); } set { SetValue(MajorTickLineColorProperty, value); } }

        static void MajorTickLineColorPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ShapeCanvas)obj).MajorTickLineColorPropertyChanged(); }
        void MajorTickLineColorPropertyChanged()
        {
            _majorTickBrush = new SolidColorBrush(MajorTickLineColor);
            Redraw();
        }
        Brush _majorTickBrush = Brushes.Black;
        #endregion

        #region dependency property ObservableCollection<ISeries> SeriesSource

        public static DependencyProperty SeriesSourceProperty = DependencyProperty.Register("SeriesSource",
                                                                                 typeof(ObservableCollection<ISeries>),
                                                                                 typeof(ShapeCanvas),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, SeriesSourcePropertyChanged));

        public ObservableCollection<ISeries> SeriesSource { get { return (ObservableCollection<ISeries>)GetValue(SeriesSourceProperty); } set { SetValue(SeriesSourceProperty, value); } }

        static void SeriesSourcePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ShapeCanvas)obj).SeriesSourcePropertyChanged(args); }
        void SeriesSourcePropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if (args.OldValue != null) {((ObservableCollection<ISeries>)args.OldValue).CollectionChanged -= SeriesSourceCollectionChanged;}
            if (args.NewValue != null) ((ObservableCollection<ISeries>)args.NewValue).CollectionChanged += SeriesSourceCollectionChanged;
            Redraw();
        }

        void SeriesSourceCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                case NotifyCollectionChangedAction.Replace:
                    foreach (ISeries newItem in args.NewItems) if (newItem.SeriesData is INotifyCollectionChanged) ((INotifyCollectionChanged)newItem.SeriesData).CollectionChanged += (s, e) => Redraw();
                    break;
            }
            Redraw();
        }
        #endregion

        public ShapeCanvas() { SizeChanged += (s, e) => Redraw(); }

        void Redraw()
        {
            Children.Clear();
            if (XAxisMinorTicks != null && XAxisMinorTicks.Count > 0) Children.Add(CreateAxisLines(XAxisMinorTicks, ActualHeight, true, _minorTickBrush, 1));
            if (YAxisMinorTicks != null && YAxisMinorTicks.Count > 0) Children.Add(CreateAxisLines(YAxisMinorTicks, ActualWidth, false, _minorTickBrush, 1));
            if (XAxisMajorTicks != null && XAxisMajorTicks.Count > 0) Children.Add(CreateAxisLines(XAxisMajorTicks.Skip(1), ActualHeight, true, _majorTickBrush, 1));
            if (YAxisMajorTicks != null && YAxisMajorTicks.Count > 0) Children.Add(CreateAxisLines(YAxisMajorTicks.Skip(1), ActualWidth, false, _majorTickBrush, 1));
            if (SeriesSource != null) foreach (var series in SeriesSource)
            {
                series.RenderShapes();
                if (series.Shapes != null) foreach (var shape in series.Shapes) Children.Add(shape);
            }
            if (Shapes != null) foreach (var shape in Shapes) Children.Add(shape);
            InvalidateVisual();
        }

        static Path CreateAxisLines(IEnumerable<AxisTick> ticks, double length, bool isVertical, Brush brush, double strokeThickness)
        {
            var geometry = new StreamGeometry();
            using (var geometryContext = geometry.Open())
                foreach (var coordinate in ticks)
                    CreateAxisLine(geometryContext, coordinate.Location, length, isVertical);
            geometry.Freeze();
            return new Path
            {
                Stroke = brush,
                StrokeThickness = strokeThickness,
                SnapsToDevicePixels = true,
                Data = geometry,
            };
        }

        static void CreateAxisLine(StreamGeometryContext geometryContext, double location, double length, bool isVertical)
        {
            if (isVertical)
            {
                geometryContext.BeginFigure(new Point(location, 0), false, false);
                geometryContext.LineTo(new Point(location, length), true, false);
            }
            else
            {
                geometryContext.BeginFigure(new Point(0, location), false, false);
                geometryContext.LineTo(new Point(length, location), true, false);
            }
        }

    }
}
