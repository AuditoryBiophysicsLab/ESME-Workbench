using System;
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
    public class DataCanvas : Canvas
    {
        static DataCanvas() { DefaultStyleKeyProperty.OverrideMetadata(typeof(DataCanvas), new FrameworkPropertyMetadata(typeof(DataCanvas))); }

        #region dependency property ObservableCollection<Shape> Shapes

        public static DependencyProperty ShapesProperty = DependencyProperty.Register("Shapes",
                                                                                 typeof(ObservableCollection<Shape>),
                                                                                 typeof(DataCanvas),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender, ShapesPropertyChanged));

        public ObservableCollection<Shape> Shapes { get { return (ObservableCollection<Shape>)GetValue(ShapesProperty); } set { SetValue(ShapesProperty, value); } }

        static void ShapesPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataCanvas)obj).ShapesPropertyChanged(args); }
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
                                                                                          typeof(DataCanvas),
                                                                                          new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender, XAxisMajorTicksPropertyChanged));

        static void XAxisMajorTicksPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataCanvas)obj).XAxisMajorTicksPropertyChanged(); }
        void XAxisMajorTicksPropertyChanged() { DrawAxes(); }

        public ObservableList<AxisTick> XAxisMajorTicks { get { return (ObservableList<AxisTick>)GetValue(XAxisMajorTicksProperty); } set { SetCurrentValue(XAxisMajorTicksProperty, value); } }
        #endregion
        #region dependency property ObservableList<AxisTick> XAxisMinorTicks
        public static DependencyProperty XAxisMinorTicksProperty = DependencyProperty.Register("XAxisMinorTicks",
                                                                                          typeof(ObservableList<AxisTick>),
                                                                                          typeof(DataCanvas),
                                                                                          new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender, XAxisMinorTicksPropertyChanged));

        static void XAxisMinorTicksPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataCanvas)obj).XAxisMinorTicksPropertyChanged(); }
        void XAxisMinorTicksPropertyChanged() { DrawAxes(); }

        public ObservableList<AxisTick> XAxisMinorTicks { get { return (ObservableList<AxisTick>)GetValue(XAxisMinorTicksProperty); } set { SetCurrentValue(XAxisMinorTicksProperty, value); } }
        #endregion
        #region dependency property ObservableList<AxisTick> YAxisMajorTicks
        public static DependencyProperty YAxisMajorTicksProperty = DependencyProperty.Register("YAxisMajorTicks",
                                                                                          typeof(ObservableList<AxisTick>),
                                                                                          typeof(DataCanvas),
                                                                                          new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender, YAxisMajorTicksPropertyChanged));

        static void YAxisMajorTicksPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataCanvas)obj).YAxisMajorTicksPropertyChanged(); }
        void YAxisMajorTicksPropertyChanged() { DrawAxes(); }

        public ObservableList<AxisTick> YAxisMajorTicks { get { return (ObservableList<AxisTick>)GetValue(YAxisMajorTicksProperty); } set { SetCurrentValue(YAxisMajorTicksProperty, value); } }
        #endregion
        #region dependency property ObservableList<AxisTick> YAxisMinorTicks
        public static DependencyProperty YAxisMinorTicksProperty = DependencyProperty.Register("YAxisMinorTicks",
                                                                                          typeof(ObservableList<AxisTick>),
                                                                                          typeof(DataCanvas),
                                                                                          new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender, YAxisMinorTicksPropertyChanged));

        static void YAxisMinorTicksPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataCanvas)obj).YAxisMinorTicksPropertyChanged(); }
        void YAxisMinorTicksPropertyChanged() { DrawAxes(); }

        public ObservableList<AxisTick> YAxisMinorTicks { get { return (ObservableList<AxisTick>)GetValue(YAxisMinorTicksProperty); } set { SetCurrentValue(YAxisMinorTicksProperty, value); } }
        #endregion

        #region dependency property Color MinorTickLineColor

        public static DependencyProperty MinorTickLineColorProperty = DependencyProperty.Register("MinorTickLineColor",
                                                                                 typeof(Color),
                                                                                 typeof(DataCanvas),
                                                                                 new FrameworkPropertyMetadata(Colors.LightGray, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, MinorTickLineColorPropertyChanged));

        public Color MinorTickLineColor { get { return (Color)GetValue(MinorTickLineColorProperty); } set { SetValue(MinorTickLineColorProperty, value); } }

        static void MinorTickLineColorPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataCanvas)obj).MinorTickLineColorPropertyChanged(); }
        void MinorTickLineColorPropertyChanged()
        {
            _minorTickBrush = new SolidColorBrush(MinorTickLineColor);
            DrawAxes();
        }
        Brush _minorTickBrush = Brushes.LightGray;
        #endregion

        #region dependency property Color MajorTickLineColor

        public static DependencyProperty MajorTickLineColorProperty = DependencyProperty.Register("MajorTickLineColor",
                                                                                 typeof(Color),
                                                                                 typeof(DataCanvas),
                                                                                 new FrameworkPropertyMetadata(Colors.Black, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, MajorTickLineColorPropertyChanged));

        public Color MajorTickLineColor { get { return (Color)GetValue(MajorTickLineColorProperty); } set { SetValue(MajorTickLineColorProperty, value); } }

        static void MajorTickLineColorPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataCanvas)obj).MajorTickLineColorPropertyChanged(); }
        void MajorTickLineColorPropertyChanged()
        {
            _majorTickBrush = new SolidColorBrush(MajorTickLineColor);
            DrawAxes();
        }
        Brush _majorTickBrush = Brushes.Black;
        #endregion

        #region dependency property ObservableCollection<ISeries> SeriesSource

        public static DependencyProperty SeriesSourceProperty = DependencyProperty.Register("SeriesSource",
                                                                                 typeof(ObservableCollection<ISeries>),
                                                                                 typeof(DataCanvas),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, SeriesSourcePropertyChanged));

        public ObservableCollection<ISeries> SeriesSource { get { return (ObservableCollection<ISeries>)GetValue(SeriesSourceProperty); } set { SetValue(SeriesSourceProperty, value); } }

        static void SeriesSourcePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataCanvas)obj).SeriesSourcePropertyChanged(args); }
        void SeriesSourcePropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if (args.OldValue != null) {((ObservableCollection<ISeries>)args.OldValue).CollectionChanged -= SeriesSourceCollectionChanged;}
            Redraw();
            if (args.NewValue != null) ((ObservableCollection<ISeries>)args.NewValue).CollectionChanged += SeriesSourceCollectionChanged;
        }

        void SeriesSourceCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (ISeries newItem in args.NewItems)
                    {
                        newItem.RenderShapes();
                        newItem.Shapes.CollectionChanged += SeriesShapesCollectionChanged;
                        foreach (var shape in newItem.Shapes) Children.Add(shape);
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (ISeries oldItem in args.OldItems)
                    {
                        oldItem.Shapes.CollectionChanged -= SeriesShapesCollectionChanged;
                        foreach (var shape in oldItem.Shapes) Children.Remove(shape);
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    foreach (ISeries oldItem in args.OldItems)
                    {
                        oldItem.Shapes.CollectionChanged -= SeriesShapesCollectionChanged;
                        foreach (var shape in oldItem.Shapes) Children.Remove(shape);
                    }
                    foreach (ISeries newItem in args.NewItems)
                    {
                        newItem.RenderShapes();
                        newItem.Shapes.CollectionChanged += SeriesShapesCollectionChanged;
                        foreach (var shape in newItem.Shapes) Children.Add(shape);
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    Children.Clear();
                    DrawAxes();
                    if (SeriesSource != null) foreach (var item in SeriesSource)
                    {
                        item.RenderShapes();
                        item.Shapes.CollectionChanged += SeriesShapesCollectionChanged;
                        foreach (var shape in item.Shapes) Children.Add(shape);
                    }
                    break;
                case NotifyCollectionChangedAction.Move:
                    throw new NotSupportedException("Move operation not yet supported on SeriesSource");
            }
            InvalidateVisual();
        }

        void SeriesShapesCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            var key = (ObservableCollection<Shape>)sender;
            if (!_seriesShapeCache.ContainsKey(key)) _seriesShapeCache.Add(key, new List<Shape>());
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (Shape newShape in args.NewItems)
                    {
                        _seriesShapeCache[key].Add(newShape);
                        Children.Add(newShape);
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (Shape oldShape in args.OldItems)
                    {
                        _seriesShapeCache[key].Remove(oldShape);
                        Children.Remove(oldShape);
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    for (var i = 0; i < args.NewItems.Count; i++)
                    {
                        var oldShape = (Shape)args.OldItems[i];
                        var newShape = (Shape)args.NewItems[i];
                        var oldShapeCacheIndex = _seriesShapeCache[key].IndexOf(oldShape);
                        if (oldShapeCacheIndex == -1) _seriesShapeCache[key].Add(newShape);
                        else _seriesShapeCache[key][oldShapeCacheIndex] = newShape;
                        var oldShapeChildIndex = Children.IndexOf(oldShape);
                        if (oldShapeChildIndex == -1) Children.Add(newShape);
                        else
                        {
                            Children.RemoveAt(oldShapeChildIndex);
                            Children.Insert(oldShapeChildIndex, newShape);
                        }
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    foreach (var oldShape in _seriesShapeCache[key]) Children.Remove(oldShape);
                    _seriesShapeCache[key].Clear();
                    break;
                case NotifyCollectionChangedAction.Move:
                    throw new NotSupportedException("Move operation not yet supported on ISeries.Shapes");
            }
            InvalidateVisual();
        }
        #endregion

        readonly Dictionary<ObservableCollection<Shape>, List<Shape>> _seriesShapeCache = new Dictionary<ObservableCollection<Shape>, List<Shape>>();
        public DataCanvas() { SizeChanged += (s, e) => Redraw(); }

        void Redraw()
        {
            Children.Clear();
            DrawAxes();
            if (SeriesSource != null) foreach (var series in SeriesSource) series.RenderShapes();
            InvalidateVisual();
        }

        void DrawAxes()
        {
            if (XAxisMinorTicks != null && XAxisMinorTicks.Count > 0) CreateOrUpdateAxisLines("XAxisMinorTicks", CreateAxisLines(XAxisMinorTicks, ActualHeight, true, _minorTickBrush, 1));
            if (YAxisMinorTicks != null && YAxisMinorTicks.Count > 0) CreateOrUpdateAxisLines("YAxisMinorTicks", CreateAxisLines(YAxisMinorTicks, ActualWidth, false, _minorTickBrush, 1));
            if (XAxisMajorTicks != null && XAxisMajorTicks.Count > 0) CreateOrUpdateAxisLines("XAxisMajorTicks", CreateAxisLines(XAxisMajorTicks.Skip(1), ActualHeight, true, _majorTickBrush, 1));
            if (YAxisMajorTicks != null && YAxisMajorTicks.Count > 0) CreateOrUpdateAxisLines("YAxisMajorTicks", CreateAxisLines(YAxisMajorTicks.Skip(1), ActualWidth, false, _majorTickBrush, 1));
        }

        void CreateOrUpdateAxisLines(string axisKey, Shape shape)
        {
            var axisIndex = -1;
            if (_axisLines.ContainsKey(axisKey) && _axisLines[axisKey] != null) axisIndex = Children.IndexOf(_axisLines[axisKey]);
            _axisLines[axisKey] = shape;
            if (axisIndex == -1) Children.Insert(0, _axisLines[axisKey]);
            else Children[axisIndex] = _axisLines[axisKey];
        }
        readonly Dictionary<string, Shape> _axisLines = new Dictionary<string, Shape>();

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
