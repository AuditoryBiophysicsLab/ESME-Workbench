using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;
using HRC.Utility;

namespace HRC.Plotting
{
    public class DataCanvas : Canvas
    {
        static DataCanvas() { DefaultStyleKeyProperty.OverrideMetadata(typeof(DataCanvas), new FrameworkPropertyMetadata(typeof(DataCanvas))); }

        #region dependency property ObservableList<DataAxisTick> XAxisTicks

        public static DependencyProperty XAxisTicksProperty = DependencyProperty.Register("XAxisTicks",
                                                                                 typeof(ObservableList<DataAxisTick>),
                                                                                 typeof(DataCanvas),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender));

        public ObservableList<DataAxisTick> XAxisTicks { get { return (ObservableList<DataAxisTick>)GetValue(XAxisTicksProperty); } set { SetValue(XAxisTicksProperty, value); } }
        #endregion

        #region dependency property ObservableList<DataAxisTick> YAxisTicks

        public static DependencyProperty YAxisTicksProperty = DependencyProperty.Register("YAxisTicks",
                                                                                 typeof(ObservableList<DataAxisTick>),
                                                                                 typeof(DataCanvas),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender));

        public ObservableList<DataAxisTick> YAxisTicks { get { return (ObservableList<DataAxisTick>)GetValue(YAxisTicksProperty); } set { SetValue(YAxisTicksProperty, value); } }
        #endregion

        #region dependency property Color MinorTickLineColor

        public static DependencyProperty MinorTickLineColorProperty = DependencyProperty.Register("MinorTickLineColor",
                                                                                 typeof(Color),
                                                                                 typeof(DataCanvas),
                                                                                 new FrameworkPropertyMetadata(Colors.LightGray, FrameworkPropertyMetadataOptions.AffectsRender));

        public Color MinorTickLineColor { get { return (Color)GetValue(MinorTickLineColorProperty); } set { SetValue(MinorTickLineColorProperty, value); } }
        #endregion

        #region dependency property Color MajorTickLineColor

        public static DependencyProperty MajorTickLineColorProperty = DependencyProperty.Register("MajorTickLineColor",
                                                                                 typeof(Color),
                                                                                 typeof(DataCanvas),
                                                                                 new FrameworkPropertyMetadata(Colors.Black, FrameworkPropertyMetadataOptions.AffectsRender));

        public Color MajorTickLineColor { get { return (Color)GetValue(MajorTickLineColorProperty); } set { SetValue(MajorTickLineColorProperty, value); } }
        #endregion

        #region dependency property ObservableCollection<ISeries> SeriesSource

        public static DependencyProperty SeriesSourceProperty = DependencyProperty.Register("SeriesSource",
                                                                                 typeof(ObservableCollection<ISeries>),
                                                                                 typeof(DataCanvas),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.None, SeriesSourcePropertyChanged));

        public ObservableCollection<ISeries> SeriesSource { get { return (ObservableCollection<ISeries>)GetValue(SeriesSourceProperty); } set { SetValue(SeriesSourceProperty, value); } }

        static void SeriesSourcePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((DataCanvas)obj).SeriesSourcePropertyChanged(args); }
        void SeriesSourcePropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if (args.OldValue != null) {((ObservableCollection<ISeries>)args.OldValue).CollectionChanged -= SeriesSourceCollectionChanged;}
            //Redraw();
            if (args.NewValue != null)
            {
                ((ObservableCollection<ISeries>)args.NewValue).CollectionChanged += SeriesSourceCollectionChanged;
                foreach (var newItem in (ObservableCollection<ISeries>)args.NewValue)
                {
                    _seriesShapeCache.Add(newItem.Shapes, new List<Shape>());
                    newItem.RenderShapes();
                    newItem.Shapes.CollectionChanged += SeriesShapesCollectionChanged;
                    foreach (var shape in newItem.Shapes)
                    {
                        _seriesShapeCache[newItem.Shapes].Add(shape);
                        Children.Add(shape);
                    }
                }
            }
        }

        void SeriesSourceCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (ISeries newItem in args.NewItems)
                    {
                        //if (newItem.SeriesName != null && newItem.SeriesName.StartsWith("(bar)")) Debugger.Break();
                        _seriesShapeCache.Add(newItem.Shapes, new List<Shape>());
                        newItem.RenderShapes();
                        newItem.Shapes.CollectionChanged += SeriesShapesCollectionChanged;
                        foreach (var shape in newItem.Shapes)
                        {
                            _seriesShapeCache[newItem.Shapes].Add(shape);
                            Children.Add(shape);
                        }
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (ISeries oldItem in args.OldItems)
                    {
                        oldItem.Shapes.CollectionChanged -= SeriesShapesCollectionChanged;
                        foreach (var oldShape in _seriesShapeCache[oldItem.Shapes]) Children.Remove(oldShape);
                        foreach (var shape in oldItem.Shapes) Children.Remove(shape);
                        _seriesShapeCache.Remove(oldItem.Shapes);
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    foreach (ISeries oldItem in args.OldItems)
                    {
                        oldItem.Shapes.CollectionChanged -= SeriesShapesCollectionChanged;
                        foreach (var oldShape in _seriesShapeCache[oldItem.Shapes]) Children.Remove(oldShape);
                        foreach (var shape in oldItem.Shapes) Children.Remove(shape);
                        _seriesShapeCache.Remove(oldItem.Shapes);
                    }
                    foreach (ISeries newItem in args.NewItems)
                    {
                        _seriesShapeCache.Add(newItem.Shapes, new List<Shape>());
                        newItem.RenderShapes();
                        newItem.Shapes.CollectionChanged += SeriesShapesCollectionChanged;
                        foreach (var shape in newItem.Shapes)
                        {
                            _seriesShapeCache[newItem.Shapes].Add(shape);
                            Children.Add(shape);
                        }
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    Children.Clear();
                    _seriesShapeCache.Clear();
                    break;
                case NotifyCollectionChangedAction.Move:
                    throw new NotSupportedException("Move operation not yet supported on SeriesSource");
            }
            InvalidateVisual();
        }

        void SeriesShapesCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            //if (SeriesSource != null && SeriesSource.Any(s => s.SeriesName.StartsWith("(bar)"))) Debugger.Break();
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

        #region dependency property Point MouseLocation

        public static DependencyPropertyKey MouseLocationPropertyKey = DependencyProperty.RegisterReadOnly("MouseLocation",
                                                                                 typeof(Point),
                                                                                 typeof(DataCanvas),
                                                                                 new FrameworkPropertyMetadata(new Point(), FrameworkPropertyMetadataOptions.None));

        public static readonly DependencyProperty MouseLocationProperty = MouseLocationPropertyKey.DependencyProperty;
        public Point MouseLocation { get { return (Point)GetValue(MouseLocationProperty); } private set { SetValue(MouseLocationPropertyKey, value); } }
        #endregion

        readonly Dictionary<ObservableCollection<Shape>, List<Shape>> _seriesShapeCache = new Dictionary<ObservableCollection<Shape>, List<Shape>>();
        public DataCanvas()
        {
            SnapsToDevicePixels = true;
            UseLayoutRounding = true;
            MouseMove += (s, e) => { MouseLocation = e.GetPosition(this); };
        }

        protected override void OnRender(DrawingContext dc)
        {
            if (MinorTickLineColor != Colors.Transparent)
            {
                var majorTickPen = new Pen(new SolidColorBrush(MinorTickLineColor), 1.0);
                if (XAxisTicks != null) foreach (var tick in XAxisTicks.Where(t => !t.IsMajorTick)) RenderTickLine(dc, majorTickPen, tick.Location, ActualHeight, true);
                if (YAxisTicks != null) foreach (var tick in YAxisTicks.Where(t => !t.IsMajorTick)) RenderTickLine(dc, majorTickPen, tick.Location, ActualWidth, false);
            }
            if (MajorTickLineColor == Colors.Transparent) return;
            var minorTickPen = new Pen(new SolidColorBrush(MajorTickLineColor), 1.0);
            if (XAxisTicks != null) foreach (var tick in XAxisTicks.Where(t => t.IsMajorTick)) RenderTickLine(dc, minorTickPen, tick.Location, ActualHeight, true);
            if (YAxisTicks != null) foreach (var tick in YAxisTicks.Where(t => t.IsMajorTick)) RenderTickLine(dc, minorTickPen, tick.Location, ActualWidth, false);
        }

        static void RenderTickLine(DrawingContext dc, Pen pen, double location, double length, bool isVertical)
        {
            if (isVertical) dc.DrawLine(pen, new Point(location, 0), new Point(location, length));
            else dc.DrawLine(pen, new Point(0, location), new Point(length, location));
        }

    }
}
