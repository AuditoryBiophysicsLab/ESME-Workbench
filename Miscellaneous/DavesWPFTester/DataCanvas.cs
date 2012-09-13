using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;

namespace DavesWPFTester
{
    public class DataCanvas : Canvas
    {
        static DataCanvas() { DefaultStyleKeyProperty.OverrideMetadata(typeof(DataCanvas), new FrameworkPropertyMetadata(typeof(DataCanvas))); }

        #region dependency property ObservableCollection<NewDataAxisTick> XAxisTicks

        public static DependencyProperty XAxisTicksProperty = DependencyProperty.Register("XAxisTicks",
                                                                                 typeof(ObservableCollection<NewDataAxisTick>),
                                                                                 typeof(DataCanvas),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender));

        public ObservableCollection<NewDataAxisTick> XAxisTicks { get { return (ObservableCollection<NewDataAxisTick>)GetValue(XAxisTicksProperty); } set { SetValue(XAxisTicksProperty, value); } }
        #endregion

        #region dependency property ObservableCollection<NewDataAxisTick> YAxisTicks

        public static DependencyProperty YAxisTicksProperty = DependencyProperty.Register("YAxisTicks",
                                                                                 typeof(ObservableCollection<NewDataAxisTick>),
                                                                                 typeof(DataCanvas),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender));

        public ObservableCollection<NewDataAxisTick> YAxisTicks { get { return (ObservableCollection<NewDataAxisTick>)GetValue(YAxisTicksProperty); } set { SetValue(YAxisTicksProperty, value); } }
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
                    //DrawAxes();
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
        public DataCanvas()
        {
            SnapsToDevicePixels = true;
            UseLayoutRounding = true;
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
