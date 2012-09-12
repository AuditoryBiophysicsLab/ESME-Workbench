using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using System.Windows.Shapes;
using HRC;
using HRC.Aspects;
using HRC.ViewModels;

namespace DavesWPFTester
{
    public abstract class SeriesViewModelBase : ViewModelBase, ISeries
    {
        [UsedImplicitly] PropertyObserver<SeriesViewModelBase> _propertyObserver;
        [UsedImplicitly] CollectionObserver _pointsObserver;
        protected SeriesViewModelBase()
        {
            _propertyObserver = new PropertyObserver<SeriesViewModelBase>(this)
                .RegisterHandler(d => d.SeriesData, SeriesDataChanged)
                .RegisterHandler(d => d.ItemToPoint, ProcessSeriesData)
                .RegisterHandler(d => d.XAxis, XAxisChanged)
                .RegisterHandler(d => d.YAxis, YAxisChanged);
            _pointsObserver = new CollectionObserver(Points).RegisterHandler(PointsCollectionChanged);
            XRange.RangeChanged += (s, e) => { if (XAxis != null) XAxis.DataRange.Add(XRange); };
            YRange.RangeChanged += (s, e) => { if (YAxis != null) YAxis.DataRange.Add(YRange); };
        }

        public DataAxisViewModel XAxis { get; set; }
        public DataAxisViewModel YAxis { get; set; }
        [Initialize] public Range XRange { get; set; }
        [Initialize] public Range YRange { get; set; }

        public Func<object, Point> ItemToPoint { get; set; }

        public ImageSource SampleImageSource { get; set; }
        protected abstract void RenderSample();

        public string SeriesName { get; set; }

        public ICollection SeriesData { get; set; }

        [Initialize] public ObservableCollection<Shape> Shapes { get; set; }

        [Initialize] protected ObservableCollection<Point> Points { get; set; }

        protected readonly Dictionary<Point, Shape> PointShapeMap = new Dictionary<Point, Shape>();

        protected virtual void ProcessSeriesData()
        {
            if (ItemToPoint == null || SeriesData == null || SeriesData.Count == 0) return;
            foreach (var point in from object item in SeriesData
                                  select ItemToPoint(item)) Points.Add(point);
            RenderShapes();
        }

        [UsedImplicitly] PropertyObserver<DataAxisViewModel> _xAxisObserver, _yAxisObserver;
        void XAxisChanged()
        {
            if (XAxis == null)
            {
                _xAxisObserver = null;
                return;
            }
            _xAxisObserver = new PropertyObserver<DataAxisViewModel>(XAxis)
                .RegisterHandler(x => x.ValueToPosition, XAxisValueToPositionChanged);
            XAxis.DataRange.Add(XRange);
        }
        void XAxisValueToPositionChanged()
        {
            Debug.WriteLine("XAxis.ValueToPosition changed, re-rendering...");
            RenderShapes();
        }
        void YAxisChanged()
        {
            if (YAxis == null)
            {
                _yAxisObserver = null;
                return;
            }
            _yAxisObserver = new PropertyObserver<DataAxisViewModel>(YAxis)
                .RegisterHandler(y => y.ValueToPosition, YAxisValueToPositionChanged);
            YAxis.DataRange.Add(YRange);
        }
        void YAxisValueToPositionChanged()
        {
            Debug.WriteLine("YAxis.ValueToPosition changed, re-rendering...");
            RenderShapes();
        }

        public abstract void RenderShapes();
        CollectionObserver _seriesDataObserver;
        void SeriesDataChanged()
        {
            Points.Clear();
            ProcessSeriesData();
            if (SeriesData == null || !(SeriesData is INotifyCollectionChanged)) return;
            var series = (INotifyCollectionChanged)SeriesData;
            if (_seriesDataObserver == null) _seriesDataObserver = new CollectionObserver(series).RegisterHandler(SeriesDataCollectionChanged);
            else
            {
                _seriesDataObserver.UnregisterHandler(SeriesDataCollectionChanged);
                _seriesDataObserver.RegisterHandler(SeriesDataCollectionChanged);
            }
        }

        void SeriesDataCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            if (ItemToPoint == null) return;
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    for (var i = 0; i < args.NewItems.Count; i++) Points.Insert(args.NewStartingIndex + i, ItemToPoint(args.NewItems[i]));
                    break;
                case NotifyCollectionChangedAction.Remove:
                    for (var i = 0; i < args.OldItems.Count; i++) Points.RemoveAt(args.OldStartingIndex);
                    break;
                case NotifyCollectionChangedAction.Replace:
                    for (var i = 0; i < args.NewItems.Count; i++) Points[args.NewStartingIndex + i] = ItemToPoint(args.NewItems[i]);
                    break;
                case NotifyCollectionChangedAction.Reset:
                    Points.Clear();
                    break;
                case NotifyCollectionChangedAction.Move:
                    throw new NotImplementedException("Move not implemented for SeriesData collection");
            }
        }

        void PointsCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (Point point in args.NewItems)
                    {
                        XRange.Add(point.X);
                        YRange.Add(point.Y);
                        AddPoint(point);
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (Point point in args.OldItems) RemovePoint(point);
                    break;
                case NotifyCollectionChangedAction.Replace:
                    for (var i = 0; i < args.NewItems.Count; i++)
                    {
                        var oldPoint = (Point)args.OldItems[i];
                        var newPoint = (Point)args.NewItems[i];
                        XRange.Add(newPoint.X);
                        YRange.Add(newPoint.Y);
                        ReplacePoint(oldPoint, newPoint);
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    Shapes.Clear();
                    PointShapeMap.Clear();
                    XRange.Reset();
                    YRange.Reset();
                    break;
                case NotifyCollectionChangedAction.Move:
                    throw new NotImplementedException("Move not implemented for Points collection");
            }
        }

        protected abstract void AddPoint(Point newPoint);

        protected abstract void RemovePoint(Point oldPoint);

        protected virtual void ReplacePoint(Point oldPoint, Point newPoint)
        {
            RemovePoint(oldPoint);
            AddPoint(newPoint);
        }
    }
}