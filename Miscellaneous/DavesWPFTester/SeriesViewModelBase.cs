using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
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
                .RegisterHandler(d => d.XAxisMappingFunction, MappingFunctionChanged)
                .RegisterHandler(d => d.YAxisMappingFunction, MappingFunctionChanged);
            _pointsObserver = new CollectionObserver(Points).RegisterHandler(PointsCollectionChanged);
        }

        [Initialize] public Range XRange { get; set; }

        [Initialize] public Range YRange { get; set; }

        public Func<object, Point> ItemToPoint { get; set; }

        public ImageSource SampleImageSource { get; set; }
        protected abstract void RenderSample();

        public Func<double, double> XAxisMappingFunction { get; set; }

        public Func<double, double> YAxisMappingFunction { get; set; }

        public string SeriesName { get; set; }

        public ICollection SeriesData { get; set; }

        [Initialize] public ObservableCollection<Shape> Shapes { get; set; }

        [Initialize] protected ObservableCollection<Point> Points { get; [UsedImplicitly] set; }

        protected readonly Dictionary<Point, Shape> PointShapeMap = new Dictionary<Point, Shape>();

        protected virtual void ProcessSeriesData()
        {
            if (ItemToPoint == null || SeriesData == null || SeriesData.Count == 0) return;
            foreach (var item in SeriesData) Points.Add(ItemToPoint(item));
            XRange.Update(Points.Select(p => p.X));
            YRange.Update(Points.Select(p => p.Y));
            RenderShapes();
        }

        protected void UpdateMinMax(Point point)
        {
            XRange.Add(point.X);
            YRange.Add(point.Y);
        }

        void MappingFunctionChanged()
        {
            if (XAxisMappingFunction == null || YAxisMappingFunction == null) return;
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
                    foreach (Point point in args.NewItems) AddPoint(point);
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (Point point in args.OldItems) RemovePoint(point);
                    break;
                case NotifyCollectionChangedAction.Replace:
                    for (var i = 0; i < args.NewItems.Count; i++) ReplacePoint((Point)args.OldItems[i], (Point)args.NewItems[i]);
                    break;
                case NotifyCollectionChangedAction.Reset:
                    Shapes.Clear();
                    PointShapeMap.Clear();
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