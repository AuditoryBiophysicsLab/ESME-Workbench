using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Windows;
using HRC;
using HRC.Aspects;
using HRC.ViewModels;

namespace DavesWPFTester
{
    public abstract class MultipleBarSeriesBase : BarSeriesBase
    {
        [UsedImplicitly] PropertyObserver<SeriesViewModelBase> _propertyObserver;
        [UsedImplicitly] PropertyObserver<DataAxisViewModel> _xAxisObserver;
        [UsedImplicitly] PropertyObserver<DataAxisViewModel> _yAxisObserver;
        [UsedImplicitly]
        CollectionObserver _seriesObserver;

        protected MultipleBarSeriesBase()
        {
            _propertyObserver = new PropertyObserver<SeriesViewModelBase>(this)
                .RegisterHandler(d => d.XAxis,
                                 () =>
                                 {
                                     if (XAxis == null) return;
                                     _xAxisObserver = new PropertyObserver<DataAxisViewModel>(XAxis)
                                         .RegisterHandler(d => d.ValueToPosition, RenderShapes);
                                     foreach (var barSeries in BarSeriesCollection) barSeries.XAxis = XAxis; 
                                     RenderShapes();
                                 })
                .RegisterHandler(d => d.YAxis,
                                 () =>
                                 {
                                     if (YAxis == null) return;
                                     _yAxisObserver = new PropertyObserver<DataAxisViewModel>(YAxis)
                                         .RegisterHandler(d => d.ValueToPosition, RenderShapes);
                                     foreach (var barSeries in BarSeriesCollection) barSeries.YAxis = YAxis;
                                     RenderShapes();
                                 });
            _seriesObserver = new CollectionObserver(BarSeriesCollection)
                .RegisterHandler(BarSeriesCollectionChanged);
        }

        [Initialize, UsedImplicitly] public ObservableCollection<BarSeriesBase> BarSeriesCollection { get; private set; }
        void BarSeriesCollectionChanged(INotifyCollectionChanged sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    for (var i = 0; i < args.NewItems.Count; i++)
                    {
                        var newItem = (SeriesViewModelBase)args.NewItems[i];
                        var newItemIndex = args.NewStartingIndex + i;
                        if (newItem.XAxis == null) newItem.XAxis = XAxis;
                        if (newItem.YAxis == null) newItem.YAxis = YAxis;
                        LegendItems.Insert(newItemIndex, new LegendItemViewModel(newItem));
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    for (var i = 0; i < args.OldItems.Count; i++)
                    {
                        var oldItemIndex = args.OldStartingIndex + i;
                        LegendItems.RemoveAt(oldItemIndex);
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    for (var i = 0; i < args.OldItems.Count; i++)
                    {
                        var newItem = (SeriesViewModelBase)args.NewItems[i];
                        var oldItemIndex = args.OldStartingIndex + i;
                        LegendItems[oldItemIndex] = new LegendItemViewModel(newItem);
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    throw new NotImplementedException("Reset");
                case NotifyCollectionChangedAction.Move:
                    throw new NotImplementedException("Move");
            }
            RenderShapes();
        }
        
        protected readonly Dictionary<BarSeriesBase, Dictionary<double, Tuple<Point, double>>> SeriesPlotPointCache = new Dictionary<BarSeriesBase, Dictionary<double, Tuple<Point, double>>>();
    }
}