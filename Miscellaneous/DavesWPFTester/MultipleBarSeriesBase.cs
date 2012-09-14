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
                    foreach (BarSeriesBase series in args.NewItems)
                    {
                        series.XAxis = XAxis;
                        series.YAxis = YAxis;
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (BarSeriesBase series in args.OldItems)
                    {
                        series.XAxis = null;
                        series.YAxis = null;
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    foreach (BarSeriesBase series in args.OldItems)
                    {
                        series.XAxis = null;
                        series.YAxis = null;
                    }
                    foreach (BarSeriesBase series in args.NewItems)
                    {
                        series.XAxis = XAxis;
                        series.YAxis = YAxis;
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                case NotifyCollectionChangedAction.Move:
                    break;
            }
            RenderShapes();
        }
        
        protected readonly Dictionary<BarSeriesBase, Dictionary<double, Tuple<Point, double>>> SeriesPlotPointCache = new Dictionary<BarSeriesBase, Dictionary<double, Tuple<Point, double>>>();
    }
}