using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Linq;
using System.Windows;
using HRC;
using HRC.Aspects;
using HRC.Plotting;
using HRC.ViewModels;

namespace ESME.Views.Simulation
{
    /// <summary>
    /// To create and show the view as a dialog:
    /// var vm = new SimulationExposuresViewModel {...};
    /// var result = _visualizerService.ShowDialog("SimulationExposuresView", vm);
    /// if ((!result.HasValue) || (!result.Value)) return;
    /// 
    /// To create and show the view as a window:
    /// var vm = new SimulationExposuresViewModel {...};
    /// var window = _visualizerService.ShowWindow("SimulationExposuresView", vm);
    /// </summary>
    public class SimulationExposuresViewModel : ViewModelBase
    {
        [UsedImplicitly] readonly PropertyObserver<SimulationExposuresViewModel> _propertyObserver;
        [UsedImplicitly] readonly CollectionObserver _collectionObserver;
        readonly List<CollectionObserver> _axisLegendCollectionObservers = new List<CollectionObserver>();
        public SimulationExposuresViewModel(ObservableCollection<HistogramBinsViewModel> histogramBinsViewModels)
        {
            HistogramBinsViewModels = histogramBinsViewModels;
            _propertyObserver = new PropertyObserver<SimulationExposuresViewModel>(this)
                .RegisterHandler(p => p.ActualHeight, () => RowHeight = (ActualHeight / HistogramBinsViewModels.Count) - 10);
            _collectionObserver = new CollectionObserver(HistogramBinsViewModels)
                .RegisterHandler((s, e) =>
                {
                    switch (e.Action)
                    {
                        case NotifyCollectionChangedAction.Add:
                            foreach (HistogramBinsViewModel vm in e.NewItems)
                            {
                                _axisLegendCollectionObservers.Add(new CollectionObserver(vm.EnergyViewModel.LegendItems).RegisterHandler(LegendItemsCollectionChangedHandler));
                                _axisLegendCollectionObservers.Add(new CollectionObserver(vm.PressureViewModel.LegendItems).RegisterHandler(LegendItemsCollectionChangedHandler));
                            }
                            break;
                    }
                });
        }

        void LegendItemsCollectionChangedHandler(INotifyCollectionChanged notifyCollectionChanged, NotifyCollectionChangedEventArgs notifyCollectionChangedEventArgs)
        {
            switch (notifyCollectionChangedEventArgs.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (LegendItemViewModel newLegendItem in notifyCollectionChangedEventArgs.NewItems) 
                        if (LegendItems.All(l => l.SeriesName != newLegendItem.SeriesName)) LegendItems.Add(newLegendItem);
                    break;
            }
        }
        [Initialize, UsedImplicitly] public ObservableCollection<LegendItemViewModel> LegendItems { get; private set; }
        public ObservableCollection<HistogramBinsViewModel> HistogramBinsViewModels { get; private set; }
        public double ActualHeight { get; set; }
        public double RowHeight { get; private set; }
    }
}
