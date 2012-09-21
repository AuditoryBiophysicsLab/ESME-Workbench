using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Windows;
using ESME.SimulationAnalysis;
using HRC;
using HRC.Aspects;
using HRC.Plotting;
using HRC.ViewModels;

namespace ESME.Views.Simulation
{
    /// <summary>
    /// To create and show the view as a dialog:
    /// var vm = new HistogramBinsViewModel {...};
    /// var result = _visualizerService.ShowDialog("HistogramBinsView", vm);
    /// if ((!result.HasValue) || (!result.Value)) return;
    /// 
    /// To create and show the view as a window:
    /// var vm = new HistogramBinsViewModel {...};
    /// var window = _visualizerService.ShowWindow("HistogramBinsView", vm);
    /// </summary>
    public class HistogramBinsViewModel : ViewModelBase
    {
        public HistogramBinsViewModel() { }

        public HistogramBinsViewModel(ObservableCollection<HistogramBins> histogramBinCollection)
        {
            if (histogramBinCollection == null) throw new ArgumentNullException("histogramBinCollection");
            HistogramBinCollection = histogramBinCollection;
            var barSeries = new StackedBarSeriesViewModel();
            HistogramBinCollection.CollectionChanged += HistogramBinsCollectionChanged;
        }

        void HistogramBinsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    break;
                case NotifyCollectionChangedAction.Remove:
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
            }
        }

        [Initialize] public FourAxisSeriesViewModel FourAxisSeriesViewModel { get; private set; }
        public ObservableCollection<HistogramBins> HistogramBinCollection { get; private set; }
        public static HistogramBinsViewModel DesignTimeData { get; set; }
        static HistogramBinsViewModel()
        {
            DesignTimeData = new HistogramBinsViewModel
            {
                FourAxisSeriesViewModel = new FourAxisSeriesViewModel
                {
                    BottomAxis =
                        {
                            Visibility = Visibility.Visible,
                            AxisTicks = new ObservableCollection<NewDataAxisTick>
                            {
                                new NewDataAxisTick(-1, null, false),
                                new NewDataAxisTick(0, "<100", false),
                                new NewDataAxisTick(1, "105", false),
                                new NewDataAxisTick(2, "115", false),
                                new NewDataAxisTick(3, "125", false),
                                new NewDataAxisTick(4, "135", false),
                                new NewDataAxisTick(5, "145", false),
                                new NewDataAxisTick(6, "155", false),
                                new NewDataAxisTick(7, "165", false),
                                new NewDataAxisTick(8, "175", false),
                                new NewDataAxisTick(9, "185", false),
                                new NewDataAxisTick(10, "195", false),
                                new NewDataAxisTick(11, ">200", false),
                                new NewDataAxisTick(12, null, false),
                            },
                            AxisType = AxisType.Enumerated,                            
                            Label = "Exposure level ±5dB (re: 1 µPa)",
                        },
                    LeftAxis =
                        {
                            Visibility = Visibility.Visible,
                            AxisType = AxisType.Logarithmic,
                            Label = "Exposure count",
                        },
                    TopAxis = { Visibility = Visibility.Collapsed },
                    RightAxis = { Visibility = Visibility.Collapsed },
                    PlotTitle = "Acoustic exposures: Tursiops truncatus",
                }
            };
            DesignTimeData.FourAxisSeriesViewModel.LeftAxis.VisibleRange.Update(.9, 100);
            DesignTimeData.FourAxisSeriesViewModel.BottomAxis.DataRange.Update(-1, 12);
        }
    }
}
