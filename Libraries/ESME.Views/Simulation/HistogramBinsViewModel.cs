using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Windows;
using ESME.SimulationAnalysis;
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

        public HistogramBinsViewModel(GroupedExposuresHistogram groupedExposuresHistogram)
        {
            if (groupedExposuresHistogram == null) throw new ArgumentNullException("groupedExposuresHistogram");
            _groupedBarSeriesViewModels = groupedExposuresHistogram.GroupedBarSeriesViewModels;
            PressureViewModel = new FourAxisSeriesViewModel
            {
                BottomAxis =
                {
                    Visibility = Visibility.Visible,
                    AxisTicks = new ObservableCollection<NewDataAxisTick>(),
                    AxisType = AxisType.Enumerated,
                    Label = string.Format("Peak pressure per ping (±{0:0}dB) [re: 1 µPa]", groupedExposuresHistogram.BinWidth / 2)
                },
                LeftAxis =
                {
                    Visibility = Visibility.Visible,
                    AxisType = AxisType.Linear,
                    Label = "Exposure count",
                },
                TopAxis = { Visibility = Visibility.Collapsed },
                RightAxis = { Visibility = Visibility.Collapsed },
                PlotTitle = groupedExposuresHistogram.GroupName,
            };

            EnergyViewModel = new FourAxisSeriesViewModel
            {
                BottomAxis =
                {
                    Visibility = Visibility.Visible,
                    AxisTicks = new ObservableCollection<NewDataAxisTick>(),
                    AxisType = AxisType.Enumerated,
                    Label = string.Format("Received energy per ping (±{0:0}dB)", groupedExposuresHistogram.BinWidth / 2)
                },
                LeftAxis =
                {
                    Visibility = Visibility.Visible,
                    AxisType = AxisType.Linear,
                    Label = "Exposure count",
                },
                TopAxis = { Visibility = Visibility.Collapsed },
                RightAxis = { Visibility = Visibility.Collapsed },
                PlotTitle = groupedExposuresHistogram.GroupName,
            };

            PressureViewModel.BottomAxis.AxisTicks.Add(new NewDataAxisTick(-1, null, false));
            EnergyViewModel.BottomAxis.AxisTicks.Add(new NewDataAxisTick(-1, null, false));
            for (var binIndex = 0; binIndex < groupedExposuresHistogram.BinNames.Length; binIndex++)
            {
                PressureViewModel.BottomAxis.AxisTicks.Add(new NewDataAxisTick(binIndex, groupedExposuresHistogram.BinNames[binIndex], false));
                EnergyViewModel.BottomAxis.AxisTicks.Add(new NewDataAxisTick(binIndex, groupedExposuresHistogram.BinNames[binIndex], false));
            }
            PressureViewModel.BottomAxis.AxisTicks.Add(new NewDataAxisTick(groupedExposuresHistogram.BinNames.Length, null, false));
            EnergyViewModel.BottomAxis.AxisTicks.Add(new NewDataAxisTick(groupedExposuresHistogram.BinNames.Length, null, false));

            PressureViewModel.DataSeriesCollection.Add(groupedExposuresHistogram.GroupedBarSeriesViewModels[0]);
            PressureViewModel.DataSeriesCollection.Add(groupedExposuresHistogram.GroupedBarSeriesViewModels[1]);

            PressureViewModel.LeftAxis.VisibleRange.Update(.9, 100);
            PressureViewModel.BottomAxis.DataRange.Update(-1, 12);
            EnergyViewModel.LeftAxis.VisibleRange.Update(.9, 100);
            EnergyViewModel.BottomAxis.DataRange.Update(-1, 12);
            //var barSeries = new StackedBarSeriesViewModel();
            //HistogramBinCollection.CollectionChanged += HistogramBinsCollectionChanged;
        }

        GroupedBarSeriesViewModel[] _groupedBarSeriesViewModels;

        [Initialize] public FourAxisSeriesViewModel PressureViewModel { get; private set; }
        [Initialize] public FourAxisSeriesViewModel EnergyViewModel { get; private set; }
        public static HistogramBinsViewModel DesignTimeData { get; set; }
        static HistogramBinsViewModel()
        {
            DesignTimeData = new HistogramBinsViewModel
            {
                PressureViewModel = new FourAxisSeriesViewModel
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
                            Label = "Peak pressure per ping (±5dB) [re: 1 µPa]",
                        },
                    LeftAxis =
                        {
                            Visibility = Visibility.Visible,
                            AxisType = AxisType.Logarithmic,
                            Label = "Exposure count",
                        },
                    TopAxis = { Visibility = Visibility.Collapsed },
                    RightAxis = { Visibility = Visibility.Collapsed },
                    PlotTitle = "1 kHz mode",
                },
                EnergyViewModel = new FourAxisSeriesViewModel
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
                            Label = "Received Energy per ping (±5dB)",
                        },
                    LeftAxis =
                        {
                            Visibility = Visibility.Visible,
                            AxisType = AxisType.Logarithmic,
                            Label = "Exposure count",
                        },
                    TopAxis = { Visibility = Visibility.Collapsed },
                    RightAxis = { Visibility = Visibility.Collapsed },
                    PlotTitle = "1 kHz mode",
                },
            };
            DesignTimeData.PressureViewModel.LeftAxis.VisibleRange.Update(.9, 100);
            DesignTimeData.PressureViewModel.BottomAxis.DataRange.Update(-1, 12);
            DesignTimeData.EnergyViewModel.LeftAxis.VisibleRange.Update(.9, 100);
            DesignTimeData.EnergyViewModel.BottomAxis.DataRange.Update(-1, 12);
        }
    }
}
