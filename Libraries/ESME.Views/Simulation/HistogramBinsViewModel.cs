using System;
using System.Windows;
using ESME.SimulationAnalysis;
using HRC.Aspects;
using HRC.Plotting;
using HRC.Utility;
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
            PressureViewModel = new FourAxisSeriesViewModel
            {
                BottomAxis =
                {
                    Visibility = Visibility.Visible,
                    AxisTicks = new ObservableList<DataAxisTick>(),
                    AxisType = AxisType.Enumerated,
                    Label = string.Format("Peak pressure per ping (±{0:0}dB) [dB re: 1 µPa]", groupedExposuresHistogram.BinWidth / 2)
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
                    AxisTicks = new ObservableList<DataAxisTick>(),
                    AxisType = AxisType.Enumerated,
                    Label = string.Format("Sound exposure level per ping (±{0:0}dB) [dB re: 1 µPa²•s]", groupedExposuresHistogram.BinWidth / 2)
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

            PressureViewModel.BottomAxis.AxisTicks.Add(new DataAxisTick(-1, null, false));
            EnergyViewModel.BottomAxis.AxisTicks.Add(new DataAxisTick(-1, null, false));
            for (var binIndex = 0; binIndex < groupedExposuresHistogram.BinNames.Length; binIndex++)
            {
                PressureViewModel.BottomAxis.AxisTicks.Add(new DataAxisTick(binIndex, groupedExposuresHistogram.BinNames[binIndex], false));
                EnergyViewModel.BottomAxis.AxisTicks.Add(new DataAxisTick(binIndex, groupedExposuresHistogram.BinNames[binIndex], false));
            }
            PressureViewModel.BottomAxis.AxisTicks.Add(new DataAxisTick(groupedExposuresHistogram.BinNames.Length, null, false));
            EnergyViewModel.BottomAxis.AxisTicks.Add(new DataAxisTick(groupedExposuresHistogram.BinNames.Length, null, false));

            PressureViewModel.DataSeriesCollection.Add(groupedExposuresHistogram.GroupedBarSeriesViewModels[0]);
            EnergyViewModel.DataSeriesCollection.Add(groupedExposuresHistogram.GroupedBarSeriesViewModels[1]);

            PressureViewModel.LeftAxis.VisibleRange.Update(.9, 100);
            PressureViewModel.BottomAxis.DataRange.Update(-1, 12);
            EnergyViewModel.LeftAxis.VisibleRange.Update(.9, 100);
            EnergyViewModel.BottomAxis.DataRange.Update(-1, 12);

            //var barSeries = new StackedBarSeriesViewModel();
            //HistogramBinCollection.CollectionChanged += HistogramBinsCollectionChanged;
        }


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
                            AxisTicks = new ObservableList<DataAxisTick>
                            {
                                new DataAxisTick(-1, null, false),
                                new DataAxisTick(0, "<100", false),
                                new DataAxisTick(1, "105", false),
                                new DataAxisTick(2, "115", false),
                                new DataAxisTick(3, "125", false),
                                new DataAxisTick(4, "135", false),
                                new DataAxisTick(5, "145", false),
                                new DataAxisTick(6, "155", false),
                                new DataAxisTick(7, "165", false),
                                new DataAxisTick(8, "175", false),
                                new DataAxisTick(9, "185", false),
                                new DataAxisTick(10, "195", false),
                                new DataAxisTick(11, ">200", false),
                                new DataAxisTick(12, null, false),
                            },
                            AxisType = AxisType.Enumerated,
                            Label = "Peak pressure per ping (±5dB) [dB re: 1 µPa]",
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
                            AxisTicks = new ObservableList<DataAxisTick>
                            {
                                new DataAxisTick(-1, null, false),
                                new DataAxisTick(0, "<100", false),
                                new DataAxisTick(1, "105", false),
                                new DataAxisTick(2, "115", false),
                                new DataAxisTick(3, "125", false),
                                new DataAxisTick(4, "135", false),
                                new DataAxisTick(5, "145", false),
                                new DataAxisTick(6, "155", false),
                                new DataAxisTick(7, "165", false),
                                new DataAxisTick(8, "175", false),
                                new DataAxisTick(9, "185", false),
                                new DataAxisTick(10, "195", false),
                                new DataAxisTick(11, ">200", false),
                                new DataAxisTick(12, null, false),
                            },
                            AxisType = AxisType.Enumerated,
                            Label = "Sound exposure level per ping (±5dB) [dB re: 1 µPa²•s]",
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
