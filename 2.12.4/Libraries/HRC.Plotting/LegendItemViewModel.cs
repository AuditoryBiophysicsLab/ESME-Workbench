using System.Windows.Media;
using HRC.ViewModels;

namespace HRC.Plotting
{
    public class LegendItemViewModel : ViewModelBase
    {
        [UsedImplicitly] PropertyObserver<SeriesViewModelBase> _propertyObserver;
        public LegendItemViewModel(SeriesViewModelBase dataSeries)
        {
            DataSeries = dataSeries;
            _propertyObserver = new PropertyObserver<SeriesViewModelBase>(dataSeries)
                .RegisterHandler(s => s.SampleImageSource, series => { SampleImageSource = series.SampleImageSource; })
                .RegisterHandler(s => s.SeriesName, series => { SeriesName = series.SeriesName; });

            SampleImageSource = dataSeries.SampleImageSource;
            SeriesName = dataSeries.SeriesName;
        }

        public ImageSource SampleImageSource { get; set; }
        public string SeriesName { get; set; }
        public SeriesViewModelBase DataSeries { get; private set; }
    }
}