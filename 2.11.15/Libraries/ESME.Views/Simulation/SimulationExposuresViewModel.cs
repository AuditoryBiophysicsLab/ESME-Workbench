using System.Collections.ObjectModel;
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
        public SimulationExposuresViewModel(ObservableCollection<HistogramBinsViewModel> histogramBinsViewModels) { HistogramBinsViewModels = histogramBinsViewModels; }

        public ObservableCollection<HistogramBinsViewModel> HistogramBinsViewModels { get; private set; }
    }
}
