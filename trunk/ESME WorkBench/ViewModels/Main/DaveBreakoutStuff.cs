using System.IO;
using ESME.Views.EnvironmentBuilder;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        void NewBathymetryHandler()
        {
            var vm = new BathymetryExtractionViewModel(Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename));
            var result = _visualizerService.ShowDialog("BathymetryExtractionView", vm);
            if ((result.HasValue) && (result.Value))
            {
            }
        }

        void NewEnvironmentHandler()
        {
            var vm = new EnvironmentExtractionViewModel(Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename));
            var result = _visualizerService.ShowDialog("EnvironmentExtractionView", vm);
            if ((result.HasValue) && (result.Value))
            {
            }
        }
    }
}
