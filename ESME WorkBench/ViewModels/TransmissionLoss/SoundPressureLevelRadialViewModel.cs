using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkbench.ViewModels.TransmissionLoss
{
    [ExportViewModel("SoundPressureLevelRadialViewModel")]
    class SoundPressureLevelRadialViewModel : ViewModelBase
    {
        [ImportingConstructor]
        public SoundPressureLevelRadialViewModel(IViewAwareStatus viewAwareStatus)
        {
            
        }

    }
}
