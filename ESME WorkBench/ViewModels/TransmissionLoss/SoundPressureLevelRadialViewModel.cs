using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
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
