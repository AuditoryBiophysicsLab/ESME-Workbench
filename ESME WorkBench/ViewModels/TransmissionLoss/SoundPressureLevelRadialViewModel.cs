using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace OneNavyModel.ViewModels.TransmissionLoss
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
