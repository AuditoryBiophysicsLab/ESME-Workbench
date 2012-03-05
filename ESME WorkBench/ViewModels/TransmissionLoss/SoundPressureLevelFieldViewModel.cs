using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkbench.ViewModels.TransmissionLoss
{
    [ExportViewModel("SoundPressureLevelFieldViewModel")]
    class SoundPressureLevelFieldViewModel: ViewModelBase 
    {
        [ImportingConstructor ]
        public SoundPressureLevelFieldViewModel() {  }
    }
}
