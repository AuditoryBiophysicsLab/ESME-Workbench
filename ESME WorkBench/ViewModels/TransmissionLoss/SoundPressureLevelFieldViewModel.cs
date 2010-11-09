using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    [ExportViewModel("SoundPressureLevelFieldViewModel")]
    class SoundPressureLevelFieldViewModel:TransmissionLossFieldViewModel
    {
        [ImportingConstructor ]
        public SoundPressureLevelFieldViewModel() {  }
    }
}
