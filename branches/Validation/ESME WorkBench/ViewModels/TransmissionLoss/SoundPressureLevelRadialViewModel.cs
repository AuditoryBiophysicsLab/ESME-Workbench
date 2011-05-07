using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;
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
