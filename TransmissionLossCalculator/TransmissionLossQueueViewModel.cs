using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;
using MEFedMVVM.ViewModelLocator;

namespace TransmissionLossCalculator
{
    [ExportViewModel("TransmissionLossQueueViewModel")]
    public class TransmissionLossQueueViewModel
    {
        [ImportingConstructor]
        public TransmissionLossQueueViewModel() {  }
    }
}
