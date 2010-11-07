using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.TransmissionLoss
{
    class SoundPressureLevelField:TransmissionLossField
    {
        public SoundPressureRadial[] SPLRadials { get; private set; }
        public float[] SPLDepths { get; set; }
        public float[] SPLRanges { get; set; }

        public SoundPressureLevelField(TransmissionLossField transmissionLossField)
        {
            
        }

        //public 

    }
}
