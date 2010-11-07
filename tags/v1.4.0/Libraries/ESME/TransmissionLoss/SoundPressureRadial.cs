using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.TransmissionLoss
{
     
    class SoundPressureRadial:TransmissionLossRadial
    {
        public float[] SPLDepths { get; internal set; }
        public float[] SPLRanges { get; internal set; }
    }
}
