using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.TransmissionLoss
{
    class SoundPressureLevelField:TransmissionLossField
    {
        public SoundPressureLevelRadial[] SPLRadials { get; private set; }
       
        public SoundPressureLevelField(TransmissionLossField transmissionLossField)
        {
            if (transmissionLossField.Radials.Length == 0) return;
           CalculateSPL();
        }

        public void CalculateSPL()
        {
            SPLRadials = new SoundPressureLevelRadial[Radials.Length];
            for (int index = 0; index < Radials.Length; index++)
            {
                var radial = Radials[index];
                for (int i = 0; i < radial.Depths.Length; i++)
                {
                    for (int j = 0; j < radial.Ranges.Length; j++)
                    {
                        //is it depths by radials or radials by depth?
                        SPLRadials[index].SoundPressureLevel[i, j] = SourceLevel - radial.TransmissionLoss[i, j];
                    }
                }
            }
        }

    }
}
