
namespace ESME.TransmissionLoss
{
    class SoundPressureLevelField : TransmissionLossField
    {
        public SoundPressureLevelRadial[] SPLRadials { get; private set; }
       
        public SoundPressureLevelField(TransmissionLossField transmissionLossField)
        {
            if (transmissionLossField.Radials.Count == 0) return;
        }

        public void CalculateSPL()
        {
            SPLRadials = new SoundPressureLevelRadial[Radials.Count];
            for (var index = 0; index < Radials.Count; index++)
            {
                var radial = Radials[index];
                for (var i = 0; i < radial.Depths.Count; i++)
                {
                    for (var j = 0; j < radial.Ranges.Count; j++)
                    {
                        //is it depths by radials or radials by depth?
                        SPLRadials[index].SoundPressureLevel[i, j] = SourceLevel - radial.TransmissionLoss[i, j];
                    }
                }
            }
        }

    }
}
