using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml.Serialization;

namespace ESME.Environment
{
    public class SoundSpeedFieldAverager : TimePeriodEnvironmentData<SoundSpeedProfileAverager>
    {
        public void Add(SoundSpeedField sourceField)
        {
            foreach (var sourceProfile in sourceField.EnvironmentData)
            {
                if (EnvironmentData[sourceProfile] == null) EnvironmentData.Add(new SoundSpeedProfileAverager(sourceProfile));
                EnvironmentData[sourceProfile].Add(sourceProfile);
            }
        }

        public SoundSpeedField Average
        {
            get
            {
                var result = new SoundSpeedField {TimePeriod = TimePeriod};
                foreach (var averageProfile in EnvironmentData)
                    result.EnvironmentData.Add(averageProfile.Average);
                return result;
            }
        }
    }

    public class SoundSpeedField : TimePeriodEnvironmentData<SoundSpeedProfile>
    {
        new public static readonly List<Type> ReferencedTypes = new List<Type>(TimePeriodEnvironmentData<SoundSpeedProfile>.ReferencedTypes);

        [XmlIgnore]
        public SoundSpeedProfile DeepestSSP { get; set; }

        public void ExtendProfilesToDepth(float maxDepth, SoundSpeedField temperatureData, SoundSpeedField salinityData)
        {
            if ((temperatureData == null) || (salinityData == null))
                throw new ApplicationException("SoundSpeedField: Unable to extend to max bathymetry depth.  Temperature and/or salinity data are missing.");

            if (maxDepth > DeepestSSP.Data.MaxDepth)
                DeepestSSP.Extend(maxDepth, temperatureData.EnvironmentData[DeepestSSP], salinityData.EnvironmentData[DeepestSSP]);

            foreach (var profile in EnvironmentData.Where(profile => profile != DeepestSSP))
                profile.Extend(DeepestSSP);
        }
    }
}
