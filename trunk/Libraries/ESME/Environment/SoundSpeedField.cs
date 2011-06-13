using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml.Serialization;
using HRC.Navigation;

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

        public SoundSpeedField() { DeepestPoint = null; }

        [XmlIgnore]
        public EarthCoordinate<float> DeepestPoint { get; set; }

        [XmlIgnore]
        public SoundSpeedProfile DeepestSSP { get { return DeepestPoint != null ? EnvironmentData[DeepestPoint] : null; } }

        public void ExtendProfiles(SoundSpeedField temperatureData, SoundSpeedField salinityData)
        {
            if (DeepestPoint == null) throw new ApplicationException("SoundSpeedField: Unable to extend to max bathymetry depth. Deepest point is not set.");

            if ((temperatureData == null) || (salinityData == null))
                throw new ApplicationException("SoundSpeedField: Unable to extend to max bathymetry depth. Temperature and/or salinity data are missing.");

            if (DeepestPoint.Data > DeepestSSP.Data.MaxDepth)
                DeepestSSP.Extend(DeepestPoint.Data, temperatureData.EnvironmentData[DeepestSSP], salinityData.EnvironmentData[DeepestSSP]);

            foreach (var profile in EnvironmentData.Where(profile => profile != DeepestSSP))
                profile.Extend(DeepestSSP);
        }
    }
}
