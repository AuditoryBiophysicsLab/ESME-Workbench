using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Xml.Serialization;
using ESME.Environment.NAVO;
using HRC.Navigation;
using HRC.Utility;

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

        public void ExtendProfiles(TimePeriodEnvironmentData<SoundSpeedProfile> temperatureData, TimePeriodEnvironmentData<SoundSpeedProfile> salinityData)
        {
            if (DeepestPoint == null) throw new ApplicationException("SoundSpeedField: Unable to extend to max bathymetry depth. Deepest point is not set.");

            if ((temperatureData == null) || (salinityData == null))
                throw new ApplicationException("SoundSpeedField: Unable to extend to max bathymetry depth. Temperature and/or salinity data are missing.");

            if (DeepestPoint.Data > DeepestSSP.Data.MaxDepth)
                DeepestSSP.Extend(DeepestPoint.Data, temperatureData.EnvironmentData[DeepestSSP], salinityData.EnvironmentData[DeepestSSP]);

            foreach (var profile in EnvironmentData.Where(profile => profile != DeepestSSP))
                profile.Extend(DeepestSSP);
        }

        public static SoundSpeedField Create(SoundSpeedField temperatureField, SoundSpeedField salinityField, BackgroundTask backgroundTask = null)
        {
            if (temperatureField.TimePeriod != salinityField.TimePeriod) throw new DataException("");
            VerifyThatProfilePointsMatch(temperatureField, salinityField);
            var result = new SoundSpeedField { TimePeriod = temperatureField.TimePeriod };
            foreach (var temperatureProfile in temperatureField.EnvironmentData)
                result.EnvironmentData.Add(ChenMilleroLi.SoundSpeed(temperatureProfile, salinityField.EnvironmentData[temperatureProfile]));
            return result;
        }

        public SoundSpeedField Extend(SoundSpeedField sourceTemperatureField, SoundSpeedField sourceSalinityField, EarthCoordinate<float> deepestPoint, GeoRect areaOfInterest = null, BackgroundTask backgroundTask = null)
        {
            if ((TimePeriod != sourceTemperatureField.TimePeriod) || (TimePeriod != sourceSalinityField.TimePeriod))
                throw new DataException("");
            var temperatureData = sourceTemperatureField.EnvironmentData;
            temperatureData.TrimToNearestPoints(areaOfInterest);
            var temperatureField = new SoundSpeedField { EnvironmentData = temperatureData };

            var salinityData = sourceSalinityField.EnvironmentData;
            salinityData.TrimToNearestPoints(areaOfInterest);
            var salinityField = new SoundSpeedField { EnvironmentData = salinityData };

            var soundSpeedData = EnvironmentData;
            if (areaOfInterest != null) soundSpeedData.TrimToNearestPoints(areaOfInterest);
            var soundSpeedField = new SoundSpeedField
            {
                EnvironmentData = soundSpeedData,
                DeepestPoint = deepestPoint,
                TimePeriod = TimePeriod
            };

            VerifyThatProfilePointsMatch(temperatureField, salinityField);
            VerifyThatProfilePointsMatch(temperatureField, soundSpeedField);

            soundSpeedField.ExtendProfiles(temperatureField, salinityField);
            return soundSpeedField;
        }

        internal static void VerifyThatProfilePointsMatch(TimePeriodEnvironmentData<SoundSpeedProfile> profile1, TimePeriodEnvironmentData<SoundSpeedProfile> profile2)
        {
            foreach (var point1 in profile1.EnvironmentData.Where(point1 => !profile2.EnvironmentData.Any(point1.Equals))) throw new DataException(string.Format("Profiles do not contain the same data points.  One has data at {0}, the other does not", point1));
            foreach (var point2 in profile2.EnvironmentData.Where(point2 => !profile1.EnvironmentData.Any(point2.Equals))) throw new DataException(string.Format("Profiles do not contain the same data points.  One has data at {0}, the other does not", point2));
        }
    }
}
