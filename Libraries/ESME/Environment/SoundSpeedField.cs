using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using HRC.Navigation;

namespace ESME.Environment
{
    public class SoundSpeedFieldAverager : TimePeriodEnvironmentData<SoundSpeedProfileAverager>
    {
        public void Add(SoundSpeedField sourceField)
        {
            var environmentDataList = EnvironmentData.ToList();
            var itemsToAdd = new List<SoundSpeedProfileAverager>();
            foreach (var sourceProfile in sourceField.EnvironmentData)
            {
                var profile = environmentDataList.Find(p => p.Equals(sourceProfile));
                if (profile == null) itemsToAdd.Add(new SoundSpeedProfileAverager(sourceProfile));
                else profile.Add(sourceProfile);
            }
            EnvironmentData.AddRange(itemsToAdd);
        }

        public SoundSpeedField Average
        {
            get
            {
                var environmentData = EnvironmentData.Select(averageProfile => averageProfile.Average);
                var result = new SoundSpeedField { TimePeriod = TimePeriod };
                result.EnvironmentData.AddRange(environmentData);
                return result;
            }
        }
    }

    [Serializable]
    public class SoundSpeedField : TimePeriodEnvironmentData<SoundSpeedProfile>
    {
        public SoundSpeedProfile GetDeepestSSP(Geo<float> deepestPoint)
        {
            var step1 = from ssp in EnvironmentData
                        orderby ssp.Data.Count
                        select ssp;
            var maxCount = step1.Last().Data.Count;
            var step2 = from ssp in step1
                        where ssp.Data.Count == maxCount
                        orderby ssp.DistanceKilometers(deepestPoint)
                        select ssp;
            return step2.First();
        }

        public void ExtendProfiles(Geo<float> deepestPoint)
        {
            if (deepestPoint == null) throw new ArgumentNullException("deepestPoint", "SoundSpeedField: Unable to extend to max bathymetry depth. deepestPoint is null.");
            var deepestSSP = GetDeepestSSP(deepestPoint);
            if (deepestPoint.Data > deepestSSP.MaxDepth)
                deepestSSP.Extend(deepestPoint.Data);

            foreach (var profile in EnvironmentData.Where(profile => profile != deepestSSP))
                profile.Extend(deepestSSP);
        }
#if false
        public static SoundSpeedField<T> Create(SoundSpeedField<T> temperatureField, SoundSpeedField<T> salinityField)
        {
            if (temperatureField.TimePeriod != salinityField.TimePeriod) throw new DataException("");
            VerifyThatProfilePointsMatch(temperatureField, salinityField);
            var environmentData = temperatureField.EnvironmentData.Select(temperatureProfile => ChenMilleroLi.SoundSpeed(temperatureProfile, salinityField.EnvironmentData.GetNearestPoint(temperatureProfile)));
            var result = new SoundSpeedField { TimePeriod = temperatureField.TimePeriod };
            result.EnvironmentData.AddRange(environmentData);
            return result;
        }

        public static SoundSpeedField<T> Create(SoundSpeedField<T> sourceTemperatureField, SoundSpeedField<T> sourceSalinityField, EarthCoordinate<float> deepestPoint, GeoRect areaOfInterest = null)
        {
            if (sourceTemperatureField.TimePeriod != sourceSalinityField.TimePeriod)
                throw new DataException("");
            var temperatureData = sourceTemperatureField.EnvironmentData;
            if (areaOfInterest != null) temperatureData.TrimToNearestPoints(areaOfInterest);
            var temperatureField = new SoundSpeedField<T> { EnvironmentData = temperatureData };

            var salinityData = sourceSalinityField.EnvironmentData;
            if (areaOfInterest != null) salinityData.TrimToNearestPoints(areaOfInterest);
            var salinityField = new SoundSpeedField<T> { EnvironmentData = salinityData };

            //VerifyThatProfilePointsMatch(temperatureField, salinityField);
            var environmentData = new List<SoundSpeedProfile<T>();
            //var sb = new StringBuilder();
            foreach (var temperatureProfile in temperatureField.EnvironmentData)
            {
                var salinityProfile = salinityField.EnvironmentData.GetExactPoint(temperatureProfile);
                if (salinityProfile == null) continue;
                var profile = ChenMilleroLi.SoundSpeed(temperatureProfile, salinityProfile);
                environmentData.Add(profile);
                if (profile.Messages.Count > 0) foreach (var message in profile.Messages)
                        Debug.WriteLine("[{0}, {1}] ({2}): {3}", temperatureProfile.Latitude, temperatureProfile.Longitude, sourceTemperatureField.TimePeriod, message);
            }
            //var environmentData = temperatureField.EnvironmentData.Select(temperatureProfile => ChenMilleroLi.SoundSpeed(temperatureProfile, salinityField.EnvironmentData[temperatureProfile]));
            var soundSpeedData = new SoundSpeedField<T> { TimePeriod = temperatureField.TimePeriod };
            soundSpeedData.EnvironmentData.AddRange(environmentData);

            if (deepestPoint == null) return soundSpeedData;

            if (areaOfInterest != null) soundSpeedData.EnvironmentData.TrimToNearestPoints(areaOfInterest);
            var soundSpeedField = new SoundSpeedField<T>
            {
                EnvironmentData = soundSpeedData.EnvironmentData,
                DeepestPoint = deepestPoint,
                TimePeriod = soundSpeedData.TimePeriod
            };
            //VerifyThatProfilePointsMatch(temperatureField, salinityField);
            //VerifyThatProfilePointsMatch(temperatureField, soundSpeedField);

            soundSpeedField.ExtendProfiles(temperatureField, salinityField);
            soundSpeedField.TimePeriod = sourceTemperatureField.TimePeriod;
            return soundSpeedField;
        }

        public SoundSpeedField<T> Extend(SoundSpeedField<T> sourceTemperatureField, SoundSpeedField<T> sourceSalinityField, EarthCoordinate<float> deepestPoint, GeoRect areaOfInterest = null)
        {
            if ((TimePeriod != sourceTemperatureField.TimePeriod) || (TimePeriod != sourceSalinityField.TimePeriod))
                throw new DataException("");
            var temperatureData = sourceTemperatureField.EnvironmentData;
            if (areaOfInterest != null) temperatureData.TrimToNearestPoints(areaOfInterest);
            var temperatureField = new SoundSpeedField<T> { EnvironmentData = temperatureData };

            var salinityData = sourceSalinityField.EnvironmentData;
            if (areaOfInterest != null) salinityData.TrimToNearestPoints(areaOfInterest);
            var salinityField = new SoundSpeedField<T> { EnvironmentData = salinityData };

            var soundSpeedData = EnvironmentData;
            if (areaOfInterest != null) soundSpeedData.TrimToNearestPoints(areaOfInterest);
            var soundSpeedField = new SoundSpeedField<T>
            {
                EnvironmentData = soundSpeedData,
                DeepestPoint = deepestPoint,
                TimePeriod = TimePeriod
            };

            VerifyThatProfilePointsMatch(temperatureField, salinityField);
            VerifyThatProfilePointsMatch(temperatureField, soundSpeedField);

            soundSpeedField.ExtendProfiles(temperatureField, salinityField);
            soundSpeedField.TimePeriod = sourceTemperatureField.TimePeriod;
            return soundSpeedField;
        }

        internal static void VerifyThatProfilePointsMatch(TimePeriodEnvironmentData<SoundSpeedProfile<T>> profile1, TimePeriodEnvironmentData<SoundSpeedProfile<T>> profile2)
        {
            foreach (var point1 in profile1.EnvironmentData.Where(point1 => !profile2.EnvironmentData.Any(point1.Equals))) throw new DataException(string.Format("Profiles do not contain the same data points.  One has data at ({0:0.0000}, {1:0.0000}), the other does not", point1.Latitude, point1.Longitude));
            foreach (var point2 in profile2.EnvironmentData.Where(point2 => !profile1.EnvironmentData.Any(point2.Equals))) throw new DataException(string.Format("Profiles do not contain the same data points.  One has data at ({0:0.0000}, {1:0.0000}), the other does not", point2.Latitude, point2.Longitude));
        }
#endif

        public static SoundSpeedField Deserialize(BinaryReader reader)
        {
            var result = new SoundSpeedField
            {
                TimePeriod = (TimePeriod)reader.ReadInt32(),
            };
            var itemCount = reader.ReadInt32();
            for (var i = 0; i < itemCount; i++ )
                result.EnvironmentData.Add(SoundSpeedProfile.Deserialize(reader));
            return result;
        }

        public void Serialize(BinaryWriter writer)
        {
            writer.Write((int)TimePeriod);
            writer.Write(EnvironmentData.Count);
            foreach (var item in EnvironmentData)
                item.Serialize(writer);
        }
    }
}
