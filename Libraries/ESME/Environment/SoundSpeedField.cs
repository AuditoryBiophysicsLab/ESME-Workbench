using System;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using System.Xml.Serialization;
using ESME.Environment.NAVO;
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
        public SoundSpeedField() { DeepestPoint = null; }

        [XmlIgnore]
        public EarthCoordinate<float> DeepestPoint { get; set; }

        [XmlIgnore]
        public SoundSpeedProfile DeepestSSP
        {
            get
            {
                var step1 = from ssp in EnvironmentData
                            orderby ssp.Data.Count
                            select ssp;
                var maxCount = step1.Last().Data.Count;
                var step2 = from ssp in step1
                            where ssp.Data.Count == maxCount
                            orderby ssp.DistanceTo(DeepestPoint)
                            select ssp;
                return step2.First();
                //return DeepestPoint != null ? EnvironmentData[DeepestPoint] : null;
            }
        }

        public async void ExtendProfilesAsync(TimePeriodEnvironmentData<SoundSpeedProfile> temperatureData, TimePeriodEnvironmentData<SoundSpeedProfile> salinityData)
        {
            if (DeepestPoint == null) throw new ApplicationException("SoundSpeedField: Unable to extend to max bathymetry depth. Deepest point is not set.");

            if ((temperatureData == null) || (salinityData == null))
                throw new ApplicationException("SoundSpeedField: Unable to extend to max bathymetry depth. Temperature and/or salinity data are missing.");

            if (DeepestPoint.Data > DeepestSSP.Data.MaxDepth)
                DeepestSSP.Extend(DeepestPoint.Data, temperatureData.EnvironmentData[DeepestSSP], salinityData.EnvironmentData[DeepestSSP]);

            var extendBlock = new ActionBlock<SoundSpeedProfile>(ssp => ssp.Extend(DeepestSSP), new ExecutionDataflowBlockOptions
            {
                MaxDegreeOfParallelism = -1,
                BoundedCapacity = -1,
                TaskScheduler = TaskScheduler.Default,
            });
            var extendBuffer = new BufferBlock<SoundSpeedProfile>();
            extendBuffer.LinkTo(extendBlock);
            extendBuffer.Completion.ContinueWith(task => extendBlock.Complete());

            foreach (var profile in EnvironmentData.Where(profile => profile != DeepestSSP)) extendBuffer.Post(profile);
            extendBuffer.Complete();
            await extendBlock.Completion;
        }

        public static SoundSpeedField Create(SoundSpeedField temperatureField, SoundSpeedField salinityField)
        {
            if (temperatureField.TimePeriod != salinityField.TimePeriod) throw new DataException("");
            VerifyThatProfilePointsMatch(temperatureField, salinityField);
            var environmentData = temperatureField.EnvironmentData.Select(temperatureProfile => ChenMilleroLi.SoundSpeed(temperatureProfile, salinityField.EnvironmentData[temperatureProfile]));
            var result = new SoundSpeedField { TimePeriod = temperatureField.TimePeriod };
            result.EnvironmentData.AddRange(environmentData);
            return result;
        }

        public static async Task<SoundSpeedField> CreateAsync(SoundSpeedField sourceTemperatureField, SoundSpeedField sourceSalinityField, EarthCoordinate<float> deepestPoint = null, GeoRect areaOfInterest = null)
        {
            if (sourceTemperatureField.TimePeriod != sourceSalinityField.TimePeriod)
                throw new DataException("");
            var temperatureData = sourceTemperatureField.EnvironmentData;
            if (areaOfInterest != null) temperatureData.TrimToNearestPoints(areaOfInterest);
            var temperatureField = new SoundSpeedField { EnvironmentData = temperatureData };

            var salinityData = sourceSalinityField.EnvironmentData;
            if (areaOfInterest != null) salinityData.TrimToNearestPoints(areaOfInterest);
            var salinityField = new SoundSpeedField { EnvironmentData = salinityData };

            VerifyThatProfilePointsMatch(temperatureField, salinityField);
            var environmentData = new List<SoundSpeedProfile>();
            //var sb = new StringBuilder();
            var sspBlock = new ActionBlock<SoundSpeedProfile>(temperatureProfile =>
            {
                var salinityProfile = salinityField.EnvironmentData[temperatureProfile];
                var profile = ChenMilleroLi.SoundSpeed(temperatureProfile, salinityProfile);
                environmentData.Add(profile);
                //if (profile != null && profile.Messages != null && profile.Messages.Count > 0) foreach (var message in profile.Messages)
                //        Debug.WriteLine("[{0}, {1}] ({2}): {3}", temperatureProfile.Latitude, temperatureProfile.Longitude, sourceTemperatureField.TimePeriod, message);
            }, new ExecutionDataflowBlockOptions
            {
                MaxDegreeOfParallelism = -1,
                BoundedCapacity = -1,
                TaskScheduler = TaskScheduler.Default,
            });
            var sspBuffer = new BufferBlock<SoundSpeedProfile>();
            sspBuffer.LinkTo(sspBlock);
            sspBuffer.Completion.ContinueWith(task => sspBlock.Complete());

            foreach (var temperatureProfile in temperatureField.EnvironmentData) sspBuffer.Post(temperatureProfile);
            sspBuffer.Complete();
            await sspBlock.Completion;

            //var environmentData = temperatureField.EnvironmentData.Select(temperatureProfile => ChenMilleroLi.SoundSpeed(temperatureProfile, salinityField.EnvironmentData[temperatureProfile]));
            var soundSpeedData = new SoundSpeedField { TimePeriod = temperatureField.TimePeriod };
            soundSpeedData.EnvironmentData.AddRange(environmentData);

            if (deepestPoint == null) return soundSpeedData;

            if (areaOfInterest != null) soundSpeedData.EnvironmentData.TrimToNearestPoints(areaOfInterest);
            var soundSpeedField = new SoundSpeedField
            {
                EnvironmentData = soundSpeedData.EnvironmentData,
                DeepestPoint = deepestPoint,
                TimePeriod = soundSpeedData.TimePeriod
            };
            //VerifyThatProfilePointsMatch(temperatureField, salinityField);
            //VerifyThatProfilePointsMatch(temperatureField, soundSpeedField);

            soundSpeedField.ExtendProfilesAsync(temperatureField, salinityField);
            soundSpeedField.TimePeriod = sourceTemperatureField.TimePeriod;
            return soundSpeedField;
        }

        public SoundSpeedField Extend(SoundSpeedField sourceTemperatureField, SoundSpeedField sourceSalinityField, EarthCoordinate<float> deepestPoint, GeoRect areaOfInterest = null)
        {
            if ((TimePeriod != sourceTemperatureField.TimePeriod) || (TimePeriod != sourceSalinityField.TimePeriod))
                throw new DataException("");
            var temperatureData = sourceTemperatureField.EnvironmentData;
            if (areaOfInterest != null) temperatureData.TrimToNearestPoints(areaOfInterest);
            var temperatureField = new SoundSpeedField { EnvironmentData = temperatureData };

            var salinityData = sourceSalinityField.EnvironmentData;
            if (areaOfInterest != null) salinityData.TrimToNearestPoints(areaOfInterest);
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

            soundSpeedField.ExtendProfilesAsync(temperatureField, salinityField);
            soundSpeedField.TimePeriod = sourceTemperatureField.TimePeriod;
            return soundSpeedField;
        }

        internal static void VerifyThatProfilePointsMatch(TimePeriodEnvironmentData<SoundSpeedProfile> profile1, TimePeriodEnvironmentData<SoundSpeedProfile> profile2)
        {
            foreach (var point1 in profile1.EnvironmentData.Where(point1 => !profile2.EnvironmentData.Any(point1.Equals))) throw new DataException(string.Format("Profiles do not contain the same data points.  One has data at ({0:0.0000}, {1:0.0000}), the other does not", point1.Latitude, point1.Longitude));
            foreach (var point2 in profile2.EnvironmentData.Where(point2 => !profile1.EnvironmentData.Any(point2.Equals))) throw new DataException(string.Format("Profiles do not contain the same data points.  One has data at ({0:0.0000}, {1:0.0000}), the other does not", point2.Latitude, point2.Longitude));
        }

        public static SoundSpeedField Deserialize(BinaryReader reader)
        {
            var result = new SoundSpeedField
            {
                TimePeriod = (NAVOTimePeriod)reader.ReadInt32(),
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
