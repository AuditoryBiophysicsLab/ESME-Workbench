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
#if false
        public void ExtendProfiles(Geo<float> deepestPoint)
        {
            if (deepestPoint == null) throw new ArgumentNullException("deepestPoint", "SoundSpeedField: Unable to extend to max bathymetry depth. deepestPoint is null.");
            var deepestSSP = GetDeepestSSP(deepestPoint);
            if (deepestPoint.Data > deepestSSP.MaxDepth)
                deepestSSP.Extend(deepestPoint.Data);

            foreach (var profile in EnvironmentData.Where(profile => profile != deepestSSP))
                profile.Extend(deepestSSP);
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
