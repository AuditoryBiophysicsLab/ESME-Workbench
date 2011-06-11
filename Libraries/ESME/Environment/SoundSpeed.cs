using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using ESME.Environment.NAVO;
using ESME.Model;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment
{
    public class SoundSpeed
    {
        static readonly List<Type> ReferencedTypes = new List<Type>(SoundSpeedField.ReferencedTypes);

        public List<SoundSpeedField> SoundSpeedFields { get; set; }

        public SoundSpeed()
        {
            SoundSpeedFields = new List<SoundSpeedField>();
        }

        public static SoundSpeed Load(string filename)
        {
            return new SoundSpeed { SoundSpeedFields = XmlSerializer<List<SoundSpeedField>>.Load(filename, ReferencedTypes) };
        }

        public void Save(string filename)
        {
            var serializer = new XmlSerializer<List<SoundSpeedField>> { Data = SoundSpeedFields };
            serializer.Save(filename, ReferencedTypes);
        }

        /// <summary>
        /// Get the data from the specified time period, if available.  If no data are available, NULL is returned.
        /// </summary>
        /// <param name="timePeriod"></param>
        /// <returns></returns>
        public SoundSpeedField this[NAVOTimePeriod timePeriod]
        {
            get { return SoundSpeedFields.Find(t => t.TimePeriod == timePeriod); }
        }
    }

    public class SoundSpeedField : TimePeriodEnvironmentData<SoundSpeedProfile>
    {
        new public static readonly List<Type> ReferencedTypes = new List<Type>(TimePeriodEnvironmentData<SoundSpeedProfile>.ReferencedTypes);

        [XmlIgnore]
        public SoundSpeedProfile DeepestSSP { get; set; }

        public SoundSpeedField(SerializedOutput serializedOutput, NAVOTimePeriod timePeriod)
        {
            TimePeriod = timePeriod;
            foreach (var profile in serializedOutput.DataPoints)
                EnvironmentData.Add(new SoundSpeedProfile(profile, serializedOutput.DepthAxis));
            foreach (var profile in EnvironmentData) DeepestSSP = (DeepestSSP != null) ? (DeepestSSP.MaxDepth < profile.MaxDepth ? profile : DeepestSSP) : profile;
        }

        public SoundSpeedField(SerializedOutput serializedOutput, NAVOTimePeriod timePeriod, EarthCoordinate deepestPoint)
        {
            TimePeriod = timePeriod;
            foreach (var profile in serializedOutput.DataPoints)
                EnvironmentData.Add(new SoundSpeedProfile(profile, serializedOutput.DepthAxis));
            var minDistance = double.MaxValue;
            foreach (var profile in EnvironmentData)
            {
                if (DeepestSSP == null) DeepestSSP = profile;
                if (profile.DistanceTo(deepestPoint) >= minDistance) continue;
                if (profile.MaxDepth < DeepestSSP.MaxDepth) continue;
                minDistance = profile.DistanceTo(deepestPoint);
                DeepestSSP = profile;
            }
        }

    }

}