using System;
using System.Collections.Generic;
using ESME.Environment.NAVO;
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
}