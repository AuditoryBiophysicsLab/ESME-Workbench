using System;
using System.Collections.Generic;
using ESME.Environment.NAVO;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment
{
    public class Wind
    {
        static readonly List<Type> ReferencedTypes = new List<Type>(TimePeriodEnvironmentData<EarthCoordinate<float>>.ReferencedTypes);

        public List<TimePeriodEnvironmentData<EarthCoordinate<float>>> TimePeriods { get; set; }

        public Wind()
        {
            TimePeriods = new List<TimePeriodEnvironmentData<EarthCoordinate<float>>>();
        }

        public static Wind Load(string filename)
        {
            return new Wind { TimePeriods = XmlSerializer<List<TimePeriodEnvironmentData<EarthCoordinate<float>>>>.Load(filename, ReferencedTypes) };
        }

        public void Save(string filename)
        {
            var serializer = new XmlSerializer<List<TimePeriodEnvironmentData<EarthCoordinate<float>>>> { Data = TimePeriods };
            serializer.Save(filename, ReferencedTypes);
        }

        /// <summary>
        /// Get the data from the specified time period, if available.  If no data are available, NULL is returned.
        /// </summary>
        /// <param name="timePeriod"></param>
        /// <returns></returns>
        public TimePeriodEnvironmentData<EarthCoordinate<float>> this[NAVOTimePeriod timePeriod]
        {
            get { return TimePeriods.Find(t => t.TimePeriod == timePeriod); }
        }
    }
}
