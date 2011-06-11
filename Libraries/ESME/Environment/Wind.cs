using System;
using System.Collections.Generic;
using ESME.Environment.NAVO;
using ESME.Model;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment
{
    public class Wind
    {
        static readonly List<Type> ReferencedTypes = new List<Type>(TimePeriodEnvironmentData<WindSample>.ReferencedTypes);

        public List<TimePeriodEnvironmentData<WindSample>> TimePeriods { get; set; }

        public Wind()
        {
            TimePeriods = new List<TimePeriodEnvironmentData<WindSample>>();
        }

        public static Wind Load(string filename)
        {
            return new Wind { TimePeriods = XmlSerializer<List<TimePeriodEnvironmentData<WindSample>>>.Load(filename, ReferencedTypes) };
        }

        public void Save(string filename)
        {
            var serializer = new XmlSerializer<List<TimePeriodEnvironmentData<WindSample>>> { Data = TimePeriods };
            serializer.Save(filename, ReferencedTypes);
        }

        /// <summary>
        /// Get the data from the specified time period, if available.  If no data are available, NULL is returned.
        /// </summary>
        /// <param name="timePeriod"></param>
        /// <returns></returns>
        public TimePeriodEnvironmentData<WindSample> this[NAVOTimePeriod timePeriod]
        {
            get { return TimePeriods.Find(t => t.TimePeriod == timePeriod); }
        }
    }

    public class WindSample : EarthCoordinate<float>
    {
        public WindSample() {  }
        public WindSample(Geo location, float sample) : base(location.Latitude, location.Longitude, sample) {  }
    }
}
