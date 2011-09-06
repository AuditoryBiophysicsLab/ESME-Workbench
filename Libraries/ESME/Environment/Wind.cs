using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using ESME.Environment.NAVO;
using HRC.Navigation;

namespace ESME.Environment
{
    [Serializable]
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
            //return new Wind { TimePeriods = XmlSerializer<List<TimePeriodEnvironmentData<WindSample>>>.Load(filename, ReferencedTypes) };
            var formatter = new BinaryFormatter();
            using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
            {
                return new Wind { TimePeriods = (List<TimePeriodEnvironmentData<WindSample>>)formatter.Deserialize(stream) };
            }
        }

        public void Save(string filename)
        {
            //var serializer = new XmlSerializer<List<TimePeriodEnvironmentData<WindSample>>> { Data = TimePeriods };
            //serializer.Save(filename, ReferencedTypes);
            var formatter = new BinaryFormatter();
            using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None))
            {
                formatter.Serialize(stream, TimePeriods);
            }
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

    [Serializable]
    public class WindSample : EarthCoordinate<float>
    {
        public WindSample() {  }
        public WindSample(Geo location, float sample) : base(location.Latitude, location.Longitude, sample) {  }
    }
}
