using System;
using System.Collections.Generic;
using System.Linq;
using ESME.Environment.NAVO;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment
{
    public class Wind
    {
        static readonly List<Type> ReferencedTypes = new List<Type>(TimeBasedEnvironmentData<EarthCoordinate<float>>.ReferencedTypes);
        public TimeBasedEnvironmentData<EarthCoordinate<float>> TimePeriods { get; private set; }

        public Wind()
        {
            TimePeriods = new TimeBasedEnvironmentData<EarthCoordinate<float>>();
        }

        public static Wind Load(string filename)
        {
            return new Wind { TimePeriods = XmlSerializer<TimeBasedEnvironmentData<EarthCoordinate<float>>>.Load(filename, ReferencedTypes) };
        }

        public void Save(string filename)
        {
            var serializer = new XmlSerializer<TimeBasedEnvironmentData<EarthCoordinate<float>>> {Data = TimePeriods};
            serializer.Save(filename, ReferencedTypes);
        }
    }

    public class TimePeriodEnvironmentData<T> : EnvironmentData<T> where T : EarthCoordinate, new()
    {
        new public static readonly List<Type> ReferencedTypes = new List<Type>(EnvironmentData<T>.ReferencedTypes);

        public NAVOTimePeriod TimePeriod { get; set; }
    }

    public class TimeBasedEnvironmentData<T> : List<TimePeriodEnvironmentData<T>> where T : EarthCoordinate, new()
    {
        public static readonly List<Type> ReferencedTypes = new List<Type>(TimePeriodEnvironmentData<T>.ReferencedTypes) { typeof (NAVOTimePeriod) };

        /// <summary>
        /// This interface is not implemented.  Do not use, an exception will be thrown.
        /// </summary>
        /// <param name="index"></param>
        /// <returns></returns>
        new public TimePeriodEnvironmentData<T> this[int index]
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }

        /// <summary>
        /// Get the data from the specified time period, if available.  If no data are available, NULL is returned.
        /// </summary>
        /// <param name="timePeriod"></param>
        /// <returns></returns>
        public T this[NAVOTimePeriod timePeriod]
        {
            get { return Find(t => t.TimePeriod == timePeriod).FirstOrDefault(); }
        }
    }
}
