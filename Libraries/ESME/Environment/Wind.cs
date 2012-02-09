using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using ESME.Environment.NAVO;
using HRC.Navigation;

namespace ESME.Environment
{
    [Serializable]
    public class Wind : EnvironmentDataSetBase
    {
        public List<TimePeriodEnvironmentData<WindSample>> TimePeriods { get; set; }

        public Wind()
        {
            TimePeriods = new List<TimePeriodEnvironmentData<WindSample>>();
        }

        public static Task<Wind> LoadAsync(string filename)
        {
            return TaskEx.Run(() => Load(filename));
        }

        public static Wind Load(string filename)
        {
            //return new Wind { TimePeriods = XmlSerializer<List<TimePeriodEnvironmentData<WindSample>>>.Load(filename, ReferencedTypes) };
            //var formatter = new BinaryFormatter();
            //using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
            //{
            //    return new Wind { TimePeriods = (List<TimePeriodEnvironmentData<WindSample>>)formatter.Deserialize(stream) };
            //}
            using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
            using (var reader = new BinaryReader(stream)) return Deserialize(reader);
        }

        public override void Save(string filename)
        {
            //var serializer = new XmlSerializer<List<TimePeriodEnvironmentData<WindSample>>> { Data = TimePeriods };
            //serializer.Save(filename, ReferencedTypes);
            //var formatter = new BinaryFormatter();
            //using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None))
            //{
            //    formatter.Serialize(stream, TimePeriods);
            //}
            Serialize(filename);
        }

        public void Serialize(string filename)
        {
            using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None))
            using (var writer = new BinaryWriter(stream))
                Serialize(writer);
        }

        public void Serialize(BinaryWriter writer)
        {
            writer.Write(TimePeriods.Count);
            foreach (var period in TimePeriods)
            {
                writer.Write((int)period.TimePeriod);
                writer.Write(period.EnvironmentData.Count);
                foreach (var item in period.EnvironmentData)
                    item.Serialize(writer);
            }
        }

        public static Wind Deserialize(BinaryReader reader)
        {
            var result = new Wind();
            var periodCount = reader.ReadInt32();
            for (var i = 0; i < periodCount; i++)
            {
                var curPeriod = new TimePeriodEnvironmentData<WindSample>
                {
                    TimePeriod = (NAVOTimePeriod)reader.ReadInt32(),
                };
                var sampleCount = reader.ReadInt32();
                for (var j = 0; j < sampleCount; j++)
                    curPeriod.EnvironmentData.Add(WindSample.Deserialize(reader));
                result.TimePeriods.Add(curPeriod);
            }
            return result;
        }

        /// <summary>
        /// Get the data from the specified time period, if available.  If no data are available, NULL is returned.
        /// </summary>
        /// <param name="timePeriod"></param>
        /// <returns></returns>
        public TimePeriodEnvironmentData<WindSample> this[NAVOTimePeriod timePeriod]
        {
            get
            {
                var result = TimePeriods.Find(t => t.TimePeriod == timePeriod);
                if (result != null) return result;

                var allMonths = new List<NAVOTimePeriod>(NAVOConfiguration.AllMonths);
                // If the data is for a month, we can't synthesize it if we don't already have it available
                if (allMonths.Contains(timePeriod)) throw new IndexOutOfRangeException(string.Format("Wind data for the requested time period {0} is not available", timePeriod));
                var requestedData = SeasonalAverage(timePeriod);
                TimePeriods.Add(requestedData);
                return requestedData;
            }
        }

        public TimePeriodEnvironmentData<WindSample> SeasonalAverage(NAVOTimePeriod season)
        {
            // Make sure the requested time period is, in fact, a season.
            if (!NAVOConfiguration.AllSeasons.Contains(season)) throw new ArgumentException(season + " is not a season", "season");
            // Create a list of the time periods that are available
            var availableTimePeriods = TimePeriods.Select(item => item.TimePeriod).Distinct().ToList();
            // Create a list of the months we're going to need in order to calculate the requested seasonal average
            var targetMonths = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(season).ToList();
            // Make sure we've got the data we need
            if (targetMonths.Any(targetMonth => !availableTimePeriods.Contains(targetMonth))) throw new ArgumentException("sourceData does not contain the required monthly data to create an average for the requested season.");
            // Create a list of the required source data we're going to average
            var requiredSources = TimePeriods.Where(source => targetMonths.Contains(source.TimePeriod)).ToList();
            // Create a list of the locations that are going to be present in the average.
            // This is created from the list of unique points across all required source data
            // The datum at each location is initialized to float.NaN so we can tell if there is no actual data at any given location
            var requiredLocations = (from source in requiredSources
                                     from location in source.EnvironmentData
                                     select new WindSample(location, float.NaN)).Distinct().ToList();
            // Create the object that will hold our averaged data
            var environmentData = new EnvironmentData<WindSample>();
            // Add the list of locations to the EnvironmentData object
            environmentData.AddRange(requiredLocations);
            // Create the object that will be returned to the caller
            var result = new TimePeriodEnvironmentData<WindSample>
            {
                TimePeriod = season,    // This is the season for which we're creating this average
                EnvironmentData = environmentData
            };
            // Set each data point in the result set to the average of each corresponding non-null point in the source data
            var sourceList = new List<float>();
            foreach (var location in result.EnvironmentData)
            {
                // Clear the list for each location
                sourceList.Clear();
                foreach (var source in requiredSources)
                {
                    WindSample sample;
                    // If a given source month contains data for the current location, add the data to the list
                    if (source.EnvironmentData.TryGetExactPoint(location, out sample)) sourceList.Add(sample.Data);
                }
                // Set the resulting data to the average of the sample values that are present for the current location in the specified source months
                if (sourceList.Count > 0) location.Data = sourceList.Average();
            }
            return result;
        }


    }

    [Serializable]
    public class WindSample : EarthCoordinate<float>
    {
        public WindSample() {  }
        public WindSample(Geo location, float sample) : base(location.Latitude, location.Longitude, sample) {  }

        public new void Serialize(BinaryWriter writer)
        {
            base.Serialize(writer);
            writer.Write(Data);
        }

        public new static WindSample Deserialize(BinaryReader reader)
        {
            return new WindSample(Geo.Deserialize(reader), reader.ReadSingle());
        }
    }
}
