using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using ESME.Environment.NAVO;
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
        public SoundSpeedField this[NAVOTimePeriod timePeriod] { get { return SoundSpeedFields.Find(t => t.TimePeriod == timePeriod); } }

        public void Add(SoundSpeed newData)
        {
            foreach (var soundSpeedField in newData.SoundSpeedFields)
            {
                if (this[soundSpeedField.TimePeriod] != null) throw new DataException(string.Format("Unable to add SoundSpeedField for {0}. Data already present.", soundSpeedField.TimePeriod));
                SoundSpeedFields.Add(soundSpeedField);
            }
        }

        public static SoundSpeed Load(string filename)
        {
            return new SoundSpeed { SoundSpeedFields = XmlSerializer<List<SoundSpeedField>>.Load(filename, ReferencedTypes) };
        }

        public static SoundSpeed Load(string temperatureFilename, string salinityFilename, EarthCoordinate<float> deepestPoint = null, GeoRect areaOfInterest = null, BackgroundTask backgroundTask = null)
        {
            var temperatureData = Load(temperatureFilename);
            var salinityData = Load(salinityFilename);
            VerifyThatTimePeriodsMatch(temperatureData, salinityData);
            var soundSpeed = Create(temperatureData, salinityData, backgroundTask);
            if (deepestPoint == null) return soundSpeed;
            soundSpeed.Extend(temperatureData, salinityData, deepestPoint, areaOfInterest);
            foreach (var soundSpeedField in soundSpeed.SoundSpeedFields)
                soundSpeedField.Extend(temperatureData[soundSpeedField.TimePeriod], salinityData[soundSpeedField.TimePeriod], deepestPoint, areaOfInterest);
            return soundSpeed;
        }

        public void Extend(SoundSpeed temperatureData, SoundSpeed salinityData, EarthCoordinate<float> deepestPoint, GeoRect areaOfInterest, BackgroundTask backgroundTask = null)
        {
            VerifyThatTimePeriodsMatch(this, temperatureData);
            VerifyThatTimePeriodsMatch(temperatureData, salinityData);

            foreach (var soundSpeedField in SoundSpeedFields)
                soundSpeedField.Extend(temperatureData[soundSpeedField.TimePeriod], salinityData[soundSpeedField.TimePeriod], deepestPoint, areaOfInterest, backgroundTask);
        }

        public static SoundSpeed Create(SoundSpeed temperatureData, SoundSpeed salinityData, BackgroundTask backgroundTask = null)
        {
            VerifyThatTimePeriodsMatch(temperatureData, salinityData);

            var soundSpeedFile = new SoundSpeed();
            foreach (var temperatureField in temperatureData.SoundSpeedFields)
                soundSpeedFile.SoundSpeedFields.Add(SoundSpeedField.Create(temperatureField, salinityData[temperatureField.TimePeriod], backgroundTask));
            return soundSpeedFile;
        }

        public static SoundSpeed Average(SoundSpeed monthlySoundSpeeds, List<NAVOTimePeriod> timePeriods, BackgroundTask backgroundTask = null)
        {
            var result = new SoundSpeed();
            if (backgroundTask != null) backgroundTask.Maximum += timePeriods.Count;
            foreach (var timePeriod in timePeriods)
            {
                if (backgroundTask != null) backgroundTask.Status = string.Format("Averaging soundspeeds for {0}", timePeriod);
                var months = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod);
                var accumulator = new SoundSpeedFieldAverager { TimePeriod = timePeriod };
                foreach (var month in months)
                    accumulator.Add(monthlySoundSpeeds[month]);
                result.SoundSpeedFields.Add(accumulator.Average);
                if ((backgroundTask != null) && backgroundTask.CancellationPending) return result;
            }
            return result;
        }

        public static SoundSpeedField Average(SoundSpeed monthlySoundSpeeds, NAVOTimePeriod timePeriod, BackgroundTask backgroundTask = null)
        {
            if (backgroundTask != null) backgroundTask.Status = string.Format("Averaging soundspeeds for {0}", timePeriod);
            var months = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod).ToList();
            if (backgroundTask != null) backgroundTask.Maximum += months.Count;
            var accumulator = new SoundSpeedFieldAverager { TimePeriod = timePeriod };
            foreach (var month in months)
            {
                accumulator.Add(monthlySoundSpeeds[month]);
                if (backgroundTask != null) backgroundTask.Value++;
            }
            return accumulator.Average;
        }

        internal static void VerifyThatTimePeriodsMatch(SoundSpeed data1, SoundSpeed data2)
        {
            foreach (var field1 in data1.SoundSpeedFields.Where(field1 => data2[field1.TimePeriod] == null)) throw new DataException(string.Format("SoundSpeeds do not contain the same time periods. Data 1 has time period {0}, data 2 does not", field1.TimePeriod));
            foreach (var field2 in data2.SoundSpeedFields.Where(field2 => data1[field2.TimePeriod] == null)) throw new DataException(string.Format("SoundSpeeds do not contain the same time periods. Data 2 has time period {0}, data 1 does not", field2.TimePeriod));
        }
    }
}