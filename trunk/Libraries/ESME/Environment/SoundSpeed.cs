using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Linq;
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

        public static SoundSpeed Create(SoundSpeed temperatureData, SoundSpeed salinityData)
        {
            VerifyThatTimePeriodsMatch(temperatureData, salinityData);

            var soundSpeedFile = new SoundSpeed();
            foreach (var temperatureField in temperatureData.SoundSpeedFields)
            {
                var salinityField = salinityData[temperatureField.TimePeriod];
                SoundSpeedField.VerifyThatProfilePointsMatch(temperatureField, salinityField);
                var field = new SoundSpeedField { TimePeriod = temperatureField.TimePeriod };
                foreach (var temperatureProfile in temperatureField.EnvironmentData)
                    field.EnvironmentData.Add(ChenMilleroLi.SoundSpeed(temperatureProfile, salinityField.EnvironmentData[temperatureProfile]));
                soundSpeedFile.SoundSpeedFields.Add(field);
            }
            return soundSpeedFile;
        }

        public delegate void MessageDelegate(string message);

        public static SoundSpeed Average(SoundSpeed monthlySoundSpeeds, List<NAVOTimePeriod> timePeriods, Delegates.Delegate<string> averageOperationMessage = null, BackgroundWorker backgroundWorker = null)
        {
            var result = new SoundSpeed();
            foreach (var timePeriod in timePeriods) 
            {
                if (averageOperationMessage != null) averageOperationMessage(string.Format("Averaging soundspeeds for {0}", timePeriod));
                var months = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod);
                var accumulator = new SoundSpeedFieldAverager { TimePeriod = timePeriod };
                foreach (var month in months)
                    accumulator.Add(monthlySoundSpeeds[month]);
                result.SoundSpeedFields.Add(accumulator.Average);
                if ((backgroundWorker != null) && backgroundWorker.CancellationPending) return result;
            }
            return result;
        }

        internal static void VerifyThatTimePeriodsMatch(SoundSpeed data1, SoundSpeed data2)
        {
            foreach (var field1 in data1.SoundSpeedFields.Where(field1 => data2[field1.TimePeriod] == null)) throw new DataException(string.Format("SoundSpeeds do not contain the same time periods. Data 1 has time period {0}, data 2 does not", field1.TimePeriod));
            foreach (var field2 in data2.SoundSpeedFields.Where(field2 => data1[field2.TimePeriod] == null)) throw new DataException(string.Format("SoundSpeeds do not contain the same time periods. Data 2 has time period {0}, data 1 does not", field2.TimePeriod));
        }

    }
}