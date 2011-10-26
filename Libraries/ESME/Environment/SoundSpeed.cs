using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading.Tasks;
using ESME.Environment.NAVO;
using HRC.Navigation;

namespace ESME.Environment
{
    [Serializable]
    public class SoundSpeed : IExtensibleDataObject, ICanSave
    {
        public List<SoundSpeedField> SoundSpeedFields { get; set; }

        public SoundSpeed()
        {
            SoundSpeedFields = new List<SoundSpeedField>();
        }

        public void Save(string filename)
        {
            //var serializer = new XmlSerializer<List<SoundSpeedField>> { Data = SoundSpeedFields };
            //serializer.Save(filename, ReferencedTypes);

            var formatter = new BinaryFormatter();
            using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None))
            {
                formatter.Serialize(stream, SoundSpeedFields);
            }
        }

        /// <summary>
        /// Get the data from the specified time period, if available.  If no data are available, NULL is returned.
        /// </summary>
        /// <param name="timePeriod"></param>
        /// <returns></returns>
        public SoundSpeedField this[NAVOTimePeriod timePeriod]
        {
            get
            {
                return SoundSpeedFields.Find(t => t.TimePeriod == timePeriod);
            }
        }

        public void Add(SoundSpeed newData)
        {
            foreach (var soundSpeedField in newData.SoundSpeedFields)
            {
                if (this[soundSpeedField.TimePeriod] != null) throw new DataException(string.Format("Unable to add SoundSpeedField for {0}. Data already present.", soundSpeedField.TimePeriod));
                SoundSpeedFields.Add(soundSpeedField);
            }
        }

        public void Add(SoundSpeedField newField)
        {
            if (this[newField.TimePeriod] != null) throw new DataException(string.Format("Unable to add SoundSpeedField for {0}. Data already present.", newField.TimePeriod));
            SoundSpeedFields.Add(newField);
        }

        public static SoundSpeed Load(string filename)
        {
            var formatter = new BinaryFormatter();
            using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
            {
                return new SoundSpeed { SoundSpeedFields = (List<SoundSpeedField>)formatter.Deserialize(stream) };
            }
        }

        public static Task<SoundSpeed> LoadAsync(string filename)
        {
            return TaskEx.Run(() => Load(filename));
        }

        public static SoundSpeed Load(string temperatureFilename, string salinityFilename, EarthCoordinate<float> deepestPoint = null, GeoRect areaOfInterest = null)
        {
            var temperatureData = Load(temperatureFilename);
            var salinityData = Load(salinityFilename);
            VerifyThatTimePeriodsMatch(temperatureData, salinityData);
            var soundSpeed = Create(temperatureData, salinityData);
            if (deepestPoint == null) return soundSpeed;
            soundSpeed.Extend(temperatureData, salinityData, deepestPoint, areaOfInterest);
            foreach (var soundSpeedField in soundSpeed.SoundSpeedFields)
                soundSpeedField.Extend(temperatureData[soundSpeedField.TimePeriod], salinityData[soundSpeedField.TimePeriod], deepestPoint, areaOfInterest);
            return soundSpeed;
        }

        public void Extend(SoundSpeed temperatureData, SoundSpeed salinityData, EarthCoordinate<float> deepestPoint, GeoRect areaOfInterest)
        {
            VerifyThatTimePeriodsMatch(this, temperatureData);
            VerifyThatTimePeriodsMatch(temperatureData, salinityData);

            foreach (var soundSpeedField in SoundSpeedFields)
                soundSpeedField.Extend(temperatureData[soundSpeedField.TimePeriod], salinityData[soundSpeedField.TimePeriod], deepestPoint, areaOfInterest);
        }

        public static SoundSpeed Create(SoundSpeed temperatureData, SoundSpeed salinityData)
        {
            VerifyThatTimePeriodsMatch(temperatureData, salinityData);

            var soundSpeedFile = new SoundSpeed();
            foreach (var temperatureField in temperatureData.SoundSpeedFields)
                soundSpeedFile.SoundSpeedFields.Add(SoundSpeedField.Create(temperatureField, salinityData[temperatureField.TimePeriod]));
            return soundSpeedFile;
        }

        public static SoundSpeed Average(SoundSpeed monthlySoundSpeeds, List<NAVOTimePeriod> timePeriods)
        {
            var result = new SoundSpeed();
            foreach (var timePeriod in timePeriods)
            {
                var months = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod);
                var accumulator = new SoundSpeedFieldAverager { TimePeriod = timePeriod };
                foreach (var month in months) accumulator.Add(monthlySoundSpeeds[month]);
                result.SoundSpeedFields.Add(accumulator.Average);
            }
            return result;
        }

        public static SoundSpeedField Average(SoundSpeed monthlySoundSpeeds, NAVOTimePeriod timePeriod)
        {
            var months = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod).ToList();
            var accumulator = new SoundSpeedFieldAverager { TimePeriod = timePeriod };
            foreach (var month in months) accumulator.Add(monthlySoundSpeeds[month]);
            return accumulator.Average;
        }

        internal static void VerifyThatTimePeriodsMatch(SoundSpeed data1, SoundSpeed data2)
        {
            foreach (var field1 in data1.SoundSpeedFields.Where(field1 => data2[field1.TimePeriod] == null)) throw new DataException(string.Format("SoundSpeeds do not contain the same time periods. Data 1 has time period {0}, data 2 does not", field1.TimePeriod));
            foreach (var field2 in data2.SoundSpeedFields.Where(field2 => data1[field2.TimePeriod] == null)) throw new DataException(string.Format("SoundSpeeds do not contain the same time periods. Data 2 has time period {0}, data 1 does not", field2.TimePeriod));
        }

        #region IExtensibleDataObject
        public virtual ExtensionDataObject ExtensionData { get; set; }
        #endregion
    }
}