using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading.Tasks;
using HRC.Navigation;

namespace ESME.Environment
{
    [Serializable]
    public class SoundSpeed<T> : EnvironmentDataSetBase where T : SoundSpeedSample, new()
    {
        public List<SoundSpeedField<T>> SoundSpeedFields { get; set; }

        public SoundSpeed()
        {
            SoundSpeedFields = new List<SoundSpeedField<T>>();
        }

        public override void Save(string filename)
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
        public SoundSpeedField<T> this[TimePeriod timePeriod]
        {
            get
            {
                return SoundSpeedFields.Find(t => t.TimePeriod == timePeriod);
            }
        }

        public void Add(SoundSpeed<T> newData)
        {
            foreach (var soundSpeedField in newData.SoundSpeedFields)
            {
                if (this[soundSpeedField.TimePeriod] != null) throw new DataException(string.Format("Unable to add SoundSpeedField for {0}. Data already present.", soundSpeedField.TimePeriod));
                SoundSpeedFields.Add(soundSpeedField);
            }
        }

        public void Add(SoundSpeedField<T> newField)
        {
            if (this[newField.TimePeriod] != null) throw new DataException(string.Format("Unable to add SoundSpeedField for {0}. Data already present.", newField.TimePeriod));
            SoundSpeedFields.Add(newField);
        }

        public static SoundSpeed<T> Load(string filename)
        {
            //var formatter = new BinaryFormatter();
            //using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
            //{
            //    return new SoundSpeed { SoundSpeedFields = (List<SoundSpeedField>)formatter.Deserialize(stream) };
            //}
            using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
            using (var reader = new BinaryReader(stream)) return Deserialize(reader);
        }

        public static Task<SoundSpeed<T>> LoadAsync(string filename)
        {
            return TaskEx.Run(() => Load(filename));
        }
#if false
        public static SoundSpeed<T> Load(string temperatureFilename, string salinityFilename, EarthCoordinate<float> deepestPoint = null, GeoRect areaOfInterest = null)
        {
            var temperatureData = Load(temperatureFilename);
            var salinityData = Load(salinityFilename);
            VerifyThatTimePeriodsMatch(temperatureData, salinityData);
            var soundSpeed = Create(temperatureData, salinityData);
            if (deepestPoint == null) return soundSpeed;
            soundSpeed.Extend(temperatureData, salinityData, deepestPoint, areaOfInterest);
            foreach (var soundSpeedField in soundSpeed.SoundSpeedFields)
                soundSpeedField.Extend(deepestPoint);
            return soundSpeed;
        }

        public static SoundSpeed Create(SoundSpeed temperatureData, SoundSpeed salinityData, EarthCoordinate<float> deepestPoint = null, IProgress<float> progress = null)
        {
            var curProgress = 0f;
            if (progress != null) lock (progress) progress.Report(curProgress);
            VerifyThatTimePeriodsMatch(temperatureData, salinityData);
            curProgress += 10f;
            if (progress != null) lock (progress) progress.Report(curProgress);
            var progressStep = 30f / temperatureData.SoundSpeedFields.Count;
            var soundSpeedFile = new SoundSpeed();
            foreach (var temperatureField in temperatureData.SoundSpeedFields)
            {
                var curField = SoundSpeedField.Create(temperatureField, salinityData[temperatureField.TimePeriod]);
                curProgress += progressStep;
                if (progress != null) lock (progress) progress.Report(curProgress);
                if (deepestPoint != null) curField.Extend(temperatureField, salinityData[temperatureField.TimePeriod], deepestPoint);
                curProgress += progressStep;
                if (progress != null) lock (progress) progress.Report(curProgress);
                soundSpeedFile.SoundSpeedFields.Add(curField);
                curProgress += progressStep;
                if (progress != null) lock (progress) progress.Report(curProgress);
            }
            return soundSpeedFile;
        }

#endif

        public void Extend(Geo<float> deepestPoint)
        {
            foreach (var soundSpeedField in SoundSpeedFields)
                soundSpeedField.ExtendProfiles(deepestPoint);
        }

        public static SoundSpeed<SoundSpeedSample> Average(SoundSpeed<T> monthlySoundSpeeds, List<TimePeriod> timePeriods)
        {
            var result = new SoundSpeed<SoundSpeedSample>();
            foreach (var timePeriod in timePeriods)
            {
                var months = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod);
                var accumulator = new SoundSpeedFieldAverager<T> { TimePeriod = timePeriod };
                foreach (var month in months) accumulator.Add(monthlySoundSpeeds[month]);
                result.SoundSpeedFields.Add(accumulator.Average);
            }
            return result;
        }

        public static SoundSpeedField<SoundSpeedSample> Average(SoundSpeed<T> monthlySoundSpeeds, TimePeriod timePeriod)
        {
            var months = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod).ToList();
            var accumulator = new SoundSpeedFieldAverager<T> { TimePeriod = timePeriod };
            foreach (var month in months) accumulator.Add(monthlySoundSpeeds[month]);
            return accumulator.Average;
        }

        internal static void VerifyThatTimePeriodsMatch(SoundSpeed<T> data1, SoundSpeed<T> data2)
        {
            foreach (var field1 in data1.SoundSpeedFields.Where(field1 => data2[field1.TimePeriod] == null)) throw new DataException(string.Format("SoundSpeeds do not contain the same time periods. Data 1 has time period {0}, data 2 does not", field1.TimePeriod));
            foreach (var field2 in data2.SoundSpeedFields.Where(field2 => data1[field2.TimePeriod] == null)) throw new DataException(string.Format("SoundSpeeds do not contain the same time periods. Data 2 has time period {0}, data 1 does not", field2.TimePeriod));
        }

        public static SoundSpeed<T> Deserialize(BinaryReader reader)
        {
            var result = new SoundSpeed<T>();
            var fieldCount = reader.ReadInt32();
            for (var i = 0; i < fieldCount; i++)
                result.SoundSpeedFields.Add(SoundSpeedField<T>.Deserialize(reader));
            return result;
        }

        public void Serialize(string filename)
        {
            using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None))
            using (var writer = new BinaryWriter(stream))
                Serialize(writer);
        }

        public void Serialize(BinaryWriter writer)
        {
            writer.Write(SoundSpeedFields.Count);
            foreach (var item in SoundSpeedFields) item.Serialize(writer);
        }

        #region IExtensibleDataObject
        public virtual ExtensionDataObject ExtensionData { get; set; }
        #endregion
    }
}