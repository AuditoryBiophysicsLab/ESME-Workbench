//#define SMGC_IMPORTER_BUILD
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using HRC.Navigation;

namespace InstallableNAVO.Databases
{
    public static class SMGC
    {
        public static Wind Import(GeoRect region, string smgcDirectory, IProgress<float> progress = null)
        {
            var result = ImportAsync(region, smgcDirectory, progress);
            return result.Result;
        }

        public async static Task<Wind> ImportAsync(GeoRect region, string smgcDirectory, IProgress<float> progress = null)
        {
            var maxDegreeOfParallelism = -1;
            if (Globals.AppSettings != null) maxDegreeOfParallelism = Globals.AppSettings.MaxImportThreadCount;
            if (progress != null) lock (progress) progress.Report(0f);

            var north = (float)Math.Ceiling(region.North);
            var south = (float)Math.Floor(region.South);
            var east = (float)Math.Ceiling(region.East);
            var west = (float)Math.Floor(region.West);

            var progressStep = 100f / (((north - south) * (east - west)) + 13);
            var totalProgress = 0f;

            var parallelReader = new TransformBlock<string, SMGCFile>(data =>
            {
                var file = new SMGCFile(data);
                if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
                return file;
            },
            new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                BoundedCapacity = -1,
                MaxDegreeOfParallelism = maxDegreeOfParallelism,
            });

            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            // Construct a list of files we will need to read out of the SMGC database
            var selectedLocations = new List<Geo>();
            var allFiles = Directory.GetFiles(smgcDirectory, "*.stt", SearchOption.AllDirectories).ToList();
#if SMGC_IMPORTER_BUILD
            // This chunk of code is ONLY for the SMGC importer and should only be compiled when running that utility
            foreach (var file in allFiles) parallelReader.Post(file);
            var batchBlock = new BatchBlock<SMGCFile>(allFiles.Count);
#else
            for (var lat = south; lat <= north; lat++)
            {
                Console.WriteLine("{0}: Processing latitude {1}", DateTime.Now, lat);
                for (var lon = west; lon <= east; lon++)
                {
                    var northSouth = (lat >= 0) ? "n" : "s";
                    var eastWest = (lon >= 0) ? "e" : "w";
                    var curFileName = string.Format("{0}{1:00}{2}{3:000}.stt", northSouth, Math.Abs(lat), eastWest, Math.Abs(lon));
                    var matchingFiles = (from curFile in allFiles
                                         where curFile.EndsWith(curFileName)
                                         select curFile).ToList();
                    if (!matchingFiles.Any()) continue;
                    parallelReader.Post(matchingFiles.First());
                    selectedLocations.Add(new Geo(lat, lon));
                }
            }
            var batchBlock = new BatchBlock<SMGCFile>(selectedLocations.Count);
#endif
            parallelReader.LinkTo(batchBlock);
            parallelReader.Complete();
            await parallelReader.Completion;
            IList<SMGCFile> selectedFiles = batchBlock.Receive().ToList();

            var parallelSelector = new TransformBlock<TimePeriod, TimePeriodEnvironmentData<WindSample>>(timePeriod =>
            {
                var curTimePeriodData = new TimePeriodEnvironmentData<WindSample> { TimePeriod = timePeriod };
                curTimePeriodData.EnvironmentData.AddRange(from selectedFile in selectedFiles
                                                           where (selectedFile.Months != null) && (selectedFile[timePeriod] != null)
                                                           select new WindSample(selectedFile.Geo, selectedFile[timePeriod].MeanWindSpeed));
                return curTimePeriodData;
            },
            new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                BoundedCapacity = -1,
                MaxDegreeOfParallelism = maxDegreeOfParallelism,
            });

            var wind = new Wind();
            foreach (var curMonth in NAVOConfiguration.AllMonths)
                parallelSelector.Post(curMonth);
            var selectorBlock = new BatchBlock<TimePeriodEnvironmentData<WindSample>>(NAVOConfiguration.AllMonths.Count());
            parallelSelector.LinkTo(selectorBlock);
            parallelSelector.Complete();
            await parallelSelector.Completion;
            wind.TimePeriods.AddRange(selectorBlock.Receive());
            return wind;
        }

        internal class SMGCFile
        {
            public List<SMGCMonth> Months { get; private set; }
            public Geo Geo { get; private set; }

            public SMGCMonth this[TimePeriod timePeriod]
            {
                get
                {
                    return Months == null ? null : Months.Find(x => x.TimePeriod == timePeriod);
                }
            }

            public SMGCMonth this[int monthNumber]
            {
                get
                {
                    return Months == null ? null : Months.Find(x => x.TimePeriod == (TimePeriod)monthNumber);
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder();
                foreach (var month in Months) sb.AppendLine(month.ToString());
                return sb.ToString();
            }

            public SMGCFile(string fileName)
            {
                Months = null;
                // Parse the filename to generate the Geo for the current data point
                var file = Path.GetFileName(fileName);
                if (file == null) throw new ApplicationException("SMGCFile: Could not get filename from path");
                var latStr = file.Substring(1, 2);
                var lonStr = file.Substring(4, 3);
                var lat = int.Parse(latStr);
                var lon = int.Parse(lonStr);
                var tmp = file.Substring(0, 1);
                switch (tmp)
                {
                    case "n":
                        break;
                    case "s":
                        lat *= -1;
                        break;
                    default:
                        throw new ApplicationException("FileName: First char MUST be 'n' or 's'. File: " + fileName);
                }

                tmp = file.Substring(3, 1);
                switch (tmp)
                {
                    case "e":
                        break;
                    case "w":
                        lon *= -1;
                        break;
                    default:
                        throw new ApplicationException("FileName: Fourth char MUST be 'e' or 'w'. File: " + fileName);
                }
                Geo = new Geo(lat, lon);

                var info = new FileInfo(fileName);
                if (info.Length == 0) return;

                // Parse the file and populate the public properties we need
                using (var reader = new BinaryReader(new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)))
                {
                    var f99 = reader.ReadSingle();
                    var f380 = reader.ReadSingle();

                    if ((Math.Abs(f99 - 99.0f) > 0.0001) || (Math.Abs(f380 - 380.0f) > 0.0001)) throw new ApplicationException("Invalid byte order in file " + fileName);

                    Months = new List<SMGCMonth>();
                    var curMonth = SMGCMonth.Read(TimePeriod.January, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(TimePeriod.February, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(TimePeriod.March, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(TimePeriod.April, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(TimePeriod.May, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(TimePeriod.June, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(TimePeriod.July, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(TimePeriod.August, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(TimePeriod.September, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(TimePeriod.October, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(TimePeriod.November, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(TimePeriod.December, reader);
                    if (curMonth != null) Months.Add(curMonth);
                }
            }
        }

        internal class SMGCMonth : SMGCRecord
        {
            public float MeanWindSpeed { get; private set; }
            public float MeanWaveHeight { get; private set; }
            public TimePeriod TimePeriod { get; private set; }

            SMGCMonth(TimePeriod timePeriod, BinaryReader reader)
            {
                TimePeriod = timePeriod;

                MeanWaveHeight = MeanWindSpeed = float.NaN;
                for (var i = 0; i <= 12; i++) SkipRecord(reader);

                // Record index 13 is wave height
                var waveHeight = new SMGCRecordTypeA(reader, 0.1f);
                MeanWaveHeight = waveHeight.MeanValue;
                if (!float.IsNaN(MeanWaveHeight))
                {
                    if ((MeanWaveHeight < 0.0f) || (MeanWaveHeight > 19.0f)) Console.WriteLine("Mean Wave Height value {0} outside stated range for {1}.  File may be corrupt.\n", MeanWaveHeight, timePeriod);
                }

                for (var i = 0; i < 3; i++) SkipRecord(reader);

                // Record index 17 is wind speed
                var windSpeed = new SMGCRecordTypeA(reader, 0.1f);
                MeanWindSpeed = windSpeed.MeanValue;
                //if (float.IsNaN(MeanWindSpeed)) Debugger.Break();
                if (!float.IsNaN(MeanWindSpeed))
                {
                    if ((MeanWindSpeed < 0.0f) || (MeanWindSpeed > 40.1f)) Console.WriteLine("Mean Wind Speed value {0} outside stated range for {1}.  File may be corrupt.\n", MeanWindSpeed, timePeriod);
                }

                // Skip the B records at the end of the current month
                for (var i = 0; i < 4; i++) SkipRecord(reader);
            }

            public static SMGCMonth Read(TimePeriod timePeriod, BinaryReader reader)
            {
                try
                {
                    var result = new SMGCMonth(timePeriod, reader);
                    if (float.IsNaN(result.MeanWindSpeed)) return null;
                    return result;
                } catch { return null; }
            }

            public override string ToString() { return string.Format("Mean Wind speed [{0}]: {1} m/s\n", TimePeriod, MeanWindSpeed); }
        }

        internal class SMGCRecordTypeA : SMGCRecord
        {
            public int SampleSize { get; private set; }
            public float MinimumValue { get; private set; }
            public float MaximumValue { get; private set; }
            public float MeanValue { get; private set; }
            public float StandardDeviation { get; private set; }
            public float MedianValue { get; private set; }
            public float Mode { get; private set; }

            public SMGCRecordTypeA(BinaryReader reader, float precision)
            {
                MinimumValue = MaximumValue = MeanValue = StandardDeviation = MedianValue = Mode = float.NaN;
                var reclen = reader.ReadUInt32();
                if (reclen == 0) return;
                if (reclen >= 28)
                {
                    //ParameterIndex = reader.ReadInt32();
                    SampleSize = reader.ReadInt32();
                    if (SampleSize == 0) Debugger.Break();

                    var tmp = reader.ReadInt32();
                    if (tmp != -9999) MinimumValue = tmp * precision;

                    tmp = reader.ReadInt32();
                    if (tmp != -9999) MaximumValue = tmp * precision;

                    tmp = reader.ReadInt32();
                    if (tmp != -9999) MeanValue = tmp * precision;

                    tmp = reader.ReadInt32();
                    if (tmp != -9999) StandardDeviation = tmp * precision;

                    tmp = reader.ReadInt32();
                    if (tmp != -9999) MedianValue = tmp * precision;

                    tmp = reader.ReadInt32();
                    if (tmp != -9999) Mode = tmp * precision;

                    reclen -= 28;
                }
                //if (float.IsNaN(MeanValue)) Debugger.Break();
                reader.BaseStream.Seek(reclen, SeekOrigin.Current);
                //SkipRecord(reader);
            }
        }

        internal abstract class SMGCRecord
        {
            protected void SkipRecord(BinaryReader reader)
            {
                var reclen = reader.ReadUInt32();
                if (reclen != 0) reader.BaseStream.Seek(reclen, SeekOrigin.Current);
            }
        }
    }
}