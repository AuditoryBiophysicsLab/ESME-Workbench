using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
    public static class SMGC
    {
        public async static Task ImportAsync(string outputPath, GeoRect region, IProgress<string> currentState = null, IProgress<float> progress = null)
        {
            if (progress != null) lock (progress) progress.Report(0f);
            if (currentState != null) lock (currentState) currentState.Report("Importing wind data");

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
                MaxDegreeOfParallelism = 4,
            });

            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            // Construct a list of files we will need to read out of the SMGC database
            var selectedLocations = new List<EarthCoordinate>();
            for (var lat = south; lat <= north; lat++)
                for (var lon = west; lon <= east; lon++)
                {
                    var northSouth = (lat >= 0) ? "n" : "s";
                    var eastWest = (lon >= 0) ? "e" : "w";
                    var curFileName = string.Format("{0}{1:00}{2}{3:000}.stt", northSouth, Math.Abs(lat), eastWest, Math.Abs(lon));
                    var matchingFiles = Directory.GetFiles(Globals.AppSettings.NAVOConfiguration.SMGCDirectory, curFileName, SearchOption.AllDirectories);
                    if (matchingFiles.Length == 0) continue;
                    parallelReader.Post(matchingFiles.First());
                    selectedLocations.Add(new EarthCoordinate(lat, lon));
                }
            var batchBlock = new BatchBlock<SMGCFile>(selectedLocations.Count);
            parallelReader.LinkTo(batchBlock);
            parallelReader.Complete();
            await parallelReader.Completion;
            IList<SMGCFile> selectedFiles = batchBlock.Receive().ToList();
            if (currentState != null) lock (currentState) currentState.Report("Creating monthly data collection");
            foreach (var curMonth in NAVOConfiguration.AllMonths)
            {
                var month = curMonth;
                var curMonthData = new TimePeriodEnvironmentData<WindSample> { TimePeriod = month };
                curMonthData.EnvironmentData.AddRange(from selectedFile in selectedFiles
                                                      where (selectedFile.Months != null) && (selectedFile[month] != null)
                                                      select new WindSample(selectedFile.EarthCoordinate, selectedFile[month].MeanWindSpeed));
                var wind = new Wind();
                wind.TimePeriods.Add(curMonthData);
                wind.Save(Path.Combine(outputPath, string.Format("{0}.wind", month.ToString().ToLower())));
                if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            }
            if (currentState != null) lock (currentState) currentState.Report("Saving");
            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
        }

        public async static Task<Wind> NewImportAsync(GeoRect region, IProgress<string> currentState = null, IProgress<float> progress = null)
        {
            if (progress != null) lock (progress) progress.Report(0f);
            if (currentState != null) lock (currentState) currentState.Report("Importing wind data");

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
                MaxDegreeOfParallelism = 4,
            });

            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            // Construct a list of files we will need to read out of the SMGC database
            var selectedLocations = new List<EarthCoordinate>();
            for (var lat = south; lat <= north; lat++)
                for (var lon = west; lon <= east; lon++)
                {
                    var northSouth = (lat >= 0) ? "n" : "s";
                    var eastWest = (lon >= 0) ? "e" : "w";
                    var curFileName = string.Format("{0}{1:00}{2}{3:000}.stt", northSouth, Math.Abs(lat), eastWest, Math.Abs(lon));
                    var matchingFiles = Directory.GetFiles(Globals.AppSettings.NAVOConfiguration.SMGCDirectory, curFileName, SearchOption.AllDirectories);
                    if (matchingFiles.Length == 0) continue;
                    parallelReader.Post(matchingFiles.First());
                    selectedLocations.Add(new EarthCoordinate(lat, lon));
                }
            var batchBlock = new BatchBlock<SMGCFile>(selectedLocations.Count);
            parallelReader.LinkTo(batchBlock);
            parallelReader.Complete();
            await parallelReader.Completion;
            IList<SMGCFile> selectedFiles = batchBlock.Receive().ToList();
            if (currentState != null) lock (currentState) currentState.Report("Creating monthly data collection");
            var wind = new Wind();
            foreach (var curMonth in NAVOConfiguration.AllMonths)
            {
                var curMonthData = new TimePeriodEnvironmentData<WindSample> { TimePeriod = curMonth };
                curMonthData.EnvironmentData.AddRange(from selectedFile in selectedFiles
                                                      where (selectedFile.Months != null) && (selectedFile[curMonth] != null)
                                                      select new WindSample(selectedFile.EarthCoordinate, selectedFile[curMonth].MeanWindSpeed));
                wind.TimePeriods.Add(curMonthData);
                if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            }
            foreach (var curSeason in NAVOConfiguration.AllSeasons)
                wind.TimePeriods.Add(wind.SeasonalAverage(curSeason));
            if (currentState != null) lock (currentState) currentState.Report("Saving");
            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);

            foreach (var period in NAVOConfiguration.AllTimePeriods)
            {
                if (wind[period].EnvironmentData.Count == 0) Debugger.Break();
                foreach (var sample in wind[period].EnvironmentData)
                {
                    if (float.IsNaN(sample.Data)) Debugger.Break();
                }
            }

            return wind;
        }

        internal class SMGCFile
        {
            public List<SMGCMonth> Months { get; private set; }
            public EarthCoordinate EarthCoordinate { get; private set; }

            public SMGCMonth this[NAVOTimePeriod navoTimePeriod]
            {
                get
                {
                    return Months == null ? null : Months.Find(x => x.NAVOTimePeriod == navoTimePeriod);
                }
            }

            public SMGCMonth this[int monthNumber]
            {
                get
                {
                    return Months == null ? null : Months.Find(x => x.NAVOTimePeriod == (NAVOTimePeriod)monthNumber);
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
                // Parse the filename to generate the EarthCoordinate for the current data point
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
                EarthCoordinate = new EarthCoordinate(lat, lon);

                var info = new FileInfo(fileName);
                if (info.Length == 0) return;

                // Parse the file and populate the public properties we need
                using (var reader = new BinaryReader(new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)))
                {
                    var f99 = reader.ReadSingle();
                    var f380 = reader.ReadSingle();

                    if ((f99 != 99.0f) || (f380 != 380.0f)) throw new ApplicationException("Invalid byte order in file " + fileName);

                    Months = new List<SMGCMonth>();
                    var curMonth = SMGCMonth.Read(NAVOTimePeriod.January, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(NAVOTimePeriod.February, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(NAVOTimePeriod.March, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(NAVOTimePeriod.April, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(NAVOTimePeriod.May, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(NAVOTimePeriod.June, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(NAVOTimePeriod.July, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(NAVOTimePeriod.August, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(NAVOTimePeriod.September, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(NAVOTimePeriod.October, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(NAVOTimePeriod.November, reader);
                    if (curMonth != null) Months.Add(curMonth);
                    curMonth = SMGCMonth.Read(NAVOTimePeriod.December, reader);
                    if (curMonth != null) Months.Add(curMonth);
                }
            }
        }

        internal class SMGCMonth : SMGCRecord
        {
            public float MeanWindSpeed { get; private set; }
            public float MeanWaveHeight { get; private set; }
            public NAVOTimePeriod NAVOTimePeriod { get; private set; }

            SMGCMonth(NAVOTimePeriod timePeriod, BinaryReader reader)
            {
                NAVOTimePeriod = timePeriod;

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

            public static SMGCMonth Read(NAVOTimePeriod timePeriod, BinaryReader reader)
            {
                try
                {
                    var result = new SMGCMonth(timePeriod, reader);
                    if (float.IsNaN(result.MeanWindSpeed)) return null;
                    return result;
                } catch { return null; }
            }

            public override string ToString() { return string.Format("Mean Wind speed [{0}]: {1} m/s\n", NAVOTimePeriod, MeanWindSpeed); }
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