﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
#if false
    public static class SurfaceMarineGriddedClimatologyDatabase
    {
        public static string DatabasePath { get; set; }

        public const float GridSpacing = 1.0f;

        public static string WindFilename(string outputPath) { return Path.Combine(outputPath, "wind.xml"); }

        public static void ExtractArea(string databasePath, string outputDirectory, IList<NAVOTimePeriod> timePeriods, IList<IEnumerable<NAVOTimePeriod>> requiredMonths, GeoRect extractionArea, bool useExpandedExtractionArea)
        {
            // Construct a list of files we will need to read out of the SMGC database
            var selectedFiles = new List<SMGCFile>();
            var selectedLocations = new List<EarthCoordinate>();
            for (var lat = (int) Math.Floor(extractionArea.South); lat <= (int) Math.Ceiling(extractionArea.North); lat++)
                for (var lon = (int) Math.Floor(extractionArea.West); lon <= (int) Math.Ceiling(extractionArea.East); lon++)
                {
                    var northSouth = (lat >= 0) ? "n" : "s";
                    var eastWest = (lon >= 0) ? "e" : "w";
                    var curFile = string.Format("{0}{1:00}{2}{3:000}.stt", northSouth, Math.Abs(lat), eastWest, Math.Abs(lon));
                    selectedFiles.Add(new SMGCFile(Directory.GetFiles(databasePath, curFile, SearchOption.AllDirectories).First()));
                    selectedLocations.Add(new EarthCoordinate(lat, lon));
                }
            var allMonths = new List<NAVOTimePeriod>();
            foreach (var curPeriod in requiredMonths) allMonths.AddRange(curPeriod);
            var uniqueMonths = allMonths.Distinct().ToList();
            uniqueMonths.Sort();
            var monthlyWindData = new Wind();
            foreach (var curMonth in uniqueMonths)
            {
                var curMonthData = new TimePeriodEnvironmentData<WindSample> { TimePeriod = curMonth };
                curMonthData.EnvironmentData.AddRange(from selectedFile in selectedFiles
                                                      where
                                                              (selectedFile.Months != null) &&
                                                              (selectedFile[curMonth] != null)
                                                      select
                                                              new WindSample(selectedFile.EarthCoordinate,
                                                                             selectedFile[curMonth].MeanWindSpeed));
                monthlyWindData.TimePeriods.Add(curMonthData);
            }
            var wind = new Wind();
            for (var timePeriodIndex = 0; timePeriodIndex < timePeriods.Count; timePeriodIndex++)
            {
                var curTimePeriodData = new TimePeriodEnvironmentData<WindSample> { TimePeriod = timePeriods[timePeriodIndex] };
                var monthsInCurTimePeriod = requiredMonths[timePeriodIndex];
                var curTimePeriodEnvironmentData = new List<WindSample>();
                foreach (var curLocation in selectedLocations)
                {
                    var sum = 0f;
                    var count = 0;
                    foreach (var curMonth in monthsInCurTimePeriod)
                    {
                        if ((monthlyWindData.TimePeriods == null) || (monthlyWindData[curMonth] == null)) continue;
                        sum += monthlyWindData[curMonth].EnvironmentData[curLocation].Data;
                        count++;
                    }
                    if (count > 0) curTimePeriodEnvironmentData.Add(new WindSample(curLocation, sum / count));
                }
                curTimePeriodData.EnvironmentData.AddRange(curTimePeriodEnvironmentData);
                if (!useExpandedExtractionArea) curTimePeriodData.EnvironmentData.TrimToNearestPoints(extractionArea);
                wind.TimePeriods.Add(curTimePeriodData);
            }
            wind.Save(Path.Combine(outputDirectory, "wind.xml"));
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

                    Months = new List<SMGCMonth>
                         {
                             new SMGCMonth(NAVOTimePeriod.January, reader),
                             new SMGCMonth(NAVOTimePeriod.February, reader),
                             new SMGCMonth(NAVOTimePeriod.March, reader),
                             new SMGCMonth(NAVOTimePeriod.April, reader),
                             new SMGCMonth(NAVOTimePeriod.May, reader),
                             new SMGCMonth(NAVOTimePeriod.June, reader),
                             new SMGCMonth(NAVOTimePeriod.July, reader),
                             new SMGCMonth(NAVOTimePeriod.August, reader),
                             new SMGCMonth(NAVOTimePeriod.September, reader),
                             new SMGCMonth(NAVOTimePeriod.October, reader),
                             new SMGCMonth(NAVOTimePeriod.November, reader),
                             new SMGCMonth(NAVOTimePeriod.December, reader)
                         };
                }
            }
        }

        internal class SMGCMonth : SMGCRecord
        {
            public float MeanWindSpeed { get; private set; }
            public float MeanWaveHeight { get; private set; }
            public NAVOTimePeriod NAVOTimePeriod { get; private set; }

            public SMGCMonth(NAVOTimePeriod timePeriod, BinaryReader reader)
            {
                NAVOTimePeriod = timePeriod;

                MeanWaveHeight = MeanWindSpeed = float.NaN;
                for (var i = 0; i <= 12; i++) SkipRecord(reader);

                // Record index 13 is wave height
                var waveHeight = new SMGCRecordTypeA(reader, 0.1f);
                MeanWaveHeight = waveHeight.MeanValue;
                if (!float.IsNaN(MeanWaveHeight))
                {
                    if ((MeanWaveHeight < 0.0f) || (MeanWaveHeight > 19.0f)) Console.WriteLine("\nMean Wave Height value {0} outside stated range for {1}.  File may be corrupt.", MeanWaveHeight, timePeriod);
                }

                for (var i = 0; i < 3; i++) SkipRecord(reader);

                // Record index 17 is wind speed
                var windSpeed = new SMGCRecordTypeA(reader, 0.1f);
                MeanWindSpeed = windSpeed.MeanValue;
                if (!float.IsNaN(MeanWindSpeed))
                {
                    if ((MeanWindSpeed < 0.0f) || (MeanWindSpeed > 40.1f)) Console.WriteLine("\nMean Wind Speed value {0} outside stated range for {1}.  File may be corrupt.", MeanWindSpeed, timePeriod);
                }

                // Skip the B records at the end of the current month
                for (var i = 0; i < 4; i++) SkipRecord(reader);
            }

            public override string ToString() { return string.Format("Mean Wind speed [{0}]: {1} m/s", NAVOTimePeriod, MeanWindSpeed); }
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
#endif
}