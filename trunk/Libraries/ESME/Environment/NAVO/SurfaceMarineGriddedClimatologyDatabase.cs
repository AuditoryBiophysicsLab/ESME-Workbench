using System;
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
        static string _databasePath;
        public static string DatabasePath
        {
            get { return _databasePath; }
            set
            {
                if (_databasePath == value) return;
                _databasePath = value;
                if (!_databasePath.EndsWith(@"\")) _databasePath = _databasePath + @"\"; //database path has to end with a trailing slash here.  For SMGC, it's a directory, not a file.
            }
        }

        public static string ExtractionProgramPath { get; set; }

        public const float GridSpacing = 1.0f;

        public static string WindFilename(string outputPath, NAVOTimePeriod timePeriod) { return Path.Combine(outputPath, string.Format("{0}-wind.txt", timePeriod)); }

        //public static void ExtractArea(NAVOExtractionPacket extractionPacket)
        public static void ExtractArea(string outputDirectory, NAVOTimePeriod timePeriod, NAVOTimePeriod startMonth, NAVOTimePeriod endMonth, int monthsDuration, GeoRect extractionArea)
        {
            var outputFilename = Path.Combine(outputDirectory, string.Format("{0}-wind.txt", timePeriod));

            var northPath = DatabasePath;
            var southPath = DatabasePath;
            if (Directory.Exists(Path.Combine(DatabasePath, "north"))) northPath = (Path.Combine(DatabasePath, @"north\"));
            if (Directory.Exists(Path.Combine(DatabasePath, "south"))) southPath = (Path.Combine(DatabasePath, @"south\"));

            System.Environment.SetEnvironmentVariable("SMGC_DATA_NORTH", northPath);
            System.Environment.SetEnvironmentVariable("SMGC_DATA_SOUTH", southPath);
            var commandArgs = string.Format("-lat {0}/{1} -lon {2}/{3} -mon {4}/{5} -par 17/1", extractionArea.South, extractionArea.North, extractionArea.West, extractionArea.East, (int)startMonth, (int)endMonth); // '-par 17/1' extracts wind speed statistical data.  don't ask. 

            var batchFilename = Path.Combine(outputDirectory, "wind_extract.bat");
            using (var batchFile = new StreamWriter(batchFilename, false))
                batchFile.WriteLine("\"{0}\" {1}", ExtractionProgramPath, commandArgs);

            var result = NAVOExtractionProgram.Execute(ExtractionProgramPath, commandArgs, outputDirectory);
            //result now contains the entire output of SMGC, i think, since it dumps data to STDOUT... so let's save it to disk in the right place. 
            using (var writer = new StreamWriter(outputFilename))
            {
                writer.WriteLine("StartMonth=" + startMonth);
                writer.WriteLine("EndMonth=" + endMonth);
                writer.WriteLine("MonthDuration=" + monthsDuration);
                writer.WriteLine("GridSpacing=" + GridSpacing);
                writer.Write(result);
            }
        }

        /// <summary>
        /// Parser for SMGC raw wind speed output. 
        /// </summary>
        /// <param name="fileName">The filename containing the SMGC output</param>
        /// <returns>a populated Environment2DData object with windspeeds per latitude/longitude.</returns>
        public static Environment2DData Parse(string fileName)
        {
            var resarray = File.ReadAllLines(fileName).ToList();
            var lats = new List<double>();
            var lons = new List<double>();
            var data = new List<EarthCoordinate<float>>();
            //  var averagevalues = new List<double>();
            var rawvalues = new List<List<string>>();
            var points = new Dictionary<string, double>();
            //split the string up into lines
            var startMonth = (NAVOTimePeriod)Enum.Parse(typeof(NAVOTimePeriod), resarray[0].Split('=')[1]);
            var endMonth = (NAVOTimePeriod)Enum.Parse(typeof(NAVOTimePeriod), resarray[1].Split('=')[1]);
            var monthDuration = int.Parse(resarray[2].Split('=')[1]);
            var gridSpacing = int.Parse(resarray[3].Split('=')[1]);

            var curLineIndex = 0;
            //var curGroupIndex = 0;
            while (curLineIndex < resarray.Count)
            {
                string thisline = resarray[curLineIndex++].Trim();
                if (curLineIndex >= resarray.Count) break;
                //if the line starts with 'Lat', add it plus everything up to the next blank line to rawvalues[i].
                if (thisline.StartsWith("Lat"))
                {
                    var curGroup = new List<string>
                                   {
                                       thisline.Trim()
                                   };
                    while (!string.IsNullOrEmpty(thisline = resarray[curLineIndex++]))
                    {
                        if (curLineIndex >= resarray.Count) break;
                        curGroup.Add(thisline.Trim());
                    }
                    rawvalues.Add(curGroup);
                    //if (curLineIndex >= resarray.Count) break;
                }
            }

            foreach (var curGroup in rawvalues)
            {
                var lat = double.NaN;
                var lon = double.NaN;
                var monthspeed = new List<double>();
                foreach (var dataline in from curLine in curGroup
                                         where !curLine.StartsWith("Lat") && !curLine.StartsWith("---") && !string.IsNullOrEmpty(curLine)
                                         select curLine.Split('\t'))
                {
                    if (double.TryParse(dataline[0], out lat) && double.TryParse(dataline[1], out lon)) monthspeed.Add(double.Parse(dataline[7]));
                    else throw new InvalidDataException("unexpected data in SMGC");
                }
                if (double.IsNaN(lat) || double.IsNaN(lon) || (monthspeed.Count <= 0)) continue;
                data.Add(new EarthCoordinate<float>(lat, lon, (float)monthspeed.Average()));
            }
            return new Environment2DData(data);
            //and finally, make a useful thing out of them. 
            //return new Environment2DData(uniqueLats.Last(), uniqueLats.First(), uniqueLons.Last(), uniqueLons.First(), gridSpacing, dataArray, 0, 0);
        }
    }
#endif

    public static class SurfaceMarineGriddedClimatologyDatabase
    {
        public static string DatabasePath { get; set; }

        public static string ExtractionProgramPath { get; set; }

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
            for (var timePeriodIndex = 0; timePeriodIndex < timePeriods.Count(); timePeriodIndex++)
            {
                var curTimePeriodData = new TimePeriodEnvironmentData<WindSample> { TimePeriod = timePeriods[timePeriodIndex] };
                var monthsInCurTimePeriod = requiredMonths[timePeriodIndex];
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
                    if (count > 0) curTimePeriodData.EnvironmentData.Add(new WindSample(curLocation, sum / count));
                }
                curTimePeriodData.EnvironmentData.RemoveDuplicates();
                if (!useExpandedExtractionArea) curTimePeriodData.EnvironmentData.TrimToNearestPoints(extractionArea);
                wind.TimePeriods.Add(curTimePeriodData);
            }
            wind.Save(Path.Combine(outputDirectory, "wind.xml"));
        }

#if false
        public static void Import(string baseDirectory, string filePattern, IList<NAVOTimePeriod> months)
        {
            var latValues = new List<float>();
            var lonValues = new List<float>();
            for (var lat = -90.0f; lat <= 90.0f; lat++) latValues.Add(lat);
            for (var lon = -180.0f; lon <= 179.0f; lon++) lonValues.Add(lon);
            var latitudeAxis = new DataAxis("latitude", latValues.ToArray());
            var longitudeAxis = new DataAxis("longitude", lonValues.ToArray());

            Console.Write("  Creating EEB Files");
            foreach (var curMonth in fileMonthNames)
            {
                var curDataFile = DataFile.Create(curMonth.FileName);
                var curWaveLayer = new DataLayer(WaveHeightLayerName, curMonth.MonthName, curMonth.FileName + " imported from the SMGC 2.0 database on " + DateTime.Now, "Units are meters", latitudeAxis, longitudeAxis, null)
                                   {
                                       DefaultValue = float.NaN
                                   };
                curDataFile.Layers.Add(curWaveLayer);
                var curWindLayer = new DataLayer(WindSpeedLayerName, curMonth.MonthName, curMonth.FileName + " imported from the SMGC 2.0 database on " + DateTime.Now, "Units are meters", latitudeAxis, longitudeAxis, null)
                                   {
                                       DefaultValue = float.NaN
                                   };
                curDataFile.Layers.Add(curWindLayer);
                DataFiles.Add(curDataFile);
                Console.Write(".");
            }
            Console.WriteLine(" done\n");
            SearchDirectoryTree(baseDirectory, filePattern);
        }

        static List<string> LocateDataFiles(string baseDirectory, List<string> selectedFiles)
        {
            Console.WriteLine("  Processing directory: " + baseDirectory);
            var subdirectories = Directory.GetDirectories(baseDirectory);

            foreach (var directory in subdirectories) SearchDirectoryTree(directory, filePattern);

            var matchingFiles = Directory.GetFiles(baseDirectory, filePattern);
            float fileCount = 0;
            float totalFiles = matchingFiles.Count();
            foreach (var file in matchingFiles)
            {
                try
                {
                    ProcessMatchingFile(file);
                }
                catch (Exception e)
                {
                    Console.WriteLine("\n\n********** ERROR **********\n");
                    Console.WriteLine("Error importing file {0}\n  {1}\nFile skipped.\n\n", Path.GetFileName(file), e.Message);
                    //throw;
                }
                if ((fileCount % 100) == 0)
                    Console.Write("    Directory progress: {0:0}%       \r", (fileCount / totalFiles) * 100.0f);
                fileCount++;
            }
        }

        static void ProcessMatchingFile(string fileName)
        {
            var file = new SMGCFile(fileName);
            if ((file.Months != null) && (file.Months.Count > 0))
            {
                for (var month = 1; month <= 12; month++)
                {
                    var curMonth = file[month];
                    if (curMonth != null)
                    {
                        var curMonthDataFile = DataFiles[month - 1];

                        // figure out the row and column of the EEB files that corresponds to file.EarthCoordinate
                        var row = (int)(file.EarthCoordinate.Latitude + 90.0f);
                        var col = (int)(file.EarthCoordinate.Longitude + 180.0f);

                        //Console.WriteLine("Data for ({0:0}, {1:0}) Month {2}",
                        //    file.EarthCoordinate.Latitude, file.EarthCoordinate.Longitude, month);
                        if (!float.IsNaN(curMonth.MeanWaveHeight))
                        {
                            //Console.WriteLine("  Wave Height = {0:0.0}", curMonth.MeanWaveHeight_m);
                            curMonthDataFile.Layers[WaveHeightLayerName][row, col].Data = new[]
                                                                                          {
                                                                                              curMonth.MeanWaveHeight
                                                                                          };
                        }
                        if (!float.IsNaN(curMonth.MeanWindSpeed))
                        {
                            //Console.WriteLine("  Wind Speed = {0:0.0}", curMonth.MeanWindSpeed_mps);
                            curMonthDataFile.Layers[WindSpeedLayerName][row, col].Data = new[]
                                                                                         {
                                                                                             curMonth.MeanWindSpeed
                                                                                         };
                        }
                    }
                }
            }
        }
#endif

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
}