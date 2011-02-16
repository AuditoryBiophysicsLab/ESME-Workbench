using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using ESME.Environment;
using HRC.Navigation;

namespace OurSMGCExtractor
{
    internal class Program
    {
        static void Main(string[] args)
        {
            if (args.Length != 1)
            {
                Console.WriteLine("Usage: ImportSMGC <SMGC Database Root Directory>");
                return;
            }
            var test = new SMGCFile(@"E:\OAML Data Sources\SMGC\Earth\n29w081.stt");

            var fileMonthMap = new List<FileNameMonthMap>
                               {
                                   new FileNameMonthMap
                                   {
                                       FileName = "smgcv2m01.eeb",
                                       MonthName = "january"
                                   },
                                   new FileNameMonthMap
                                   {
                                       FileName = "smgcv2m02.eeb",
                                       MonthName = "february"
                                   },
                                   new FileNameMonthMap
                                   {
                                       FileName = "smgcv2m03.eeb",
                                       MonthName = "march"
                                   },
                                   new FileNameMonthMap
                                   {
                                       FileName = "smgcv2m04.eeb",
                                       MonthName = "april"
                                   },
                                   new FileNameMonthMap
                                   {
                                       FileName = "smgcv2m05.eeb",
                                       MonthName = "may"
                                   },
                                   new FileNameMonthMap
                                   {
                                       FileName = "smgcv2m06.eeb",
                                       MonthName = "june"
                                   },
                                   new FileNameMonthMap
                                   {
                                       FileName = "smgcv2m07.eeb",
                                       MonthName = "july"
                                   },
                                   new FileNameMonthMap
                                   {
                                       FileName = "smgcv2m08.eeb",
                                       MonthName = "august"
                                   },
                                   new FileNameMonthMap
                                   {
                                       FileName = "smgcv2m09.eeb",
                                       MonthName = "september"
                                   },
                                   new FileNameMonthMap
                                   {
                                       FileName = "smgcv2m10.eeb",
                                       MonthName = "october"
                                   },
                                   new FileNameMonthMap
                                   {
                                       FileName = "smgcv2m11.eeb",
                                       MonthName = "november"
                                   },
                                   new FileNameMonthMap
                                   {
                                       FileName = "smgcv2m12.eeb",
                                       MonthName = "december"
                                   }
                               };

            try
            {
                SMGCDatabase.Import(args[0], "*.stt", fileMonthMap.ToArray());
            }
            catch (EndOfStreamException) {}
            catch (Exception e)
            {
                Console.WriteLine("Import failed.");
                Console.WriteLine(e.ToString());
                return;
            }
        }
    }

    internal class FileNameMonthMap
    {
        public string FileName { get; set; }
        public string MonthName { get; set; }
    }

    internal static class SMGCDatabase
    {
        public static void Import(string baseDirectory, string filePattern, FileNameMonthMap[] fileMonthNames)
        {
            if (fileMonthNames.Length != 12) throw new ApplicationException("There must be 12 EEB file / month names provided to this function.  In month order, please!");

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

        static void SearchDirectoryTree(string baseDirectory, string filePattern)
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
                        var row = (int) (file.EarthCoordinate.Latitude_degrees + 90.0f);
                        var col = (int) (file.EarthCoordinate.Longitude_degrees + 180.0f);

                        //Console.WriteLine("Data for ({0:0}, {1:0}) Month {2}",
                        //    file.EarthCoordinate.Latitude_degrees, file.EarthCoordinate.Longitude_degrees, month);
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

        static readonly List<DataFile> DataFiles = new List<DataFile>();
        const string WaveHeightLayerName = "waveheight";
        const string WindSpeedLayerName = "windspeed";
    }

    internal class SMGCFile
    {
        public List<SMGCMonth> Months { get; private set; }
        public EarthCoordinate EarthCoordinate { get; private set; }

        public SMGCMonth this[string monthName]
        {
            get
            {
                return Months == null ? null : Months.Find(x => x.Name == monthName);
            }
        }

        public SMGCMonth this[int monthNumber]
        {
            get
            {
                return Months == null ? null : Months.Find(x => x.MonthNumber == monthNumber);
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
                             new SMGCMonth("jan", 1, reader),
                             new SMGCMonth("feb", 2, reader),
                             new SMGCMonth("mar", 3, reader),
                             new SMGCMonth("apr", 4, reader),
                             new SMGCMonth("may", 5, reader),
                             new SMGCMonth("jun", 6, reader),
                             new SMGCMonth("jul", 7, reader),
                             new SMGCMonth("aug", 8, reader),
                             new SMGCMonth("sep", 9, reader),
                             new SMGCMonth("oct", 10, reader),
                             new SMGCMonth("nov", 11, reader),
                             new SMGCMonth("dec", 12, reader)
                         };
            }
        }
    }

    internal class SMGCMonth : SMGCRecord
    {
        public float MeanWindSpeed { get; private set; }
        public float MeanWaveHeight { get; private set; }
        public string Name { get; private set; }
        public int MonthNumber { get; private set; }

        public SMGCMonth(string name, int monthNumber, BinaryReader reader)
        {
            Name = name;
            MonthNumber = monthNumber;

            MeanWaveHeight = MeanWindSpeed = float.NaN;
            for (var i = 0; i <= 12; i++) SkipRecord(reader);

            // Record index 13 is wave height
            var waveHeight = new SMGCRecordTypeA(reader, 0.1f);
            MeanWaveHeight = waveHeight.MeanValue;
            if (!float.IsNaN(MeanWaveHeight))
            {
                if ((MeanWaveHeight < 0.0f) || (MeanWaveHeight > 19.0f)) Console.WriteLine("\nMean Wave Height value {0} outside stated range for {1}.  File may be corrupt.", MeanWaveHeight, name);
            }

            for (var i = 0; i < 3; i++) SkipRecord(reader);

            // Record index 17 is wind speed
            var windSpeed = new SMGCRecordTypeA(reader, 0.1f);
            MeanWindSpeed = windSpeed.MeanValue;
            if (!float.IsNaN(MeanWindSpeed))
            {
                if ((MeanWindSpeed < 0.0f) || (MeanWindSpeed > 40.1f)) Console.WriteLine("\nMean Wind Speed value {0} outside stated range for {1}.  File may be corrupt.", MeanWindSpeed, name);
            }

            // Skip the B records at the end of the current month
            for (var i = 0; i < 4; i++) SkipRecord(reader);
        }

        public override string ToString() { return string.Format("Mean Wind speed [{0}]: {1} m/s", Name, MeanWindSpeed); }
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