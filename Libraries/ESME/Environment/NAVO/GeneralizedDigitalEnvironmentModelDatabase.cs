﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace ESME.Environment.NAVO
{
    public static class GeneralizedDigitalEnvironmentModelDatabase
    {
        static readonly string[] ShortMonthNames = new[]
                                                  {
                                                      "noneuary", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"
                                                  };

        const string SalinityVariableName = "salinity";
        const string TemperatureVariableName = "water_temp";
        const string SoundspeedVariableName = "soundspeed";

        public const float GridSpacing = 0.25f;
        public static string DatabasePath { get; set; }
        public static string ExtractionProgramPath { get; set; }

        static string OutputFileBaseName(string outputPath, int monthIndex) { return Path.Combine(outputPath, ((NAVOTimePeriod) monthIndex).ToString()); }
        static string OutputFileName(string outputPath, int monthIndex, string dataType) { return OutputFileBaseName(outputPath, monthIndex) + "-" + dataType + ".xml"; }

        static string SalinityFile(int monthIndex)
        {
            var gdem = Path.Combine(DatabasePath, GDEMSalinityFileName(monthIndex));
            var nuwc = Path.Combine(DatabasePath, NUWCSalinityFileName(monthIndex));
            if (File.Exists(gdem)) return gdem;
            if (File.Exists(nuwc)) return nuwc;
            throw new FileNotFoundException(string.Format("Could not find requested salinity file, tried {0} and {1}", gdem, nuwc));
        }

        static string TemperatureFile(int monthIndex)
        {
            var gdem = Path.Combine(DatabasePath, GDEMTemperatureFileName(monthIndex));
            var nuwc = Path.Combine(DatabasePath, NUWCTemperatureFileName(monthIndex));
            if (File.Exists(gdem)) return gdem;
            if (File.Exists(nuwc)) return nuwc;
            throw new FileNotFoundException(string.Format("Could not find requested temperature file, tried {0} and {1}", gdem, nuwc));
        }

        static string GDEMTemperatureFileName(int monthIndex) { return "t" + BaseGDEMFileName(monthIndex); }
        static string GDEMSalinityFileName(int monthIndex) { return "s" + BaseGDEMFileName(monthIndex); }
        static string BaseGDEMFileName(int monthIndex) { return "gdemv3s" + string.Format("{0:00}", monthIndex) + ".nc"; }
        static string NUWCTemperatureFileName(int monthIndex) { return ShortMonthNames[monthIndex] + "_t.nc"; }
        static string NUWCSalinityFileName(int monthIndex) { return ShortMonthNames[monthIndex] + "_s.nc"; }

        public static void ExtractAreaFromMonthFile(string outputPath, double north, double south, double east, double west, int monthIndex)
        {
            ExtractAreaFromMonthFile(SalinityFile(monthIndex), OutputFileName(outputPath, monthIndex, SalinityVariableName), SalinityVariableName, north, south, east, west);
            ExtractAreaFromMonthFile(TemperatureFile(monthIndex), OutputFileName(outputPath, monthIndex, TemperatureVariableName), TemperatureVariableName, north, south, east, west);
        }

        static void ExtractAreaFromMonthFile(string sourceFileName, string outputFileName, string dataType, double north, double south, double east, double west)
        {
            if (File.Exists(outputFileName)) File.Delete(outputFileName);

            //extract temperature data into a XML file
            //for sanity:
            const string lonParamName = "lon";
            const string latParamName = "lat";
            const string depthParamName = "depth";
            const string missingParamName = "missing_value";
            const string scaleParamName = "scale_factor";
            const string offsetParamName = "add_offset";
            var commandArgs = string.Format("-in \"{0}\" -lon {1} -lat {2} -north {3} -south {4} -east {5} -west {6} -dep {7}  -mv {8} -data {9} -sf {10} -offset {11}  -dataout \"{12}\"",
                sourceFileName, lonParamName, latParamName, north, south, east, west, depthParamName, missingParamName, dataType, scaleParamName, offsetParamName, outputFileName);
            NAVOExtractionProgram.Execute(ExtractionProgramPath, commandArgs, Path.GetDirectoryName(outputFileName));
        }

        public static void AverageMonthlyData(string outputPath, IEnumerable<int> monthIndices, NAVOTimePeriod outputTimePeriod)
        {
            var temperatureFileNames = monthIndices.Select(monthIndex => OutputFileName(outputPath, monthIndex, TemperatureVariableName)).ToList();
            AverageMonthlyData(temperatureFileNames, outputTimePeriod, TemperatureVariableName);
            var salinityFileNames = monthIndices.Select(monthIndex => OutputFileName(outputPath, monthIndex, SalinityVariableName)).ToList();
            AverageMonthlyData(salinityFileNames, outputTimePeriod, SalinityVariableName);
        }

        static void AverageMonthlyData(IList<string> monthFileNames, NAVOTimePeriod outputTimePeriod, string dataType)
        {
            if (monthFileNames.Count <= 1) throw new ApplicationException("Can't average data over several months if the list of months is not longer than one");
            var outputFileName = OutputFileName(Path.GetDirectoryName(monthFileNames[0]), (int) outputTimePeriod, dataType);
            Environment3DAverager accumulator = null;
            var depthAxisAggregator = new List<float>();
            foreach (var monthFileName in monthFileNames)
            {
                var dataOutput = SerializedOutput.Load(monthFileName, null);

                var results = ExtractValues(dataOutput);
                var depthAxis = results.Depths.Select(x => (float)x).ToArray();
                depthAxisAggregator.AddRange(depthAxis);
                var data = new List<AverageDatum>[results.Longitudes.Length, results.Latitudes.Length];
                if (accumulator == null) accumulator = new Environment3DAverager(results.Latitudes.Last(), results.Latitudes.First(), results.Longitudes.Last(), results.Longitudes.First(), GridSpacing, results.Depths, data);
                //and sum them.
                accumulator.Add(results);
            }
            if (accumulator == null) throw new ApplicationException("AverageMonthData: No data was averaged");
            accumulator.Average();
            var result = new SerializedOutput();
            result.DepthAxis.AddRange(accumulator.Depths);
            for (var lonIndex = 0; lonIndex < accumulator.Values.GetLength(0); lonIndex++)
                for (var latIndex = 0; latIndex < accumulator.Values.GetLength(1); latIndex++)
                {
                    var dataValues = from location in accumulator.Values[lonIndex, latIndex]
                                     select location.Value;
                    var dataPoint = new EnvironmentalDataPoint
                    {
                        Latitude_degrees = accumulator.Latitudes[latIndex],
                        Longitude_degrees = accumulator.Longitudes[lonIndex],
                    };
                    dataPoint.Data.AddRange(dataValues.ToList());
                    result.DataPoints.Add(dataPoint);
                }
            result.Save(outputFileName, null);
        }

        public static void CreateSoundSpeedFile(string outputPath, NAVOTimePeriod outputTimePeriod)
        {
            var temperatureFileName = OutputFileName(outputPath, (int)outputTimePeriod, TemperatureVariableName);
            var salinityFileName = OutputFileName(outputPath, (int)outputTimePeriod, SalinityVariableName);
            var soundspeedFileName = OutputFileName(outputPath, (int)outputTimePeriod, SoundspeedVariableName);
            CreateSoundSpeedFile(temperatureFileName, salinityFileName, soundspeedFileName);
        }

        static void CreateSoundSpeedFile(string temperatureFilename, string salinityFilename, string soundspeedFilename)
        {
            var averageSalinity = SerializedOutput.Load(salinityFilename, null);
            var latitudes = averageSalinity.Latitudes;
            var longitudes = averageSalinity.Longitudes;

            var averageTemp = SerializedOutput.Load(temperatureFilename, null);
            
            var soundSpeedField = new SerializedOutput();
            soundSpeedField.DepthAxis.AddRange(averageTemp.DepthAxis);
            var depthArray = (from depth in averageTemp.DepthAxis
                              select (float)depth).ToArray();
            
            foreach (var latitude in latitudes)
                foreach (var longitude in longitudes)
                {
                    var curLat = latitude;
                    var curLon = longitude;
                    var temperatureData = averageTemp.DataPoints.Find(x => x.Latitude_degrees == curLat && x.Longitude_degrees == curLon);
                    var salinityData = averageSalinity.DataPoints.Find(x => x.Latitude_degrees == curLat && x.Longitude_degrees == curLon);
                    var soundSpeedData = new EnvironmentalDataPoint
                    {
                        Latitude_degrees = curLat,
                        Longitude_degrees = curLon,
                    };
                    if ((temperatureData != null) && (salinityData != null))
                    {
                        var curPointDepths = new float[Math.Max(temperatureData.Data.Count, salinityData.Data.Count)];
                        Array.Copy(depthArray, curPointDepths, curPointDepths.Length);
                        var curPointTemps = (from temperature in temperatureData.Data
                                             select (float)temperature).ToArray();
                        var curPointSalinities = (from salinity in salinityData.Data
                                                  select (float)salinity).ToArray();
                        var curPointSoundSpeeds = UNESCO.SoundSpeed(temperatureData, ref curPointDepths, ref curPointTemps, ref curPointSalinities);
                        soundSpeedData.Data.AddRange(from soundSpeed in curPointSoundSpeeds select (double)soundSpeed);
                        soundSpeedField.DataPoints.Add(soundSpeedData);
                    }
                }
            soundSpeedField.Save(soundspeedFilename, null);
        }

        private static Environment3DData ExtractValues(SerializedOutput data)
        {
            var points = new Dictionary<string, List<double>>();
            var lats = new List<double>();
            var lons = new List<double>();
            var depths = data.DepthAxis;

            foreach (var point in data.DataPoints.Where(point => point.Data != null))
            {
                points.Add(string.Format("{0:#.00000},{1:#.00000}", point.Latitude_degrees, point.Longitude_degrees), point.Data);
                lats.Add(point.Latitude_degrees);
                lons.Add(point.Longitude_degrees);
            }
            var uniqueLats = lats.Distinct().ToList();
            var uniqueLons = lons.Distinct().ToList();
            uniqueLats.Sort();
            uniqueLons.Sort();

            var dataArray = new List<double>[uniqueLons.Count, uniqueLats.Count];
            for (var latIndex = 0; latIndex < uniqueLats.Count; latIndex++)
            {
                var lat = uniqueLats[latIndex];
                for (var lonIndex = 0; lonIndex < uniqueLons.Count; lonIndex++)
                {
                    var lon = uniqueLons[lonIndex];
                    var key = string.Format("{0:#.00000},{1:#.00000}", lat, lon);
                    dataArray[lonIndex, latIndex] = points[key];
                }
            }
            return new Environment3DData(uniqueLats.Last(), uniqueLats.First(), uniqueLons.Last(), uniqueLons.First(), GridSpacing, depths, dataArray);
        }
#if false
        public override void ExtractArea(NAVOExtractionPacket extractionPacket)
        {
            OutputFilename = Path.Combine(extractionPacket.Filename, string.Format("{0}-soundspeed.xml", extractionPacket.TimePeriod));
            var north = extractionPacket.North;
            var south = extractionPacket.South;
            var east = extractionPacket.East;
            var west = extractionPacket.West;

            
            //Determine which netCDF files we need to read.
            var ncTemps = new List<string>();
            var ncSalts = new List<string>();

            var filepath = Path.GetDirectoryName(OutputFilename);
            var monthList = new List<int>();
            for (var curMonth = 0; curMonth < MonthsDuration; curMonth++ ) monthList.Add(NAVODataSources.MonthMap[StartMonth + curMonth]);
            foreach (var curMonth in monthList)
            {
                var originalSalinityFilename = Path.Combine(DatabasePath, "sgdemv3s" + string.Format("{0:00}", curMonth) + ".nc");
                var originalTemperatureFilename = Path.Combine(DatabasePath, "tgdemv3s" + string.Format("{0:00}", curMonth) + ".nc");
                var nuwcSalinityFilename = Path.Combine(DatabasePath, ShortMonthNames[curMonth] + "_s.nc");
                var nuwcTemperatureFilename = Path.Combine(DatabasePath, ShortMonthNames[curMonth] + "_t.nc");

                if (File.Exists(originalSalinityFilename)) ncSalts.Add(originalSalinityFilename);
                else if (File.Exists(nuwcSalinityFilename)) ncSalts.Add(nuwcSalinityFilename);
                else throw new FileNotFoundException("Could not find requested salinity file " + originalSalinityFilename);

                if (File.Exists(originalTemperatureFilename)) ncTemps.Add(originalTemperatureFilename);
                else if (File.Exists(nuwcTemperatureFilename)) ncTemps.Add(nuwcTemperatureFilename);
                else throw new FileNotFoundException("Could not find requested temperature file " + originalTemperatureFilename);
            }

            //extract average temperature and salinity from the right files for the season
            var averageTemp = AveragedGDEMOutput(ncTemps, TemperatureVariableName, filepath, north, south, east, west);
            TemperatureSourceFilename = Path.Combine(extractionPacket.Filename, string.Format("{0}-average-water-temp.xml", extractionPacket.TimePeriod));
            averageTemp.Save(TemperatureSourceFilename, null);
            var averageSalinity = AveragedGDEMOutput(ncSalts, SalinityVariableName, filepath, north, south, east, west);
            SalinitySourceFilename = Path.Combine(extractionPacket.Filename, string.Format("{0}-average-salinity.xml", extractionPacket.TimePeriod));
            averageSalinity.Save(SalinitySourceFilename, null);
            //sanity check: are averageTemps and averageSalinity in the same place?
            AreEqual(averageTemp.DepthAxis.ToArray(), averageSalinity.DepthAxis.ToArray());
            AreEqual(averageTemp.Latitudes.ToArray(), averageSalinity.Latitudes.ToArray());
            AreEqual(averageTemp.Longitudes, averageSalinity.Longitudes);
            //if so, it doesn't matter which one's GIS data we use. 
            var depths = averageSalinity.DepthAxis.ToArray();
            var latitudes = averageSalinity.Latitudes;
            var longitudes = averageSalinity.Longitudes;

#if false
    //combine and get ssp.
            var soundSpeeds = new Environment3DData(latitudes.Last(), latitudes.First(), longitudes.Last(), longitudes.First(), GridSpacing, depths, new List<float>[longitudes.Length, latitudes.Length]);
            for (var latIndex = 0; latIndex < latitudes.Length; latIndex++)
            {
                var lat = latitudes[latIndex];
                for (var lonIndex = 0; lonIndex < longitudes.Length; lonIndex++)
                {
                    var lon = longitudes[lonIndex];
                    var location = new EarthCoordinate(lat, lon);
                    if ((averageTemps.Values[lonIndex, latIndex] != null) && (averageSalinity.Values[lonIndex, latIndex] != null))
                    {
                        var temps = averageTemps.Values[lonIndex, latIndex].Select(x => x.Value).ToArray();
                        var sals = averageSalinity.Values[lonIndex, latIndex].Select(x => x.Value).ToArray();
                        soundSpeeds.Values[lonIndex, latIndex] = UNESCO.SoundSpeed(location, ref depths, ref temps, ref sals).ToList();
                    }
                }
            } 

            var soundSpeedField = new SoundSpeedField();
            for (var latIndex = 0; latIndex < latitudes.Length; latIndex++)
            {
                var lat = latitudes[latIndex];
                for (var lonIndex = 0; lonIndex < longitudes.Length; lonIndex++)
                {
                    var lon = longitudes[lonIndex];
                    var location = new EarthCoordinate(lat, lon);
                    if ((averageTemp.Values[lonIndex, latIndex] != null) && (averageSalinity.Values[lonIndex, latIndex] != null))
                    {
                        var temps = averageTemps.Values[lonIndex, latIndex].Select(x => x.Value).ToArray();
                        var sals = averageSalinity.Values[lonIndex, latIndex].Select(x => x.Value).ToArray();
                        var speeds = UNESCO.SoundSpeed(location, ref depths, ref temps, ref sals).ToList();
                        var soundSpeedProfile = new SoundSpeedProfile(TimePeriod.ToString(), location, depths, speeds);
                        soundSpeedField.SoundSpeedProfiles.Add(soundSpeedProfile);
                    }
                }
            }
            soundSpeedField.Initialize();//only once.
            SoundSpeedField = soundSpeedField;
            SoundSpeedField.Save(filename+"-soundspeeds.xml");
#endif
            var soundSpeedField = new SerializedOutput();
            soundSpeedField.DepthAxis.AddRange(averageTemp.DepthAxis);
            var depthArray = (from depth in averageTemp.DepthAxis
                              select (float) depth).ToArray();
            foreach (var latitude in latitudes)
                foreach (var longitude in longitudes)
                {
                    var curLat = latitude;
                    var curLon = longitude;
                    var temperatureData = averageTemp.DataPoints.Find(x => x.Latitude_degrees == curLat && x.Longitude_degrees == curLon);
                    var salinityData = averageSalinity.DataPoints.Find(x => x.Latitude_degrees == curLat && x.Longitude_degrees == curLon);
                    var soundSpeedData = new EnvironmentalDataPoint
                                         {
                                             Latitude_degrees = curLat,
                                             Longitude_degrees = curLon,
                                         };
                    if ((temperatureData != null) && (salinityData != null))
                    {
                        var curPointDepths = new float[Math.Max(temperatureData.Data.Count, salinityData.Data.Count)];
                        Array.Copy(depthArray, curPointDepths, curPointDepths.Length);
                        var curPointTemps = (from temperature in temperatureData.Data
                                             select (float) temperature).ToArray();
                        var curPointSalinities = (from salinity in salinityData.Data
                                                  select (float) salinity).ToArray();
                        var curPointSoundSpeeds = UNESCO.SoundSpeed(temperatureData, ref curPointDepths, ref curPointTemps, ref curPointSalinities);
                        soundSpeedData.Data.AddRange(from soundSpeed in curPointSoundSpeeds select (double)soundSpeed);
                        soundSpeedField.DataPoints.Add(soundSpeedData);
                    }
                }
            soundSpeedField.Save(OutputFilename, null);
        }

        public override bool ValidateDataSource() { return false; }

        internal void AreEqual<T>(IEnumerable<T> arrayOne, IEnumerable<T> arrayTwo)
        {
            var result = arrayOne.Except(arrayTwo); // Produces the set difference of two sequences.
            if (result.Count() != 0) throw new ApplicationException("arrays are not equal.");
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="ncFilenames"></param>
        /// <param name="dataType">The NetCDF parameter name of the data type to be extracted.  Commonly "salinity" or "water_temp". </param>
        /// <param name="outputPath">A path to the desired XML output directory.  Output files will be of the form outputPath/ncFilename-dataType.xml.  </param>
        /// <param name="north"></param>
        /// <param name="south"></param>
        /// <param name="east"></param>
        /// <param name="west"></param>
        /// <returns></returns>
        internal SerializedOutput AveragedGDEMOutput(List<string> ncFilenames, string dataType, string outputPath, double north, double south, double east, double west)
        {
            Environment3DAverager accumulator = null;
            var depthAxisAggregator = new List<float>();
            foreach (var file in ncFilenames)
            {
                var dataString = Path.GetFileNameWithoutExtension(file) + "-" + dataType + ".xml" ;
                var dataFilePath = Path.Combine(outputPath, dataString);
                if(File.Exists(dataFilePath)) File.Delete(dataFilePath);

                //extract temperature data into a XML file
                //for sanity:
                const string lonParamName = "lon";
                const string latParamName = "lat";
                const string depthParamName = "depth";
                const string missingParamName = "missing_value";
                const string scaleParamName = "scale_factor";
                const string offsetParamName = "add_offset";
                CommandArgs = string.Format("-in \"{0}\" -lon {1} -lat {2} -north {3} -south {4} -east {5} -west {6} -dep {7}  -mv {8} -data {9} -sf {10} -offset {11}  -dataout \"{12}\""
                    , file, lonParamName, latParamName, north, south, east, west, depthParamName, missingParamName, dataType, scaleParamName, offsetParamName, dataFilePath);
                Execute();

                //read it back in
                var dataOutput = SerializedOutput.Load(dataFilePath, null);

                //delete it, 
                //File.Delete(dataFilePath);
                //and add the values to the list

                var results = ExtractValues(dataOutput);
                var depthAxis = results.Depths.Select(x => (float)x).ToArray();
                depthAxisAggregator.AddRange(depthAxis);
                var data = new List<AverageDatum>[results.Longitudes.Length, results.Latitudes.Length];
                if (accumulator == null) accumulator = new Environment3DAverager(results.Latitudes.Last(), results.Latitudes.First(), results.Longitudes.Last(), results.Longitudes.First(), GridSpacing, results.Depths, data);
                //and sum them.
                accumulator.Add(results);
            }
            if (accumulator == null) throw new ApplicationException(string.Format("no {0} data was extracted.", dataType));

            //then average them. 
            if (ncFilenames.Count > 1) accumulator.Average();
            // accumulator.Save();
            var result = new SerializedOutput();
            result.DepthAxis.AddRange(accumulator.Depths);
            for (var lonIndex = 0; lonIndex < accumulator.Values.GetLength(0); lonIndex++)
                for (var latIndex = 0; latIndex < accumulator.Values.GetLength(1); latIndex++)
                {
                    var dataValues = from location in accumulator.Values[lonIndex, latIndex]
                                     select (double)location.Value;
                    var dataPoint = new EnvironmentalDataPoint
                                    {
                                        Latitude_degrees = accumulator.Latitudes[latIndex],
                                        Longitude_degrees = accumulator.Longitudes[lonIndex],
                                    };
                    dataPoint.Data.AddRange(dataValues.ToList());
                    result.DataPoints.Add(dataPoint);
                }

            return result;
        }
#endif
    }
}