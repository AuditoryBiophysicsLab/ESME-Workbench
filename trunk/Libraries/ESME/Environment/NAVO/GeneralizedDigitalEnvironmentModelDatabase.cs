using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESME.Model;

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

        static string OutputFileBaseName(string outputPath, int monthIndex) { return Path.Combine(outputPath, ((NAVOTimePeriod)monthIndex).ToString()); }
        static string OutputFileBaseName(string outputPath, NAVOTimePeriod timePeriod) { return Path.Combine(outputPath, timePeriod.ToString()); }
        static string OutputFileName(string outputPath, int monthIndex, string dataType) { return OutputFileBaseName(outputPath, monthIndex) + "-" + dataType + ".xml"; }
        static string OutputFileName(string outputPath, NAVOTimePeriod timePeriod, string dataType) { return OutputFileBaseName(outputPath, timePeriod) + "-" + dataType + ".xml"; }

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

        public static string SalinityFilename(string outputPath, NAVOTimePeriod timePeriod) { return OutputFileName(outputPath, timePeriod, SalinityVariableName); }
        public static string TemperatureFilename(string outputPath, NAVOTimePeriod timePeriod) { return OutputFileName(outputPath, timePeriod, TemperatureVariableName); }
        public static string SoundspeedFilename(string outputPath, NAVOTimePeriod timePeriod) { return OutputFileName(outputPath, timePeriod, SoundspeedVariableName); }

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
            var soundspeedFileNames = monthIndices.Select(monthIndex => OutputFileName(outputPath, monthIndex, SoundspeedVariableName)).ToList();
            AverageMonthlyData(soundspeedFileNames, outputTimePeriod, SoundspeedVariableName);
            //var temperatureFileNames = monthIndices.Select(monthIndex => OutputFileName(outputPath, monthIndex, TemperatureVariableName)).ToList();
            //AverageMonthlyData(temperatureFileNames, outputTimePeriod, TemperatureVariableName);
            //var salinityFileNames = monthIndices.Select(monthIndex => OutputFileName(outputPath, monthIndex, SalinityVariableName)).ToList();
            //AverageMonthlyData(salinityFileNames, outputTimePeriod, SalinityVariableName);
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
                var data = new List<AverageDatum>[results.Longitudes.Count, results.Latitudes.Count];
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

        public static void CreateSoundSpeedFile(string outputPath, NAVOTimePeriod outputTimePeriod, float maxDepth)
        {
            var temperatureFileName = OutputFileName(outputPath, (int)outputTimePeriod, TemperatureVariableName);
            var salinityFileName = OutputFileName(outputPath, (int)outputTimePeriod, SalinityVariableName);
            var soundspeedFileName = OutputFileName(outputPath, (int)outputTimePeriod, SoundspeedVariableName);
            CreateSoundSpeedFile(temperatureFileName, salinityFileName, soundspeedFileName, maxDepth);
            File.Delete(temperatureFileName);
            File.Delete(salinityFileName);
        }

        static void CreateSoundSpeedFile(string temperatureFilename, string salinityFilename, string soundspeedFilename, float maxDepth)
        {
            var salinityField = SerializedOutput.Load(salinityFilename, null);
            var latitudes = salinityField.Latitudes;
            var longitudes = salinityField.Longitudes;

            var temperatureField = SerializedOutput.Load(temperatureFilename, null);
            
            var soundSpeedField = new SerializedOutput();
            soundSpeedField.DepthAxis.AddRange(temperatureField.DepthAxis);
            var depthArray = (from depth in temperatureField.DepthAxis
                              select (float)depth).ToArray();
            
            foreach (var latitude in latitudes)
                foreach (var longitude in longitudes)
                {
                    var curLat = latitude;
                    var curLon = longitude;
                    var temperaturePoint = temperatureField.DataPoints.Find(x => x.Latitude_degrees == curLat && x.Longitude_degrees == curLon);
                    var salinityPoint = salinityField.DataPoints.Find(x => x.Latitude_degrees == curLat && x.Longitude_degrees == curLon);
                    var soundSpeedData = new EnvironmentalDataPoint
                    {
                        Latitude_degrees = curLat,
                        Longitude_degrees = curLon,
                    };
                    if ((temperaturePoint == null) || (salinityPoint == null)) continue;
                    var curPointDepths = new float[Math.Max(temperaturePoint.Data.Count, salinityPoint.Data.Count)];
                    Array.Copy(depthArray, curPointDepths, curPointDepths.Length);
                    var curPointTemps = (from temperature in temperaturePoint.Data
                                         select (float)temperature).ToArray();
                    var curPointSalinities = (from salinity in salinityPoint.Data
                                              select (float)salinity).ToArray();
                    var curPointSoundSpeeds = UNESCO.SoundSpeed(temperaturePoint, ref curPointDepths, ref curPointTemps, ref curPointSalinities);
                    soundSpeedData.Data.AddRange(from soundSpeed in curPointSoundSpeeds select (double)soundSpeed);
                    soundSpeedField.DataPoints.Add(soundSpeedData);
                }
            var ssf = new SoundSpeedField(soundSpeedField, "");
            ssf.ExtendProfilesToDepth(Math.Abs(maxDepth), temperatureField, salinityField);
            ((SerializedOutput)ssf).Save(soundspeedFilename, null);
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
    }
}