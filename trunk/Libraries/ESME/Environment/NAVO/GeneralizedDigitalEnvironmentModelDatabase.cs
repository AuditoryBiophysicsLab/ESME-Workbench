using System;
using System.Collections.Generic;
using System.Data;
using System.Windows;
using System.IO;
using System.Linq;
using HRC.Navigation;

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

        static List<string> RequiredSupportFiles { get; set; }

        static string _extractionProgramPath;
        public static string ExtractionProgramPath
        {
            get { return _extractionProgramPath; }
            set
            {
                if (_extractionProgramPath == value) return;
                _extractionProgramPath = value;
                RequiredSupportFiles = new List<string>();
                var directory = Path.GetDirectoryName(_extractionProgramPath) ?? "";
                RequiredSupportFiles.Add(Path.Combine(directory, "netcdf.dll"));
                RequiredSupportFiles.Add(Path.Combine(directory, "NetCDF_Wrapper.dll"));
            }
        }

        static string OutputFileBaseName(string outputPath, int monthIndex) { return Path.Combine(outputPath, ((NAVOTimePeriod)monthIndex).ToString()); }
        static string OutputFileBaseName(string outputPath, NAVOTimePeriod timePeriod) { return Path.Combine(outputPath, timePeriod.ToString()); }
        static string OutputFileName(string outputPath, int monthIndex, string dataType) { return OutputFileBaseName(outputPath, monthIndex) + "-" + dataType + ".xml"; }
        static string OutputFileName(string outputPath, NAVOTimePeriod timePeriod, string dataType) { return OutputFileBaseName(outputPath, timePeriod) + "-" + dataType + ".xml"; }

        static string SalinityFile(NAVOTimePeriod monthIndex)
        {
            var gdem = Path.Combine(DatabasePath, GDEMSalinityFileName(monthIndex));
            var nuwc = Path.Combine(DatabasePath, NUWCSalinityFileName(monthIndex));
            if (File.Exists(gdem)) return gdem;
            if (File.Exists(nuwc)) return nuwc;
            throw new FileNotFoundException(string.Format("Could not find requested salinity file, tried {0} and {1}", gdem, nuwc));
        }

        static string TemperatureFile(NAVOTimePeriod monthIndex)
        {
            var gdem = Path.Combine(DatabasePath, GDEMTemperatureFileName(monthIndex));
            var nuwc = Path.Combine(DatabasePath, NUWCTemperatureFileName(monthIndex));
            if (File.Exists(gdem)) return gdem;
            if (File.Exists(nuwc)) return nuwc;
            throw new FileNotFoundException(string.Format("Could not find requested temperature file, tried {0} and {1}", gdem, nuwc));
        }

        static string GDEMTemperatureFileName(NAVOTimePeriod monthIndex) { return "t" + BaseGDEMFileName(monthIndex); }
        static string GDEMSalinityFileName(NAVOTimePeriod monthIndex) { return "s" + BaseGDEMFileName(monthIndex); }
        static string BaseGDEMFileName(NAVOTimePeriod monthIndex) { return "gdemv3s" + string.Format("{0:00}", (int)monthIndex) + ".nc"; }
        static string NUWCTemperatureFileName(NAVOTimePeriod monthIndex) { return ShortMonthNames[(int)monthIndex] + "_t.nc"; }
        static string NUWCSalinityFileName(NAVOTimePeriod monthIndex) { return ShortMonthNames[(int)monthIndex] + "_s.nc"; }

        public static void ExtractAreaFromMonthFile(string outputPath, GeoRect extractionArea, NAVOTimePeriod month, bool useExpandedExtractionArea)
        {
            ExtractAreaFromMonthFile(SalinityFile(month), OutputFileName(outputPath, month, SalinityVariableName), SalinityVariableName, extractionArea, month, useExpandedExtractionArea);
            ExtractAreaFromMonthFile(TemperatureFile(month), OutputFileName(outputPath, month, TemperatureVariableName), TemperatureVariableName, extractionArea, month, useExpandedExtractionArea);
        }

        public static string SalinityFilename(string outputPath, NAVOTimePeriod timePeriod) { return OutputFileName(outputPath, timePeriod, SalinityVariableName); }
        public static string TemperatureFilename(string outputPath, NAVOTimePeriod timePeriod) { return OutputFileName(outputPath, timePeriod, TemperatureVariableName); }
        public static string SoundspeedFilename(string outputPath, NAVOTimePeriod timePeriod) { return OutputFileName(outputPath, timePeriod, SoundspeedVariableName); }

        static void ExtractAreaFromMonthFile(string sourceFileName, string outputFileName, string dataType, GeoRect extractionArea, NAVOTimePeriod month, bool useExpandedExtractionArea)
        {
            if (File.Exists(outputFileName)) File.Delete(outputFileName);

            var expandedArea = new GeoRect(Math.Ceiling(extractionArea.North), Math.Floor(extractionArea.South), Math.Ceiling(extractionArea.East), Math.Floor(extractionArea.West));
            //extract temperature data into a XML file
            //for sanity:
            const string lonParamName = "lon";
            const string latParamName = "lat";
            const string depthParamName = "depth";
            const string missingParamName = "missing_value";
            const string scaleParamName = "scale_factor";
            const string offsetParamName = "add_offset";
            var commandArgs = string.Format("-in \"{0}\" -lon {1} -lat {2} -north {3} -south {4} -east {5} -west {6} -dep {7}  -mv {8} -data {9} -sf {10} -offset {11}  -dataout \"{12}\" -month {13}",
                sourceFileName, lonParamName, latParamName, expandedArea.North, expandedArea.South, expandedArea.East, expandedArea.West, depthParamName, missingParamName, dataType, scaleParamName, offsetParamName, outputFileName, month);
            NAVOExtractionProgram.Execute(ExtractionProgramPath, commandArgs, Path.GetDirectoryName(outputFileName), RequiredSupportFiles);

            if (useExpandedExtractionArea) return;
            
            var field = SoundSpeed.Load(outputFileName);
            foreach (var dataField in field.SoundSpeedFields)
                dataField.EnvironmentData.TrimToNearestPoints(extractionArea);
            field.Save(outputFileName);
        }

        public static void AverageMonthlyData(string outputPath, IEnumerable<NAVOTimePeriod> monthIndices, NAVOTimePeriod outputTimePeriod)
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
            var accumulator = new SoundSpeedFieldAverager { TimePeriod = outputTimePeriod };
            foreach (var monthFileName in monthFileNames)
            {
                var dataOutput = SoundSpeed.Load(monthFileName).SoundSpeedFields[0];
                accumulator.Add(dataOutput);
            }
            var outputFile = new SoundSpeed();
            outputFile.SoundSpeedFields.Add(accumulator.Average);
            outputFile.Save(outputFileName);
        }

        public static void CreateSoundSpeedFile(string outputPath, NAVOTimePeriod outputTimePeriod, EarthCoordinate<float> deepestPoint)
        {
            var temperatureFileName = OutputFileName(outputPath, (int)outputTimePeriod, TemperatureVariableName);
            var salinityFileName = OutputFileName(outputPath, (int)outputTimePeriod, SalinityVariableName);
            var soundspeedFileName = OutputFileName(outputPath, (int)outputTimePeriod, SoundspeedVariableName);
            CreateSoundSpeedFile(temperatureFileName, salinityFileName, soundspeedFileName, deepestPoint, outputTimePeriod);
            File.Delete(temperatureFileName);
            File.Delete(salinityFileName);
        }

        static void CreateSoundSpeedFile(string temperatureFilename, string salinityFilename, string soundspeedFilename, EarthCoordinate<float> deepestPoint, NAVOTimePeriod outputTimePeriod)
        {
            var salinityField = SoundSpeed.Load(salinityFilename).SoundSpeedFields[0];
            var temperatureField = SoundSpeed.Load(temperatureFilename).SoundSpeedFields[0];

            ValidateThatProfilePointsMatch(salinityField, temperatureField);
            var result = new SoundSpeed();
            var field = new SoundSpeedField {TimePeriod = outputTimePeriod};
            foreach (var salinityPoint in salinityField.EnvironmentData)
                field.EnvironmentData.Add(ChenMilleroLi.SoundSpeed(salinityPoint, temperatureField.EnvironmentData[salinityPoint]));
            result.SoundSpeedFields.Add(field);
            result.Save(soundspeedFilename);
        }

        static void ValidateThatProfilePointsMatch(TimePeriodEnvironmentData<SoundSpeedProfile> profile1, TimePeriodEnvironmentData<SoundSpeedProfile> profile2)
        {
            foreach (var point1 in profile1.EnvironmentData.Where(point1 => !profile2.EnvironmentData.Any(point1.Equals))) throw new DataException(string.Format("Profiles do not contain the same data points.  One has data at {0}, the other does not", point1));
            foreach (var point2 in profile2.EnvironmentData.Where(point2 => !profile1.EnvironmentData.Any(point2.Equals))) throw new DataException(string.Format("Profiles do not contain the same data points.  One has data at {0}, the other does not", point2));
        }
    }
}