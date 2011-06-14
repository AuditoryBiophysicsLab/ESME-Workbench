using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Text;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
    public static class GeneralizedDigitalEnvironmentModelDatabase
    {
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

        public static void ExtractArea(string outputPath, GeoRect extractionArea, IEnumerable<NAVOTimePeriod> months)
        {
            var expandedArea = new GeoRect(Math.Ceiling(extractionArea.North), Math.Floor(extractionArea.South), Math.Ceiling(extractionArea.East), Math.Floor(extractionArea.West));
            var monthNames = new StringBuilder();
            foreach (var month in months) monthNames.Append(month + ",");
            monthNames.Remove(monthNames.Length - 1, 1);    // drop trailing comma
            var commandArgs = string.Format("-out \"{0}\" -gdem \"{1}\" -months {2} -north {3} -south {4} -east {5} -west {6}", outputPath, DatabasePath, monthNames, expandedArea.North, expandedArea.South, expandedArea.East, expandedArea.West);

            NAVOExtractionProgram.Execute(ExtractionProgramPath, commandArgs, outputPath, RequiredSupportFiles);
        }

        public static void CreateMonthlySoundSpeeds(string outputPath, IEnumerable<NAVOTimePeriod> months)
        {
            var temperatureFile = SoundSpeed.Load(Path.Combine(outputPath, "temperature.xml"));
            var salinityFile = SoundSpeed.Load(Path.Combine(outputPath, "salinity.xml"));
            var soundSpeedFile = new SoundSpeed();
            foreach (var month in months)
            {
                var temperatureField = temperatureFile[month];
                var salinityField = salinityFile[month];
                VerifyThatProfilePointsMatch(temperatureField, salinityField);
                var field = new SoundSpeedField { TimePeriod = month };
                foreach (var temperatureProfile in temperatureField.EnvironmentData)
                    field.EnvironmentData.Add(ChenMilleroLi.SoundSpeed(temperatureProfile, salinityField.EnvironmentData[temperatureProfile]));
                soundSpeedFile.SoundSpeedFields.Add(field);
            }
            soundSpeedFile.Save(Path.Combine(outputPath, "soundspeed.xml"));
        }

        public static SoundSpeed ExtendMonthlySoundSpeeds(string outputPath, IEnumerable<NAVOTimePeriod> months, EarthCoordinate<float> deepestPoint, GeoRect areaOfInterest)
        {
            var temperatureFile = SoundSpeed.Load(Path.Combine(outputPath, "temperature.xml"));
            var salinityFile = SoundSpeed.Load(Path.Combine(outputPath, "salinity.xml"));
            var soundSpeedFile = SoundSpeed.Load(Path.Combine(outputPath, "soundspeed.xml"));
            var result = new SoundSpeed();
            foreach (var month in months)
            {
                var temperatureData = temperatureFile[month].EnvironmentData;
                temperatureData.TrimToNearestPoints(areaOfInterest);
                var temperatureField = new SoundSpeedField {EnvironmentData = temperatureData};

                var salinityData = salinityFile[month].EnvironmentData;
                salinityData.TrimToNearestPoints(areaOfInterest);
                var salinityField = new SoundSpeedField {EnvironmentData = salinityData};

                var soundSpeedData = soundSpeedFile[month].EnvironmentData;
                soundSpeedData.TrimToNearestPoints(areaOfInterest);
                var soundSpeedField = new SoundSpeedField
                {
                        EnvironmentData = soundSpeedData,
                        DeepestPoint = deepestPoint,
                        TimePeriod = month
                };

                VerifyThatProfilePointsMatch(temperatureField, salinityField);
                VerifyThatProfilePointsMatch(temperatureField, soundSpeedField);
                
                soundSpeedField.ExtendProfiles(temperatureField, salinityField);
                result.SoundSpeedFields.Add(soundSpeedField);
            }
            return result;
        }

        public static SoundSpeed AddAverageSoundSpeeds(SoundSpeed monthlySoundSpeeds, List<NAVOTimePeriod> timePeriods, List<List<NAVOTimePeriod>> timePeriodMonths)
        {
            for (var timePeriodIndex = 0; timePeriodIndex < timePeriods.Count(); timePeriodIndex++)
            {
                var timePeriod = timePeriods[timePeriodIndex];
                var months = timePeriodMonths[timePeriodIndex];
                var accumulator = new SoundSpeedFieldAverager { TimePeriod = timePeriod };
                foreach (var month in months)
                    accumulator.Add(monthlySoundSpeeds[month]);
                monthlySoundSpeeds.SoundSpeedFields.Add(accumulator.Average);
            }
            return monthlySoundSpeeds;
        }

        static void VerifyThatProfilePointsMatch(TimePeriodEnvironmentData<SoundSpeedProfile> profile1, TimePeriodEnvironmentData<SoundSpeedProfile> profile2)
        {
            foreach (var point1 in profile1.EnvironmentData.Where(point1 => !profile2.EnvironmentData.Any(point1.Equals))) throw new DataException(string.Format("Profiles do not contain the same data points.  One has data at {0}, the other does not", point1));
            foreach (var point2 in profile2.EnvironmentData.Where(point2 => !profile1.EnvironmentData.Any(point2.Equals))) throw new DataException(string.Format("Profiles do not contain the same data points.  One has data at {0}, the other does not", point2));
        }
    }
}