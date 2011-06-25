using System;
using System.Collections.Generic;
using System.IO;
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

        public static void ExtractArea(NAVOBackgroundExtractor backgroundExtractor, out SoundSpeedField temperatureProfile, out SoundSpeedField salinityProfile)
        {
            var north = (int)Math.Ceiling(backgroundExtractor.ExtractionArea.North);
            var south = (int)Math.Floor(backgroundExtractor.ExtractionArea.South);
            var east = (int)Math.Ceiling(backgroundExtractor.ExtractionArea.East);
            var west = (int)Math.Floor(backgroundExtractor.ExtractionArea.West);

            var expandedArea = new GeoRect(north, south, east, west);
            var commandArgs = string.Format("-out \"{0}\" -gdem \"{1}\" -months {2} -north {3} -south {4} -east {5} -west {6}", backgroundExtractor.DestinationPath, DatabasePath, backgroundExtractor.TimePeriod, expandedArea.North, expandedArea.South, expandedArea.East, expandedArea.West);

            NAVOExtractionProgram.Execute(ExtractionProgramPath, commandArgs, backgroundExtractor.DestinationPath, RequiredSupportFiles);
            backgroundExtractor.Value++;

            var temperatureFileName = Path.Combine(backgroundExtractor.DestinationPath, string.Format("{0}-temperature.xml", backgroundExtractor.TimePeriod));
            var salinityFileName = Path.Combine(backgroundExtractor.DestinationPath, string.Format("{0}-salinity.xml", backgroundExtractor.TimePeriod));
            
            var field = SoundSpeed.Load(temperatureFileName);
            File.Delete(temperatureFileName);
            temperatureProfile = field.SoundSpeedFields[0];
            backgroundExtractor.Value++;
            
            field = SoundSpeed.Load(salinityFileName);
            File.Delete(salinityFileName);
            salinityProfile = field.SoundSpeedFields[0];
            backgroundExtractor.Value++;
        }
    }
}