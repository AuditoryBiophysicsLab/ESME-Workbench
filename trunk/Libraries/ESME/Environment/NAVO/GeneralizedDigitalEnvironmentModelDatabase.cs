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
    }
}