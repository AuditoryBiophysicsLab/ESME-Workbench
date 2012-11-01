using System;
using System.Linq;
using System.IO;
using ESME.Environment;
using HRC.Navigation;
using HRC.Utility;

namespace NAVODatabaseAdapter
{
    public static class DBDB
    {
        public static Bathymetry Extract(string databaseFile, string extractionProgram, float selectedResolution, GeoRect region, PercentProgress progress = null)
        {
            if (progress != null)
            {
                progress.MaximumValue = 4;
                progress.Report(0);
            }

            var outputPath = Path.GetTempFileName();
            outputPath = outputPath.Remove(outputPath.Length - 4, 4);

            var outputDirectory = Path.GetDirectoryName(outputPath);
            var commandArgs = string.Format(" area \"{0}\" 0.05min 2.00min nearest 0 meters G {1} {2} {3} {4} {5:0.0##} YXZ=\"{6}\"", 
                databaseFile, region.South, region.West, region.North, region.East, selectedResolution, string.Format("{0}.yxz", Path.GetFileName(outputPath)));
            //extract the area and look for success or failure in the output string.

            if (progress != null) lock (progress) progress.Report(1);

            var result = NAVOExtractionProgram.Execute(extractionProgram, commandArgs, outputDirectory).Result;
            var resarray = result.Split('\n');
            foreach (var line in resarray.Where(line => line.Contains("ERROR"))) throw new ApplicationException("DigitalBathymetricDatabase: Error extracting requested area: " + line);

            if (progress != null) lock (progress) progress.Report(2);

            var bathymetry = Bathymetry.FromYXZ(Path.Combine(outputPath + ".yxz"), -1);
            bathymetry.Samples.TrimToNearestPoints(region);
            if (progress != null) lock (progress) progress.Report(3);

            File.Delete(outputPath + ".yxz");
            File.Delete(outputPath + ".yxz_SECURITY_README.txt");

            if (progress != null) lock (progress) progress.Report(4);
            return bathymetry;
        }
    }
}