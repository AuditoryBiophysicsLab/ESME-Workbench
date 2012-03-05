using System;
using System.Linq;
using System.IO;
using ESME.Environment;
using HRC.Navigation;

namespace NAVODatabaseAdapter
{
    public static class DBDB
    {
        public static Bathymetry Extract(string databaseFile, string extractionProgram, float selectedResolution, GeoRect region, IProgress<float> progress = null)
        {
            if (progress != null) lock(progress) progress.Report(0f);

            const float progressStep = 100f / 4;
            var totalProgress = 0f;

            var outputPath = Path.GetTempFileName();
            outputPath = outputPath.Remove(outputPath.Length - 4, 4);

            var outputDirectory = Path.GetDirectoryName(outputPath);
            var commandArgs = string.Format(" area \"{0}\" 0.05min 2.00min nearest 0 meters G {1} {2} {3} {4} {5:0.0##} YXZ=\"{6}\"", 
                databaseFile, region.South, region.West, region.North, region.East, selectedResolution, string.Format("{0}.yxz", Path.GetFileName(outputPath)));
            //extract the area and look for success or failure in the output string.

            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);

            var result = NAVOExtractionProgram.Execute(extractionProgram, commandArgs, outputDirectory).Result;
            var resarray = result.Split('\n');
            foreach (var line in resarray.Where(line => line.Contains("ERROR"))) throw new ApplicationException("DigitalBathymetricDatabase: Error extracting requested area: " + line);

            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);

            var bathymetry = Bathymetry.FromYXZ(Path.Combine(outputPath + ".yxz"), -1);

            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);

            File.Delete(outputPath + ".yxz");
            File.Delete(outputPath + ".yxz_SECURITY_README.txt");

            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            return bathymetry;
        }
    }
}