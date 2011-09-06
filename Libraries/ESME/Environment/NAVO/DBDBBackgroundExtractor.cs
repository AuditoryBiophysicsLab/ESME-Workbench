using System;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.NAVO
{
    public class DBDBBackgroundExtractor : NAVOBackgroundExtractor
    {
        protected async override void Run(object sender, DoWorkEventArgs e)
        {
            RunState = "Running";
            TaskName = "Bathymetry data extraction";
            var backgroundExtractor = (DBDBBackgroundExtractor)e.Argument;
            backgroundExtractor.Maximum = 3;
            backgroundExtractor.Value++;

            //from documentation, area extractions for DBDB are of the form <dbv5_command path> area <database path> <finest_resolution> <coarsest resolution> nearest 0 meters G <south west north east> 
            var commandArgs = string.Format(" area \"{0}\" {1} {2} nearest 0 meters G {3} {4} {5} {6} {7:0.0##} YXZ=\"{8}\"", backgroundExtractor.NAVOConfiguration.DBDBDirectory, "0.05min", "2.00min", backgroundExtractor.ExtractionArea.South, backgroundExtractor.ExtractionArea.West, backgroundExtractor.ExtractionArea.North, backgroundExtractor.ExtractionArea.East, backgroundExtractor.SelectedResolution, backgroundExtractor.SaveAsFilename);
            //extract the area and look for success or failure in the output string.

            var result = await NAVOExtractionProgram.ExecuteAsync(backgroundExtractor.NAVOConfiguration.DBDBEXEPath, commandArgs, backgroundExtractor.DestinationPath);
            backgroundExtractor.Value++;
            var resarray = result.Split('\n');
            foreach (var line in resarray.Where(line => line.Contains("ERROR"))) throw new ApplicationException("DigitalBathymetricDatabase: Error extracting requested area: " + line);
            backgroundExtractor.Value++;
        }

        //public override void ExtractArea(NAVOExtractionPacket extractionPacket)
        public async static void ExtractAsync(string outputPath, string selectedResolution, GeoRect extractionArea, IProgress<TaskProgressInfo> progress = null)
        {
            var taskProgress = new TaskProgressInfo
            {
                TaskName = "DBDB Extraction",
                CurrentActivity = "Initializing",
                ProgressPercent = 0,
            };
            if (progress != null) progress.Report(taskProgress);

            var resTemp = selectedResolution.EndsWith("min") ? selectedResolution.Remove(selectedResolution.Length - 3) : selectedResolution;
            double desiredResolution;
            if (!double.TryParse(resTemp, out desiredResolution)) throw new FormatException("Illegal number format for selectedResolution: " + selectedResolution);

            var outputDirectory = Path.GetDirectoryName(outputPath);
            var commandArgs = string.Format(" area \"{0}\" 0.05min 2.00min nearest 0 meters G {1} {2} {3} {4} {5:0.0##} YXZ=\"{6}\"", Globals.AppSettings.NAVOConfiguration.DBDBDirectory, extractionArea.South, extractionArea.West, extractionArea.North, extractionArea.East, desiredResolution, string.Format("{0}.yxz", Path.GetFileName(outputPath)));
            //extract the area and look for success or failure in the output string.

            taskProgress.CurrentActivity = "Waiting for extraction program to complete";
            taskProgress.ProgressPercent = 5;
            if (progress != null) progress.Report(taskProgress);

            var result = await NAVOExtractionProgram.ExecuteAsync(Globals.AppSettings.NAVOConfiguration.DBDBEXEPath, commandArgs, outputDirectory);
            var resarray = result.Split('\n');
            foreach (var line in resarray.Where(line => line.Contains("ERROR"))) throw new ApplicationException("DigitalBathymetricDatabase: Error extracting requested area: " + line);

            taskProgress.CurrentActivity = "Reading YXZ output file";
            taskProgress.ProgressPercent = 55;
            if (progress != null) progress.Report(taskProgress);
            var bathymetry = Bathymetry.FromYXZ(Path.Combine(outputPath + ".yxz"), -1);

            taskProgress.CurrentActivity = "Saving binary file";
            taskProgress.ProgressPercent = 90;
            if (progress != null) progress.Report(taskProgress);
            bathymetry.Save(outputPath + ".bathymetry");

            taskProgress.CurrentActivity = "Cleaning up";
            taskProgress.ProgressPercent = 95;
            if (progress != null) progress.Report(taskProgress);
            File.Delete(outputPath + ".yxz");
            File.Delete(outputPath + ".yxz_SECURITY_README.txt");

            taskProgress.CurrentActivity = "Done";
            taskProgress.ProgressPercent = 100;
            if (progress != null) progress.Report(taskProgress);
        }
    }
}