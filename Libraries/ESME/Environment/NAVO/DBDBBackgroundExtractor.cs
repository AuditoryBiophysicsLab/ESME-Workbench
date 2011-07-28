using System;
using System.ComponentModel;
using System.Linq;

namespace ESME.Environment.NAVO
{
    public class DBDBBackgroundExtractor : NAVOBackgroundExtractor
    {
        protected override void Run(object sender, DoWorkEventArgs e)
        {
            RunState = "Running";
            TaskName = "Bathymetry data extraction";
            var backgroundExtractor = (DBDBBackgroundExtractor)e.Argument;
            backgroundExtractor.Maximum = 3;
            backgroundExtractor.Value++;

            //from documentation, area extractions for DBDB are of the form <dbv5_command path> area <database path> <finest_resolution> <coarsest resolution> nearest 0 meters G <south west north east> 
            var commandArgs = string.Format(" area \"{0}\" {1} {2} nearest 0 meters G {3} {4} {5} {6} {7:0.0##} YXZ=\"{8}\"", backgroundExtractor.NAVOConfiguration.DBDBDirectory, "0.05min", "2.00min", backgroundExtractor.ExtractionArea.South, backgroundExtractor.ExtractionArea.West, backgroundExtractor.ExtractionArea.North, backgroundExtractor.ExtractionArea.East, backgroundExtractor.SelectedResolution, backgroundExtractor.SaveAsFilename);
            //extract the area and look for success or failure in the output string.

            var result = NAVOExtractionProgram.Execute(backgroundExtractor.NAVOConfiguration.DBDBEXEPath, commandArgs, backgroundExtractor.DestinationPath);
            backgroundExtractor.Value++;
            var resarray = result.Split('\n');
            foreach (var line in resarray.Where(line => line.Contains("ERROR"))) throw new ApplicationException("DigitalBathymetricDatabase: Error extracting requested area: " + line);
            backgroundExtractor.Value++;
        }
    }
}