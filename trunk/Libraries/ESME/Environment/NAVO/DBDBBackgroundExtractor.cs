using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using Cinch;

namespace ESME.Environment.NAVO
{
    public class DBDBBackgroundExtractor : NAVOBackgroundExtractor
    {
        #region public Bathymetry Bathymetry { get; set; }

        public Bathymetry Bathymetry
        {
            get { return _bathymetry; }
            set
            {
                if (_bathymetry == value) return;
                _bathymetry = value;
                NotifyPropertyChanged(BathymetryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetryChangedEventArgs = ObservableHelper.CreateArgs<DBDBBackgroundExtractor>(x => x.Bathymetry);
        Bathymetry _bathymetry;

        #endregion

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            RunState = "Running";
            TaskName = "Bathymetry data extraction";
            var backgroundExtractor = (DBDBBackgroundExtractor)e.Argument;
            backgroundExtractor.Maximum = 3;
            var resolutions = new List<string>();
            var commandArgs = string.Format("resolutions \"{0}\"", backgroundExtractor.NAVOConfiguration.DBDBDirectory);
            var result = NAVOExtractionProgram.Execute(backgroundExtractor.NAVOConfiguration.DBDBEXEPath, commandArgs, null).Trim();
            var resarray = result.Split('\n');
            for (var index = 0; index < resarray.Length; index++)
            {
                var line = resarray[index].Trim();
                if (line.Contains("Available resolutions:"))
                {
                    for (var j = index + 1; j < resarray.Length; j++)
                    {
                        var resline = resarray[j].Trim();
                        if (resline.Equals("")) break;
                        resolutions.Add(resline);
                    }
                }
                if (line.Contains("ERROR:"))
                    throw new ApplicationException("DigitalBathymetricDatabase: Error reading available resolutions: " + line);
            }
            backgroundExtractor.Value++;

            //from documentation, area extractions for DBDB are of the form <dbv5_command path> area <database path> <finest_resolution> <coarsest resolution> nearest 0 meters G <south west north east> 
            //var commandArgs = string.Format(" area \"{0}\" {1} {2} nearest 0 meters G {3:0.0000} {4:0.0000} {5:0.0000} {6:0.0000} {7:0.0##} YXZ=\"{8}\"", DatabasePath, resolutions.First(), resolutions.Last(), RoundToResolution(extractionArea.South, desiredResolution), RoundToResolution(extractionArea.West, desiredResolution), RoundToResolution(extractionArea.North, desiredResolution), RoundToResolution(extractionArea.East, desiredResolution), desiredResolution, string.Format("bathymetry-{0}.yxz", selectedResolution));
            //var commandArgs = string.Format(" area \"{0}\" {1} {2} nearest 0 meters G {3:0.0000} {4:0.0000} {5:0.0000} {6:0.0000} {7:0.0##} YXZ=\"{8}\"", DatabasePath, resolutions.First(), resolutions.Last(), extractionArea.South, extractionArea.West, extractionArea.North, extractionArea.East, desiredResolution, string.Format("bathymetry-{0}.yxz", selectedResolution));
            var yxzFilename = string.Format("bathymetry-{0}min.yxz", backgroundExtractor.SelectedResolution);
            commandArgs = string.Format(" area \"{0}\" {1} {2} nearest 0 meters G {3} {4} {5} {6} {7:0.0##} YXZ=\"{8}\"", backgroundExtractor.NAVOConfiguration.DBDBDirectory, resolutions.First(), resolutions.Last(), backgroundExtractor.ExtractionArea.South, backgroundExtractor.ExtractionArea.West, backgroundExtractor.ExtractionArea.North, backgroundExtractor.ExtractionArea.East, backgroundExtractor.SelectedResolution, yxzFilename);
            //extract the area and look for success or failure in the output string.

            result = NAVOExtractionProgram.Execute(backgroundExtractor.NAVOConfiguration.DBDBEXEPath, commandArgs, backgroundExtractor.DestinationPath);
            resarray = result.Split('\n');
            foreach (var line in resarray.Where(line => line.Contains("ERROR"))) throw new ApplicationException("DigitalBathymetricDatabase: Error extracting requested area: " + line);
            backgroundExtractor.Value++;
            var yxzPath = Path.Combine(backgroundExtractor.DestinationPath, yxzFilename);
            Bathymetry = Bathymetry.FromYXZ(yxzPath, -1);
            File.Delete(yxzPath);
            backgroundExtractor.Value++;
            if (backgroundExtractor.SaveAsFilename != null)
            {
                var filename = backgroundExtractor.SaveAsFilename.ToLower();
                if (filename.EndsWith("yxz") || filename.EndsWith("txt")) Bathymetry.ToYXZ(backgroundExtractor.SaveAsFilename, -1);
                else if (filename.EndsWith("xml")) Bathymetry.Save(backgroundExtractor.SaveAsFilename);
                else throw new ApplicationException("Unknown file extension");
            }
        }
    }
}