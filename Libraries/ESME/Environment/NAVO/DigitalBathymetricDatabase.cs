using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Cinch;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
    public class DigitalBathymetricDatabase : ViewModelBase
    {
        static DigitalBathymetricDatabase()
        {
            Resolutions = new List<string>();
            var thread = new Thread(Initialize);
            thread.Start();
        }

        public static string DatabasePath { get; set; }

        public static List<string> Resolutions { get; set; }

        #region public string SelectedResolution { get; set; }

        public string SelectedResolution
        {
            get { return _selectedResolution; }
            set
            {
                if (_selectedResolution == value) return;
                _selectedResolution = value;
                NotifyPropertyChanged(SelectedResolutionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedResolutionChangedEventArgs = ObservableHelper.CreateArgs<DigitalBathymetricDatabase>(x => x.SelectedResolution);
        string _selectedResolution;

        #endregion

        public static string BathymetryYXZFilename(string outputPath, string selectedResolution) { return Path.Combine(outputPath, string.Format("bathymetry-{0}.yxz", selectedResolution)); }
        
        public static void Initialize()
        {
            Resolutions.Clear();
            var commandArgs = string.Format("resolutions \"{0}\"", DatabasePath);
            var result = NAVOExtractionProgram.Execute(Globals.AppSettings.NAVOConfiguration.DBDBEXEPath, commandArgs, null);
            var resarray = result.Trim().Split('\n');
            for (var index = 0; index < resarray.Length; index++)
            {
                var line = resarray[index].Trim();
                if (line.Contains("Available resolutions:"))
                {
                    for (var j = index + 1; j < resarray.Length; j++)
                    {
                        var resline = resarray[j].Trim();
                        if (resline.Equals("")) break;
                        Resolutions.Add(resline);
                    }
                }
                if (line.Contains("ERROR:")) throw new ApplicationException("DigitalBathymetricDatabase: Error reading available resolutions: " + line);
            }
        }

        //public override void ExtractArea(NAVOExtractionPacket extractionPacket)
        public static Bathymetry ExtractAsync(string outputPath, string selectedResolution, GeoRect extractionArea)
        {
            var resTemp = selectedResolution.EndsWith("min") ? selectedResolution.Remove(selectedResolution.Length - 3) : selectedResolution;
            double desiredResolution;
            if (!double.TryParse(resTemp, out desiredResolution)) throw new FormatException("Illegal number format for selectedResolution: " + selectedResolution);

            var outputDirectory = Path.GetDirectoryName(outputPath);
            var commandArgs = string.Format(" area \"{0}\" 0.05min 2.00min nearest 0 meters G {1} {2} {3} {4} {5:0.0##} YXZ=\"{6}\"", Globals.AppSettings.NAVOConfiguration.DBDBDirectory, extractionArea.South, extractionArea.West, extractionArea.North, extractionArea.East, desiredResolution, string.Format("{0}.yxz", Path.GetFileName(outputPath)));
            //extract the area and look for success or failure in the output string.

            var result = NAVOExtractionProgram.Execute(Globals.AppSettings.NAVOConfiguration.DBDBEXEPath, commandArgs, outputDirectory);
            var resarray = result.Split('\n');
            foreach (var line in resarray.Where(line => line.Contains("ERROR"))) throw new ApplicationException("DigitalBathymetricDatabase: Error extracting requested area: " + line);
            var bathymetry = Bathymetry.FromYXZ(Path.Combine(outputPath + ".yxz"), -1);
            bathymetry.Save(outputPath + ".bathymetry");
            File.Delete(outputPath + ".yxz");
            return bathymetry;
        }

        public static Bathymetry ExtractArea(NAVOBackgroundExtractor backgroundExtractor)
        {
            backgroundExtractor.Maximum = 1;
            //from documentation, area extractions for DBDB are of the form <dbv5_command path> area <database path> <finest_resolution> <coarsest resolution> nearest 0 meters G <south west north east> 
            //var commandArgs = string.Format(" area \"{0}\" {1} {2} nearest 0 meters G {3:0.0000} {4:0.0000} {5:0.0000} {6:0.0000} {7:0.0##} YXZ=\"{8}\"", DatabasePath, resolutions.First(), resolutions.Last(), RoundToResolution(extractionArea.South, desiredResolution), RoundToResolution(extractionArea.West, desiredResolution), RoundToResolution(extractionArea.North, desiredResolution), RoundToResolution(extractionArea.East, desiredResolution), desiredResolution, string.Format("bathymetry-{0}.yxz", selectedResolution));
            //var commandArgs = string.Format(" area \"{0}\" {1} {2} nearest 0 meters G {3:0.0000} {4:0.0000} {5:0.0000} {6:0.0000} {7:0.0##} YXZ=\"{8}\"", DatabasePath, resolutions.First(), resolutions.Last(), extractionArea.South, extractionArea.West, extractionArea.North, extractionArea.East, desiredResolution, string.Format("bathymetry-{0}.yxz", selectedResolution));
            var yxzFilename = string.Format("bathymetry-{0}min.yxz", backgroundExtractor.SelectedResolution);
            var commandArgs = string.Format(" area \"{0}\" {1} {2} nearest 0 meters G {3} {4} {5} {6} {7:0.0##} YXZ=\"{8}\"", Globals.AppSettings.NAVOConfiguration.DBDBDirectory, Resolutions.First(), Resolutions.Last(), backgroundExtractor.ExtractionArea.South, backgroundExtractor.ExtractionArea.West, backgroundExtractor.ExtractionArea.North, backgroundExtractor.ExtractionArea.East, backgroundExtractor.SelectedResolution, yxzFilename);
            //extract the area and look for success or failure in the output string.

            var result = NAVOExtractionProgram.Execute(Globals.AppSettings.NAVOConfiguration.DBDBEXEPath, commandArgs, backgroundExtractor.DestinationPath);
            var resarray = result.Split('\n');
            foreach (var line in resarray.Where(line => line.Contains("ERROR"))) throw new ApplicationException("DigitalBathymetricDatabase: Error extracting requested area: " + line);
            backgroundExtractor.Value = 1;
            var bathymetry = Bathymetry.FromYXZ(Path.Combine(backgroundExtractor.DestinationPath, yxzFilename), -1);
            File.Delete(Path.Combine(backgroundExtractor.DestinationPath, yxzFilename));
            return bathymetry;
        }
    }
}