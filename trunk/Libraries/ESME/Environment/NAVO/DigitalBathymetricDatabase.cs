using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using Cinch;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
    public class DigitalBathymetricDatabase : ViewModelBase
    {
        public DigitalBathymetricDatabase() { Resolutions = new ObservableCollection<string>(); }

        public static string DatabasePath { get; set; }
        public static string ExtractionProgramPath { get; set; }
        
        #region public ObservableCollection<string> Resolutions { get; set; }

        public ObservableCollection<string> Resolutions
        {
            get { return _resolutions; }
            set
            {
                if (_resolutions == value) return;
                if (_resolutions != null) _resolutions.CollectionChanged -= ResolutionsCollectionChanged;
                _resolutions = value;
                if (_resolutions != null) _resolutions.CollectionChanged += ResolutionsCollectionChanged;
                NotifyPropertyChanged(ResolutionsChangedEventArgs);

                
            }
        }

        void ResolutionsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(ResolutionsChangedEventArgs); SelectedResolution = Resolutions.Last(); }
        static readonly PropertyChangedEventArgs ResolutionsChangedEventArgs = ObservableHelper.CreateArgs<DigitalBathymetricDatabase>(x => x.Resolutions);
        ObservableCollection<string> _resolutions;

        #endregion

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

        public static string BathymetryCHBFilename(string outputPath, string selectedResolution) { return Path.Combine(outputPath, string.Format("bathymetry-{0}.chb", selectedResolution)); }
        public static string BathymetryYXZFilename(string outputPath, string selectedResolution) { return Path.Combine(outputPath, string.Format("bathymetry-{0}.yxz", selectedResolution)); }
        
        public void Initialize()
        {
            var commandArgs = string.Format("resolutions \"{0}\"", DatabasePath);
            var result = NAVOExtractionProgram.Execute(ExtractionProgramPath, commandArgs, null).Trim();
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
                        Resolutions.Add(resline);
                    }
                }
                if (line.Contains("ERROR:")) throw new ApplicationException("DigitalBathymetricDatabase: Error reading available resolutions: " + line);
            }
        }

        //public override void ExtractArea(NAVOExtractionPacket extractionPacket)
        public static void ExtractArea(string outputDirectory, string selectedResolution, GeoRect extractionArea, IList<string> resolutions)
        {
            string resTemp;
            if (selectedResolution.EndsWith("min")) resTemp = selectedResolution.Remove(selectedResolution.Length - 3);
            else resTemp = selectedResolution;
            double desiredResolution;
            if (!double.TryParse(resTemp, out desiredResolution)) throw new FormatException("Illegal number format for selectedResolution: " + selectedResolution);

            //from documentation, area extractions for DBDB are of the form <dbv5_command path> area <database path> <finest_resolution> <coarsest resolution> nearest 0 meters G <south west north east> 
            //var commandArgs = string.Format(" area \"{0}\" {1} {2} nearest 0 meters G {3:0.0000} {4:0.0000} {5:0.0000} {6:0.0000} {7:0.0##} YXZ=\"{8}\"", DatabasePath, resolutions.First(), resolutions.Last(), RoundToResolution(extractionArea.South, desiredResolution), RoundToResolution(extractionArea.West, desiredResolution), RoundToResolution(extractionArea.North, desiredResolution), RoundToResolution(extractionArea.East, desiredResolution), desiredResolution, string.Format("bathymetry-{0}.yxz", selectedResolution));
            //var commandArgs = string.Format(" area \"{0}\" {1} {2} nearest 0 meters G {3:0.0000} {4:0.0000} {5:0.0000} {6:0.0000} {7:0.0##} YXZ=\"{8}\"", DatabasePath, resolutions.First(), resolutions.Last(), extractionArea.South, extractionArea.West, extractionArea.North, extractionArea.East, desiredResolution, string.Format("bathymetry-{0}.yxz", selectedResolution));
            var commandArgs = string.Format(" area \"{0}\" {1} {2} nearest 0 meters G {3} {4} {5} {6} {7:0.0##} YXZ=\"{8}\"", DatabasePath, resolutions.First(), resolutions.Last(), extractionArea.South, extractionArea.West, extractionArea.North, extractionArea.East, desiredResolution, string.Format("bathymetry-{0}.yxz", selectedResolution));
            //extract the area and look for success or failure in the output string.

            var batchFilename = Path.Combine(outputDirectory, "bathy_extract.bat");
            using (var batchFile = new StreamWriter(batchFilename, false))
                batchFile.WriteLine("\"{0}\" {1}", ExtractionProgramPath, commandArgs);

            var result = NAVOExtractionProgram.Execute(ExtractionProgramPath, commandArgs, outputDirectory);
            var resarray = result.Split('\n');
            foreach (var line in resarray.Where(line => line.Contains("ERROR"))) throw new ApplicationException("DigitalBathymetricDatabase: Error extracting requested area: " + line);
        }

        static double RoundToResolution(double originalValueInDegrees, double desiredResolutionInMinutes)
        {
            var scaleFactor = 60.0f / desiredResolutionInMinutes;
            return Math.Round(originalValueInDegrees * scaleFactor) / scaleFactor;
        }
    }
}