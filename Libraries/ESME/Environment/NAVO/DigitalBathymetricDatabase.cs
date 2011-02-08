using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using Cinch;

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

        public static string BathymetryFilename(string outputPath, string selectedResolution) { return Path.Combine(outputPath, string.Format("bathymetry-{0}.chb", selectedResolution)); }
        
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
        public static void ExtractArea(string outputDirectory, string selectedResolution, double north, double south, double east, double west, IList<string> resolutions)
        {
            //from documentation, area extractions for DBDB are of the form <dbv5_command path> area <database path> <finest_resolution> <coarsest resolution> nearest 0 meters G <south west north east> 
            var commandArgs = string.Format(" area \"{0}\" {1} {2} nearest 0 meters G {3} {4} {5} {6} {7} CHB=\"{8}\"", DatabasePath, resolutions.First(), resolutions.Last(), south, west, north, east, selectedResolution, string.Format("bathymetry-{0}.chb", selectedResolution));
            //extract the area and look for success or failure in the output string.
            var result = NAVOExtractionProgram.Execute(ExtractionProgramPath, commandArgs, outputDirectory);
            var resarray = result.Split('\n');
            foreach (var line in resarray.Where(line => line.Contains("ERROR"))) throw new ApplicationException("DigitalBathymetricDatabase: Error extracting requested area: " + line);
        }
    }
}