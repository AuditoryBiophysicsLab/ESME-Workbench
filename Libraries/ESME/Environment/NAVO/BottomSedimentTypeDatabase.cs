using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using Cinch;

namespace ESME.Environment.NAVO
{
    public class BottomSedimentTypeDatabase : ViewModelBase
    {
        public BottomSedimentTypeDatabase() { Resolutions = new ObservableCollection<string>(); }

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

        void ResolutionsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(ResolutionsChangedEventArgs); SelectedResolution = Resolutions.First(); }
        static readonly PropertyChangedEventArgs ResolutionsChangedEventArgs = ObservableHelper.CreateArgs<BottomSedimentTypeDatabase>(x => x.Resolutions);
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

        static readonly PropertyChangedEventArgs SelectedResolutionChangedEventArgs = ObservableHelper.CreateArgs<BottomSedimentTypeDatabase>(x => x.SelectedResolution);
        string _selectedResolution;

        #endregion

        public void Initialize()
        {
            const string contents = "set";
            var scriptfile = Path.GetTempFileName();
            File.WriteAllText(scriptfile, contents);

            var commandArgs = string.Format("\"{0}\" \"{1}\"", DatabasePath, scriptfile);

            var result = NAVOExtractionProgram.Execute(ExtractionProgramPath, commandArgs, null).Trim();
            var resarray = result.Split('\n');
            if (!resarray[0].Contains("1.0")) throw new ApplicationException("Bottom Sediment Type Database extraction utility version is not 1.0; parser should be checked/rewritten");
            for (var index = 0; index < resarray.Length; index++)
            {
                var line = resarray[index].Trim();
                if (line.Contains("Database Resolutions (minutes):"))
                {
                    for (var j = index + 1; j < resarray.Length; j++)
                    {
                        var resline = resarray[j].Trim();
                        if (resline.Equals("")) break;
                        Resolutions.Add(resline.Split('-')[1].Trim()); //?  dear LORD is this ugly and brittle.
                    }
                }
                if (line.Contains("could not be opened")) throw new ApplicationException("BottomSedimentTypeDatabase: Error reading available resolutions: " + line);
            }

            File.Delete(scriptfile);
        }

        public static string SedimentFilename(string outputPath, string selectedResolution) { return Path.Combine(outputPath, string.Format("sediment-{0}.chb", selectedResolution)); }

        public static void ExtractArea(string outputDirectory, string selectedResolution, double north, double south, double east, double west)
        {
            var contents = string.Format("area {0} {1} {2} {3} {4} {5}", west, east, south, north, selectedResolution, string.Format("sediment-{0}.chb", selectedResolution));
            var scriptfile = Path.GetTempFileName();
            File.WriteAllText(scriptfile, contents);

            var commandArgs = string.Format("\"{0}\" \"{1}\"", DatabasePath, scriptfile);
            var result = NAVOExtractionProgram.Execute(ExtractionProgramPath, commandArgs, outputDirectory).Trim();
            //have a look at result and throw an error if needed.
            var resarray = result.Split('\n');
            foreach (var line in resarray.Where(line => line.Contains("error"))) throw new ApplicationException("BottomSedimentTypeDatabase: Error extracting requested area: " + line); //hope this is sufficient..
            File.Delete(scriptfile);
        }
    }
}