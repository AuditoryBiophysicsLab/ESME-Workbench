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
    public class DBDB : NAVODataSource
    {
        public DBDB() { Resolutions = new ObservableCollection<string>(); }

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
        static readonly PropertyChangedEventArgs ResolutionsChangedEventArgs = ObservableHelper.CreateArgs<DBDB>(x => x.Resolutions);
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

        static readonly PropertyChangedEventArgs SelectedResolutionChangedEventArgs = ObservableHelper.CreateArgs<DBDB>(x => x.SelectedResolution);
        string _selectedResolution;

        #endregion

        public void GetAllResolutions()
        {
            CommandArgs = string.Format("resolutions \"{0}\"", DatabasePath);
            var result = Execute().Trim();
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
                if (line.Contains("ERROR:")) throw new ApplicationException("DBDB " + line);
            }
        }

        public override void ExtractArea(NAVOExtractionPacket extractionPacket)
        {
            var filename = Path.Combine(extractionPacket.Filename, "DBDB.chb");
            WorkingDirectory = Path.GetDirectoryName(filename);
            var north = extractionPacket.North;
            var south = extractionPacket.South;
            var east = extractionPacket.East;
            var west = extractionPacket.West;
            //from documentation, area extractions for DBDB are of the form <dbv5_command path> area <database path> <finest_resolution> <coarsest resolution> nearest 0 meters G <south west north east> 
            CommandArgs = string.Format(" area \"{0}\" {1} {2} nearest 0 meters G {3} {4} {5} {6} {7} CHB=\"{8}\"", DatabasePath, Resolutions[0], Resolutions[Resolutions.Count - 1], south, west, north, east, SelectedResolution, Path.GetFileName(filename));
            //extract the area and look for success or failure in the output string.
            var result = Execute();
            var resarray = result.Split('\n');
            foreach (var line in resarray.Where(line => line.Contains("ERROR"))) throw new ApplicationException("DBDB: " + line);

            //return the extracted data from file as Environment2DData
            ExtractedArea = Parse(filename);
        }
        public static Environment2DData Parse(string filename)
        {
            return Environment2DData.ReadChrtrBinaryFile(filename);
        }

        public override bool ValidateDataSource() { return false; } //DBDB provides no test data. 
    }
}