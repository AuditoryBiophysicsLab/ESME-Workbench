﻿using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using Cinch;
using HRC.Navigation;
using HDF5DotNet;

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

        public static void ExtractArea(string outputDirectory, string selectedResolution, GeoRect extractionArea)
        {
            ExtractAreaNew(outputDirectory, extractionArea);
            var contents = string.Format("area {0} {1} {2} {3} {4} {5}", extractionArea.West, extractionArea.East, extractionArea.South, extractionArea.North, selectedResolution, string.Format("sediment-{0}.chb", selectedResolution));
            var scriptfile = Path.Combine(outputDirectory, "sediment_extract.script");
            File.WriteAllText(scriptfile, contents);

            var commandArgs = string.Format("\"{0}\" \"{1}\"", DatabasePath, scriptfile);

            var batchFilename = Path.Combine(outputDirectory, "sediment_extract.bat");
            using (var batchFile = new StreamWriter(batchFilename, false))
                batchFile.WriteLine("\"{0}\" {1}", ExtractionProgramPath, commandArgs);

            var result = NAVOExtractionProgram.Execute(ExtractionProgramPath, commandArgs, outputDirectory).Trim();
            //have a look at result and throw an error if needed.
            var resarray = result.Split('\n');
            foreach (var line in resarray.Where(line => line.Contains("error"))) throw new ApplicationException("BottomSedimentTypeDatabase: Error extracting requested area: " + line); //hope this is sufficient..
            //File.Delete(scriptfile);
        }

        public static void ExtractAreaNew(string outputDirectory, GeoRect extractionArea)
        {
            var results = new SedimentNew();
            var fileID = H5F.open(DatabasePath, H5F.OpenMode.ACC_RDONLY);
            var highResGroup = H5G.open(fileID, "0.10000/G/UNCLASSIFIED/");
            var lowResGroup = H5G.open(fileID, "5.00000/G/UNCLASSIFIED/");

            for (var lat = (int)extractionArea.South; lat <= (int)extractionArea.North; lat++)
                for (var lon = (int)extractionArea.West; lon <= (int)extractionArea.East; lon++)
                {
                    var data = ReadDataset(highResGroup, lowResGroup, lat, lon);
                    if (data != null) results.AddRange(data.Where(extractionArea.Contains));
                }

            H5G.close(lowResGroup);
            H5G.close(highResGroup);
            H5F.close(fileID);

            results.Save(Path.Combine(outputDirectory, "sediment.xml"));
        }

        static IEnumerable<SedimentSampleNew> ReadDataset(H5FileOrGroupId highResGroup, H5FileOrGroupId lowResGroup, int latitude, int longitude)
        {
            var result = ReadDataset(highResGroup, latitude, longitude);
            double resolutionStep;
            if (result != null)
            {
                resolutionStep = 6.0 / 3600.0;
            }
            else
            {
                result = ReadDataset(lowResGroup, latitude, longitude);
                //if (result == null) throw new KeyNotFoundException(string.Format("Unable to locate sediment data for lat: {0}, lon: {1}", latitude, longitude));
                if (result == null) return null;
                resolutionStep = 5.0 / 60.0;
            }
            var sedimentList = new List<SedimentSampleNew>();
            for (var i = 0; i < result.GetLength(0); i++)
                for (var j = 0; j < result.GetLength(1); j++)
                    sedimentList.Add(new SedimentSampleNew(latitude + (i * resolutionStep), longitude + (j * resolutionStep), result[i, j]));
            return sedimentList;
        }

        static short[,] ReadDataset(H5FileOrGroupId groupId, int latitude, int longitude)
        {
            try
            {
                var data = H5D.open(groupId, string.Format("{0}_{1}", latitude, longitude));
                var sid = H5D.getSpace(data);
                var dims = H5S.getSimpleExtentDims(sid);
                var readBuf = new short[dims[0], dims[1]];
                H5D.read(data, H5T.copy(H5T.H5Type.NATIVE_SHORT), new H5Array<short>(readBuf));
                H5D.close(data);
                return readBuf;
            }
            catch (H5DopenException)
            {
                return null;
            }
        }

    }
}