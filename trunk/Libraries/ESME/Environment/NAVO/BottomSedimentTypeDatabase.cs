using System;
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

        public static void ExtractAreaNew(string outputDirectory, string selectedResolution, GeoRect extractionArea)
        {
            const double highResInMinutes = 0.1;
            const double lowResInMinutes = 5.0;
            const double highResSecondsPerStep = 60 * highResInMinutes;
            const double lowResSecondsPerStep = 60 * lowResInMinutes;
            const string rootFormat = "/{0:0.00000}";
            var highResRootName = string.Format(rootFormat, highResInMinutes);
            var lowResRootName = string.Format(rootFormat, lowResInMinutes);
            const string gGroupName = "G";
            const string leafGroupName = "UNCLASSIFIED";

            var fileID = H5F.open(DatabasePath, H5F.OpenMode.ACC_RDONLY);
            int[] dims;
            var infoResult = H5LTgetDatasetInfo(fileID, "0.10000/G/UNCLASSIFIED/", out dims);
            var highResRootGroup = H5G.open(fileID, highResRootName);
            var highResMiddleGroup = H5G.open(highResRootGroup, gGroupName);
            var highRezLeafGroup = H5G.open(highResMiddleGroup, leafGroupName);
            var lowResRootGroup = H5G.open(fileID, lowResRootName);
            var lowResMiddleGroup = H5G.open(lowResRootGroup, gGroupName);
            var lowRezLeafGroup = H5G.open(lowResMiddleGroup, leafGroupName);
            BottomTypeAt(highRezLeafGroup, highResSecondsPerStep, lowRezLeafGroup, lowResSecondsPerStep, new EarthCoordinate());

            var latStart = (int) extractionArea.West;
            var latEnd = (int) extractionArea.East;
            var lonStart = (int) extractionArea.South;
            var lonEnd = (int) extractionArea.North;
        }

        const string Hdf5DLL = @"hdf5.dll";

        [DllImport(Hdf5DLL, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        private static unsafe extern int H5LTget_dataset_info(void* h5FileId, string dataSetName, int* dims, void* classId, int* size);
        protected static int H5LTgetDatasetInfo(H5FileId fileId, string dataSetName, out int[] dims)
        {
            unsafe
            {
            }
            dims = null;
            return 0;
        }
        [DllImport(Hdf5DLL, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        private static unsafe extern int H5LTread_dataset_short(void* h5FileId, string dataSetName, short* buffer);
        protected static int H5LTreadDatasetShort(H5FileId fileId, string dataSetName, out short[] data)
        {
            unsafe
            {
            }
            data = null;
            return 0;
        }

        public static short[,] BottomTypeAt(H5GroupId highResGroup, double highResSecondsPerStep, H5GroupId lowResGroup, double lowResSecondsPerStep, EarthCoordinate location)
        {
            var dataSetName = string.Format("{0}_{1}", (int) location.Latitude, (int) location.Longitude);
            short[,] array;
            int stepsPerDegree;
            var dataTypeId = new H5DataTypeId(H5T.H5Type.NATIVE_SHORT);
            
            var group = H5D.open(highResGroup, dataSetName);
            //H5S.getSimpleExtentDims(group);
            //var space = H5D.getSpace(group);

            if (group != null)
            {
                stepsPerDegree = (int) (3600 / highResSecondsPerStep);
                array = new short[stepsPerDegree, stepsPerDegree];
            }
            else
            {
                group = H5D.open(lowResGroup, dataSetName);
                stepsPerDegree = (int)(3600 / lowResSecondsPerStep);
                array = new short[stepsPerDegree, stepsPerDegree];
            }
            
            H5D.read(group, dataTypeId, new H5Array<short>(array));
            H5D.close(group);
            return array;
        }

        static int GetArrayIndex(double degrees, double secondsPerStep)
        {
            var pos = Math.Abs(degrees);

            var deg = (int)pos;

            var fraction = pos - deg;
            var seconds = fraction * 3600.0;

            var index = (int)(seconds / secondsPerStep);

            return index;
        }

        public static void ExtractArea(string outputDirectory, string selectedResolution, GeoRect extractionArea)
        {
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
    }
}