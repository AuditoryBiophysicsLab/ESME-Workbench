using System;
using System.Collections.Generic;
using System.ComponentModel;
using Cinch;
using HDF5DotNet;
using C5;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.NAVO
{
#if false
    public class BSTBackgroundExtractor : NAVOBackgroundExtractor
    {
        #region public Sediment Sediment { get; set; }

        public Sediment Sediment
        {
            get { return _sediment; }
            set
            {
                if (_sediment == value) return;
                _sediment = value;
                NotifyPropertyChanged(SedimentChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SedimentChangedEventArgs = ObservableHelper.CreateArgs<BSTBackgroundExtractor>(x => x.Sediment);
        Sediment _sediment;

        #endregion

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            RunState = "Running";
            TaskName = "Sediment data extraction";
            var backgroundExtractor = (NAVOBackgroundExtractor)e.Argument;
            backgroundExtractor.Status = "Extracting sediment data";

            var north = (int)Math.Ceiling(backgroundExtractor.ExtractionArea.North);
            var south = (int)Math.Floor(backgroundExtractor.ExtractionArea.South);
            var east = (int)Math.Ceiling(backgroundExtractor.ExtractionArea.East);
            var west = (int)Math.Floor(backgroundExtractor.ExtractionArea.West);

            backgroundExtractor.Maximum = (north - south) * (east - west) + 3;

            var fileId = H5F.open(backgroundExtractor.NAVOConfiguration.BSTDirectory, H5F.OpenMode.ACC_RDONLY);
            var highResGroup = H5G.open(fileId, "0.10000/G/UNCLASSIFIED/");
            var lowResGroup = H5G.open(fileId, "5.00000/G/UNCLASSIFIED/");
            var dedupeList = new HashedArrayList<SedimentSample>();
            for (var lat = south; lat < north; lat++)
                for (var lon = west; lon < east; lon++)
                {
                    var data = ReadDataset(null, lowResGroup, lat, lon);
                    //if (data != null) results.Samples.AddRange(data.Where(extractionArea.Contains));
                    if (data != null) dedupeList.AddAll(data);
                    backgroundExtractor.Value++;
                }
            Sediment = new Sediment();
            Sediment.Samples.AddRange(dedupeList);
            Sediment.Samples.Sort();
            backgroundExtractor.Status = string.Format("Sediment: Extracted {0} raw points from database", Sediment.Samples.Count);

            //TaskName = "Sediment: Removing duplicate data";
            //Sediment.Samples.RemoveDuplicates(backgroundExtractor);
            backgroundExtractor.Value++;
            RunState = "Trimming data to selected region";
            if (!backgroundExtractor.UseExpandedExtractionArea) Sediment.Samples.TrimToNearestPoints(backgroundExtractor.ExtractionArea);
            backgroundExtractor.Value++;

            H5G.close(lowResGroup);
            H5G.close(highResGroup);
            H5F.close(fileId);
            if (backgroundExtractor.SaveAsFilename != null)
            {
                RunState = "Saving";
                Sediment.Save(backgroundExtractor.SaveAsFilename);
                RunState = "Saved";
            }
            backgroundExtractor.Value++;
        }

        public static void Extract(float north, float south, float east, float west, System.Collections.Generic.IList<NAVOTimePeriod> timePeriods, string outputFilename, IProgress<TaskProgressInfo> progress = null)
        {
            var taskProgress = new TaskProgressInfo
            {
                TaskName = "BST Extraction",
                CurrentActivity = "Initializing",
                ProgressPercent = 0,
            };
            if (progress != null) progress.Report(taskProgress);

            var trimRect = new GeoRect(north, south, east, west);
            north = (float)Math.Ceiling(north);
            south = (float)Math.Floor(south);
            east = (float)Math.Ceiling(east);
            west = (float)Math.Floor(west);

            var maxProgress = (north - south) * (east - west) + 3;
            var curProgress = 0f;

            var fileId = H5F.open(Globals.AppSettings.NAVOConfiguration.BSTDirectory, H5F.OpenMode.ACC_RDONLY);
            var highResGroup = H5G.open(fileId, "0.10000/G/UNCLASSIFIED/");
            var lowResGroup = H5G.open(fileId, "5.00000/G/UNCLASSIFIED/");
            var dedupeList = new HashedArrayList<SedimentSample>();
            for (var lat = south; lat < north; lat++)
                for (var lon = west; lon < east; lon++)
                {
                    var data = ReadDataset(null, lowResGroup, (int)lat, (int)lon);
                    //if (data != null) results.Samples.AddRange(data.Where(extractionArea.Contains));
                    if (data != null) dedupeList.AddAll(data);
                    curProgress++;
                }
            var sediment = new Sediment();
            sediment.Samples.AddRange(dedupeList);
            sediment.Samples.Sort();
            //TaskName = "Sediment: Removing duplicate data";
            //Sediment.Samples.RemoveDuplicates(backgroundExtractor);
            curProgress++;
            taskProgress.CurrentActivity = "Trimming data to selected region";
            sediment.Samples.TrimToNearestPoints(trimRect);
            curProgress++;

            H5G.close(lowResGroup);
            H5G.close(highResGroup);
            H5F.close(fileId);
            sediment.Save(outputFilename + ".sediment");
            taskProgress.CurrentActivity = "Done";
            taskProgress.ProgressPercent = 100;
            if (progress != null) progress.Report(taskProgress);
        }

        static IEnumerable<SedimentSample> ReadDataset(H5FileOrGroupId highResGroup, H5FileOrGroupId lowResGroup, int latitude, int longitude)
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
            var sedimentList = new HashedArrayList<SedimentSample>();
            for (var i = 0; i < result.GetLength(0); i++)
                for (var j = 0; j < result.GetLength(1); j++)
                    sedimentList.Add(new SedimentSample(latitude + (i * resolutionStep), longitude + (j * resolutionStep), new SedimentSampleBase {SampleValue = result[i, j]}));
            return sedimentList;
        }

        static short[,] ReadDataset(H5FileOrGroupId groupId, int latitude, int longitude)
        {
            if (groupId == null) return null;
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
#endif
}