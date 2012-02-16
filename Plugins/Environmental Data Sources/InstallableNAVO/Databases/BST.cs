using System;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using C5;
using ESME;
using ESME.Environment;
using HDF5DotNet;
using HRC.Navigation;
using System.Diagnostics;

namespace InstallableNAVO.Databases
{
    public static class BST
    {
        readonly static object LockObject = new object();
        static ActionBlock<Tuple<string, GeoRect, IProgress<string>, IProgress<float>, Action>> _bstWorkQueue;
        public static void ImportAsync(string outputPath, GeoRect region, Action completionAction, IProgress<string> currentState = null, IProgress<float> progress = null)
        {
            lock (LockObject)
            {
                if (_bstWorkQueue == null)
                {
                    _bstWorkQueue = new ActionBlock<Tuple<string, GeoRect, IProgress<string>, IProgress<float>, Action>>(item => TaskEx.Run(() =>
                    {
                        Debug.WriteLine("{0}: About to import sediment data for {1}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(item.Item1)));
                        var result = Extract(item.Item2, item.Item3, item.Item4);
                        if (item.Item3 != null) lock (item.Item3) item.Item3.Report("Saving");
                        result.Save(Path.Combine(item.Item1, "data.sediment"));
                        Debug.WriteLine("{0}: Sediment import completed for {1}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(item.Item1)));
                        item.Item5();
                    }), new ExecutionDataflowBlockOptions
                    {
                        TaskScheduler = TaskScheduler.Default,
                        MaxDegreeOfParallelism = 1,
                    });
                }
            }
            Debug.WriteLine("{0}: Queueing task to import sediment data for {1}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(outputPath)));
            if (currentState != null) lock (currentState) currentState.Report("Queued");
            _bstWorkQueue.Post(new Tuple<string, GeoRect, IProgress<string>, IProgress<float>, Action>(outputPath, region, currentState, progress, completionAction));
        }

        public static Sediment Extract(GeoRect region, IProgress<string> currentState = null, IProgress<float> progress = null)
        {
            if (progress != null) lock (progress) progress.Report(0f);
            if (currentState != null) lock (currentState) currentState.Report("Importing sediment data");

            var north = (float)Math.Ceiling(region.North);
            var south = (float)Math.Floor(region.South);
            var east = (float)Math.Ceiling(region.East);
            var west = (float)Math.Floor(region.West);

            var progressStep = 100f / (((north - south) * (east - west)) + 3);
            var totalProgress = 0f;

            var fileId = H5F.open(Globals.AppSettings.NAVOConfiguration.BSTDirectory, H5F.OpenMode.ACC_RDONLY);
            //var highResGroup = H5G.open(fileId, "0.10000/G/UNCLASSIFIED/");
            var lowResGroup = H5G.open(fileId, "5.00000/G/UNCLASSIFIED/");
            var dedupeList = new HashedArrayList<SedimentSample>();
            if (currentState != null) lock (currentState) currentState.Report("Reading sediment database");
            for (var lat = south; lat < north; lat++)
                for (var lon = west; lon < east; lon++)
                {
                    var data = ReadDataset(null /* highResGroup */, lowResGroup, (int)lat, (int)lon);
                    if (data != null) dedupeList.AddAll(data);
                    if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
                }
            var sediment = new Sediment();
            if (currentState != null) lock (currentState) currentState.Report("Removing duplicate data");
            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            sediment.Samples.AddRange(dedupeList);
            sediment.Samples.Sort();
            if (currentState != null) lock (currentState) currentState.Report("Trimming excess data");
            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            if (lowResGroup != null) H5G.close(lowResGroup);
            //if (highResGroup != null) H5G.close(highResGroup);
            H5F.close(fileId);
            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            return sediment;
        }

        static IEnumerable<SedimentSample> ReadDataset(H5FileOrGroupId highResGroup, H5FileOrGroupId lowResGroup, int latitude, int longitude)
        {
            short[,] result = null;
            double resolutionStep;
            int sampleStepSize;

            if (highResGroup != null) result = ReadDataset(highResGroup, latitude, longitude);
            if (result != null)
            {
                resolutionStep = 6.0 / 3600.0;
                sampleStepSize = 50;
            }
            else
            {
                if (lowResGroup != null) result = ReadDataset(lowResGroup, latitude, longitude);
                //if (result == null) throw new KeyNotFoundException(string.Format("Unable to locate sediment data for lat: {0}, lon: {1}", latitude, longitude));
                if (result == null) return null;
                resolutionStep = 5.0 / 60.0;
                sampleStepSize = 1;
            }

            var sedimentList = new HashedArrayList<SedimentSample>();
            for (var i = 0; i < result.GetLength(0); i += sampleStepSize)
                for (var j = 0; j < result.GetLength(1); j += sampleStepSize)
                    if (result[i, j] > 0) sedimentList.Add(new SedimentSample(latitude + (i * resolutionStep), longitude + (j * resolutionStep), new SedimentSampleBase {SampleValue = result[i, j]}));
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
            catch (HDFException)
            {
                return null;
            }
        }
    }
}