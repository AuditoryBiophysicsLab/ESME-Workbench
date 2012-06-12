using System;
using System.Collections.Generic;
using C5;
using ESME.Environment;
using HDF5DotNet;
using HRC.Navigation;
using HRC.Utility;

namespace NAVODatabaseAdapter
{
    public static class BST
    {
        public static Sediment Extract(string bstDirectory, GeoRect region, float resolution, PercentProgress progress = null)
        {
            if (progress != null) lock (progress) progress.Report(0);

            var north = (float)Math.Ceiling(region.North);
            var south = (float)Math.Floor(region.South);
            var east = (float)Math.Ceiling(region.East);
            var west = (float)Math.Floor(region.West);

            if (progress != null) progress.MaximumValue = (((north - south) * (east - west)) + 3);
            var totalProgress = 0;

            var fileId = H5F.open(bstDirectory, H5F.OpenMode.ACC_RDONLY);
            var highResGroup = H5G.open(fileId, "0.10000/G/UNCLASSIFIED/");
            highResGroup = null;
            var lowResGroup = H5G.open(fileId, "5.00000/G/UNCLASSIFIED/");
            var dedupeList = new HashedArrayList<SedimentSample>();
            for (var lat = south; lat < north; lat++)
                for (var lon = west; lon < east; lon++)
                {
                    var data = ReadDataset(highResGroup, lowResGroup, (int)lat, (int)lon, resolution);
                    if (data != null) dedupeList.AddAll(data);
                    if (progress != null) lock (progress) progress.Report(totalProgress++);
                }
            var sediment = new Sediment();
            if (progress != null) lock (progress) progress.Report(totalProgress++);
            sediment.Samples.AddRange(dedupeList);
            sediment.Samples.Sort();
            if (progress != null) lock (progress) progress.Report(totalProgress++);
            if (lowResGroup != null) H5G.close(lowResGroup);
            if (highResGroup != null) H5G.close(highResGroup);
            H5F.close(fileId);
            if (progress != null) lock (progress) progress.Report(totalProgress);
            return sediment;
        }

        static IEnumerable<SedimentSample> ReadDataset(H5FileOrGroupId highResGroup, H5FileOrGroupId lowResGroup, int latitude, int longitude, float resolution)
        {
            short[,] result = null;
            double resolutionStep;
            float sampleStepSize;

            if (highResGroup != null) result = ReadDataset(highResGroup, latitude, longitude);
            if (result != null)
            {
                resolutionStep = 6.0 / 3600.0;
                sampleStepSize = resolution / 0.1f;
            }
            else
            {
                if (lowResGroup != null) result = ReadDataset(lowResGroup, latitude, longitude);
                //if (result == null) throw new KeyNotFoundException(string.Format("Unable to locate sediment data for lat: {0}, lon: {1}", latitude, longitude));
                if (result == null) return null;
                resolutionStep = 5.0 / 60.0;
                sampleStepSize = resolution / 5f;
            }

            var sedimentList = new HashedArrayList<SedimentSample>();
            for (var i = 0f; i < result.GetLength(0); i += sampleStepSize)
                for (var j = 0f; j < result.GetLength(1); j += sampleStepSize)
                    if (result[(int)i, (int)j] > 0) sedimentList.Add(new SedimentSample(latitude + (i * resolutionStep), longitude + (j * resolutionStep), new SedimentSampleBase { SampleValue = result[(int)i, (int)j] }));
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