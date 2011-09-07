using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using Cinch;
using HDF5DotNet;
using C5;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.NAVO
{
    public static class BST
    {
        public async static Task<Sediment> ExtractAsync(GeoRect region, IProgress<float> progress = null)
        {
            if (progress != null) lock (progress) progress.Report(0f);

            var north = (float)Math.Ceiling(region.North);
            var south = (float)Math.Floor(region.South);
            var east = (float)Math.Ceiling(region.East);
            var west = (float)Math.Floor(region.West);

            var progressStep = 100f / (((north - south) * (east - west) * 2) + 3);
            var totalProgress = 0f;

            var fileId = H5F.open(Globals.AppSettings.NAVOConfiguration.BSTDirectory, H5F.OpenMode.ACC_RDONLY);
            var highResGroup = H5G.open(fileId, "0.10000/G/UNCLASSIFIED/");
            var lowResGroup = H5G.open(fileId, "5.00000/G/UNCLASSIFIED/");
            var dedupeList = new HashedArrayList<SedimentSample>();
            for (var lat = south; lat < north; lat++)
                for (var lon = west; lon < east; lon++)
                {
                    var data = await ReadDataset(highResGroup, lowResGroup, (int)lat, (int)lon);
                    if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
                    if (data != null) dedupeList.AddAll(data);
                    if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
                }
            var sediment = new Sediment();
            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            sediment.Samples.AddRange(dedupeList);
            sediment.Samples.Sort();
            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            sediment.Samples.TrimToNearestPoints(region);
            H5G.close(lowResGroup);
            H5G.close(highResGroup);
            H5F.close(fileId);
            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            return sediment;
        }

        static async Task<IEnumerable<SedimentSample>> ReadDataset(H5FileOrGroupId highResGroup, H5FileOrGroupId lowResGroup, int latitude, int longitude)
        {
            var result = await ReadDataset(highResGroup, latitude, longitude);
            double resolutionStep;
            if (result != null)
            {
                resolutionStep = 6.0 / 3600.0;
            }
            else
            {
                result = await ReadDataset(lowResGroup, latitude, longitude);
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

        static async Task<short[,]> ReadDataset(H5FileOrGroupId groupId, int latitude, int longitude)
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
}