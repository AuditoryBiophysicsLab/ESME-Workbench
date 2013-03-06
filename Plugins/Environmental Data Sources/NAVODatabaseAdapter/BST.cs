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
        const short NoData = 888;

        public static Sediment Extract(string bstDirectory, GeoRect region, float resolution, PercentProgress progress = null)
        {
            if (progress != null) lock (progress) progress.Report(0);

            var north = (float)Math.Round(region.North + 1);
            var south = (float)Math.Round(region.South - 1);
            var east = (float)Math.Round(region.East + 1);
            var west = (float)Math.Round(region.West - 1);

            if (progress != null) progress.MaximumValue = (((north - south) * (east - west)) + 3);
            var totalProgress = 0;

            var fileId = H5F.open(bstDirectory, H5F.OpenMode.ACC_RDONLY);
            var highResGroup = H5G.open(fileId, "0.10000/G/UNCLASSIFIED/");
            var lowResGroup = H5G.open(fileId, "5.00000/G/UNCLASSIFIED/");
            var dedupeList = new HashedArrayList<SedimentSample>();
            for (var lat = south; lat < north; lat++)
                for (var lon = west; lon < east; lon++)
                {
                    //var data = ReadDataset(highResGroup, 0.1, lowResGroup, 5.0, resolution, (int)lat, (int)lon);
                    var data = ReadDatasetHierarchical(highResGroup, 0.1, lowResGroup, 5.0, resolution, (int)lat, (int)lon);
                    if (data != null) dedupeList.AddAll(data);
                    if (progress != null) lock (progress) progress.Report(totalProgress++);
                }
            var sediment = new Sediment();
            if (progress != null) lock (progress) progress.Report(totalProgress++);
            sediment.Samples.AddRange(dedupeList);
            sediment.Samples.Sort();
            sediment.Samples.TrimToNearestPoints(region);
            if (progress != null) lock (progress) progress.Report(totalProgress++);
            if (lowResGroup != null) H5G.close(lowResGroup);
            if (highResGroup != null) H5G.close(highResGroup);
            H5F.close(fileId);
            if (progress != null) lock (progress) progress.Report(totalProgress);
            return sediment;
        }

        /// <summary>
        /// Read a 1x1 degree square from the HDF5 file, preferentially using data from the high-resolution dataset if it exists, and filling
        /// in any holes in the high-res data from the corresponding area of the low-resolution dataset
        /// </summary>
        /// <param name="highResGroup">Group ID of the high resolution group</param>
        /// <param name="highResolution">Resolution of the high resolution group, in minutes per sample</param>
        /// <param name="lowResGroup">Group ID of the low resolution group</param>
        /// <param name="lowResolution">Resolution of the low resolution group, in minutes per sample</param>
        /// <param name="desiredResolution">Desired resolution of the output data, in minutes per sample</param>
        /// <param name="latitude">Latitude of the south-west corner of the 1x1 degree square to be extracted from the HDF5 file</param>
        /// <param name="longitude">Longitude of the south-west corner of the 1x1 degree square to be extracted from the HDF5 file</param>
        /// <returns></returns>
        static IEnumerable<SedimentSample> ReadDataset(H5FileOrGroupId highResGroup, double highResolution, H5FileOrGroupId lowResGroup, double lowResolution, double desiredResolution, int latitude, int longitude)
        {
            short[,] result = null;
            double resolutionStep;
            double sampleStepSize;

            if (highResGroup != null) result = ReadDataset(highResGroup, latitude, longitude);
            if (result != null)
            {
                resolutionStep = highResolution / 60;
                sampleStepSize = desiredResolution / highResolution;
            }
            else
            {
                if (lowResGroup != null) result = ReadDataset(lowResGroup, latitude, longitude);
                //if (result == null) throw new KeyNotFoundException(string.Format("Unable to locate sediment data for lat: {0}, lon: {1}", latitude, longitude));
                if (result == null) return null;
                resolutionStep = lowResolution / 60.0;
                sampleStepSize = desiredResolution / lowResolution;
            }

            var sedimentList = new HashedArrayList<SedimentSample>();
            for (var i = 0.0; i < result.GetLength(0); i += sampleStepSize)
                for (var j = 0.0; j < result.GetLength(1); j += sampleStepSize)
                    if (result[(int)i, (int)j] > 0) sedimentList.Add(new SedimentSample(latitude + (i * resolutionStep), longitude + (j * resolutionStep), new SedimentSampleBase { SampleValue = result[(int)i, (int)j] }));
            return sedimentList;
        }

        /// <summary>
        /// Read a 1x1 degree square from the HDF5 file, preferentially using data from the high-resolution dataset if it exists, and filling
        /// in any holes in the high-res data from the corresponding area of the low-resolution dataset
        /// </summary>
        /// <param name="highResGroup">Group ID of the high resolution group</param>
        /// <param name="highResolution">Resolution of the high resolution group, in minutes per sample</param>
        /// <param name="lowResGroup">Group ID of the low resolution group</param>
        /// <param name="lowResolution">Resolution of the low resolution group, in minutes per sample</param>
        /// <param name="desiredResolution">Desired resolution of the output data, in minutes per sample</param>
        /// <param name="latitude">Latitude of the south-west corner of the 1x1 degree square to be extracted from the HDF5 file</param>
        /// <param name="longitude">Longitude of the south-west corner of the 1x1 degree square to be extracted from the HDF5 file</param>
        /// <returns></returns>
        static IEnumerable<SedimentSample> ReadDatasetHierarchical(H5FileOrGroupId highResGroup, double highResolution, H5FileOrGroupId lowResGroup, double lowResolution, double desiredResolution, int latitude, int longitude)
        {
            short[,] highResData = null;
            short[,] lowResData = null;
            if (highResGroup != null) highResData = ReadDataset(highResGroup, highResolution, desiredResolution, latitude, longitude);
            if (lowResGroup != null) lowResData = ReadDataset(lowResGroup, lowResolution, desiredResolution, latitude, longitude);
            if (highResData == null && lowResData == null) 
                throw new KeyNotFoundException(string.Format("Unable to locate sediment data for lat: {0}, lon: {1}", latitude, longitude));
            var resolutionStepSize = desiredResolution / 60;
            var resolutionStepCount = (int)(1.0 / resolutionStepSize);
            if (highResData != null && (highResData.GetLength(0) != resolutionStepCount || highResData.GetLength(1) != resolutionStepCount)) throw new IndexOutOfRangeException("High resolution dataset is not the correct size");
            if (lowResData != null && (lowResData.GetLength(0) != resolutionStepCount || lowResData.GetLength(1) != resolutionStepCount)) throw new IndexOutOfRangeException("Low resolution dataset is not the correct size");
            Func<int, int, short> highResDataFunc = (i, j) => highResData[i, j] == NoData ? (short)0 : highResData[i, j];
            Func<int, int, short> lowResDataFunc = (i, j) => lowResData[i, j] == NoData ? (short)0 : lowResData[i, j];
            Func<int, int, short> dataFunc;
            if (highResData != null && lowResData != null) dataFunc = (i, j) => highResDataFunc(i, j) == 0 ? lowResDataFunc(i, j) : highResDataFunc(i, j);
            else
                if (highResData != null) dataFunc = highResDataFunc;
                else dataFunc = lowResDataFunc;
            return BuildSedimentSampleList(desiredResolution, latitude, longitude, dataFunc);
        }

        static IEnumerable<SedimentSample> BuildSedimentSampleList(double desiredResolution, int latitude, int longitude, Func<int, int, short> dataFunc)
        {
            var resolutionStepSize = desiredResolution / 60;
            var resolutionStepCount = (int)(1.0 / resolutionStepSize);
            var sedimentList = new HashedArrayList<SedimentSample>();
            for (var i = 0; i < resolutionStepCount; i++)
                for (var j = 0; j < resolutionStepCount; j++)
                {
                    var sampleValue = dataFunc(i, j);
                    if (sampleValue > 0)
                        sedimentList.Add(new SedimentSample(latitude + (i * resolutionStepSize),
                                                            longitude + (j * resolutionStepSize),
                                                            new SedimentSampleBase { SampleValue = sampleValue }));
                }
            return sedimentList;
        }

        static short[,] ReadDataset(H5FileOrGroupId groupId, double nativeResolution, double desiredResolution, int latitude, int longitude)
        {
            var decimationStepSize = desiredResolution / nativeResolution;
            var rawData = ReadDataset(groupId, latitude, longitude);
            if (rawData == null) return null;
            var resampledData = new short[(int)((rawData.GetLength(0) - 1) / decimationStepSize), (int)((rawData.GetLength(1) - 1) / decimationStepSize)];
            for (var i = 0; i < resampledData.GetLength(0); i++) 
                for (var j = 0; j < resampledData.GetLength(1); j++) 
                    resampledData[i, j] = rawData[(int)(i * decimationStepSize), (int)(j * decimationStepSize)];
            return resampledData;
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