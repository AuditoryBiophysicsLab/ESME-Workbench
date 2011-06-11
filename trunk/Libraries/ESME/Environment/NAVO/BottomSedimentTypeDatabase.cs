﻿using System;
using System.Collections.Generic;
using System.IO;
using HRC.Navigation;
using HDF5DotNet;

namespace ESME.Environment.NAVO
{
    public class BottomSedimentTypeDatabase
    {
        public static string DatabasePath { get; set; }

        public static string SedimentFilename(string outputPath) { return Path.Combine(outputPath, "sediment.xml"); }

        public static void ExtractArea(string outputDirectory, GeoRect extractionArea)
        {
            var results = new Sediment();
            var fileId = H5F.open(DatabasePath, H5F.OpenMode.ACC_RDONLY);
            var highResGroup = H5G.open(fileId, "0.10000/G/UNCLASSIFIED/");
            var lowResGroup = H5G.open(fileId, "5.00000/G/UNCLASSIFIED/");

            for (var lat = (int)Math.Floor(extractionArea.South); lat <= (int)Math.Ceiling(extractionArea.North); lat++)
                for (var lon = (int)Math.Floor(extractionArea.West); lon <= (int)Math.Ceiling(extractionArea.East); lon++)
                {
                    var data = ReadDataset(highResGroup, lowResGroup, lat, lon);
                    //if (data != null) results.Samples.AddRange(data.Where(extractionArea.Contains));
                    if (data != null) results.Samples.AddRange(data);
                }
            results.Samples.TrimToNearestPoints(extractionArea);

            H5G.close(lowResGroup);
            H5G.close(highResGroup);
            H5F.close(fileId);

            results.Save(Path.Combine(outputDirectory, "sediment.xml"));
        }

        static IEnumerable<SedimentSample> ReadDataset(H5FileOrGroupId highResGroup, H5FileOrGroupId lowResGroup, int latitude, int longitude)
        {
            var result = ReadDataset(highResGroup, latitude, longitude);
            double resolutionStep;
            string resolution;
            if (result != null)
            {
                resolutionStep = 6.0 / 3600.0;
                resolution = "6s";
            }
            else
            {
                result = ReadDataset(lowResGroup, latitude, longitude);
                //if (result == null) throw new KeyNotFoundException(string.Format("Unable to locate sediment data for lat: {0}, lon: {1}", latitude, longitude));
                if (result == null) return null;
                resolutionStep = 5.0 / 60.0;
                resolution = "5m";
            }
            var sedimentList = new List<SedimentSample>();
            for (var i = 0; i < result.GetLength(0); i++)
                for (var j = 0; j < result.GetLength(1); j++)
                    sedimentList.Add(new SedimentSample(latitude + (i * resolutionStep), longitude + (j * resolutionStep), new SedimentSampleBase
                                                                                                                           {
                                                                                                                               SampleValue = result[i, j],
                                                                                                                               Resolution = resolution,
                                                                                                                           }));
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