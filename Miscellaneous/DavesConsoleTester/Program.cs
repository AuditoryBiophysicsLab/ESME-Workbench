using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using HDF5DotNet;
using HRC.Navigation;

namespace DavesConsoleTester
{
    class Program
    {
        static void Main(string[] args)
        {
            var area = new GeoRect(31.1627, 29.3590, -79.2195, -81.2789);
            ExtractAreaNew(@"E:\OAML Data Sources\BST\Sediments2.0_QAV_Analysis\Sediments\Version2.0\databases\hfevav2.h5", area);
        }

        public static void ExtractAreaNew(string databaseFile, GeoRect extractionArea)
        {
            var results = new List<EarthCoordinate<short>>();
            var fileID = H5F.open(databaseFile, H5F.OpenMode.ACC_RDONLY);
            var highResGroup = H5G.open(fileID, "0.10000/G/UNCLASSIFIED/");
            var lowResGroup = H5G.open(fileID, "5.00000/G/UNCLASSIFIED/");

            var latStart = (int)extractionArea.South;
            var latEnd = (int)extractionArea.North;
            var lonStart = (int)extractionArea.West;
            var lonEnd = (int)extractionArea.East;

            for (var lat = latStart; lat <= latEnd; lat++)
                for (var lon = lonStart; lon <= lonEnd; lon++ )
                {
                    var data = ReadDataset(highResGroup, lowResGroup, lat, lon);
                    if (data != null) results.AddRange(data.Where(extractionArea.Contains));
                }

            H5G.close(lowResGroup);
            H5G.close(highResGroup);
            H5F.close(fileID);
        }

        static List<EarthCoordinate<short>> ReadDataset(H5FileOrGroupId highResGroup, H5FileOrGroupId lowResGroup, int latitude, int longitude)
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
            var sedimentList = new List<EarthCoordinate<short>>();
            for (var i = 0; i < result.GetLength(0); i++)
                for (var j = 0; j < result.GetLength(1); j++)
                    sedimentList.Add(new EarthCoordinate<short>(latitude + (i * resolutionStep), longitude + (j * resolutionStep), result[i, j]));
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
