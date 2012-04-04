using System.Collections.Generic;
using System.IO;

namespace ESME.Database
{
    public static class BlobHelpers
    {
        public static byte[] ToBlob(this ICollection<double> array)
        {
            if (array == null) return null;
            using (var memoryStream = new MemoryStream())
            {
                using (var writer = new BinaryWriter(memoryStream))
                {
                    writer.Write(array.Count);
                    foreach (var range in array) writer.Write(range);
                }
                return memoryStream.ToArray();
            }
        }

        public static double[] ToArray(this byte[] blob)
        {
            if (blob == null) return null;
            using (var reader = new BinaryReader(new MemoryStream(blob)))
            {
                var result = new double[reader.ReadInt32()];
                for (var index = 0; index < result.Length; index++) result[index] = reader.ReadDouble();
                return result;
            }
        }
    }
}
