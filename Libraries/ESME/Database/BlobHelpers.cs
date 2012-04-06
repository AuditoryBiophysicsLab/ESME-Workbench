using System;
using System.Collections.Generic;
using System.IO;
using ESME.TransmissionLoss.Bellhop;

namespace ESME.Database
{
    public static class BlobHelpers
    {
        public static byte[] ToBlob(this ICollection<float> array)
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
        public static byte[] ToBlob(this BottomProfilePoint[] array)
        {
            if (array == null) return null;
            using (var memoryStream = new MemoryStream())
            {
                using (var writer = new BinaryWriter(memoryStream))
                {
                    writer.Write(array.Length);
                    foreach (var bottomProfilePoint in array)
                    {
                        writer.Write(bottomProfilePoint.Range);
                        writer.Write(bottomProfilePoint.Depth);
                    }
                }
                return memoryStream.ToArray();
            }
        }

        public static float[] ToArray(this byte[] blob)
        {
            if (blob == null) return null;
            using (var reader = new BinaryReader(new MemoryStream(blob)))
            {
                var result = new float[reader.ReadInt32()];
                for (var index = 0; index < result.Length; index++) result[index] = reader.ReadSingle();
                return result;
            }
        }

        public static BottomProfilePoint[] ToBottomProfileArray(this byte[] blob)
        {
            if (blob == null) return null;
            using (var reader = new BinaryReader(new MemoryStream(blob)))
            {
                var result = new BottomProfilePoint[reader.ReadInt32()];
                for (var index = 0; index < result.Length; index++) result[index] = new BottomProfilePoint {Range = reader.ReadSingle(), Depth = reader.ReadSingle()};
                return result;
            }
        }
    }
}
