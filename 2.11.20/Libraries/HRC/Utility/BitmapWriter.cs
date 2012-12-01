using System.IO;

namespace HRC.Utility
{
    public static class BitmapWriter
    {
       public static void Write(string fileName, uint[,] bitmapDataArgb)
        {
            using (var stream = new FileStream(fileName, FileMode.Create, FileAccess.Write, FileShare.None))
            using (var writer = new BinaryWriter(stream))
            {
                // BMP file header
                writer.Write('B');
                writer.Write('M');
                writer.WriteInt(0);  // Size of bitmap file in bytes
                writer.WriteInt(0);  // Application specific tag bytes
                writer.WriteInt(0);  // Byte offset to start of image data
                // DIB header BITMAPINFOHEADER
                writer.WriteInt(40); // Header length (bytes)
                writer.WriteInt(bitmapDataArgb.GetLength(0));  // bitmap width (pixels)
                writer.WriteInt(bitmapDataArgb.GetLength(1));  // bitmap height (pixels)
                writer.WriteShort(1); // Number of color planes (must be 1)
                writer.WriteShort(32);    // Bits per pixel
                writer.WriteInt(0);  // Compression method (0 = none)
                writer.WriteInt(bitmapDataArgb.GetLength(0) * bitmapDataArgb.GetLength(1) * 4);  // number of image bytes
                writer.WriteInt(3780); // Horizontal resolution in pixels per meter (96 dpi)
                writer.WriteInt(3780); // Vertical resolution in pixels per meter (96 dpi)
                writer.WriteInt(0);    // Number of colors in the palette (0 = 2 ** (bitsPerPixel))
                writer.WriteInt(0);    // Number of 'important' colors.  0 = every color is important
                var bitmapOffset = (int)stream.Seek(0, SeekOrigin.Current);
                stream.Seek(10, SeekOrigin.Begin);
                writer.WriteInt(bitmapOffset);  // Write the offset to the beginning of the pixel values
                stream.Seek(bitmapOffset, SeekOrigin.Begin);

                // Write the actual pixel values to the file
                for (var y = bitmapDataArgb.GetLength(1) - 1; y >= 0; y--)
                    for (var x = 0; x < bitmapDataArgb.GetLength(0); x++)
                        writer.WriteUInt(bitmapDataArgb[x, y]);
                var bitmapSize = (int)stream.Seek(0, SeekOrigin.Current);
                stream.Seek(2, SeekOrigin.Begin);
                writer.WriteInt(bitmapSize);  // Write the offset to the beginning of the pixel values
            }
        }

        public static void WriteUInt(this BinaryWriter writer, uint value)
        {
            var bytes = new byte[4];
            bytes[0] = (byte)((value & 0x000000ff) >> 0);
            bytes[1] = (byte)((value & 0x0000ff00) >> 8);
            bytes[2] = (byte)((value & 0x00ff0000) >> 16);
            bytes[3] = (byte)((value & 0xff000000) >> 24);
            writer.Write(bytes);
        }

        public static void WriteInt(this BinaryWriter writer, int value)
        {
            var bytes = new byte[4];
            bytes[0] = (byte)((value & 0x000000ff) >> 0);
            bytes[1] = (byte)((value & 0x0000ff00) >> 8);
            bytes[2] = (byte)((value & 0x00ff0000) >> 16);
            bytes[3] = (byte)((value & 0xff000000) >> 24);
            writer.Write(bytes);
        }

        public static void WriteShort(this BinaryWriter writer, short value)
        {
            var bytes = new byte[2];
            bytes[0] = (byte)((value & 0x00ff) >> 0);
            bytes[1] = (byte)((value & 0xff00) >> 8);
            writer.Write(bytes);
        }
    }
}
