using System.IO;

namespace HRC.NetCDF
{
    internal static class NetCDFReaders
    {
        internal static unsafe float ReadNetCDFFloat(this BinaryReader reader)
        {
            var bytes = reader.ReadBytes(4);
            if (bytes.Length != 4) throw new EndOfStreamException("ReadNetCDFFloat encountered an end of stream");
            var result = float.NaN;
            var pointer = (byte*)&result;
            pointer[0] = bytes[3];
            pointer[1] = bytes[2];
            pointer[2] = bytes[1];
            pointer[3] = bytes[0];
            return result;
        }

        internal static unsafe double ReadNetCDFDouble(this BinaryReader reader)
        {
            var bytes = reader.ReadBytes(8);
            var result = double.NaN;
            if (bytes.Length != 8) throw new EndOfStreamException("ReadNetCDFDouble encountered an end of stream");
            var pointer = (byte*)&result;
            pointer[0] = bytes[7];
            pointer[1] = bytes[6];
            pointer[2] = bytes[5];
            pointer[3] = bytes[4];
            pointer[4] = bytes[3];
            pointer[5] = bytes[2];
            pointer[6] = bytes[1];
            pointer[7] = bytes[0];
            return result;
        }

        internal static uint ReadNetCDFUint(this BinaryReader reader)
        {
            var bytes = reader.ReadBytes(4);
            if (bytes.Length != 4) throw new EndOfStreamException("ReadNetCDFUint encountered an end of stream");
            return (uint)(bytes[0] << 24 | bytes[1] << 16 | bytes[2] << 8 | bytes[3]);
        }

        internal static int ReadNetCDFInt(this BinaryReader reader)
        {
            var bytes = reader.ReadBytes(4);
            if (bytes.Length != 4) throw new EndOfStreamException("ReadNetCDFInt encountered an end of stream");
            return bytes[0] << 24 | bytes[1] << 16 | bytes[2] << 8 | bytes[3];
        }

        internal static short ReadNetCDFShort(this BinaryReader reader)
        {
            var bytes = reader.ReadBytes(2);
            if (bytes.Length != 2) throw new EndOfStreamException("ReadNetCDFShort encountered an end of stream");
            return (short)(bytes[0] << 8 | bytes[1]);
        }
    }
}