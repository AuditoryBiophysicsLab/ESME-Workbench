using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace HRC.NetCDF
{
    public class NetCDF
    {
        public NetCDF(string fileName) 
        {
            using (var stream = new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read))
            using (var reader = new BinaryReader(stream))
            {
                var magic = reader.ReadBytes(3);
                if (magic[0] != 'C' || magic[1] != 'D' || magic[2] != 'F') throw new FormatException("This is not a NetCDF file");
                Version = reader.ReadByte();
                if (Version != 1) throw new FormatException("This NetCDF file specifies 64 bit offsets, which are not supported by this reader");
                RecordCount = reader.ReadNetCDFUint();
                if (reader.ReadNetCDFUint() == (uint)NcField.Dimension) Dimensions = NcDim.ReadAll(reader);
                if (reader.ReadNetCDFUint() == (uint)NcField.Attribute) Attributes = NcAtt.ReadAll(reader);
                if (reader.ReadNetCDFUint() == (uint)NcField.Variable) Variables = NcVar.ReadAll(reader, Dimensions);
            }
        }

        public byte Version { get; private set; }
        public uint RecordCount { get; private set; }
        public List<NcDim> Dimensions { get; private set; }
        public List<NcAtt> Attributes { get; private set; }
        public List<NcVar> Variables { get; private set; }

        public string Dump()
        {
            var sb = new StringBuilder();
            sb.AppendFormat("Offset format: {0} bits", Version == 1 ? 32 : 64);
            sb.AppendLine();
            sb.AppendFormat(" Record count: {0}", RecordCount);
            sb.AppendLine();
            sb.Append("   Dimensions: ");
            foreach (var dim in Dimensions) sb.AppendFormat("{0} ", dim);
            sb.AppendLine();
            sb.Append("Global attributes:");
            sb.AppendLine();
            foreach (var att in Attributes) sb.AppendFormat("{0}\r\n", att);
            sb.AppendLine();
            sb.Append("Variables:");
            sb.AppendLine();
            foreach (var variable in Variables)
            {
                sb.AppendFormat("{0}\r\n", variable);
                if (variable.Attributes.Count > 0)
                {
                    sb.Append("  Attributes:\r\n");
                    foreach (var att in variable.Attributes) sb.AppendFormat("    {0}\r\n", att);
                }
            }
            return sb.ToString();
        }
    }

    public static class NetCDFReaders
    {
        public static unsafe float ReadNetCDFFloat(this BinaryReader reader)
        {
            var bytes = reader.ReadBytes(4);

            var result = float.NaN;
            var pointer = (byte*)&result;
            pointer[0] = bytes[3];
            pointer[1] = bytes[2];
            pointer[2] = bytes[1];
            pointer[3] = bytes[0];
            return result;
        }

        public static unsafe double ReadNetCDFDouble(this BinaryReader reader)
        {
            var bytes = reader.ReadBytes(8);

            var result = double.NaN;
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

        public static uint ReadNetCDFUint(this BinaryReader reader)
        {
            var bytes = reader.ReadBytes(4);
            return (uint)(bytes[0] << 24 | bytes[1] << 16 | bytes[2] << 8 | bytes[3]);
        }

        public static int ReadNetCDFInt(this BinaryReader reader)
        {
            var bytes = reader.ReadBytes(4);
            return bytes[0] << 24 | bytes[1] << 16 | bytes[2] << 8 | bytes[3];
        }

        public static short ReadNetCDFShort(this BinaryReader reader)
        {
            var bytes = reader.ReadBytes(2);
            return (short)(bytes[0] << 8 | bytes[1]);
        }
    }
}
