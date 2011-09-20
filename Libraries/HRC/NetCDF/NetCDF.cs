using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace HRC.NetCDF
{
    public class NetCDF : IDisposable
    {
        readonly BinaryReader _reader;

        public NetCDF(string fileName)
        {
            var stream = new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read);
            _reader = new BinaryReader(stream);
            var magic = _reader.ReadBytes(3);
            if (magic[0] != 'C' || magic[1] != 'D' || magic[2] != 'F') throw new FormatException("This is not a NetCDF file");
            Version = _reader.ReadByte();
            if (Version != 1) throw new FormatException("This NetCDF file specifies 64 bit offsets, which are not supported by this reader");
            RecordCount = _reader.ReadNetCDFUint();
            if (_reader.ReadNetCDFUint() == (uint)NcField.Dimension) Dimensions = NcDim.ReadAll(_reader);
            if (_reader.ReadNetCDFUint() == (uint)NcField.Attribute) Attributes = NcAtt.ReadAll(_reader);
            if (_reader.ReadNetCDFUint() == (uint)NcField.Variable) Variables = NcVar.ReadAll(_reader, Dimensions);
            //_buffer[0] = 1;
        }

        public void Close() {_reader.Close();}
        public void Dispose() {Close();}
        ~NetCDF(){Close();}
        //private readonly int[] _buffer = new int[100000];

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
        public static Action<string> Logger;

        public static unsafe float ReadNetCDFFloat(this BinaryReader reader)
        {
            try
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
            catch (Exception e)
            {
                if (Logger != null)
                {
                    Logger(string.Format("ReadNetCDFFloat() Caught exception: {0}", e.Message));
                    Logger(string.Format("Source: {0}", e.Source));
                    Logger(string.Format("Stack trace: {0}", e.StackTrace));
                }
                throw;
            }
        }

        public static unsafe double ReadNetCDFDouble(this BinaryReader reader)
        {
            try
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
            catch (Exception e)
            {
                if (Logger != null)
                {
                    Logger(string.Format("ReadNetCDFDouble() Caught exception: {0}", e.Message));
                    Logger(string.Format("Source: {0}", e.Source));
                    Logger(string.Format("Stack trace: {0}", e.StackTrace));
                }
                throw;
            }
        }

        public static uint ReadNetCDFUint(this BinaryReader reader)
        {
            try
            {
                var bytes = reader.ReadBytes(4);
                if (bytes.Length != 4) throw new EndOfStreamException("ReadNetCDFUint encountered an end of stream");
                return (uint)(bytes[0] << 24 | bytes[1] << 16 | bytes[2] << 8 | bytes[3]);
            }
            catch (Exception e)
            {
                if (Logger != null)
                {
                    Logger(string.Format("ReadNetCDFUint() Caught exception: {0}", e.Message));
                    Logger(string.Format("Source: {0}", e.Source));
                    Logger(string.Format("Stack trace: {0}", e.StackTrace));
                }
                throw;
            }
        }

        public static int ReadNetCDFInt(this BinaryReader reader)
        {
            try
            {
                var bytes = reader.ReadBytes(4);
                if (bytes.Length != 4) throw new EndOfStreamException("ReadNetCDFInt encountered an end of stream");
                return bytes[0] << 24 | bytes[1] << 16 | bytes[2] << 8 | bytes[3];
            }
            catch (Exception e)
            {
                if (Logger != null)
                {
                    Logger(string.Format("ReadNetCDFInt() Caught exception: {0}", e.Message));
                    Logger(string.Format("Source: {0}", e.Source));
                    Logger(string.Format("Stack trace: {0}", e.StackTrace));
                }
                throw;
            }
        }

        public static short ReadNetCDFShort(this BinaryReader reader)
        {
            try
            {
                var bytes = reader.ReadBytes(2);
                if (bytes.Length != 2) throw new EndOfStreamException("ReadNetCDFShort encountered an end of stream");
                return (short)(bytes[0] << 8 | bytes[1]);
            }
            catch (Exception e)
            {
                if (Logger != null)
                {
                    Logger(string.Format("ReadNetCDFShort() Caught exception: {0}", e.Message));
                    Logger(string.Format("Source: {0}", e.Source));
                    Logger(string.Format("Stack trace: {0}", e.StackTrace));
                }
                throw;
            }
        }
    }
}
