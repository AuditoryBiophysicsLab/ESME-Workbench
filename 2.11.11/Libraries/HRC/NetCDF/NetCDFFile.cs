using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace HRC.NetCDF
{
    public class NetCDFFile : IDisposable
    {
        public static Action<string> Logger { get; set; }

        string _fileName;
        BinaryReader _reader;
        protected NetCDFFile() { }

        public static NetCDFFile Open(string fileName)
        {
            var result = new NetCDFFile()
            {
                _fileName = fileName
            };
            if (Logger != null) Logger(string.Format("Opening NetCDF file {0}", result._fileName));
            var stream = new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read);
            result._reader = new BinaryReader(stream);
            var magic = result._reader.ReadBytes(3);
            if (magic[0] != 'C' || magic[1] != 'D' || magic[2] != 'F') throw new FormatException("This is not a NetCDF file");
            result.Version = result._reader.ReadByte();
            if (result.Version != 1) throw new FormatException("This NetCDF file specifies 64 bit offsets, which are not supported by this reader");
            result.RecordCount = result._reader.ReadNetCDFUint();
            if (result._reader.ReadNetCDFUint() == (uint)NcField.Dimension) result.Dimensions = NcDim.ReadAll(result._reader);
            if (result._reader.ReadNetCDFUint() == (uint)NcField.Attribute) result.Attributes = NcAtt.ReadAll(result._reader);
            if (result._reader.ReadNetCDFUint() == (uint)NcField.Variable) result.Variables = NcVar.ReadAll(result._reader, result.Dimensions);
            return result;
        }

        public void Close()
        {
            if (Logger != null) Logger(string.Format("Closing NetCDF file {0}", _fileName));
            _reader.Close();
        }

        public void Dispose()
        {
            if (Logger != null) Logger(string.Format("Disposing NetCDF file {0}", _fileName));
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        // Dispose(bool disposing) executes in two distinct scenarios.
        // If disposing equals true, the method has been called directly
        // or indirectly by a user's code. Managed and unmanaged resources
        // can be disposed.
        // If disposing equals false, the method has been called by the
        // runtime from inside the finalizer and you should not reference
        // other objects. Only unmanaged resources can be disposed.
        protected virtual void Dispose(bool disposing)
        {
            // Check to see if Dispose has already been called.
            if (_disposed) return;
            // If disposing equals true, dispose all managed
            // and unmanaged resources.
            if (disposing)
            {
                // Dispose managed resources.
                if (_reader != null)
                {
                    try
                    {
                        _reader.Close();
                    }
                    finally
                    {
                        _reader.Dispose();
                        _reader = null;
                    }
                }
            }

            // Note disposing has been done.
            _disposed = true;
        }
        bool _disposed;

        ~NetCDFFile()
        {
            if (Logger != null) Logger(string.Format("Destructing NetCDF file {0}", _fileName));
            Dispose(false);
        }
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
}
