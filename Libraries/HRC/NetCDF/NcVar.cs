using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace HRC.NetCDF
{
    public abstract class NcVar
    {
        public string Name { get; private set; }
        public NcType NcType { get; private set; }
        public List<NcDim> Dimensions { get; private set; }
        public List<NcAtt> Attributes { get; private set; }
        public uint Size { get; private set; }
        public uint Offset { get; private set; }
        protected uint UnitSize { get; set; }
        protected uint[] Strides { get; set; }
        protected BinaryReader Reader { get; set; }

        public static List<NcVar> ReadAll(BinaryReader reader, List<NcDim> dimensions)
        {
            var result = new List<NcVar>();
            var varCount = reader.ReadNetCDFUint();
            for (var i = 0; i < varCount; i++) result.Add(Read(reader, dimensions));
            return result;
        }

        public static NcVar Read(BinaryReader reader, List<NcDim> dimensions)
        {
            NcVar result;
            var name = new NcString(reader);
            var dimCount = reader.ReadNetCDFUint();
            var dims = new List<NcDim>();
            for (var i = 0; i < dimCount; i++) dims.Add(dimensions[(int)reader.ReadNetCDFUint()]);
            var attrs = new List<NcAtt>();
            if (reader.ReadNetCDFUint() == (uint)NcField.Attribute) attrs = NcAtt.ReadAll(reader);
            var ncType = (NcType)reader.ReadNetCDFUint();
            switch (ncType)
            {
                case NcType.Byte:
                    result = new NcVarByte();
                    break;
                case NcType.Short:
                    result = new NcVarShort();
                    break;
                case NcType.Int:
                    result = new NcVarInt();
                    break;
                case NcType.Float:
                    result = new NcVarFloat();
                    break;
                case NcType.Double:
                    result = new NcVarDouble();
                    break;
                case NcType.Char:
                    throw new NotImplementedException("NcVarChar type is not implemented");
                default:
                    throw new FormatException("Unknown base type ID: " + ncType);
            }
            result.Name = name.Value;
            result.NcType = ncType;
            result.Dimensions = dims;
            result.Attributes = attrs;
            result.Size = reader.ReadNetCDFUint();
            result.Offset = reader.ReadNetCDFUint();
            result.Reader = reader;
            result.ComputeStrides();
            return result;
        }

        protected void ComputeStrides()
        {
            Strides = new uint[Dimensions.Count];
            Strides[Dimensions.Count - 1] = 1;
            if (Dimensions.Count < 2) return;
            for (var i = Dimensions.Count - 2; i >= 0; i--) Strides[i] = Strides[i + 1] * Dimensions[i + 1].Length;
        }

        public override string ToString()
        {
            var sb = new StringBuilder();
            foreach (var dim in Dimensions) sb.AppendFormat("{0}, ", dim);
            sb.Remove(sb.Length - 2, 2);
            return string.Format("{0} {1} [{2}]", NcType, Name, sb);
        }

        public abstract void ReadAll();
    }

    public abstract class NcVar<T> : NcVar, IEnumerable<T> where T : struct
    {
        public static Action<string> Logger;

        protected NcVar()
        {
            var t = typeof(T);
            if (t.IsValueType) UnitSize = (uint)Marshal.SizeOf(t);
            else throw new ApplicationException("Cannot instantiate NcVar<T> with a reference type");
        }
        public T this[params uint[] indices]
        {
            get
            {
                if (indices.Length != Dimensions.Count) throw new ArrayTypeMismatchException(string.Format("This variable requires {0} dimensions, you provided {1}", Dimensions.Count, indices.Length));
                long requestedDataOffset = 0;
                for (var i = 0; i < indices.Length; i++)
                {
                    var index = indices[i];
                    if (index >= Dimensions[i].Length) throw new IndexOutOfRangeException(string.Format("Dimension {0} out of bounds.  You requested {1}, max value is {2}", i, index, Dimensions[i].Length));
                    requestedDataOffset += Strides[i] * index;
                }
                if (_values != null) return _values[requestedDataOffset];
                Reader.BaseStream.Seek(Offset + (requestedDataOffset * UnitSize), SeekOrigin.Begin);
                try
                {
                    return Read();
                }
                catch (Exception e)
                {
                    if (Logger != null)
                    {
                        var idxs = string.Join(", ", indices.Select(index => index.ToString()));
                        Logger(string.Format("NcVar<T>.this[{0}] Caught exception: {1}", idxs, e.Message));
                        Logger(string.Format("Source: {0}", e.Source));
                        Logger(string.Format("Stack trace: {0}", e.StackTrace));
                    }
                    throw;
                }
            }
        }

        protected abstract T Read();
        T[] _values;

        public override void ReadAll()
        {
            _values = new T[Size / UnitSize];
            Reader.BaseStream.Seek(Offset, SeekOrigin.Begin);
            for (var i = 0; i < _values.Length; i++) _values[i] = Read();
        }

        public IEnumerator<T> GetEnumerator()
        {
            if (_values == null) ReadAll();
            if (_values != null) return _values.ToList().GetEnumerator();
            throw new ApplicationException("Unknown error!");
        }

        IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }
    }

    public class NcVarByte : NcVar<byte>
    {
        protected override byte Read()
        {
            try
            {
                return Reader.ReadByte();
            }
            catch (Exception e)
            {
                if (Logger != null)
                {
                    Logger(string.Format("NcVarByte.Read() Caught exception: {0}", e.Message));
                    Logger(string.Format("Source: {0}", e.Source));
                    Logger(string.Format("Stack trace: {0}", e.StackTrace));
                }
                throw;
            }
        }
    }

    public class NcVarShort : NcVar<short>
    {
        protected override short Read()
        {
            try
            {
                return Reader.ReadNetCDFShort();
            }
            catch (Exception e)
            {
                if (Logger != null)
                {
                    Logger(string.Format("NcVarShort.Read() Caught exception: {0}", e.Message));
                    Logger(string.Format("Source: {0}", e.Source));
                    Logger(string.Format("Stack trace: {0}", e.StackTrace));
                }
                throw;
            }
        }
    }

    public class NcVarInt : NcVar<int>
    {
        protected override int Read()
        {
            try
            {
                return Reader.ReadNetCDFInt();
            }
            catch (Exception e)
            {
                if (Logger != null)
                {
                    Logger(string.Format("NcVarInt.Read() Caught exception: {0}", e.Message));
                    Logger(string.Format("Source: {0}", e.Source));
                    Logger(string.Format("Stack trace: {0}", e.StackTrace));
                }
                throw;
            }
        }
    }

    public class NcVarFloat : NcVar<float>
    {
        protected override float Read()
        {
            try
            {
                return Reader.ReadNetCDFFloat();
            }
            catch (Exception e)
            {
                if (Logger != null)
                {
                    Logger(string.Format("NcVarFloat.Read() Caught exception: {0}", e.Message));
                    Logger(string.Format("Source: {0}", e.Source));
                    Logger(string.Format("Stack trace: {0}", e.StackTrace));
                }
                throw;
            }
        }
    }

    public class NcVarDouble : NcVar<double>
    {
        protected override double Read()
        {
            try
            {
                return Reader.ReadNetCDFDouble();
            }
            catch (Exception e)
            {
                if (Logger != null)
                {
                    Logger(string.Format("NcVarDouble.Read() Caught exception: {0}", e.Message));
                    Logger(string.Format("Source: {0}", e.Source));
                    Logger(string.Format("Stack trace: {0}", e.StackTrace));
                }
                throw;
            }
        }
    }
}
