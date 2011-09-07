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

        public static List<NcVar>  ReadAll(BinaryReader reader, List<NcDim> dimensions)
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
                return Read();
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
        protected override byte Read() { return Reader.ReadByte(); }
    }

    public class NcVarShort : NcVar<short>
    {
        protected override short Read() { return Reader.ReadNetCDFShort(); }
    }

    public class NcVarInt : NcVar<int>
    {
        protected override int Read() { return Reader.ReadNetCDFInt(); }
    }

    public class NcVarFloat : NcVar<float>
    {
        protected override float Read() { return Reader.ReadNetCDFFloat(); }
    }

    public class NcVarDouble : NcVar<double>
    {
        protected override double Read() { return Reader.ReadNetCDFDouble(); }
    }
}
