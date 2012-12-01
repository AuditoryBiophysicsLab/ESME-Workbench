using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace HRC.NetCDF
{
    public abstract class NcAtt
    {
        public string Name { get; private set; }
        public NcType NcType { get; private set; }

        public uint Length { get; protected set; }

        public static List<NcAtt> ReadAll(BinaryReader reader)
        {
            var result = new List<NcAtt>();
            var count = reader.ReadNetCDFUint();
            for (var i = 0; i < count; i++)
                result.Add(Read(reader));
            return result;
        }

        public static NcAtt Read(BinaryReader reader)
        {
            var name = new NcString(reader);
            var ncType = (NcType)reader.ReadNetCDFUint();
            NcAtt result;
            switch (ncType)
            {
                case NcType.Byte:
                    result = new NcAttByte(reader);
                    break;
                case NcType.Char:
                    result = new NcAttChar(reader);
                    break;
                case NcType.Int:
                    result = new NcAttInt(reader);
                    break;
                case NcType.Short:
                    result = new NcAttShort(reader);
                    break;
                case NcType.Float:
                    result = new NcAttFloat(reader);
                    break;
                case NcType.Double:
                    throw new NotImplementedException("");
                default:
                    throw new NotImplementedException("Unknown base type");
            }
            result.Name = name.Value;
            result.NcType = ncType;
            return result;
        }
    }

    public abstract class NcAtt<T>: NcAtt
    {
        public T[] Value { get; protected set; }
        public T this[uint index] { get { return Value[index]; } set { Value[index] = value; } }

        protected void ReadPadding(BinaryReader reader, int elementSize)
        {
            if ((elementSize % 4) == 0)
                return;

            var dataSize = elementSize * Length;
            for (var i = 0; i < 4 - (dataSize % 4); i++)
                reader.ReadByte();
        }
        public override string ToString()
        {
            var sb = new StringBuilder();
            foreach (var t in Value) sb.AppendFormat("{0}, ", t);
            sb.Remove(sb.Length - 2, 2);
            return string.Format("{0} {1} [{2}]", NcType, Name, sb);
        }
    }

    public class NcAttByte : NcAtt<byte>
    {
        public NcAttByte(BinaryReader reader)
        {
            Value = reader.ReadBytes((int)reader.ReadNetCDFUint());
            ReadPadding(reader, sizeof(byte));
        }
    }

    public class NcAttChar : NcAtt<char>
    {
        public NcAttChar(BinaryReader reader)
        {
            Value = new NcString(reader);
        }

        public new NcString Value { get; set; }

        public override string ToString() { return string.Format("{0} {1} [{2}]", NcType, Name, Value); }
    }

    public class NcAttShort : NcAtt<short>
    {
        public NcAttShort(BinaryReader reader)
        {
            var count = reader.ReadNetCDFUint();
            Value = new short[count];
            for (var i = 0; i < count; i++) Value[i] = reader.ReadNetCDFShort();
            if ((count & 1) == 1) reader.ReadNetCDFShort();
        }
    }

    public class NcAttInt : NcAtt<int>
    {
        public NcAttInt(BinaryReader reader)
        {
            var count = reader.ReadNetCDFUint();
            Value = new int[count];
            for (var i = 0; i < count; i++) Value[i] = reader.ReadNetCDFInt();
        }
    }

    public class NcAttFloat : NcAtt<float>
    {
        public NcAttFloat(BinaryReader reader)
        {
            var count = reader.ReadNetCDFUint();
            Value = new float[count];
            for (var i = 0; i < count; i++) Value[i] = reader.ReadNetCDFFloat();
        }
    }

    public class NcAttDouble : NcAtt<double>
    {
        public NcAttDouble(BinaryReader reader)
        {
            var count = reader.ReadNetCDFUint();
            Value = new double[count];
            for (var i = 0; i < count; i++) Value[i] = reader.ReadNetCDFDouble();
        }
    }
}
