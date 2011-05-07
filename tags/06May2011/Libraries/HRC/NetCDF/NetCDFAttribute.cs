using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace HRC.NetCDF
{
    public abstract class NetCDFAttribute
    {
        public NetCDFString Name { get; set; }
        public NcType NcType { get; private set; }

        protected virtual void Read(BinaryReader reader)
        {
            Name.Read(reader);
            NcType = (NcType)reader.ReadUInt32();
        }

        protected virtual void Write(BinaryWriter writer)
        {
            Name.Write(writer);
            writer.Write((uint)NcType);
        }

        protected abstract int Length { get; }

        public static NetCDFAttribute Create(string AttributeName, NcType AttributeType)
        {
            switch (AttributeType)
            {
                case NcType.Byte:
                    return new NetCDFByte { Name = new NetCDFString { Value = AttributeName }, NcType = AttributeType };
                case NcType.Char:
                case NcType.Short:
                case NcType.Int:
                case NcType.Float:
                case NcType.Double:
                    throw new NotImplementedException("");
                default:
                    throw new NotImplementedException("Unknown base type");
            }
        }
    }

    public class NetCDFGenericAttribute<T>: NetCDFAttribute
        where T : struct
    {
        public T[] Value { get; set; }
        public T this[uint index] { get { return Value[index]; } set { Value[index] = value; } }
        protected override int Length { get { return Value.Length; } }
        protected override void Read(BinaryReader reader)
        {
            base.Read(reader);
        }

        protected override void Write(BinaryWriter writer)
        {
            base.Write(writer);
        }

        protected void ReadPadding(BinaryReader reader, int ElementSize)
        {
            if ((ElementSize % 4) == 0)
                return;

            int DataSize = ElementSize * Length;
            for (int i = 0; i < 4 - (DataSize % 4); i++)
                reader.ReadByte();
        }

        protected void WritePadding(BinaryWriter writer, int ElementSize)
        {
            if ((ElementSize % 4) == 0)
                return;

            int DataSize = ElementSize * Length;

            for (int i = 0; i < 4 - (DataSize % 4); i++)
                writer.Write((byte)0x00);
        }
    }

    public class NetCDFByte : NetCDFGenericAttribute<byte>
    {
        protected override void Read(BinaryReader reader)
        {
            base.Read(reader);
            Value = reader.ReadBytes((int)reader.ReadUInt32());
            ReadPadding(reader, sizeof(byte));
        }

        protected override void Write(BinaryWriter writer)
        {
            base.Write(writer);
            writer.Write(Value);
            WritePadding(writer, sizeof(byte));
        }
    }
}
