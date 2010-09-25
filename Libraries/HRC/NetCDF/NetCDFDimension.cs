using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace HRC.NetCDF
{
    public class NetCDFDimension
    {
        public NetCDFString Name { get; set; }
        public uint Length { get; set; }

        void Read(BinaryReader reader)
        {
            (Name as INetCDF_FileElement).Read(reader);
            Length = reader.ReadUInt32();
        }

        void Write(BinaryWriter writer)
        {
            (Name as INetCDF_FileElement).Write(writer);
            writer.Write(Length);
        }
    }
}
