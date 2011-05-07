using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace HRC.NetCDF
{
    internal interface INetCDF_FileElement
    {
        void Read(BinaryReader reader);
        void Write(BinaryWriter writer);
        uint Length { get; }
        byte[] bytes { get; set; }
    }
}
