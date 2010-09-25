using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HRC.NetCDF
{
    public enum NcType : uint
    {
        Byte   = 1,
        Char   = 2,
        Short  = 3,
        Int    = 4,
        Float  = 5,
        Double = 6
    }

    internal enum NcField : uint
    {
        Zero      = 0,
        Dimension = 10,
        Variable  = 11,
        Attribute = 12
    }
#if false
    internal static class NetCDFConstants
    {
        static readonly byte FillChar = 0;
        static readonly byte FillByte = 0x81;
        static readonly short FillShort = -32767;
        static readonly int FillInt = -2147483647;
        static readonly float FillFloat = (float)9.9692099683868690e+36;
        static readonly double FillDouble = 9.9692099683868690e+36;
        static readonly uint StreamingRecords = 0xFFFFFFFF;
    }
#endif
}
