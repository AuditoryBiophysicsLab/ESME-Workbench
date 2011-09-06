using System.Collections.Generic;
using System.IO;

namespace HRC.NetCDF
{
    public class NcDim
    {
        public NcString Name { get; set; }
        public uint Length { get; set; }

        public static List<NcDim> ReadAll(BinaryReader reader)
        {
            var result = new List<NcDim>();
            var count = reader.ReadNetCDFUint();
            for (var i = 0; i < count; i++)
                result.Add(new NcDim(reader));
            return result;
        }

        public NcDim(BinaryReader reader)
        {
            Name = new NcString(reader);
            Length = reader.ReadNetCDFUint();
        }
        public override string ToString() { return string.Format("{0} ({1})", Name, Length); }
    }
}
