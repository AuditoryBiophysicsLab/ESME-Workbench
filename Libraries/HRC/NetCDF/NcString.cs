using System.IO;
using System.Text;

namespace HRC.NetCDF
{
    public class NcString
    {
        public string Value { get; set; }

        public static implicit operator string(NcString s) { return s.Value; }

        public byte[] Bytes
        {
            get
            {
                var utf = new UTF8Encoding();
                return utf.GetBytes(Value);
            }
            private set
            {
                var utf = new UTF8Encoding();
                Value = utf.GetString(value);
            }
        }

        public NcString(BinaryReader reader)
        {
            Bytes = reader.ReadBytes((int)reader.ReadNetCDFUint());
            if ((Length % 4) != 0)
                reader.ReadBytes((int)(4 - (Length % 4)));
        }

        public uint Length { get { return (uint)Value.Length; } }

        public override string ToString() { return Value; }
    }
}
