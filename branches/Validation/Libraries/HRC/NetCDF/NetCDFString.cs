using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace HRC.NetCDF
{
    public class NetCDFString
    {
        public string Value { get; set; }

        byte[] bytes
        {
            get
            {
                UTF8Encoding mUTF8 = new UTF8Encoding(); 
                return mUTF8.GetBytes(Value);
            }
            set
            {
                UTF8Encoding mUTF8 = new UTF8Encoding();
                Value = mUTF8.GetString(value);
            }
        }

        internal void Read(BinaryReader reader)
        {
            bytes = reader.ReadBytes((int)reader.ReadUInt32());
            if ((Length % 4) != 0)
                reader.ReadBytes(4 - (Length % 4));
        }

        internal void Write(BinaryWriter writer)
        {
            writer.Write(Length);
            writer.Write(bytes);
            for (int i = 0; i < 4 - (Length % 4); i++)
                writer.Write((byte)0x00);
        }

        public int Length { get { return Value.Length; } }
    }
}
