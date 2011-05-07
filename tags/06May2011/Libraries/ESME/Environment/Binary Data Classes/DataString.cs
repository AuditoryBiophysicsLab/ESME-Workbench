using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace ESME.Environment
{
    public class DataString
    {
        private readonly string String;

        /// <summary>
        /// Constructs a DataString from a stream.  The source of the stream should have originally been written by a call to the DataString.Save() method
        /// </summary>
        /// <param name="Stream"></param>
        public DataString(BinaryReader Stream)
        {
            ushort length = Stream.ReadUInt16();
            char[] theChars;
            if (length > 0)
            {
                theChars = Stream.ReadChars(length);
                String = new String(theChars);
            }

        }

        /// <summary>
        /// Constructs a DataString from a source string
        /// </summary>
        /// <param name="Source"></param>
        public DataString(string Source)
        {
            String = Source;
            if (String.Length > UInt16.MaxValue)
                throw new ApplicationException("DataString: Initial string too long.  Max size is " + UInt16.MaxValue + " characters");
        }

        /// <summary>
        /// Copies an existing DataString
        /// </summary>
        /// <param name="Source"></param>
        public DataString(DataString Source)
        {
            this.String = String.Copy(Source.String);
        }

        /// <summary>
        /// Save the DataString to an already-opened stream
        /// </summary>
        /// <param name="stream">The stream to be written to</param>
        public void Save(BinaryWriter stream)
        {
            UInt16 length;
            length = (UInt16)String.Length;

            stream.Write(length);
            if (length > 0)
                stream.Write(String.ToCharArray());
        }

        public string Value { get { return String; } }
    }
}
