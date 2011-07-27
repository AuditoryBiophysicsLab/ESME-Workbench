using System.IO;

namespace ESME.Overlay
{
    internal class LineReader
    {
        public LineReader()
        {
            Lines = null;
            LineNumber = 0;
        }

        string _fileName;
        public string FileName
        {
            get { return _fileName; }
            set
            {
                _fileName = value;
                if ((string.IsNullOrEmpty(_fileName)) || (!File.Exists(_fileName)))
                    throw new FileNotFoundException("LineReader.NextLine: Could not open requested file '" + _fileName + "'");
                Lines = File.ReadAllLines(_fileName);
            }
        }

        public int LineNumber { get; private set; }

        public string[] Lines { get; private set; }

        public string NextLine
        {
            get
            {
                // Skip over any blank lines encountered
                while ((LineNumber < Lines.Length) && string.IsNullOrEmpty(Lines[LineNumber].Trim()))
                    LineNumber++;
                if (LineNumber >= Lines.Length) return null;
                var result = Lines[LineNumber++].Trim().ToLower();
                return result;
            }
        }
    }
}