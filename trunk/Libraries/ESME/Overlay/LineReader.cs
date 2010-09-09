using System.IO;

namespace ESME.Overlay
{
    internal class LineReader
    {
        private int _lineNumber;
        private StreamReader _reader;

        public string CommentIndicators { get; set; }

        public string FileName { get; set; }

        public int LineNumber
        {
            get
            {
                if (_reader == null)
                    return 0;
                return _lineNumber;
            }
        }

        public string NextLine()
        {
            if (_reader == null)
            {
                if ((string.IsNullOrEmpty(FileName)) || (!File.Exists(FileName)))
                    throw new FileNotFoundException("LineReader.NextLine: Could not open requested file '" + FileName + "'");
                _reader = new StreamReader(FileName);
            }

            string curLine = string.Empty;

            while (curLine == string.Empty)
            {
                curLine = _reader.ReadLine();
                if (curLine == null)
                    return null;
                curLine = curLine.Trim().ToLower();
                _lineNumber++;
            }
            // Now, curLine contains a non-blank line

            // if (CommentIndicators.Length > 0)
            //{
            //  foreach (string curComment in CommentIndicators)
            // {
            string comment = CommentIndicators.ToLower();
            while ((curLine.StartsWith(comment)) || (curLine == string.Empty))
            {
                curLine = _reader.ReadLine();
                if (curLine == null)
                    return null;
                curLine = curLine.Trim().ToLower();
                _lineNumber++;
            }
            //}
            // }
            return curLine;
        }
    }
}