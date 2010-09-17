using System.Drawing;
using System.IO;

namespace ESME.Overlay
{
    public partial class OverlayFile
    {
        private string _fileName;

        public OverlayFile()
        {
        }

        public OverlayFile(string fileName)
        {
            _fileName = fileName;
            Shapes = Parse(fileName);
        }

        public OverlayShape[] Shapes { get; private set; }

        public string FileName
        {
            get { return _fileName; }
            set
            {
                _fileName = value;
                Shapes = Parse(value);
            }
        }

        public Color Color
        {
            get { return Shapes[0].Color; }
            set
            {
                foreach (OverlayShape s in Shapes)
                    s.Color = value;
            }
        }

        public override string ToString()
        {
            return Path.GetFileName(_fileName);
        }
    }
}