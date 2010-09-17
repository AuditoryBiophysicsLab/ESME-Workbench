using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace ESME.Overlay
{
    public class OverlayFiles : List<OverlayFile>
    {
        public bool Display { get; set; }

        public OverlayFile this[string filename]
        {
            get { return this.FirstOrDefault(f => Path.GetFileName(f.FileName) == filename); }
        }

        public void Add(string filename)
        {
            if (Find(f => f.FileName == filename) != null)
                Add(new OverlayFile(filename));
        }

        public void Delete(string filename)
        {
            Remove(this[filename]);
        }
    }
}