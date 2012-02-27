using System.Collections.Generic;

namespace ESME.Databases
{
    public class Source
    {
        public int SourceID { get; set; }
        public string SourceName { get; set; }
        public string SourceType { get; set; }
        public virtual Platform Platform { get; set; }
        public virtual ICollection<Mode> Modes { get; set; }
    }
}