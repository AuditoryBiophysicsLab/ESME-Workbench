using System.Collections.Generic;

namespace ESME.Database
{
    public class Platform
    {
        public int PlatformID { get; set; }
        public string PlatformName { get; set; }
        public string PlatformType { get; set; }
        public virtual ICollection<Source> Sources { get; set; }
    }
}