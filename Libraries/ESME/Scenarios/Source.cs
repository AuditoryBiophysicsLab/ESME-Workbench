using System;
using System.Collections.Generic;
using ESME.Locations;
using HRC.Aspects;

namespace ESME.Scenarios
{
    public class Source : IHaveGuid
    {
        [Initialize(IsGuid = true)]
        public Guid Guid { get; set; }
        public string PSMSourceGuid { get; set; }
        public string SourceName { get; set; }
        public string SourceType { get; set; }

        public virtual Platform Platform { get; set; }
        public virtual ICollection<Mode> Modes { get; set; }
        public virtual ICollection<LogEntry> Logs { get; set; }
    }
}