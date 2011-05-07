using System;
using System.Xml;

namespace ESME.NEMO
{
    public class NemoPSM : NemoBase
    {
        public NemoPSM(XmlNode mode) : base(mode)
        {
            PSMName = GetString("psmName");
            PSMId = GetString("psmId");
            Name = GetString("name");
            Id = GetString("id");
            StartTime = GetDateTime("startTime");
            Duration = GetTimeSpan("duration");
            Priority = GetInt("priority");
            Type = GetString("type");
        }

        public NemoPSM() {  }
        public string PSMName { get; set; }
        public string PSMId { get; set; }
        public string Name { get; set; }
        public string Id { get; set; }
        public DateTime StartTime { get; set; }
        public TimeSpan Duration { get; set; }
        public int Priority { get; set; }
        public string Type { get; set; }
    }
}