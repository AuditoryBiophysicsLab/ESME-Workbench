using System;
using System.Xml;

namespace ESME.NEMO
{
    public class NemoPSM : NemoBase
    {
        public NemoPSM(XmlNode mode)
            : base(mode)
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

        public string PSMName { get; private set; }
        public string PSMId { get; private set; }
        public string Name { get; private set; }
        public string Id { get; private set; }
        public DateTime StartTime { get; private set; }
        public TimeSpan Duration { get; private set; }
        public int Priority { get; private set; }
        public string Type { get; private set; }
    }
}