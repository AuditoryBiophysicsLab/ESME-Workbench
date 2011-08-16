using System;
using System.Collections.Generic;
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

        public override IEnumerable<KeyValuePair<string, string>> Properties
        {
            get
            {
                yield return new KeyValuePair<string, string>("PSM Name", PSMName);
                yield return new KeyValuePair<string, string>("PSM ID", PSMId);
                yield return new KeyValuePair<string, string>("Name", Name);
                yield return new KeyValuePair<string, string>("ID", Id);
                yield return new KeyValuePair<string, string>("Start Time", StartTime.ToString());
                yield return new KeyValuePair<string, string>("Duration", Duration.ToString());
                yield return new KeyValuePair<string, string>("Priority", Priority.ToString());
                yield return new KeyValuePair<string, string>("Type", Type);
            }
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