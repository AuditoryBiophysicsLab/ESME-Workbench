using System.Collections.Generic;
using System.Xml;

//for XML 

namespace ESME.NEMO
{
    public class NemoSource : NemoPSM
    {
        public NemoSource(XmlNode source) : base(source)
        {
            Modes = new List<NemoMode>();

            Description = GetString("description");

            foreach (XmlNode cur in source.ChildNodes) if (cur.Name == "Mode") Modes.Add(new NemoMode(cur));
        }

        public string Description { get; private set; }
        public List<NemoMode> Modes { get; private set; }
    }
}