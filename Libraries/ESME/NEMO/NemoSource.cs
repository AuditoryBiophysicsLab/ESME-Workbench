using System.Collections.Generic;
using System.Xml;

//for XML 

namespace ESME.NEMO
{
    public class NemoSource : NemoPSM
    {
        public NemoSource(XmlNode source, float platformHeight, ref int modeID) : base(source)
        {
            Modes = new List<NemoMode>();

            Description = GetString("description");

            foreach (XmlNode cur in source.ChildNodes) if (cur.Name == "Mode") Modes.Add(new NemoMode(cur, platformHeight, modeID++));
        }

        public string Description { get; private set; }
        public List<NemoMode> Modes { get; private set; }
    }
}