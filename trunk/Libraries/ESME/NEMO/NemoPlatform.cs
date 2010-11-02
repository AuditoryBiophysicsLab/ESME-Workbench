using System;
using System.Collections.Generic;
using System.Xml;
using ESME.Platform;

namespace ESME.NEMO
{
    public class NemoPlatform : NemoPSM
    {
        public NemoPlatform(XmlNode platform, string scenarioDirectory, NemoScenario nemoScenario, ref int modeID) : base(platform)
        {
            try
            {
                Sources = new List<NemoSource>();
                Trackdefs = new List<NemoTrackdef>();

                Description = GetString("description");

                Launcher = GetString("launcher");
                Towwer = GetString("towwer");

                foreach (XmlNode cur in platform.ChildNodes) if (cur.Name == "trackDef") Trackdefs.Add(new NemoTrackdef(cur, scenarioDirectory));

                foreach (XmlNode cur in platform.ChildNodes) if (cur.Name == "Source") Sources.Add(new NemoSource(cur, Trackdefs[0].InitialHeight, ref modeID));
                
                if (Trackdefs.Count == 0) throw new FormatException("Platform.trackDef: At least one trackDef is required for each Platform");

                NemoScenario = nemoScenario;
            }
            catch (Exception e)
            {
                throw new PlatformException(string.Format("Error initializing platform {0}", Name), e);
            }
        }

        public void CalculateBehavior()
        {
            BehaviorModel = new BehaviorModel(this);
        }

        public string Description { get; private set; }
        public string Launcher { get; private set; }
        public string Towwer { get; private set; }
        public List<NemoTrackdef> Trackdefs { get; private set; }
        public List<NemoSource> Sources { get; private set; }
        public BehaviorModel BehaviorModel { get; private set; }

        public NemoScenario NemoScenario { get; private set; }
    }
}