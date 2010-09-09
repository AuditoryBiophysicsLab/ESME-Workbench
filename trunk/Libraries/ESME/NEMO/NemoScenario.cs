using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml;
using ESME.Overlay;

namespace ESME.NEMO
{
    public class NemoScenario : NemoBase
    {
        public NemoScenario(XmlNode scenario, string nemoDataDirectory)
            : base(scenario)
        {
            Platforms = new List<NemoPlatform>();
            Animals = new List<NemoAnimals>();

            BuilderVersion = GetString("builderVersion");
            EventName = GetString("eventName");
            CreationTime = GetDateTime("creationTime");
            Description = GetString("description");
            AnalystName = GetString("analystName");
            StartTime = GetDateTime("startTime");
            Duration = GetTimeSpan("duration");
            SimAreaName = GetString("simAreaName");
            TimeFrame = GetString("timeFrame");

            string scenarioDirectory = Path.Combine(nemoDataDirectory, SimAreaName);

            foreach (XmlNode cur in scenario.ChildNodes)
                if (cur.Name == "Platform")
                    Platforms.Add(new NemoPlatform(cur, scenarioDirectory, this));

            foreach (XmlNode cur in scenario.ChildNodes)
                if (cur.Name == "animals")
                    Animals.Add(new NemoAnimals(cur, scenarioDirectory));

            string[] simAreaOverlays = Directory.GetFiles(Path.Combine(scenarioDirectory, "Areas"), "*_SIM_AREA.ovr");
            if ((simAreaOverlays != null) && (simAreaOverlays.Length > 0))
                OverlayFile = new OverlayFile(simAreaOverlays[0]);
        }

        public string BuilderVersion { get; private set; }
        public string EventName { get; private set; }
        public DateTime CreationTime { get; private set; }
        public string Description { get; private set; }
        public string AnalystName { get; private set; }
        public DateTime StartTime { get; private set; }
        public TimeSpan Duration { get; private set; }
        public string SimAreaName { get; private set; }
        public string TimeFrame { get; private set; }
        public List<NemoPlatform> Platforms { get; private set; }
        public List<NemoAnimals> Animals { get; private set; }

        public OverlayFile OverlayFile { get; private set; }

        public IEnumerable<NemoMode> ActiveModes()
        {
            return from platform in Platforms from source in platform.Sources from mode in source.Modes select mode;
        }
    }
}