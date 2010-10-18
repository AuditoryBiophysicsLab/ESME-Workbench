using System;
using System.Xml;

namespace ESME.NEMO
{
    public class NemoMode : NemoPSM
    {
        public NemoMode(XmlNode mode) : base(mode)
        {
            State = GetString("state");
            Linked = GetString("linked");
            ActiveTime = GetFloat("activeTime");
            DepthOffset = GetFloat("depthOffset");
            SourceLevel = GetFloat("sourceLevel");
            LowFrequency = GetFloat("lowFrequency");
            HighFrequency = GetFloat("highFrequency");
            PulseInterval = GetTimeSpan("pulseInterval");
            PulseLength = new TimeSpan(0, 0, 0, 0, (int) Math.Round(GetFloat("pulseLength")));
            HorizontalBeamWidth = GetFloat("horizontalBeamWidth");
            VerticalBeamWidth = GetFloat("verticalBeamWidth");
            DepressionElevationAngle = GetFloat("depthElAngle");
            RelativeBeamAngle = GetFloat("relativeBeamAngle");
            Radius = GetFloat("radius");
            //UsePlatformTimes = GetBool("usePlatformTimes");
            //PropagationPath = GetString("propagationPath");

            EndTime = StartTime + Duration;
        }

        public string State { get; private set; }
        public string Linked { get; private set; }
        public float ActiveTime { get; private set; }
        public float DepthOffset { get; private set; }
        public float SourceLevel { get; private set; }
        public float LowFrequency { get; private set; }
        public float HighFrequency { get; private set; }
        public TimeSpan PulseInterval { get; private set; }
        public TimeSpan PulseLength { get; private set; }
        public float HorizontalBeamWidth { get; private set; }
        public float VerticalBeamWidth { get; private set; }
        public float DepressionElevationAngle { get; private set; }
        public float RelativeBeamAngle { get; private set; }
        public float Radius { get; private set; }
        //public bool UsePlatformTimes { get; private set; }
        //public string PropagationPath { get; private set; }

        // Derived properties, for convenience
        public DateTime EndTime { get; private set; }

        internal bool Contains(DateTime simulationTime)
        {
            if ((StartTime <= simulationTime) && (simulationTime <= EndTime)) return true;
            return false;
        }
    }
}