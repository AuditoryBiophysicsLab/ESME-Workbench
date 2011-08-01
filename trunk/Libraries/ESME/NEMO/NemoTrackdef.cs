using System;
using System.IO;
using System.Xml;
using ESME.NEMO.Overlay;
using HRC.Navigation;

namespace ESME.NEMO
{
    public class NemoTrackdef : NemoBase, IComparable<NemoTrackdef>
    {
        public NemoTrackdef(XmlNode trackDef, string scenarioDirectory) : base(trackDef)
        {
            TrackType = GetString("trackType");
            StartTime = GetDateTime("startTime");
            Duration = GetTimeSpan("duration");
            Random = GetBool("random");
            OpsBounds = GetBool("opsBounds");
            OpsTimes = GetBool("opsTimes");
            InitialLatitude = GetFloat("initialLatitude");
            InitialLongitude = GetFloat("initialLongitude");
            InitialHeight = GetFloat("initialHeight");
            InitialCourse = GetFloat("initialCourse");
            InitialSpeed = GetFloat("initialSpeed");
            LimitFileName = GetString("limitFileName");

            InitialLocation = new EarthCoordinate3D(InitialLatitude, InitialLongitude, InitialHeight);
            EndTime = StartTime + Duration;
            if (!string.IsNullOrEmpty(LimitFileName)) OverlayFile = new OverlayFile(Path.Combine(Path.Combine(scenarioDirectory, "Areas"), LimitFileName));
        }

        public string TrackType { get; private set; }
        public DateTime StartTime { get; private set; }
        public TimeSpan Duration { get; private set; }
        public bool Random { get; private set; }
        public bool OpsBounds { get; private set; }
        public bool OpsTimes { get; private set; }
        public float InitialLatitude { get; private set; }
        public float InitialLongitude { get; private set; }
        public float InitialHeight { get; private set; }
        public float InitialCourse { get; private set; }
        public float InitialSpeed { get; private set; }
        public string LimitFileName { get; private set; }

        public EarthCoordinate3D InitialLocation { get; private set; }
        public DateTime EndTime { get; private set; }
        public OverlayFile OverlayFile { get; private set; }
        //public OverlayPoint StartPoint { get; private set; }

        #region IComparable<NemoTrackdef> Members

        public int CompareTo(NemoTrackdef other) { return StartTime.CompareTo(other.StartTime); }

        #endregion

        internal bool Contains(DateTime simulationTime)
        {
            if ((StartTime <= simulationTime) && (simulationTime <= EndTime)) return true;
            return false;
        }
    }
}