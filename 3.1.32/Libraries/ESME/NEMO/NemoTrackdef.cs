using System;
using System.Collections.Generic;
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

            InitialLocation = new Geo<float>(InitialLatitude, InitialLongitude, InitialHeight);
            EndTime = StartTime + Duration;
            if (!string.IsNullOrEmpty(LimitFileName)) OverlayFile = new OverlayFile(Path.Combine(Path.Combine(scenarioDirectory, "Areas"), LimitFileName));
        }

        public override IEnumerable<KeyValuePair<string, string>> Properties
        {
            get
            {
                yield return new KeyValuePair<string, string>("Track type", TrackType);
                yield return new KeyValuePair<string, string>("Start time", StartTime.ToString());
                yield return new KeyValuePair<string, string>("Duration", Duration.ToString());
                yield return new KeyValuePair<string, string>("Random", Random.ToString());
                yield return new KeyValuePair<string, string>("Ops bounds", OpsBounds.ToString());
                yield return new KeyValuePair<string, string>("Ops times", OpsTimes.ToString());
                yield return new KeyValuePair<string, string>("Initial latitude", InitialLatitude.ToString());
                yield return new KeyValuePair<string, string>("Initial longitude", InitialLongitude.ToString());
                yield return new KeyValuePair<string, string>("Initial height", InitialHeight.ToString());
                yield return new KeyValuePair<string, string>("Initial course", InitialCourse.ToString());
                yield return new KeyValuePair<string, string>("Initial speed", InitialSpeed.ToString());
                yield return new KeyValuePair<string, string>("Limit file", LimitFileName);
            }
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

        public Geo<float> InitialLocation { get; private set; }
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