using System;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Serialization;
using ESME.Model;
using HRC.Utility;

namespace ESME.NEMO
{
    [Serializable]
    public class NemoMode : NemoPSM, IEquatable<NemoMode>, IEquatable<AcousticProperties>
    {
        public NemoMode(XmlNode mode, float platformHeight, int modeID) : base(mode)
        {
            ModeID = modeID;
            State = GetString("state");
            Linked = GetString("linked");
            ActiveTime = GetFloat("activeTime");
            DepthOffset = GetFloat("depthOffset");
            SourceLevel = GetFloat("sourceLevel");
            SourceDepth = Math.Abs(platformHeight) + DepthOffset;
            LowFrequency = GetFloat("lowFrequency");
            HighFrequency = GetFloat("highFrequency");
            PulseInterval = GetTimeSpan("pulseInterval");
            PulseLength = new TimeSpan(0, 0, 0, 0, (int) Math.Round(GetFloat("pulseLength")));
            HorizontalBeamWidth = GetFloat("horizontalBeamWidth");
            VerticalBeamWidth = GetFloat("verticalBeamWidth");
            DepressionElevationAngle = GetFloat("depthElAngle");
            RelativeBeamAngle = GetFloat("relativeBeamAngle");
            Radius = GetFloat("radius");
            try
            {
                ClusterCount = GetInt("clusterCount");
            }
            catch { }
            
            //UsePlatformTimes = GetBool("usePlatformTimes");
            //PropagationPath = GetString("propagationPath");

            EndTime = StartTime + Duration;
            _hashCode = PrimitiveConversion.ToInt(new NemoModeHashCode(this));
        }

        public NemoMode() { }

        public void CalculateActiveTimeSteps(NemoScenario nemoScenario)
        {
            ActiveTimeSteps = new ActiveTimeSteps();
            var scenarioEndTime = nemoScenario.StartTime + nemoScenario.Duration;
            var pulsesPerStep = SimulationStepTime.TotalSeconds/PulseInterval.TotalSeconds;
            //var durationPerStep = pulsesPerStep*nemoMode.PulseLength.TotalSeconds;
            var fractionalPulseCount = 0.0;
            var realPulseCount = 0;
            for (var curTime = nemoScenario.StartTime; curTime <= scenarioEndTime; curTime += SimulationStepTime)
            {
                fractionalPulseCount += pulsesPerStep;
                if ((int) fractionalPulseCount > realPulseCount)
                {
                    var actualPulses = (int) fractionalPulseCount - realPulseCount;
                    ActiveTimeSteps.Add(new ActiveTimeStep
                                        {
                                            SimulationTime = curTime,
                                            ActiveTime = new TimeSpan(0, 0, 0, 0, (int) (PulseLength.TotalMilliseconds*actualPulses)),
                                        });
                    realPulseCount += actualPulses;
                }
            }
        }

        public override IEnumerable<KeyValuePair<string, string>> Properties
        {
            get
            {
                foreach (var property in base.Properties) yield return property;
                yield return new KeyValuePair<string, string>("State", State);
                yield return new KeyValuePair<string, string>("Linked", Linked);
                yield return new KeyValuePair<string, string>("Active time", ActiveTime.ToString());
                yield return new KeyValuePair<string, string>("Depth offset", DepthOffset.ToString());
                yield return new KeyValuePair<string, string>("Source level", SourceLevel.ToString());
                yield return new KeyValuePair<string, string>("Source depth", SourceDepth.ToString());
                yield return new KeyValuePair<string, string>("Low frequency", LowFrequency.ToString());
                yield return new KeyValuePair<string, string>("High frequency", HighFrequency.ToString());
                yield return new KeyValuePair<string, string>("Pulse interval", PulseInterval.ToString());
                yield return new KeyValuePair<string, string>("Pulse length",PulseLength.ToString());
                yield return new KeyValuePair<string, string>("Horizontal beam width", HorizontalBeamWidth.ToString());
                yield return new KeyValuePair<string, string>("Vertical beam width", VerticalBeamWidth.ToString());
                yield return new KeyValuePair<string, string>("Depression/elevation angle",DepressionElevationAngle.ToString());
                yield return new KeyValuePair<string, string>("Relative beam angle", RelativeBeamAngle.ToString());
                yield return new KeyValuePair<string, string>("Radius", Radius.ToString());
                yield return new KeyValuePair<string, string>("ClusterCount", ClusterCount.ToString());
            }
        }

        public int ModeID { get; set; }
        public string State { get; set; }
        public string Linked { get; set; }
        public float ActiveTime { get; set; }
        public float DepthOffset { get; set; }
        public float SourceLevel { get; set; }
        public float SourceDepth { get; set; }
        public float LowFrequency { get; set; }
        public float HighFrequency { get; set; }
        public TimeSpan PulseInterval { get; set; }
        public TimeSpan PulseLength { get; set; }
        public float HorizontalBeamWidth { get; set; }
        public float VerticalBeamWidth { get; set; }
        public float DepressionElevationAngle { get; set; }
        public float RelativeBeamAngle { get; set; }
        public float Radius { get; set; }
        public int ClusterCount { get; set; }
        public ActiveTimeSteps ActiveTimeSteps { get; set; }
        //public bool UsePlatformTimes { get; set; }
        //public string PropagationPath { get; set; }

        // Derived properties, for convenience
        public DateTime EndTime { get; set; }

        internal bool Contains(DateTime simulationTime)
        {
            if ((StartTime <= simulationTime) && (simulationTime <= EndTime)) return true;
            return false;
        }

        public bool Equals(NemoMode other)
        {
            const double tolerance = 0.1;
            return Compare(SourceDepth, other.SourceDepth, tolerance) && 
                   Compare(SourceLevel, other.SourceLevel, tolerance) && 
                   Compare(LowFrequency, other.LowFrequency, tolerance) && 
                   Compare(HighFrequency, other.HighFrequency, tolerance) && 
                   Compare(VerticalBeamWidth, other.VerticalBeamWidth, tolerance) && 
                   Compare(DepressionElevationAngle, other.DepressionElevationAngle, tolerance) && 
                   Compare(Radius, other.Radius, tolerance);
        }

        #region public AcousticProperties AcousticProperties { get; set; }
        [XmlIgnore]
        public AcousticProperties AcousticProperties
        {
            get
            {
                return _acousticProperties ?? (_acousticProperties = new AcousticProperties
                {
                        DepressionElevationAngle = DepressionElevationAngle,
                        HighFrequency = HighFrequency,
                        LowFrequency = LowFrequency,
                        SourceDepth = SourceDepth,
                        VerticalBeamWidth = VerticalBeamWidth
                });
            }
        }

        AcousticProperties _acousticProperties;
        public bool Equals(AcousticProperties other) { return AcousticProperties.Equals(other); }

        #endregion


        readonly int _hashCode;
        public override int GetHashCode() { return _hashCode; }

        static bool Compare(double left, double right, double tolerance) { return Math.Abs(left - right) <= tolerance; }
    }

    internal struct NemoModeHashCode
    {
        [BitfieldLength(5)] internal int DepthHash;
        [BitfieldLength(4)] internal int LevelHash;
        [BitfieldLength(5)] internal int LowFreqHash;
        [BitfieldLength(5)] internal int HighFreqHash;
        [BitfieldLength(4)] internal int VerticalBeamWidthHash;
        [BitfieldLength(5)] internal int DepElevAngleHash;
        [BitfieldLength(4)] internal int RadiusHash;

        public NemoModeHashCode(NemoMode nemoMode)
        {
            DepthHash = nemoMode.SourceDepth.GetHashCode();
            LevelHash = nemoMode.SourceLevel.GetHashCode();
            LowFreqHash = nemoMode.LowFrequency.GetHashCode();
            HighFreqHash = nemoMode.HighFrequency.GetHashCode();
            VerticalBeamWidthHash = nemoMode.HighFrequency.GetHashCode();
            DepElevAngleHash = nemoMode.DepressionElevationAngle.GetHashCode();
            RadiusHash = nemoMode.Radius.GetHashCode();
        }
    }
}