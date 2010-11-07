using System;
using System.Xml;
using HRC.Utility;

namespace ESME.NEMO
{
    public class NemoMode : NemoPSM, IEquatable<NemoMode>
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
            //UsePlatformTimes = GetBool("usePlatformTimes");
            //PropagationPath = GetString("propagationPath");

            EndTime = StartTime + Duration;
            _hashCode = PrimitiveConversion.ToInt(new NemoModeHashCode(this));
        }

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

        public int ModeID { get; private set; }
        public string State { get; private set; }
        public string Linked { get; private set; }
        public float ActiveTime { get; private set; }
        public float DepthOffset { get; private set; }
        public float SourceLevel { get; private set; }
        public float SourceDepth { get; private set; }
        public float LowFrequency { get; private set; }
        public float HighFrequency { get; private set; }
        public TimeSpan PulseInterval { get; private set; }
        public TimeSpan PulseLength { get; private set; }
        public float HorizontalBeamWidth { get; private set; }
        public float VerticalBeamWidth { get; private set; }
        public float DepressionElevationAngle { get; private set; }
        public float RelativeBeamAngle { get; private set; }
        public float Radius { get; private set; }
        public ActiveTimeSteps ActiveTimeSteps { get; private set; }
        //public bool UsePlatformTimes { get; private set; }
        //public string PropagationPath { get; private set; }

        // Derived properties, for convenience
        public DateTime EndTime { get; private set; }

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