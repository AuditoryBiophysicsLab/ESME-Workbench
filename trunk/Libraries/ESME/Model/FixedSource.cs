using System.Collections.Generic;
using System.Xml.Serialization;
using ESME.TransmissionLoss;

namespace ESME.Model
{
    public class FixedSource
    {
        public FixedSource(NewAnalysisPoint analysisPoint)
        {
            AnalysisPoint = analysisPoint;
            AnalysisPointID = analysisPoint.IDField;
        }

/*
        FixedSource() { }
*/
        public int AnalysisPointID { get; set; }
        public string Name { get; set; }
        public double BearingDegrees { get; set; }
        public double HorizontalHalfAngleDegrees { get; set; }
        public double PingIntervalSeconds { get; set; }
        public double PingDurationSeconds { get; set; }

        [XmlIgnore]
        public NewAnalysisPoint AnalysisPoint { get; set; }
    }

    public class FixedSourceList : List<FixedSource>
    {
        internal void Initialize(NewAnalysisPointList analysisPoints) { foreach (var f in this)
        {
            FixedSource f1 = f;
            f.AnalysisPoint = analysisPoints.Find(a => a.IDField == f1.AnalysisPointID);
        }
        }
    }
}