using System;
using System.Collections.ObjectModel;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Serialization;

namespace ESME.Model
{
    public class FixedSource
    {
        public int AnalysisPointID { get; set; }
        public string Name { get; set; }
        public double Bearing_degrees { get; set; }
        public double HorizontalHalfAngle_degrees { get; set; }
        public double PingInterval_seconds { get; set; }
        public double PingDuration_seconds { get; set; }

        [XmlIgnore]
        public AnalysisPoint AnalysisPoint { get; set; }

        public FixedSource(AnalysisPoint AnalysisPoint)
        {
            this.AnalysisPoint = AnalysisPoint;
            AnalysisPointID = AnalysisPoint.IDField;
        }

        private FixedSource() { }
    }

    public class FixedSourceList : List<FixedSource>
    {
        internal void Initialize(AnalysisPointList AnalysisPoints)
        {
            foreach (var f in this)
                f.AnalysisPoint = AnalysisPoints.Find(a => a.IDField == f.AnalysisPointID);
        }
    }
}
