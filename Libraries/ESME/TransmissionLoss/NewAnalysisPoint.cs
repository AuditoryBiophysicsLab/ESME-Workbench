using System;
using System.Xml.Serialization;
using ESME.Model;
using HRC.Navigation;

namespace ESME.TransmissionLoss
{
    public class NewAnalysisPoint : IEquatable<NewAnalysisPoint>, IHasIDField
    {
        public EarthCoordinate Location { get; set; }
        public float RadialBearing { get; set; }
        public int RadialCount { get; set; }

        //todo: plus a list of all relevent Sources.

        #region IEquatable<AnalysisPoint> Members

        bool IEquatable<NewAnalysisPoint>.Equals(NewAnalysisPoint that)
        {
            if (!Location.Equals(that.Location))
                return false;
            if (RadialBearing != that.RadialBearing)
                return false;
            return RadialCount == that.RadialCount;
        }

        #endregion

        #region IHasIDField Members

        [XmlElement("AnalysisPointID")]
        public int IDField { get; set; }

        #endregion
        
    }

    public class AnalysisPointDeletedEventArgs : ItemDeletedEventArgs<NewAnalysisPoint>
    {
    }

    public class NewAnalysisPointList : UniqueAutoIncrementList<NewAnalysisPoint>
    {
    }
}