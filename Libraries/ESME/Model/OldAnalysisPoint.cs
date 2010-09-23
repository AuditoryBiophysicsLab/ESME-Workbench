using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.IO;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Schema;
using System.Xml.Serialization;
using HRC.Navigation;
//todo: deprecated.
namespace ESME.Model
{
    public class OldAnalysisPoint : IEquatable<OldAnalysisPoint>, IHasIDField
    {
        [XmlElement("AnalysisPointID")]
        public int IDField { get; set; }
        public EarthCoordinate Location { get; set; }
        public float FieldRadius_meters { get; set; }
        public float RangeCellSize_meters { get; set; }
        public float DepthCellSize_meters { get; set; }
        public AcousticProperties AcousticProperties { get; set; }

        bool IEquatable<OldAnalysisPoint>.Equals(OldAnalysisPoint that)
        {
            if (!this.Location.Equals(that.Location))
                return false;
            if (this.FieldRadius_meters != that.FieldRadius_meters)
                return false;
            if (this.RangeCellSize_meters != that.RangeCellSize_meters)
                return false;
            if (this.DepthCellSize_meters != that.DepthCellSize_meters)
                return false;
            if (!this.AcousticProperties.Equals(that.AcousticProperties))
                return false;
            return true;
        }

        public OldAnalysisPoint() { }

        public OldAnalysisPoint(OldAnalysisPoint that)
        {
            if (that.Location == null)
                this.Location = null;
            else
                this.Location = new EarthCoordinate(that.Location);
            this.FieldRadius_meters = that.FieldRadius_meters;
            this.RangeCellSize_meters = that.RangeCellSize_meters;
            this.DepthCellSize_meters = that.DepthCellSize_meters;
            this.AcousticProperties = new AcousticProperties(that.AcousticProperties);
        }
    }

    public class OldAnalysisPointDeletedEventArgs : ItemDeletedEventArgs<OldAnalysisPoint> { }

    public class OldAnalysisPointList : UniqueAutoIncrementList<OldAnalysisPoint>
    {
    }
}
