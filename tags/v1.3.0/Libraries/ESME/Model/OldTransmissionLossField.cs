using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;
using ESME.TransmissionLoss;
using System.IO;
using System.Windows.Forms;
//todo: deprecated.
namespace ESME.Model
{
    public class OldTransmissionLossField
    {
        public int AnalysisPointID { get; set; }
        public string Filename { get; set; }
        public int MaxTLDepth_meters { get; set; }
        public OldTransmissionLossRadialList TransmissionLossRadials { get; set; }
        [XmlIgnore]
        public float RadialBearing_degrees
        {
            get { return mRadialBearing_degrees; }
            set
            {
                mRadialBearing_degrees = value;
                float BearingStep_degrees = 360f / (float)TransmissionLossRadials.Count();
                for (int i = 0; i < TransmissionLossRadials.Count(); i++)
                    TransmissionLossRadials[i].BearingFromSource_degrees = mRadialBearing_degrees + (BearingStep_degrees * i);
            }
        }
        [XmlIgnore]
        public int RadialCount 
        {
            get { return TransmissionLossRadials.Count(); }
            set { TransmissionLossRadials = new OldTransmissionLossRadialList(value); }
        }

        [XmlIgnore]
        public OldAnalysisPoint OldAnalysisPoint { get; set; }
        //[XmlIgnore]
        //public FieldData FieldData { get; set; }
        [XmlIgnore]
        private float mRadialBearing_degrees;

        internal OldTransmissionLossField() { }

        public OldTransmissionLossField(OldAnalysisPoint OldAnalysisPoint) 
        {
            this.OldAnalysisPoint = OldAnalysisPoint;
            AnalysisPointID = OldAnalysisPoint.IDField; 
        }
    }

    public class OldTransmissionLossFieldList : List<OldTransmissionLossField>
    {
        internal void Initialize(OldAnalysisPointList oldAnalysisPoints)
        {
            foreach (var f in this)
                f.OldAnalysisPoint = oldAnalysisPoints.Find(a => a.IDField == f.AnalysisPointID);
        }

    }

    public class OldTransmissionLossRadialList : List<OldTransmissionLossRadial>
    {
        public OldTransmissionLossRadialList() { }

        public OldTransmissionLossRadialList(int Count)
            : base(Count)
        {
            float BearingStep_degrees = 360f / (float)Count;
            for (int i = 0; i < Count; i++)
            {
                OldTransmissionLossRadial Radial = new OldTransmissionLossRadial();
                Radial.SetBearing_Degrees(BearingStep_degrees * i);
                Radial.BearingChanged += new EventHandler<EventArgs>(Radial_BearingChanged);
                base.Add(Radial);
            }
            Sort();
        }

        void Radial_BearingChanged(object sender, EventArgs e)
        {
            OldTransmissionLossRadial Source = sender as OldTransmissionLossRadial;
            foreach (var Radial in this)
                if (Source != Radial)
                    Radial.ChangeBearing_degrees(Source.BearingChange_degrees);
            Sort();
        }
    }

    public class OldTransmissionLossRadial : IComparable<OldTransmissionLossRadial>
    {
        [XmlIgnore]
        private float mBearingFromSource_degrees;
        [XmlIgnore]
        internal float BearingChange_degrees { get; set; }

        public float BearingFromSource_degrees
        {
            get { return mBearingFromSource_degrees; }
            set
            {
                BearingChange_degrees = value - mBearingFromSource_degrees;
                SetBearing_Degrees(value);
                OnBearingChanged();
            }
        }

        internal event EventHandler<EventArgs> BearingChanged;
        protected virtual void OnBearingChanged()
        {
            if (BearingChanged != null)
                BearingChanged(this, new EventArgs());
        }

        internal void SetBearing_Degrees(float NewBearing_degrees)
        {
            mBearingFromSource_degrees = NewBearing_degrees;
            while (mBearingFromSource_degrees > 360)
                mBearingFromSource_degrees -= 360;
            while (mBearingFromSource_degrees < 0)
                mBearingFromSource_degrees += 360;
        }

        internal void ChangeBearing_degrees(float DeltaBearing_degrees)
        {
            SetBearing_Degrees(mBearingFromSource_degrees + DeltaBearing_degrees);
        }

        public int CompareTo(OldTransmissionLossRadial that)
        {
            if (this.mBearingFromSource_degrees < that.mBearingFromSource_degrees)
                return -1;
            if (this.mBearingFromSource_degrees > that.mBearingFromSource_degrees)
                return -1;
            return 0;
        }
    }
}
