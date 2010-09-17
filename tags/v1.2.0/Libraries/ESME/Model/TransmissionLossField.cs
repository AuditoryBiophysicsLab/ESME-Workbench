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

namespace ESME.Model
{
    public class TransmissionLossField
    {
        public int AnalysisPointID { get; set; }
        public string Filename { get; set; }
        public int MaxTLDepth_meters { get; set; }
        public TransmissionLossRadialList TransmissionLossRadials { get; set; }
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
            set { TransmissionLossRadials = new TransmissionLossRadialList(value); }
        }

        [XmlIgnore]
        public AnalysisPoint AnalysisPoint { get; set; }
        //[XmlIgnore]
        //public FieldData FieldData { get; set; }
        [XmlIgnore]
        private float mRadialBearing_degrees;

        internal TransmissionLossField() { }

        public TransmissionLossField(AnalysisPoint AnalysisPoint) 
        {
            this.AnalysisPoint = AnalysisPoint;
            AnalysisPointID = AnalysisPoint.IDField; 
        }
    }

    public class TransmissionLossFieldList : List<TransmissionLossField>
    {
        internal void Initialize(AnalysisPointList AnalysisPoints)
        {
            foreach (var f in this)
                f.AnalysisPoint = AnalysisPoints.Find(a => a.IDField == f.AnalysisPointID);
        }

    }

    public class TransmissionLossRadialList : List<TransmissionLossRadial>
    {
        public TransmissionLossRadialList() { }

        public TransmissionLossRadialList(int Count) : base(Count)
        {
            float BearingStep_degrees = 360f / (float)Count;
            for (int i = 0; i < Count; i++)
            {
                TransmissionLossRadial Radial = new TransmissionLossRadial();
                Radial.SetBearing_Degrees(BearingStep_degrees * i);
                Radial.BearingChanged += new EventHandler<EventArgs>(Radial_BearingChanged);
                base.Add(Radial);
            }
            Sort();
        }

        void Radial_BearingChanged(object sender, EventArgs e)
        {
            TransmissionLossRadial Source = sender as TransmissionLossRadial;
            foreach (TransmissionLossRadial Radial in this)
                if (Source != Radial)
                    Radial.ChangeBearing_degrees(Source.BearingChange_degrees);
            Sort();
        }
    }

    public class TransmissionLossRadial : IComparable<TransmissionLossRadial>
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

        public int CompareTo(TransmissionLossRadial that)
        {
            if (this.mBearingFromSource_degrees < that.mBearingFromSource_degrees)
                return -1;
            if (this.mBearingFromSource_degrees > that.mBearingFromSource_degrees)
                return -1;
            return 0;
        }
    }
}
