using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace IESME_GEO.Controls
{
    public partial class SelectionAwareControl : UserControl
    {
        float mNorth, mSouth, mEast, mWest;

        public SelectionAwareControl()
        {
            InitializeComponent();
        }

        public event EventHandler<EventArgs> CoordinateChanged;

        protected virtual void OnCoordinateChanged(EventArgs e)
        {
            if (CoordinateChanged != null)
                CoordinateChanged(this, e);
        }

        public float North
        {
            get { return mNorth; }
            set { SomeCoordinateChanged(value, ref mNorth); }
        }

        public float South
        {
            get { return mSouth; }
            set { SomeCoordinateChanged(value, ref mSouth); }
        }

        public float East
        {
            get { return mEast; }
            set { SomeCoordinateChanged(value, ref mEast); }
        }

        public float West
        {
            get { return mWest; }
            set { SomeCoordinateChanged(value, ref mWest); }
        }

        private void SomeCoordinateChanged(float newValue, ref float whichMember)
        {
            float difference;

            difference = Math.Abs(newValue - whichMember);
            if (difference > 0.00001f)
            {
                // Only trigger an event if the new value is significantly different from the old one (ratio change > 0.00001)
                whichMember = newValue;
                OnCoordinateChanged(new EventArgs());
            }
        }

    }
}
