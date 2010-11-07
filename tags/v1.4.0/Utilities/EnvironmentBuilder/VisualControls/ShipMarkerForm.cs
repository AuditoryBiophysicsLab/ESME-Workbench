using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using ESME.Environment;
using ESME.NEMO;
using EnvironmentBuilder.ClassControls;
using EnvironmentBuilder.VisualControls;
using System.Xml;

namespace EnvironmentBuilder.VisualControls
{
    public partial class ShipMarkerForm : Form
    {
        private PointF mShipPoint = new PointF();
        private int mcontAccess = 0;
        public ShipMarkerForm()
        {
            InitializeComponent();
        }

        private void shipSbtn_Click(object sender, EventArgs e)
        {
            float shiplatF, shiplongF;
            string lat = shiplat.Text.ToString();
            string lon = shiplong.Text.ToString();
            if (float.TryParse(lat,out shiplatF) && float.TryParse(lon, out shiplongF))
                mShipPoint = new PointF(shiplongF, shiplatF);
            this.Hide();
            
        }

        public PointF ShipPoint { get { return mShipPoint; } }
       
    }
}
