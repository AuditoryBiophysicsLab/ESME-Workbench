using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace MBSGUI
{
    public partial class FormTest : Form
    {
        Point m_displayLocation;
        int m_placementXFudgeQty = 5;
        int m_placementYFudgeQty = 0;
        double m_bathymetyDepth;

        public FormTest(int LocX, int LocY, double CurrentBathymetyDepth)
        {
            InitializeComponent();
            m_displayLocation.X = LocX + m_placementXFudgeQty;
            m_displayLocation.Y = LocY + m_placementYFudgeQty;
            m_bathymetyDepth = CurrentBathymetyDepth;
        }

        private void MyAcceptButton_Click(object sender, EventArgs e)
        {
            Dispose();
        }

        private void FormTest_Activated(object sender, EventArgs e)
        {
            MyAcceptButton.SendToBack();
            this.SetDesktopLocation(m_displayLocation.X, m_displayLocation.Y);
            this.textBox1.Text = "" + string.Format("{0:0.000000}", m_bathymetyDepth);
            //this.Invalidate();
        }
    }
}
