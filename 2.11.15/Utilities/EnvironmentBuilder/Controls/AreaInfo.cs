using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace IESME_GEO.Controls
{
    public partial class AreaInfo : SelectionAwareControl
    {
        public AreaInfo()
        {
            InitializeComponent();
            PopulateTextboxes();
            toolTip1.IsBalloon = true;
            toolTip1.UseFading = true;
            
        }

        public void PopulateTextboxes()
        {
            txtNorth.Text = North.ToString("0.00000");
            txtSouth.Text = South.ToString("0.00000");
            txtEast.Text = East.ToString("0.00000");
            txtWest.Text = West.ToString("0.00000");
        }

       
     
        /*private void txtNorth_Leave(object sender, EventArgs e)
        {
            float newValue;

            if (float.TryParse(((TextBox)sender).Text, out newValue))
                North = newValue;
            
            toolTip1.Show("Click Tab to show update on map", this.txtNorth, 0, 0, 5000);
            PopulateTextboxes();
        }

        private void txtSouth_Leave(object  sender, EventArgs e)
        {
            float newValue;

            if (float.TryParse(((TextBox)sender).Text, out newValue))
                South = newValue;
            toolTip1.Show("Click Tab to show update on map", this.txtSouth, 0, 0, 5000);
            PopulateTextboxes();
        }

        private void txtEast_Leave(object sender, EventArgs e)
        {
            float newValue;

            if (float.TryParse(((TextBox)sender).Text, out newValue))
                East = newValue;
            toolTip1.Show("Click Tab to show update on map", this.txtEast, 0, 0, 5000);
            PopulateTextboxes();
        }

        private void txtWest_Leave(object sender, EventArgs e)
        {
            float newValue;

            if (float.TryParse(((TextBox)sender).Text, out newValue))
                West = newValue;
            toolTip1.Show("Click Tab to show update on map", this.txtWest,0,0, 5000);
            PopulateTextboxes();
        }*/

        private void update_btn_Click(object sender, EventArgs e)
        {
            float newValue;

            String NtxtBoxNow = txtNorth.Text;
            String StxtBoxNow = txtSouth.Text;
            String EtxtBoxNow = txtEast.Text;
            String WtxtBoxNow = txtWest.Text;


            if (float.TryParse(NtxtBoxNow, out newValue))
                North = newValue;

            if (float.TryParse(StxtBoxNow, out newValue))
                South = newValue;

            if (float.TryParse(WtxtBoxNow, out newValue))
                West = newValue;

            if (float.TryParse(EtxtBoxNow, out newValue))
                East = newValue;

            PopulateTextboxes();
        }

    }
}
