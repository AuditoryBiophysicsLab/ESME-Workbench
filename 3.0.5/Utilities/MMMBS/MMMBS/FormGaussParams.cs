using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using MMMBSLib;  // c# code code and data types.

namespace MBSGUI
{
    public struct GAUSSPARAM
    {
        public double mean;
        public double std;
        public double termCoeff;
        public Boolean termCoeffEnabled;      
    }

    public partial class FormGaussParams: Form
    {
        GAUSSPARAM m_gp;
        private Boolean m_modified = false;
        public Boolean modified { get { return m_modified; } }
        private Boolean m_initializing;
        private string m_szTitle = "Gaussian Description";



        public FormGaussParams(GAUSSPARAM Gp)
        {
            InitializeComponent();

            m_initializing = true;
            this.Text = m_szTitle;
       
            m_gp = Gp;

            MeanTextBox.Text = "" + Gp.mean;
            StdTextBox.Text = "" + Gp.std;

            CoeffGTextBox.Enabled = false;
            CoeffGTextBox.Text = "---";
            if(Gp.termCoeffEnabled == true)
            {
                CoeffGTextBox.Enabled = true;
                CoeffGTextBox.Text = "" + Gp.termCoeff;
            }
            m_initializing = false;
        }

        // Returns true if initializing
        private Boolean UpdateModifiedStatus()
        {
            if(m_initializing == true)
                return true;

            this.Text = m_szTitle + "*";
            m_modified = true;

            return false;
        }


        public GAUSSPARAM res { get { return m_gp; } }

        private void CancelButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                FormConfirmDecision dlg = new FormConfirmDecision();
                dlg.Text = "Confirm Cancel";
                dlg.messageString = "Abandon changes to model?";
                dlg.button1String = "Confirm Cancel";
                dlg.button2String = "Do Not Cancel";
                dlg.SetLocation(OkButton);
                dlg.ShowDialog(this);

                if(dlg.buttonSelected == 2)
                    return;
                m_modified = false;
            }
            this.BringToFront();
            Dispose();
        }

        private void OkButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                m_gp.mean = CStringUtil.SzToDouble(MeanTextBox.Text);
                m_gp.std = CStringUtil.SzToDouble(StdTextBox.Text);

                if(CoeffGTextBox.Enabled == true)
                    m_gp.termCoeff = CStringUtil.SzToDouble(CoeffGTextBox.Text);
                else
                    m_gp.termCoeff = 0;
            }
            Dispose();
        }

        private void MeanTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                MeanTextBox.Text = CStringUtil.SzEnforceDoubleFmt(MeanTextBox.Text);
        }

        private void StdTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                StdTextBox.Text = CStringUtil.SzEnforceDoubleFmt(StdTextBox.Text);
        }

        private void CoeffGTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                CoeffGTextBox.Text = CStringUtil.SzEnforceDoubleFmt(CoeffGTextBox.Text);
        }
    }
}