using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using MMMBSLib; // C# code code and data types.

namespace MBSGUI
{
    public partial class FormReversalGauss: Form
    {
        CParamsReversalGaussn m_p;
        private Boolean m_modified = false;
        public Boolean modified { get { return m_modified; } }
        private Boolean m_initializing;
        private string m_szTitle = "Reversal Gaussian Description";

        public FormReversalGauss(CParamsReversalGaussn P)
        {
            InitializeComponent();
            this.Text = m_szTitle;

            m_p = P;

            m_initializing = true;
            MeanCntTextBox.Text = "" + P.meanCnt;
            StdCntTextBox.Text = "" + P.stdCnt;
            GaussProbTextBox.Text = "" + P.prob;
            GaussMeanTimeTextBox.Text = "" + P.meanTime;
            GaussStdTimeTextBox.Text = "" + P.stdTime;
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


        private void MeanCntTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                MeanCntTextBox.Text = CStringUtil.SzEnforceDoubleFmt(MeanCntTextBox.Text);
        }

        private void StdCntTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                StdCntTextBox.Text = CStringUtil.SzEnforceDoubleFmt(StdCntTextBox.Text);
        }

        private void GaussProbTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                GaussProbTextBox.Text = CStringUtil.SzEnforceDoubleFmt(GaussProbTextBox.Text);
        }

        private void GaussMeanTimeTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                GaussMeanTimeTextBox.Text = CStringUtil.SzEnforceDoubleFmt(GaussMeanTimeTextBox.Text);
        }

        private void GaussStdTimeTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                GaussStdTimeTextBox.Text = CStringUtil.SzEnforceDoubleFmt(GaussStdTimeTextBox.Text);
        }

        private void OkButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                m_p.meanCnt = CStringUtil.SzToDouble(MeanCntTextBox.Text);
                m_p.stdCnt = CStringUtil.SzToDouble(StdCntTextBox.Text);
                m_p.prob = CStringUtil.SzToDouble(GaussProbTextBox.Text);
                m_p.meanTime = CStringUtil.SzToDouble(GaussMeanTimeTextBox.Text);
                m_p.stdTime = CStringUtil.SzToDouble(GaussStdTimeTextBox.Text);
            }
            Dispose();
        }

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
                this.BringToFront();

                if(dlg.buttonSelected == 2)
                    return;
                m_modified = false;
            }
            Dispose();
        }
    }
}