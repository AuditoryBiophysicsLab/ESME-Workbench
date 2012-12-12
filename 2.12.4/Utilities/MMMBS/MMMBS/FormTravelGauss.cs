using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using MMMBSLib;  // mmmb C# code code and data types.

namespace MBSGUI
{
    public partial class FormTravelGauss: Form
    {
        CParamsDirectionCRndmWalkDirBias m_r;
        private Boolean m_modified = false;
        public Boolean modified { get { return m_modified; } }
        private Boolean m_initializing;
        private string m_szTitle = "AE Gaussian Travel Direction Description";

        public FormTravelGauss(CParamsDirectionCRndmWalkDirBias P)
        {
            InitializeComponent();
            m_initializing = true;
            m_r = P;

            this.Text = m_szTitle;

            crwdbArcStepTextBox.Text = "" + P.arcStep;
            crwdbArcStepTextBox.Enabled = false;

            crwdbBiasTextBox.Text = "" + P.bias;
            crwdbBiasTextBox.Enabled = false;

            crwdbCoeffTextBox.Text = "" + P.coeff;
            crwdbPertTextBox.Text = "" + P.pert;
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


        private void crwdbPertTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                crwdbPertTextBox.Text = CStringUtil.SzEnforceDoubleFmt(crwdbPertTextBox.Text);
        }

        private void crwdbBiasTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                crwdbBiasTextBox.Text = CStringUtil.SzEnforceDoubleFmt(crwdbBiasTextBox.Text);
        }

        private void crwdbArcStepTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                crwdbArcStepTextBox.Text = CStringUtil.SzEnforceDoubleFmt(crwdbArcStepTextBox.Text);
        }

        private void crwdbCoeffTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                crwdbCoeffTextBox.Text = CStringUtil.SzEnforceDoubleFmt(crwdbCoeffTextBox.Text);
        }


        private void OkButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                m_r.arcStep = CStringUtil.SzToDouble(crwdbArcStepTextBox.Text);
                m_r.bias = CStringUtil.SzToDouble(crwdbBiasTextBox.Text);
                m_r.coeff = CStringUtil.SzToDouble(crwdbCoeffTextBox.Text);
                m_r.pert = CStringUtil.SzToDouble(crwdbPertTextBox.Text);
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