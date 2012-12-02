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
    public partial class FormRate: Form
    {
        CRateParameters m_r;
        private Boolean m_modified = false;
        string m_szDescription;
        private Boolean m_refreshing = false;
        Boolean m_initializing = true;
        private string m_szTitle;
        

        public Boolean modified { get { return m_modified; } }

        public FormRate(CRateParameters P, string szDescription)
        {
            InitializeComponent();
            m_szTitle = "3MB " + szDescription + " Rate Description - Biomimetica";
            m_r = P;
            m_szDescription = szDescription;


            this.Text = m_szTitle;

            MeanTextBox.Text = "" + P.gauss.mean;
            StdTextBox.Text = "" + P.gauss.std;
            CoeffGTextBox.Text = "" + P.gauss.coeff;
            MaxTextBox.Text = "" + P.randm.max;
            MinTextBox.Text = "" + P.randm.min;
            CoeffRTextBox.Text = "" + P.randm.coeff;

            RateVectorTextBox.Text = P.vectorMdl.vector.ConvertToString();
            StepTextBox.Text = "" + P.vectorMdl.step.a;
            TermCoeffTextBox.Text = "" + P.vectorMdl.termination.a;

            OkButton.Enabled = true;
            RateRefreshButton.Enabled = false;

            GaussGroupBox.Enabled = RndGroupBox.Enabled = VectorGroupBox.Enabled = false;
            if(P.modelType == MODELTYPE.GAUSSIAN)
                GaussGroupBox.Enabled = true;
            else if(P.modelType == MODELTYPE.RANDOM)
                RndGroupBox.Enabled = true;
            else if(P.modelType == MODELTYPE.MATRIX)
                VectorGroupBox.Enabled = true;

            UpdateVectorModel();
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

        private void MeanTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // return true if initializing
               MeanTextBox.Text = CStringUtil.SzEnforceDoubleFmt(MeanTextBox.Text);
        }

        private void StdTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // return true if initializing
                StdTextBox.Text = CStringUtil.SzEnforceDoubleFmt(StdTextBox.Text);
        }

        private void CoeffGTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // return true if initializing
                CoeffGTextBox.Text = CStringUtil.SzEnforceDoubleFmt(CoeffGTextBox.Text);
        }

        private void MaxTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // return true if initializing
                MaxTextBox.Text = CStringUtil.SzEnforceDoubleFmt(MaxTextBox.Text);
        }

        private void MinTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // return true if initializing
                MinTextBox.Text = CStringUtil.SzEnforceDoubleFmt(MinTextBox.Text);
        }

        private void CoeffRTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // return true if initializing
                CoeffRTextBox.Text = CStringUtil.SzEnforceDoubleFmt(CoeffRTextBox.Text);
        }

        private void RateTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // return true if initializing
                RateVectorTextBox.Text = CStringUtil.SzForceIntoVectorDoubleFormat(RateVectorTextBox.Text);
        }

        private void UpdateVectorModel()
        {
            VECTORDIMENSIONS vd = CStringUtil.SzVectorGetDimensions(RateVectorTextBox.Text);
            Boolean vectorFormatCorrect = CStringUtil.SzVectorVerifyDoubleFormat(RateVectorTextBox.Text);

            //RateVectorTextBox
            if(vectorFormatCorrect == true)
            {
                VectorLabel.Text = m_szDescription + "rate,  [" + vd.rowCnt + "x" + vd.colCnt +"]";

                // Error Format Label
                FrmErrLabel.Text = "";
                FrmErrLabel.BackColor = System.Drawing.SystemColors.Control;
            }
            else
            {
               
                VectorLabel.Text = m_szDescription + "rate,  [1x????]";

                // Error Format Label
                FrmErrLabel.Text = "Input Warning";
                FrmErrLabel.BackColor = System.Drawing.Color.Tomato;
            }

            // Maximum Travel Rate Calculation
            if(vectorFormatCorrect == true && StepTextBox.Text.Length > 0)
                MaxVctrTextBox.Text = "" + ((vd.colCnt-1) * Convert.ToDouble(StepTextBox.Text));
            else
                MaxVctrTextBox.Text = "...";

        }


        private void RateVectorTextBox_TextChanged(object sender, EventArgs e)
        {
            if(m_refreshing == true)
                return;

            if(UpdateModifiedStatus() == true) // return true if initializing
                return;
            OkButton.Enabled = false;
            RateRefreshButton.Enabled = true;
            UpdateVectorModel();
        }

        private void StepTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == true) // return true if initializing
                return;
            StepTextBox.Text = CStringUtil.SzEnforceDoubleFmt(StepTextBox.Text);
            UpdateVectorModel();
        }


        private void TermCoeffTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // return true if initializing
                TermCoeffTextBox.Text = CStringUtil.SzEnforceDoubleFmt(TermCoeffTextBox.Text);
        }

        private void RateRefreshButton_Click(object sender, EventArgs e)
        {
            CVector vector = new CVector();
            OkButton.Enabled = true;
            RateRefreshButton.Enabled = false;

            vector.a = CStringUtil.SzVectorToDoubleArray(RateVectorTextBox.Text);
            m_refreshing = true;
            RateVectorTextBox.Text = vector.ConvertToString();
            m_refreshing = false;
            UpdateVectorModel();
        }

        private void OkButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                m_r.gauss.mean = CStringUtil.SzToDouble(MeanTextBox.Text);
                m_r.gauss.std = CStringUtil.SzToDouble(StdTextBox.Text);
                m_r.gauss.coeff = CStringUtil.SzToDouble(CoeffGTextBox.Text);
                m_r.randm.max = CStringUtil.SzToDouble(MaxTextBox.Text);
                m_r.randm.min = CStringUtil.SzToDouble(MinTextBox.Text);
                m_r.randm.coeff = CStringUtil.SzToDouble(CoeffRTextBox.Text);
                m_r.vectorMdl.vector.a = CStringUtil.SzVectorToDoubleArray(RateVectorTextBox.Text);
                m_r.vectorMdl.step.a = CStringUtil.SzToDouble(StepTextBox.Text);
                m_r.vectorMdl.termination.a = CStringUtil.SzToDouble(TermCoeffTextBox.Text);
            }
            Dispose();
        }

        private void CancelButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                FormConfirmDecision dlg = new FormConfirmDecision();
                dlg.Text = "Confirm Cancel";
                dlg.messageString = "CAbandon changes to rate model?";
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