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
    public partial class FormReversal: Form
    {
        CReversalParameters m_r;
        private Boolean m_modified = false;
        private Boolean m_initializing = true;
        private string m_szTitle = "3MB Dive Reversal Description - Biomimetica";
        private Boolean m_refreshing = false;


        public Boolean modified { get { return m_modified; } }

        public FormReversal(CReversalParameters P)
        {
            InitializeComponent();

            m_r = P;
            this.Text = m_szTitle;

            MeanCntTextBox.Text = "" + P.gauss.meanCnt;
            StdCntTextBox.Text = "" + P.gauss.stdCnt;
            GaussProbTextBox.Text = "" + P.gauss.prob;
            GaussMeanTimeTextBox.Text = "" + P.gauss.meanTime;
            GaussStdTimeTextBox.Text = "" + P.gauss.stdTime;

            MaxCntTextBox.Text = "" + P.randm.maxCnt;
            MinCntTextBox.Text = "" + P.randm.minCnt;
            RandProbTextBox.Text = "" + P.randm.prob;
            RndMeanTimeTextBox.Text = "" + P.randm.meanTime;
            RndStdTimeTextBox.Text = "" + P.randm.stdTime;

            if(P.reversalDiveRateType == REVERSAL_DIVE_RATE_TYPE.INDEPENDENT)
            {
                DescentAscentLabel.Text = "Dive Rate";
                DescentAscentLabel.Enabled = true;

                MeanDescentAscentRateTextBox.Enabled = true;
                StdDescentAscentRateTextBox.Enabled = true;
                CoeffDescentAscentRateTextBox.Enabled = true;

                MeanDescentAscentLabel.Enabled = true;
                StdDescentAscentLabel.Enabled = true;
                CoefficientDescentAscentGaussLabel.Enabled = true;

                AscentLabel.Text = "---";
                AscentLabel.Enabled = false;
                MeanAscentRateTextBox.Enabled = false;
                StdAscentRateTextBox.Enabled = false;
                CoeffAscentRateTextBox.Enabled = false;

                MeanAscentLabel.Enabled = false;
                StdAscentLabel.Enabled = false;
                CoefficientAscentGaussLabel.Enabled = false;

            }
            else if(P.reversalDiveRateType == REVERSAL_DIVE_RATE_TYPE.INDEPENDENT_DIVE_AND_ASCENT)
            {
                DescentAscentLabel.Text = "Descent Rate";
                DescentAscentLabel.Enabled = true;

                MeanDescentAscentRateTextBox.Enabled = true;
                StdDescentAscentRateTextBox.Enabled = true;
                CoeffDescentAscentRateTextBox.Enabled = true;

                MeanDescentAscentLabel.Enabled = true;
                StdDescentAscentLabel.Enabled = true;
                CoefficientDescentAscentGaussLabel.Enabled = true;


                AscentLabel.Text = "Ascent Rate";
                AscentLabel.Enabled = true;
                MeanAscentRateTextBox.Enabled = true;
                StdAscentRateTextBox.Enabled = true;
                CoeffAscentRateTextBox.Enabled = true;

                MeanAscentLabel.Enabled = true;
                StdAscentLabel.Enabled = true;
                CoefficientAscentGaussLabel.Enabled = true;
            }
            else
            {
                DescentAscentLabel.Text = "Dive Rate";
                DescentAscentLabel.Enabled = false;

                MeanDescentAscentRateTextBox.Enabled = false;
                StdDescentAscentRateTextBox.Enabled = false;
                CoeffDescentAscentRateTextBox.Enabled = false;

                MeanDescentAscentLabel.Enabled = false;
                StdDescentAscentLabel.Enabled = false;
                CoefficientDescentAscentGaussLabel.Enabled = false;


                AscentLabel.Text = "---";
                AscentLabel.Enabled = false;

                MeanAscentRateTextBox.Enabled = false;
                StdAscentRateTextBox.Enabled = false;
                CoeffAscentRateTextBox.Enabled = false;

                MeanAscentLabel.Enabled = false;
                StdAscentLabel.Enabled = false;
                CoefficientAscentGaussLabel.Enabled = false;

            }

            MeanDescentAscentRateTextBox.Text = "" + P.diveRate.mean;
            StdDescentAscentRateTextBox.Text = "" + P.diveRate.std;
            CoeffDescentAscentRateTextBox.Text = "" + P.diveRate.coeff;

            MeanAscentRateTextBox.Text = "" + P.ascentRate.mean;
            StdAscentRateTextBox.Text = "" + P.ascentRate.std;
            CoeffAscentRateTextBox.Text = "" + P.ascentRate.coeff;


            ProbTextBox.Text = P.vector.probabilityElement.ConvertToString();
            CountVectorTextBox.Text = P.vector.countVector.ConvertToString();
            DurTextBox.Text = P.vector.durationVector.ConvertToString();
            StepDurTextBox.Text = P.vector.durationStepElement.ConvertToString();
            UpdateVectorModel();


            groupBox2.Enabled = false; // Gaussian
            groupBox1.Enabled = false; // Random
            groupBox3.Enabled = false; // Vector

            if(P.type == MODELTYPE.GAUSSIAN)
                groupBox2.Enabled = true;
            else if(P.type == MODELTYPE.RANDOM)
                groupBox1.Enabled = true;
            else if(P.type == MODELTYPE.MATRIX)
                groupBox3.Enabled = true;


            RefreshButton.Enabled = false;
            OkButton.Enabled = true;
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
            if(UpdateModifiedStatus() == false) // returns true if initializing
                MeanCntTextBox.Text = CStringUtil.SzEnforceDoubleFmt(MeanCntTextBox.Text);
        }

        private void StdCntTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                StdCntTextBox.Text = CStringUtil.SzEnforceDoubleFmt(StdCntTextBox.Text);
        }

        private void GaussProbTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                GaussProbTextBox.Text = CStringUtil.SzEnforceDoubleFmt(GaussProbTextBox.Text);
        }

        private void GaussMeanTimeTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                GaussMeanTimeTextBox.Text = CStringUtil.SzEnforceDoubleFmt(GaussMeanTimeTextBox.Text);
        }

        private void GaussStdTimeTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                GaussStdTimeTextBox.Text = CStringUtil.SzEnforceDoubleFmt(GaussStdTimeTextBox.Text);
        }


        private void MaxCntTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                MaxCntTextBox.Text = CStringUtil.SzEnforceIntFmt(MaxCntTextBox.Text);
        }


        private void MinCntTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                MinCntTextBox.Text = CStringUtil.SzEnforceIntFmt(MinCntTextBox.Text);
        }

        private void RandProbTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                RandProbTextBox.Text = CStringUtil.SzEnforceDoubleFmt(RandProbTextBox.Text);
        }

        private void RndMeanTimeTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                RndMeanTimeTextBox.Text = CStringUtil.SzEnforceDoubleFmt(RndMeanTimeTextBox.Text);
        }

        private void RndStdTimeTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                RndStdTimeTextBox.Text = CStringUtil.SzEnforceDoubleFmt(RndStdTimeTextBox.Text);
        }

        private void MeanDiveRateTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                MeanDescentAscentRateTextBox.Text = CStringUtil.SzEnforceDoubleFmt(MeanDescentAscentRateTextBox.Text);
        }

        private void StdDiveRateTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                StdDescentAscentRateTextBox.Text = CStringUtil.SzEnforceDoubleFmt(StdDescentAscentRateTextBox.Text);
        }

        private void CoeffDiveRateTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                CoeffDescentAscentRateTextBox.Text = CStringUtil.SzEnforceDoubleFmt(CoeffDescentAscentRateTextBox.Text);
        }

        private void MeanAscentRateTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                CoeffAscentRateTextBox.Text = CStringUtil.SzEnforceDoubleFmt(CoeffAscentRateTextBox.Text);
        }

        private void StdAscentRateTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                CoeffAscentRateTextBox.Text = CStringUtil.SzEnforceDoubleFmt(CoeffAscentRateTextBox.Text);
        }

        private void CoeffAscentRateTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                CoeffAscentRateTextBox.Text = CStringUtil.SzEnforceDoubleFmt(CoeffAscentRateTextBox.Text);
        }

        private void UpdateVectorModel()
        {
            VECTORDIMENSIONS vd = CStringUtil.SzVectorGetDimensions(CountVectorTextBox.Text);
            Boolean vectorFormatCorrect = CStringUtil.SzVectorVerifyDoubleFormat(CountVectorTextBox.Text);

            /*
             * ProbTextBox
             * VectorCountLabel
             * FrmErrCntLabel
             * CountVectorTextBox
             * CntRefreshButton
             * MaxVctrCntTextBox
             * 
             * VectorDurLabel
             * FrmErrDurLabel
             * DurTextBox
             * DurRefreshButton
             * StepDurTextBox
             * MaxVctrDurTextBox
             */

            // Reversal Counts
            if(vectorFormatCorrect == true)
            {
                VectorCountLabel.Text = "Reversal Count [" + vd.rowCnt + "x" + vd.colCnt +"]";

                // Error Format Label
                FrmErrCntLabel.Text = "";
                FrmErrCntLabel.BackColor = System.Drawing.SystemColors.Control;
            }
            else
            {
                VectorCountLabel.Text = "Reversal Count [1x????]";

                // Error Format Label
                FrmErrCntLabel.Text = "Input Warning";
                FrmErrCntLabel.BackColor = System.Drawing.Color.Tomato;
            }

            // Maximum Reversal Counts Calculation
            if(vectorFormatCorrect == true)
                MaxVctrCntTextBox.Text = "" + (vd.colCnt-1) * 1;
            else
                MaxVctrCntTextBox.Text = "...";


            /*
             * VectorDurLabel
             * FrmErrDurLabel
             * DurTextBox
             * DurRefreshButton
             * StepDurTextBox
             * MaxVctrDurTextBox
             */

            vd = CStringUtil.SzVectorGetDimensions(DurTextBox.Text);
            vectorFormatCorrect = CStringUtil.SzVectorVerifyDoubleFormat(DurTextBox.Text);

            // Reversal Duration
            if(vectorFormatCorrect == true)
            {
                VectorDurLabel.Text = "Reversal Duration [" + vd.rowCnt + "x" + vd.colCnt +"]";

                // Error Format Label
                FrmErrDurLabel.Text = "";
                FrmErrDurLabel.BackColor = System.Drawing.SystemColors.Control;
            }
            else
            {
                VectorDurLabel.Text = "Reversal Duration [1x????]";

                // Error Format Label
                FrmErrDurLabel.Text = "Input Warning";
                FrmErrDurLabel.BackColor = System.Drawing.Color.Tomato;
            }

            // Maximum Reversal Counts Calculation
            if(vectorFormatCorrect == true && StepDurTextBox.Text.Length != 0)
                MaxVctrDurTextBox.Text = "" + ((vd.colCnt-1) * Convert.ToDouble(StepDurTextBox.Text));
            else
                MaxVctrDurTextBox.Text = "...";
        }


        private void ProbTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                ProbTextBox.Text = CStringUtil.SzEnforceDoubleFmt(ProbTextBox.Text);
        }

        private void CountVectorTextBox_TextChanged(object sender, EventArgs e)
        {
            if(m_refreshing == true)
                return;

            UpdateVectorModel();

            if(UpdateModifiedStatus() == true) // returns true if initializing
                return;

            RefreshButton.Enabled = true;
            OkButton.Enabled = false;

        }

        private void DurTextBox_TextChanged(object sender, EventArgs e)
        {
            if(m_refreshing == true)
                return;

            UpdateVectorModel();
            if(UpdateModifiedStatus() == true) // returns true if initializing
                return;
            RefreshButton.Enabled = true;
            OkButton.Enabled = false;
        }

        private void StepDurTextBox_TextChanged(object sender, EventArgs e)
        {
            if(m_refreshing == true)
                return;

            UpdateVectorModel();
            if(UpdateModifiedStatus() == true) // returns true if initializing
                return;
            StepDurTextBox.Text = CStringUtil.SzEnforceDoubleFmt(StepDurTextBox.Text);
        }

        private void DurRefreshButton_Click(object sender, EventArgs e)
        {
            CVector v = new CVector();
            RefreshButton.Enabled = false;
            OkButton.Enabled = true;

            v.a = CStringUtil.SzVectorToDoubleArray(DurTextBox.Text);
            m_refreshing = true;
            DurTextBox.Text = v.ConvertToString();
            m_refreshing = false;


            v.a = CStringUtil.SzVectorToDoubleArray(CountVectorTextBox.Text);
            m_refreshing = true;
            CountVectorTextBox.Text = v.ConvertToString();
            m_refreshing = false;

            UpdateVectorModel();
        }

        private void OkButton_Click(object sender, EventArgs e)
        {
            m_r.gauss.meanCnt = CStringUtil.SzToDouble(MeanCntTextBox.Text);
            m_r.gauss.stdCnt = CStringUtil.SzToDouble(StdCntTextBox.Text);
            m_r.gauss.prob = CStringUtil.SzToDouble(GaussProbTextBox.Text);
            m_r.gauss.meanTime = CStringUtil.SzToDouble(GaussMeanTimeTextBox.Text);
            m_r.gauss.stdTime = CStringUtil.SzToDouble(GaussStdTimeTextBox.Text);

            m_r.randm.maxCnt = CStringUtil.SzToIntOrMin0(MaxCntTextBox.Text);
            m_r.randm.minCnt = CStringUtil.SzToIntOrMin0(MinCntTextBox.Text);
            m_r.randm.prob = CStringUtil.SzToDouble(RandProbTextBox.Text);
            m_r.randm.meanTime = CStringUtil.SzToDouble(RndMeanTimeTextBox.Text);
            m_r.randm.stdTime = CStringUtil.SzToDouble(RndStdTimeTextBox.Text);

            m_r.diveRate.mean = CStringUtil.SzToDouble(MeanDescentAscentRateTextBox.Text);
            m_r.diveRate.std = CStringUtil.SzToDouble(StdDescentAscentRateTextBox.Text);
            m_r.diveRate.coeff = CStringUtil.SzToDouble(CoeffDescentAscentRateTextBox.Text);

            m_r.ascentRate.mean = CStringUtil.SzToDouble(MeanAscentRateTextBox.Text);
            m_r.ascentRate.std = CStringUtil.SzToDouble(StdAscentRateTextBox.Text);
            m_r.ascentRate.coeff = CStringUtil.SzToDouble(CoeffAscentRateTextBox.Text);

            m_r.vector.countVector.a = CStringUtil.SzVectorToDoubleArray(CountVectorTextBox.Text);
            m_r.vector.probabilityElement.a = CStringUtil.SzToDouble(ProbTextBox.Text);
            m_r.vector.durationVector.a = CStringUtil.SzVectorToDoubleArray(DurTextBox.Text);
            m_r.vector.durationStepElement.a = CStringUtil.SzToDouble(StepDurTextBox.Text);

            Dispose();
        }

        private void CancelButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                FormConfirmDecision dlg = new FormConfirmDecision();
                dlg.Text = "Confirm Cancel";
                dlg.messageString = "Abandon Changes To Reversal Model?";
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