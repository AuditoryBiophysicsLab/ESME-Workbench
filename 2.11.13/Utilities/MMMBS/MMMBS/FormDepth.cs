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
    public partial class FormDepth: Form
    {
        CDepthParameters m_r;
        private Boolean m_modified = false;
        public Boolean modified { get { return m_modified; } }
        Boolean m_initializing = true;
        private string m_stTitle = "3MB Dive Depth Description - Biomimetica";


        public FormDepth(CDepthParameters P)
        {
            InitializeComponent();
            this.Text = m_stTitle;

            m_r = P;

            MeanTextBox.Text = "" + P.gauss.mean;
            StdTextBox.Text = "" + P.gauss.std;
            MaxTextBox.Text = "" + P.randm.max;

            VectorTextBox.Text = P.vectorMdl.vector.ConvertToString();
            StepTextBox.Text = "" + P.vectorMdl.step.ConvertToString();

            GaussGroupBox.Enabled = false;
            RndGroupBox.Enabled = false;
            VectorGroupBox.Enabled = false;
            if(P.type == MODELTYPE.GAUSSIAN)
                GaussGroupBox.Enabled = true;
            else if(P.type == MODELTYPE.RANDOM)
                RndGroupBox.Enabled = true;
            else if(P.type == MODELTYPE.MATRIX)
                VectorGroupBox.Enabled = true;

            RefreshButton.Enabled = false;
            OkButton.Enabled = true;
            m_initializing = false;
        }

        // Returns true if initializing
        private Boolean UpdateModifiedStatus()
        {
            if(m_initializing == true)
                return true;

            this.Text = m_stTitle + "*";
            m_modified = true;

            return false;
        }


        private void MeanTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                MeanTextBox.Text = CStringUtil.SzEnforceDoubleFmt(MeanTextBox.Text);
        }

        private void StdTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                StdTextBox.Text = CStringUtil.SzEnforceDoubleFmt(StdTextBox.Text);
        }

        private void MaxTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns true if initializing
                MaxTextBox.Text = CStringUtil.SzEnforceDoubleFmt(MaxTextBox.Text);
        }

        private void UpdateVectorModel()
        {
            VECTORDIMENSIONS vd = CStringUtil.SzVectorGetDimensions(VectorTextBox.Text);
            Boolean vectorFormatCorrect = CStringUtil.SzVectorVerifyDoubleFormat(VectorTextBox.Text);

            if(vectorFormatCorrect == true)
            {
                VectorLabel.Text = "Depth Vector,  [" + vd.rowCnt + "x" + vd.colCnt +"]";

                // Error Format Label
                FrmErrLabel.Text = "";
                FrmErrLabel.BackColor = System.Drawing.SystemColors.Control;
            }
            else
            {
                VectorLabel.Text = "Depth Vector,  [1x????]";

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


        private void VectorTextBox_TextChanged(object sender, EventArgs e)
        {
            UpdateVectorModel();
            if(UpdateModifiedStatus() == true) // returns true if initializing
                return;
            RefreshButton.Enabled = true;
            OkButton.Enabled = false;
        }

        private void StepTextBox_TextChanged(object sender, EventArgs e)
        {
            UpdateVectorModel();
            if(UpdateModifiedStatus() == true) // returns true if initializing
                return;
            RefreshButton.Enabled = false;
            OkButton.Enabled = true;
            
        }

        private void RefreshButton_Click(object sender, EventArgs e)
        {
            CVector vector = new CVector();
            RefreshButton.Enabled = false;
            OkButton.Enabled = true;

            m_initializing = true;
            vector.a = CStringUtil.SzVectorToDoubleArray(VectorTextBox.Text);
            VectorTextBox.Text = vector.ConvertToString();
            m_initializing = false;
            UpdateVectorModel();
        }

        private void OkButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                m_r.gauss.mean = CStringUtil.SzToDouble(MeanTextBox.Text);
                m_r.gauss.std = CStringUtil.SzToDouble(StdTextBox.Text);
                m_r.randm.max = CStringUtil.SzToDouble(MaxTextBox.Text);

                m_r.vectorMdl.vector.a = CStringUtil.SzVectorToDoubleArray(VectorTextBox.Text);
                m_r.vectorMdl.step.a = CStringUtil.SzToDouble(StepTextBox.Text);
            }
            Dispose();
        }
        private void CancelButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                FormConfirmDecision dlg = new FormConfirmDecision();
                dlg.Text = "Confirm Cancel";
                dlg.messageString = "Abandon changes to depth model?";
                dlg.button1String = "Confirm Cancel";
                dlg.button2String = "Do Not Cancel";
                dlg.SetLocation(OkButton);
                dlg.ShowDialog(this);
                if(dlg.buttonSelected == 2)
                    return;
                m_modified = false;
            }
            Dispose();
        }
    }
}