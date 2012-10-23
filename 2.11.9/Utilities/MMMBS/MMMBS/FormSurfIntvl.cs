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
    public partial class FormSurfIntvl : Form
    {
        CSurfaceIntervalParams m_r;
        private Boolean m_modified = false;
        private Boolean m_initializing = true;
        private string m_stTitle = "3MB Surface Interval Description - Biomimetica";
        private Boolean m_refreshing = false;
        
        public Boolean modified { get { return m_modified; } }

        public FormSurfIntvl(CSurfaceIntervalParams P)
        {
            InitializeComponent();

            m_r = P;
            this.Text = m_stTitle;

            MeanTextBox.Text = "" + P.gauss.mean;
            StdTextBox.Text = "" + P.gauss.std;

            VectorTextBox.Text = P.vectorMdl.vector.ConvertToString();
            StepTextBox.Text = P.vectorMdl.step.ConvertToString();

            RefreshButton.Enabled = false;
            OkButton.Enabled = true;

            GaussGroupBox.Enabled = VectorGroupBox.Enabled = false;

            if(m_r.type == SRFINVMODELTYPE.GAUSSIAN)
                GaussGroupBox.Enabled = true;
            else if(m_r.type == SRFINVMODELTYPE.MATRIX)
                VectorGroupBox.Enabled = true;

            UpdateVectorModel();
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
            if(UpdateModifiedStatus() == false) // Returns true if initializing
                MeanTextBox.Text = CStringUtil.SzEnforceDoubleFmt(MeanTextBox.Text);
        }



        private void UpdateVectorModel()
        {
            VECTORDIMENSIONS vd = CStringUtil.SzVectorGetDimensions(VectorTextBox.Text);
            Boolean vectorFormatCorrect = CStringUtil.SzVectorVerifyDoubleFormat(VectorTextBox.Text);

            if(vectorFormatCorrect == true)
            {
                VectorLabel.Text = "SurfIntrvl Vector,  [" + vd.rowCnt + "x" + vd.colCnt +"]";

                // Error Format Label
                FrmErrLabel.Text = "";
                FrmErrLabel.BackColor = System.Drawing.SystemColors.Control;
            }
            else
            {
                VectorLabel.Text = "SurfIntrvl Vector,  [1x????]";

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


        private void StdTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // Returns true if initializing
                StdTextBox.Text = CStringUtil.SzEnforceDoubleFmt(StdTextBox.Text);
        }


        private void VectorTextBox_TextChanged(object sender, EventArgs e)
        {
            if(m_refreshing == true)
                return;

            UpdateVectorModel();
            if(UpdateModifiedStatus() == true) // Returns true if initializing
                return;
            RefreshButton.Enabled = true;
            OkButton.Enabled = false;
        }

        private void StepTextBox_TextChanged(object sender, EventArgs e)
        {
            UpdateVectorModel();
            if(UpdateModifiedStatus() == false) // Returns true if initializing
                StepTextBox.Text = CStringUtil.SzEnforceDoubleFmt(StepTextBox.Text);
        }

        private void RefreshButton_Click(object sender, EventArgs e)
        {
            CVector vector = new CVector();
            RefreshButton.Enabled = false;
            OkButton.Enabled = true;

            vector.a = CStringUtil.SzVectorToDoubleArray(VectorTextBox.Text);
            m_refreshing = true;
            VectorTextBox.Text = vector.ConvertToString();
            m_refreshing = false;
            UpdateVectorModel();
        }

        private void OkButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                m_r.gauss.mean = CStringUtil.SzToDouble(MeanTextBox.Text);
                m_r.gauss.std = CStringUtil.SzToDouble(StdTextBox.Text);

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
                dlg.messageString = "Cancel Changes To Surface Interval Model?";
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