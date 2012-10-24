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
    public partial class FormTravel: Form
    {
        CDirectionalParameters m_r;
        private Boolean m_modified = false;
        public Boolean modified { get { return m_modified; } }
        private Boolean m_initializing = true;
        private string m_stTitle = "3MB Travel Direction Description - Biomimetica";

        public FormTravel(CDirectionalParameters P)
        {
            InitializeComponent();
            this.Text = m_stTitle;
            m_r = P;
            rwCoeffTextBox.Text = "" + P.rndmWalk.coeff;

            crwPertTextBox.Text = "" + P.correlatedRndmWalk.pert;
            crwCoeffTextBox.Text = "" + P.correlatedRndmWalk.coeff;

            crwdbArcStepTextBox.Text = "" + P.correlatedRndmWalkDirBias.arcStep;
            crwdbBiasTextBox.Text = "" + P.correlatedRndmWalkDirBias.bias;
            crwdbCoeffTextBox.Text = "" + P.correlatedRndmWalkDirBias.coeff;
            crwdbDirOfBiasTextBox.Text = "" + P.correlatedRndmWalkDirBias.biasDir;
            crwdbPertTextBox.Text = "" + P.correlatedRndmWalkDirBias.pert;

            ProbTurnVectorTextBox.Text = m_r.matrix.directionVector.ConvertToString();
            BiasTextBox.Text = m_r.matrix.directionBiasMatrix.ConvertToStringA();
            TermCoeffTextBox.Text = m_r.matrix.term.ConvertToString();
            UpdateProbTurnVectorModeling();

            OkButton.Enabled = true;
            RefreshButton.Enabled = false;

            RndWlkGroupBox.Enabled = CrRndWlkGroupBox.Enabled = CrRndWlkDbGroupBox.Enabled = VectorGroupBox.Enabled = false;

            if(m_r.type == DIRECTIONMODELTYPE.RANDOM_WALK)
                RndWlkGroupBox.Enabled = true;
            else if(m_r.type == DIRECTIONMODELTYPE.CORR_RAND_WALK)
                CrRndWlkGroupBox.Enabled = true;
            else if(m_r.type == DIRECTIONMODELTYPE.CORR_RAND_WALK_DIR_BIAS)
                CrRndWlkDbGroupBox.Enabled = true;
            else if(m_r.type == DIRECTIONMODELTYPE.VECTOR)
            {
                ProbTurnVectorTextBox.Enabled = true;
                BiasTextBox.Enabled = false;
                VectorGroupBox.Enabled = true;
            }
            else if(m_r.type == DIRECTIONMODELTYPE.VECTOR_DIRBIAS)
            {
                ProbTurnVectorTextBox.Enabled = true;
                BiasTextBox.Enabled = true;
                VectorGroupBox.Enabled = true;
            }

            UdateBiasMatrixModeling();
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

        private void rwCoeffTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                rwCoeffTextBox.Text = CStringUtil.SzEnforceDoubleFmt(rwCoeffTextBox.Text);
        }

        private void crwPertTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                crwPertTextBox.Text = CStringUtil.SzEnforceDoubleFmt(crwPertTextBox.Text);
        }

        private void crwCoeffTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                crwCoeffTextBox.Text = CStringUtil.SzEnforceDoubleFmt(crwCoeffTextBox.Text);
        }

        private void crwdbPertTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                crwdbPertTextBox.Text = CStringUtil.SzEnforceDoubleFmt(crwdbPertTextBox.Text);
        }

        private void crwdbDirOfBiasTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false)
                crwdbDirOfBiasTextBox.Text = CStringUtil.SzEnforceDoubleFmt(crwdbDirOfBiasTextBox.Text);
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
            if(UpdateModifiedStatus() == false) // returns initializing status
                crwdbCoeffTextBox.Text = CStringUtil.SzEnforceDoubleFmt(crwdbCoeffTextBox.Text);
        }


        private void UdateBiasMatrixModeling()
        {
            MATRIXDIMENSIONS md = CStringUtil.SzMatrixDimensions(BiasTextBox.Text);
            BiasLabel.Text = "Probability of Turning Bias Matrix, [" + md.rowCnt + "x" + md.maxCols + "]";
            VECTORDIMENSIONS vd = CStringUtil.SzVectorGetDimensions(ProbTurnVectorTextBox.Text);

            // Check for proper characters
            if(CStringUtil.SzVerifyMatrixFormatDouble(BiasTextBox.Text) == true)
            {
                BiasFormatErrorLabel.Text = "";
                BiasFormatErrorLabel.BackColor = System.Drawing.SystemColors.Control;
            }
            else
            {
                BiasFormatErrorLabel.Text = "Input Warning";
                BiasFormatErrorLabel.BackColor = System.Drawing.Color.Tomato;
            }
        }

        private void UpdateProbTurnVectorModeling()
        {
            VECTORDIMENSIONS vd = CStringUtil.SzVectorGetDimensions(ProbTurnVectorTextBox.Text);
            ProbOfTurningLabel.Text = "Probability of Turning,  [" + vd.rowCnt + "x" + vd.colCnt +"]";

            if(CStringUtil.SzVectorVerifyDoubleFormat(ProbTurnVectorTextBox.Text) == true)
            {
                ProbTurningFrmErrLabel.Text = "";
                ProbTurningFrmErrLabel.BackColor = System.Drawing.SystemColors.Control;
            }
            else
            {
                ProbTurningFrmErrLabel.Text = "Input Warning";
                ProbTurningFrmErrLabel.BackColor = System.Drawing.Color.Tomato;
            }
        }

        private void ProbTurnVectorTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == true) // returns initializing status
                return; // return if initializing 
            RefreshButton.Enabled = true;
            OkButton.Enabled = false;
            UpdateProbTurnVectorModeling();
        }


        private void BiasTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == true) // returns true if initializing
                return; // return if initializing 
            RefreshButton.Enabled = true;
            OkButton.Enabled = false;
        }

        private void TermCoeffTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == false) // returns initializing status
                TermCoeffTextBox.Text = CStringUtil.SzEnforceDoubleFmt(TermCoeffTextBox.Text);
        }


        private void RefreshButton_Click(object sender, EventArgs e)
        {
            CVector v = new CVector();
            CMatrix m = new CMatrix();
            int ccnt;
            string szTemp;

            szTemp = CStringUtil.SzForceSquareMatrixFormatDouble(BiasTextBox.Text);
            m.a = CStringUtil.SzMatrixToDouble2DArray(szTemp);
            BiasTextBox.Text = m.ConvertToStringA();
            UdateBiasMatrixModeling();

            // Make matrix m into a square matix
            m.a = CStringUtil.SzMatrixToDouble2DArray(BiasTextBox.Text);

            // Determin minimum nuber of columns
            ccnt = m.ColumnCount;
            if(ccnt < m.RowCount)
                ccnt = m.RowCount;
            v.a = CStringUtil.SzVectorToDoubleArray(ProbTurnVectorTextBox.Text);
            if(m_r.type == DIRECTIONMODELTYPE.VECTOR_DIRBIAS && v.columnCount > ccnt)
                ccnt = v.columnCount;

            while(m.RowCount > ccnt)
                m.DeleteRow(m.RowCount-1);
            while(m.RowCount < ccnt)
                m.AddRow();
            while(m.ColumnCount > ccnt)
                m.DeleteColumn(m.ColumnCount-1);
            while(m.ColumnCount < ccnt)
                m.AddColumn();
            BiasTextBox.Text = m.ConvertToStringA();
            UdateBiasMatrixModeling();



            while(v.columnCount > ccnt)
                v.DeleteColumn(v.columnCount-1);
            while(v.columnCount < ccnt)
                v.AddColumn();
            ProbTurnVectorTextBox.Text = v.ConvertToString();
            UpdateProbTurnVectorModeling();


            RefreshButton.Enabled = false;
            OkButton.Enabled = true;
        }

        private void OkButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                m_r.rndmWalk.coeff = CStringUtil.SzToDouble(rwCoeffTextBox.Text);

                m_r.correlatedRndmWalk.pert = CStringUtil.SzToDouble(crwPertTextBox.Text);
                m_r.correlatedRndmWalk.coeff = CStringUtil.SzToDouble(crwCoeffTextBox.Text);

                m_r.correlatedRndmWalkDirBias.pert = CStringUtil.SzToDouble(crwdbPertTextBox.Text);
                m_r.correlatedRndmWalkDirBias.biasDir = CStringUtil.SzToDouble(crwdbDirOfBiasTextBox.Text);
                m_r.correlatedRndmWalkDirBias.bias = CStringUtil.SzToDouble(crwdbBiasTextBox.Text);
                m_r.correlatedRndmWalkDirBias.arcStep = CStringUtil.SzToDouble(crwdbArcStepTextBox.Text);
                m_r.correlatedRndmWalkDirBias.coeff = CStringUtil.SzToDouble(crwdbCoeffTextBox.Text);

                m_r.matrix.directionVector.a = CStringUtil.SzVectorToDoubleArray(ProbTurnVectorTextBox.Text);
                m_r.matrix.directionBiasMatrix.a = CStringUtil.SzMatrixToDouble2DArray(BiasTextBox.Text);
                m_r.matrix.term.a = CStringUtil.SzToDouble(TermCoeffTextBox.Text);
            }
            Dispose();
        }

        private void CancelButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                FormConfirmDecision dlg = new FormConfirmDecision();
                dlg.Text = "Confirm Cancel";
                dlg.messageString = "Abandon changes to travel model?";
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