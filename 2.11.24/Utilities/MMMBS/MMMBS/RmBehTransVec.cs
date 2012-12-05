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
    public partial class RmBehTransVec: Form
    {
        //CBehaviorTransitionModel m_p;
        static int NUM_NRML_BUTTONS = 8;

        string m_szTitle;
        int m_behaviorCnt;
        TextBox[] m_vectorTextBoxArray = new TextBox[NUM_NRML_BUTTONS];
        Label[] m_behNameLabelArray = new Label[NUM_NRML_BUTTONS];
        TextBox[] m_T50TextBoxArray = new TextBox[NUM_NRML_BUTTONS];
        TextBox[] m_slopeTextBoxArray = new TextBox[NUM_NRML_BUTTONS];
        C3mbSpeciesModel m_spe;
        CBehavior m_beh;
        //CBehaviorTransitionModel m_transModel;
        private TRANSITIONALMODELTYPE m_transModelType;
        int m_behIndex;
        private Boolean m_initializing;
        //private Boolean m_refreshing;
        private Boolean m_modified = false;
        public Boolean modified { get { return m_modified; } }


        public RmBehTransVec(TRANSITIONALMODELTYPE TransType, C3mbSpeciesModel Species, CBehavior Behavior, int BehaviorIndex, string szTitle)
        {
            int i;

            InitializeComponent();
            m_initializing = true;
            m_spe = Species;
            m_beh = Behavior;
            m_behIndex = BehaviorIndex;
            m_szTitle = szTitle;
            m_behaviorCnt = Species.BehaviorCount;
            m_transModelType = TransType;

            CBehavior beh;
            CEnvAttrBehTrans transModel;

            this.Text = szTitle;

            m_vectorTextBoxArray[0] = VectorTextBox00;
            m_vectorTextBoxArray[1] = VectorTextBox01;
            m_vectorTextBoxArray[2] = VectorTextBox02;
            m_vectorTextBoxArray[3] = VectorTextBox03;
            m_vectorTextBoxArray[4] = VectorTextBox04;
            m_vectorTextBoxArray[5] = VectorTextBox05;
            m_vectorTextBoxArray[6] = VectorTextBox06;
            m_vectorTextBoxArray[7] = VectorTextBox07;

            m_behNameLabelArray[0] = NameLabel00;
            m_behNameLabelArray[1] = NameLabel01;
            m_behNameLabelArray[2] = NameLabel02;
            m_behNameLabelArray[3] = NameLabel03;
            m_behNameLabelArray[4] = NameLabel04;
            m_behNameLabelArray[5] = NameLabel05;
            m_behNameLabelArray[6] = NameLabel06;
            m_behNameLabelArray[7] = NameLabel07;

            m_T50TextBoxArray[0] = T50TextBox00;
            m_T50TextBoxArray[1] = T50TextBox01;
            m_T50TextBoxArray[2] = T50TextBox02;
            m_T50TextBoxArray[3] = T50TextBox03;
            m_T50TextBoxArray[4] = T50TextBox04;
            m_T50TextBoxArray[5] = T50TextBox05;
            m_T50TextBoxArray[6] = T50TextBox06;
            m_T50TextBoxArray[7] = T50TextBox07;

            m_slopeTextBoxArray[0] = SlopeTextBox00;
            m_slopeTextBoxArray[1] = SlopeTextBox01;
            m_slopeTextBoxArray[2] = SlopeTextBox02;
            m_slopeTextBoxArray[3] = SlopeTextBox03;
            m_slopeTextBoxArray[4] = SlopeTextBox04;
            m_slopeTextBoxArray[5] = SlopeTextBox05;
            m_slopeTextBoxArray[6] = SlopeTextBox06;
            m_slopeTextBoxArray[7] = SlopeTextBox07;

            //m_refreshing = true;
            for(i=0; i<m_spe.BehaviorCount && i<NUM_NRML_BUTTONS; i++)
            {
                if(i == m_behIndex)
                    beh = m_beh;
                else
                    beh = m_spe.GetBehaviorCopy(i);

                switch(m_transModelType)
                {
                //case TRANSITIONALMODELTYPE.BEHAVIORAL:
                  //  transModel = beh.nrmlBehTrans;
                   // break;
                case TRANSITIONALMODELTYPE.DEPTHENV:
                    transModel = beh.depthBehTrans;
                    break;
                case TRANSITIONALMODELTYPE.TEMPERATUREENV:
                    transModel = beh.temperatureBehTrans;
                    break;
                default:
                    transModel = beh.depthBehTrans;
//                    transModel = beh.nrmlBehTrans;
                    break;
                }

                m_behNameLabelArray[i].Text = beh.name;
                m_vectorTextBoxArray[i].Text = transModel.vector.ConvertToString();
                m_T50TextBoxArray[i].Text = "" + transModel.meanTimeInBehMinuites;
                m_slopeTextBoxArray[i].Text = "" + transModel.slopeCoeff_goesAway;
            }


            for(; i<NUM_NRML_BUTTONS; i++)
            {
                m_behNameLabelArray[i].Text = "";
                m_behNameLabelArray[i].Enabled = false;

                m_vectorTextBoxArray[i].Text = "";
                m_vectorTextBoxArray[i].Enabled = false;

                m_T50TextBoxArray[i].Text = "";
                m_T50TextBoxArray[i].Enabled = false;

                m_slopeTextBoxArray[i].Text = "";
                m_slopeTextBoxArray[i].Enabled = false;
            }


            DoneButton.Enabled = true;
            RefreshButton.Enabled = false;
            UpdateVectorModel();
            m_initializing = false;
            //m_refreshing = false;
        }

        private void UpdateVectorModel()
        {
            Boolean valid = CStringUtil.SzVectorVerifyDoubleFormat(VectorTextBox00.Text);

            if(valid)
            {
                FrmErrLabel.Text = "";
                FrmErrLabel.BackColor = System.Drawing.SystemColors.Control;
            }
            else
            {
                FrmErrLabel.Text = "Bad Input";
                FrmErrLabel.BackColor = System.Drawing.Color.Tomato;
            }
        }

        private void VectorTextBox00_TextChanged(object sender, EventArgs e) { VectorTextBoxChanged(); }
        private void VectorTextBox01_TextChanged(object sender, EventArgs e) { VectorTextBoxChanged(); }
        private void VectorTextBox02_TextChanged(object sender, EventArgs e) { VectorTextBoxChanged(); }
        private void VectorTextBox03_TextChanged(object sender, EventArgs e) { VectorTextBoxChanged(); }
        private void VectorTextBox04_TextChanged(object sender, EventArgs e) { VectorTextBoxChanged(); }
        private void VectorTextBox05_TextChanged(object sender, EventArgs e) { VectorTextBoxChanged(); }
        private void VectorTextBox06_TextChanged(object sender, EventArgs e) { VectorTextBoxChanged(); }
        private void VectorTextBox07_TextChanged(object sender, EventArgs e) { VectorTextBoxChanged(); }
        private void VectorTextBox08_TextChanged(object sender, EventArgs e) { VectorTextBoxChanged(); }
        private void VectorTextBox09_TextChanged(object sender, EventArgs e) { VectorTextBoxChanged(); }
        private void VectorTextBox10_TextChanged(object sender, EventArgs e) { VectorTextBoxChanged(); }
        private void VectorTextBox11_TextChanged(object sender, EventArgs e) { VectorTextBoxChanged(); }
        private void VectorTextBox12_TextChanged(object sender, EventArgs e) { VectorTextBoxChanged(); }
        private void VectorTextBox13_TextChanged(object sender, EventArgs e) { VectorTextBoxChanged(); }
        private void VectorTextBox14_TextChanged(object sender, EventArgs e) { VectorTextBoxChanged(); }


        private void VectorTextBoxChanged()
        {
            if(m_initializing == true)
                return;
            m_modified = true;
            DoneButton.Enabled = false;
            RefreshButton.Enabled = true;
            UpdateVectorModel();
        }

        private void RefreshButton_Click(object sender, EventArgs e)
        {
            int i;
            CVector v = new CVector();
            string sz;

            for(i=0; i<m_spe.BehaviorCount && i<NUM_NRML_BUTTONS; i++)
            {
                sz = CStringUtil.SzForceIntoVectorDoubleFormat(m_vectorTextBoxArray[i].Text);
                v.a = CStringUtil.SzVectorToDoubleArray(sz);
                while(v.columnCount > m_behaviorCnt+1)
                    v.DeleteColumn(v.columnCount-1);
                while(v.columnCount < m_behaviorCnt+1)
                    v.AddColumn();

                m_vectorTextBoxArray[i].Text = v.ConvertToString();
            }
            DoneButton.Enabled = true;
            RefreshButton.Enabled = false;
            UpdateVectorModel();
        }

        private void TerminateTextBox00_TextChanged(object sender, EventArgs e) { T50BoxTextChanged(0); }
        private void TerminateTextBox01_TextChanged(object sender, EventArgs e) { T50BoxTextChanged(1); }
        private void TerminateTextBox02_TextChanged(object sender, EventArgs e) { T50BoxTextChanged(2); }
        private void TerminateTextBox03_TextChanged(object sender, EventArgs e) { T50BoxTextChanged(3); }
        private void TerminateTextBox04_TextChanged(object sender, EventArgs e) { T50BoxTextChanged(4); }
        private void TerminateTextBox05_TextChanged(object sender, EventArgs e) { T50BoxTextChanged(5); }
        private void TerminateTextBox06_TextChanged(object sender, EventArgs e) { T50BoxTextChanged(6); }
        private void TerminateTextBox07_TextChanged(object sender, EventArgs e) { T50BoxTextChanged(7); }
        private void TerminateTextBox08_TextChanged(object sender, EventArgs e) { T50BoxTextChanged(8); }
        private void TerminateTextBox09_TextChanged(object sender, EventArgs e) { T50BoxTextChanged(9); }
        private void TerminateTextBox10_TextChanged(object sender, EventArgs e) { T50BoxTextChanged(10); }
        private void TerminateTextBox11_TextChanged(object sender, EventArgs e) { T50BoxTextChanged(11); }
        private void TerminateTextBox12_TextChanged(object sender, EventArgs e) { T50BoxTextChanged(12); }
        private void TerminateTextBox13_TextChanged(object sender, EventArgs e) { T50BoxTextChanged(13); }
        private void TerminateTextBox14_TextChanged(object sender, EventArgs e) { T50BoxTextChanged(14); }

        private void T50BoxTextChanged(int Index)
        {
            if(m_initializing == true)
                return;
            m_modified = true;
            m_T50TextBoxArray[Index].Text = CStringUtil.SzEnforceDoubleFmt(m_T50TextBoxArray[Index].Text);
        }

        private void SlopeTextBox00_TextChanged(object sender, EventArgs e) { SlopeBoxTextChanged(0); }
        private void SlopeTextBox01_TextChanged(object sender, EventArgs e) { SlopeBoxTextChanged(1); }
        private void SlopeTextBox02_TextChanged(object sender, EventArgs e) { SlopeBoxTextChanged(2); }
        private void SlopeTextBox03_TextChanged(object sender, EventArgs e) { SlopeBoxTextChanged(3); }
        private void SlopeTextBox04_TextChanged(object sender, EventArgs e) { SlopeBoxTextChanged(4); }
        private void SlopeTextBox05_TextChanged(object sender, EventArgs e) { SlopeBoxTextChanged(5); }
        private void SlopeTextBox06_TextChanged(object sender, EventArgs e) { SlopeBoxTextChanged(6); }
        private void SlopeTextBox07_TextChanged(object sender, EventArgs e) { SlopeBoxTextChanged(7); }
        private void SlopeTextBox08_TextChanged(object sender, EventArgs e) { SlopeBoxTextChanged(8); }
        private void SlopeTextBox09_TextChanged(object sender, EventArgs e) { SlopeBoxTextChanged(9); }
        private void SlopeTextBox10_TextChanged(object sender, EventArgs e) { SlopeBoxTextChanged(10); }
        private void SlopeTextBox11_TextChanged(object sender, EventArgs e) { SlopeBoxTextChanged(11); }
        private void SlopeTextBox12_TextChanged(object sender, EventArgs e) { SlopeBoxTextChanged(12); }
        private void SlopeTextBox13_TextChanged(object sender, EventArgs e) { SlopeBoxTextChanged(13); }
        private void SlopeTextBox14_TextChanged(object sender, EventArgs e) { SlopeBoxTextChanged(14); }
        private void SlopeBoxTextChanged(int Index)
        {
            if(m_initializing == true)
                return;
            m_modified = true;
            m_slopeTextBoxArray[Index].Text = CStringUtil.SzEnforceDoubleFmt(m_slopeTextBoxArray[Index].Text);
        }

        private void DoneButton_Click(object sender, EventArgs e)
        {
            int i;
            CBehavior beh;
            CEnvAttrBehTrans p;

            if(m_modified == true)
            {
                FormConfirmDecision dlg = new FormConfirmDecision();
                dlg.Text = "Confirm Changes";
                dlg.messageString = "Confirming changes updates all behaviors immediately.";
                dlg.button2String = "Confirm Changes And Done";
                dlg.button1String = "Continue Editing";
                dlg.button3String = "Abandon Updates And Done";
                dlg.button3Visible = true;
                dlg.SetLocation(DoneButton);
                dlg.ShowDialog(this);
                this.BringToFront();
                if(dlg.buttonSelected == 1)
                {
                    return;
                }
                if(dlg.buttonSelected == 3)
                {
                    Dispose();
                    return;
                }

                m_modified = false;

                for(i=0; i<m_spe.BehaviorCount && i<NUM_NRML_BUTTONS; i++)
                {
                    if(i == m_behIndex)
                        beh = m_beh;
                    else
                        beh = m_spe.GetBehaviorCopy(i);

                    switch(m_transModelType)
                    {
                   // case TRANSITIONALMODELTYPE.BEHAVIORAL:
                     //   p = beh.nrmlBehTrans;
                       // break;
                    case TRANSITIONALMODELTYPE.DEPTHENV:
                        p = beh.depthBehTrans;
                        break;
                    case TRANSITIONALMODELTYPE.TEMPERATUREENV:
                        p = beh.temperatureBehTrans;
                        break;
                    default:
                        p = beh.depthBehTrans;
//                        p = beh.nrmlBehTrans;
                        break;
                    }

                    p.vector.a = CStringUtil.SzVectorToDoubleArray(m_vectorTextBoxArray[i].Text);
                    //p.element.a = CStringUtil.SzToDouble(m_T50TextBoxArray[i].Text);
                    p.meanTimeInBehMinuites = CStringUtil.SzToDouble(m_T50TextBoxArray[i].Text);
                    p.slopeCoeff_goesAway = CStringUtil.SzToDouble(m_slopeTextBoxArray[i].Text);
                    m_spe.SetBehavior(i, beh);
                }
            }
            Dispose();
        }


        private void CancelButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                FormConfirmDecision dlg = new FormConfirmDecision();
                dlg.Text = "Confirm Cancel";
                dlg.messageString = "Cancel Changes To Behavior Transition Model?";
                dlg.button1String = "Confirm Cancel";
                dlg.button2String = "Do Not Cancel";
                dlg.SetLocation(DoneButton);
                dlg.ShowDialog(this);

                if(dlg.buttonSelected == 2)
                    return;
                m_modified = false;
            }
            Dispose();
        }
     }
}