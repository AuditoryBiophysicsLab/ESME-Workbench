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
    public partial class FormNormalModelSelect: Form
    {
        static int NUM_NRML_BUTTONS = 15;
        int m_myIndex;
        int m_selectIndex;
        RESLT m_res;
        C3mbSpeciesModel m_speMdl;
        Button[] m_nrmlAddEditButton = new Button[NUM_NRML_BUTTONS];
        Label[] m_nrmlLabel = new Label[NUM_NRML_BUTTONS];


        public FormNormalModelSelect(C3mbSpeciesModel Species, int ThisIndex)
        {
            InitializeComponent();
            m_speMdl = Species;
            m_res = RESLT.CANCEL;
            m_myIndex = ThisIndex;
            CopyNormalButtons();
            UpdateNormalBehaviorStates();
        }
        private void NmlBehButton00_Click(object sender, EventArgs e) { SetSelectedBehavior(0); }
        private void NmlBehButton01_Click(object sender, EventArgs e) { SetSelectedBehavior(1); }
        private void NmlBehButton02_Click(object sender, EventArgs e) { SetSelectedBehavior(2); }
        private void NmlBehButton03_Click(object sender, EventArgs e) { SetSelectedBehavior(3); }
        private void NmlBehButton04_Click(object sender, EventArgs e) { SetSelectedBehavior(4); }
        private void NmlBehButton05_Click(object sender, EventArgs e) { SetSelectedBehavior(5); }
        private void NmlBehButton06_Click(object sender, EventArgs e) { SetSelectedBehavior(6); }
        private void NmlBehButton07_Click(object sender, EventArgs e) { SetSelectedBehavior(7); }
        private void NmlBehButton08_Click(object sender, EventArgs e) { SetSelectedBehavior(8); }
        private void NmlBehButton09_Click(object sender, EventArgs e) { SetSelectedBehavior(9); }
        private void NmlBehButton10_Click(object sender, EventArgs e) { SetSelectedBehavior(10); }
        private void NmlBehButton11_Click(object sender, EventArgs e) { SetSelectedBehavior(11); }
        private void NmlBehButton12_Click(object sender, EventArgs e) { SetSelectedBehavior(12); }
        private void NmlBehButton13_Click(object sender, EventArgs e) { SetSelectedBehavior(13); }
        private void NmlBehButton14_Click(object sender, EventArgs e) { SetSelectedBehavior(14); }


        private void CancelButton_Click(object sender, EventArgs e)
        {
            m_res = RESLT.CANCEL;
            Dispose();
        }

        private void CopyNormalButtons()
        {

            // To make accessing easier...
            m_nrmlAddEditButton[0] = NmlBehButton00;
            m_nrmlAddEditButton[1] = NmlBehButton01;
            m_nrmlAddEditButton[2] = NmlBehButton02;
            m_nrmlAddEditButton[3] = NmlBehButton03;
            m_nrmlAddEditButton[4] = NmlBehButton04;
            m_nrmlAddEditButton[5] = NmlBehButton05;
            m_nrmlAddEditButton[6] = NmlBehButton06;
            m_nrmlAddEditButton[7] = NmlBehButton07;
            m_nrmlAddEditButton[8] = NmlBehButton08;
            m_nrmlAddEditButton[9] = NmlBehButton09;
            m_nrmlAddEditButton[10] = NmlBehButton10;
            m_nrmlAddEditButton[11] = NmlBehButton11;
            m_nrmlAddEditButton[12] = NmlBehButton12;
            m_nrmlAddEditButton[13] = NmlBehButton13;
            m_nrmlAddEditButton[14] = NmlBehButton14;

            m_nrmlLabel[0] = NmlBehLabel00;
            m_nrmlLabel[1] = NmlBehLabel01;
            m_nrmlLabel[2] = NmlBehLabel02;
            m_nrmlLabel[3] = NmlBehLabel03;
            m_nrmlLabel[4] = NmlBehLabel04;
            m_nrmlLabel[5] = NmlBehLabel05;
            m_nrmlLabel[6] = NmlBehLabel06;
            m_nrmlLabel[7] = NmlBehLabel07;
            m_nrmlLabel[8] = NmlBehLabel08;
            m_nrmlLabel[9] = NmlBehLabel09;
            m_nrmlLabel[10] = NmlBehLabel10;
            m_nrmlLabel[11] = NmlBehLabel11;
            m_nrmlLabel[12] = NmlBehLabel12;
            m_nrmlLabel[13] = NmlBehLabel13;
            m_nrmlLabel[14] = NmlBehLabel14;
        }


        private void UpdateNormalBehaviorStates()
        {
            int i;
            CBehavior mdl;
            // Set the state of normal behavior buttons that have a normal behavior
            // associated with them.
            for(i=0; i<NUM_NRML_BUTTONS; i++)
            {
                if(i == m_myIndex)
                {
                    // Unpopulated Slots
                    m_nrmlAddEditButton[i].Enabled = false;
                    m_nrmlAddEditButton[i].BackColor = System.Drawing.SystemColors.GradientInactiveCaption;

                    mdl = m_speMdl.GetBehaviorCopy(i);
                    m_nrmlLabel[i].Text = mdl.name;
                    m_nrmlLabel[i].Font =
                        new System.Drawing.Font("Microsoft Sans Serif", 8.25F,
                        System.Drawing.FontStyle.Regular,
                        System.Drawing.GraphicsUnit.Point, ((byte)(0)));
                    m_nrmlLabel[i].Enabled = false;
                }
                else if(i<m_speMdl.BehaviorCount)
                {
                    // Populated slots
                    m_nrmlAddEditButton[i].Enabled = true;
                    m_nrmlAddEditButton[i].BackColor = System.Drawing.SystemColors.Control;

                    mdl = m_speMdl.GetBehaviorCopy(i);
                    m_nrmlLabel[i].Text = mdl.name;
                    m_nrmlLabel[i].Font =
                        new System.Drawing.Font("Microsoft Sans Serif", 8.25F,
                        System.Drawing.FontStyle.Regular,
                        System.Drawing.GraphicsUnit.Point, ((byte)(0)));
                    m_nrmlLabel[i].Enabled = true;

                }
                else
                {
                    // Unpopulated Slots
                    m_nrmlAddEditButton[i].Enabled = false;
                    m_nrmlAddEditButton[i].BackColor = System.Drawing.SystemColors.GradientInactiveCaption;

                    m_nrmlLabel[i].Text = "empty...";
                    m_nrmlLabel[i].Font =
                        new System.Drawing.Font("Microsoft Sans Serif", 8.25F,
                        System.Drawing.FontStyle.Italic,
                        System.Drawing.GraphicsUnit.Point, ((byte)(0)));
                    m_nrmlLabel[i].Enabled = false;
                }
            }

            // The first normal add/edit behavior button is always enabled as well as
            // 1+ the number of behaviors (so a new behavior may be added).
            //m_nrmlAddEditButton[0].Enabled = true;
           // m_nrmlAddEditButton[0].BackColor = System.Drawing.SystemColors.Control;

        }

        private void SetSelectedBehavior(int Index)
        {
            m_selectIndex = Index;
            m_res = RESLT.OK;
            Dispose();
        }
        public RESLT result { get { return m_res; } }
        public int selectedIndex { get { return m_selectIndex; } }
    }
}