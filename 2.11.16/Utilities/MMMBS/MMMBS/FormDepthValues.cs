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
    public partial class FormDepthValues : Form
    {
        //-----------------//
        // Member Variables
        //-----------------//
        C3mbSpeciesModel m_speMdl;
        double m_shoreFollowingValue;
        double m_shallowSeedingLimit;
        Boolean m_deepSeedingLimitEnabled;
        double m_deepSeedingLimitValue;
        Boolean m_limitsBad;

        private Boolean m_modified = false;
        public Boolean modified { get { return m_modified; } }

        public FormDepthValues(C3mbSpeciesModel SpeMdl)
        {
            InitializeComponent();
            m_speMdl = SpeMdl;
            Text = "Species Depth Values";
            m_shoreFollowingValue = m_speMdl.shoreFollowDepth;
            m_shallowSeedingLimit = m_speMdl.seedMinDepth;
            m_deepSeedingLimitEnabled = m_speMdl.seedingDepthLimitEnabled;
            m_deepSeedingLimitValue = m_speMdl.seedingDepthLimit;
            
            ShoreFollowingTextBox.Text = "" + m_shoreFollowingValue;
            ShallowSeedingTextBox.Text = "" + m_shallowSeedingLimit;
            DeepSeedingTextBox.Text = "" + m_deepSeedingLimitValue;
            DeepSeedingTextBox.Enabled = m_deepSeedingLimitEnabled;
            DeepSeedingDisabledLabel.Visible = !m_deepSeedingLimitEnabled;

            m_limitsBad = false;
            EnforceLimitRules();
        }

        private void EnforceLimitRules()
        {
            m_limitsBad = false;
            WarningLabel.Text = "";
            ShoreFollowingViolationLabel.Text = "";
            ShallowSeedingViolationLabel.Text = "";
            DeepSeedingViolationLabel.Text = "";

            string szShoreFollow = "Bathymetry Shore Following Value (" + m_shoreFollowingValue + " m)";
            string szShallowSeeding = "Seeding Depth - Mimimum (" + m_shallowSeedingLimit + " m)";
            string szDeepSeeding = "Seeding Depth - Maximum (" + m_deepSeedingLimitValue +" m)";
            string szDeeperThan = " must be deeper than ";

            if(m_shoreFollowingValue >= MBSDEFAULTS.BEACHING_DEPTH)
            {
                m_limitsBad = true;
                WarningLabel.Text = szShoreFollow +
                    " must be deeper than beaching depth (" + MBSDEFAULTS.BEACHING_DEPTH + " m)";
                ShoreFollowingViolationLabel.Text = "! ";
                OkButton.Enabled = !m_limitsBad;
                return;
            }

            if(m_shoreFollowingValue < m_shallowSeedingLimit)
            {
                m_limitsBad = true;
                WarningLabel.Text = szShallowSeeding + szDeeperThan + szShoreFollow + "\n";
                ShoreFollowingViolationLabel.Text = "! ";
                ShallowSeedingViolationLabel.Text = "! ";
            }

            OkButton.Enabled = !m_limitsBad;
        
#if FALSE
            m_limitsBad = false;
            WarningLabel.Text = "";
            ShoreFollowingViolationLabel.Text = "";
            ShallowSeedingViolationLabel.Text = "";
            DeepSeedingViolationLabel.Text = "";

            string szShoreFollow = "Shore Following Depth (" + m_shoreFollowingValue + " m)";
            string szShallowSeeding = "Shallow Seeding Depth Limit (" + m_shallowSeedingLimit + " m)";
            string szDeepSeeding = "Deep Seeding Depth Limit (" + m_deepSeedingLimitValue +" m)";
            string szDeeperThan = " must be deeper than ";

            if(m_shoreFollowingValue >= MBSDEFAULTS.BEACHING_DEPTH)
            {
                m_limitsBad = true;
                WarningLabel.Text = szShoreFollow +
                    " must be deeper than beaching depth (" + MBSDEFAULTS.BEACHING_DEPTH + " m)";
                ShoreFollowingViolationLabel.Text = "! ";
                OkButton.Enabled = !m_limitsBad;
                return;
            }

            if(m_shoreFollowingValue < m_shallowSeedingLimit)
            {
                m_limitsBad = true;
                WarningLabel.Text = szShallowSeeding + szDeeperThan + szShoreFollow + "\n";
                ShoreFollowingViolationLabel.Text = "! ";
                ShallowSeedingViolationLabel.Text = "! ";
            }
            if(m_deepSeedingLimitEnabled == true && m_shoreFollowingValue < m_deepSeedingLimitValue)
            {
                m_limitsBad = true;
                WarningLabel.Text = WarningLabel.Text + szDeepSeeding + szDeeperThan + szShoreFollow + "\n";
                ShoreFollowingViolationLabel.Text = ShoreFollowingViolationLabel.Text + "!";
                DeepSeedingViolationLabel.Text = "! ";
            }
            if(m_deepSeedingLimitEnabled == true && m_shallowSeedingLimit < m_deepSeedingLimitValue)
            {
                m_limitsBad = true;
                WarningLabel.Text = WarningLabel.Text + szDeepSeeding + szDeeperThan + szShallowSeeding + "\n";
                ShallowSeedingViolationLabel.Text = ShallowSeedingViolationLabel.Text + "!";
                DeepSeedingViolationLabel.Text = DeepSeedingViolationLabel.Text + "!";
            }

            OkButton.Enabled = !m_limitsBad;
#endif
        }
        private void ShoreFollowingTextBox_TextChanged(object sender, EventArgs e)
        {
            m_modified = true;
            ShoreFollowingTextBox.Text = CStringUtil.SzEnforceDoubleFmt(ShoreFollowingTextBox.Text);
            m_shoreFollowingValue = CStringUtil.SzToDouble(ShoreFollowingTextBox.Text);
            EnforceLimitRules();
        }

        private void ShallowSeedingTextBox_TextChanged(object sender, EventArgs e)
        {
            m_modified = true;
            ShallowSeedingTextBox.Text = CStringUtil.SzEnforceDoubleFmt(ShallowSeedingTextBox.Text);
            m_shallowSeedingLimit = CStringUtil.SzToDouble(ShallowSeedingTextBox.Text);
            EnforceLimitRules();
        }

        private void DeepSeedingTextBox_TextChanged(object sender, EventArgs e)
        {
            m_modified = true;
            DeepSeedingTextBox.Text = CStringUtil.SzEnforceDoubleFmt(DeepSeedingTextBox.Text);
            m_deepSeedingLimitValue = CStringUtil.SzToDouble(DeepSeedingTextBox.Text);
            //EnforceLimitRules();
        }

        private void DeepSeedingEnableButton_Click(object sender, EventArgs e)
        {
            m_modified = true;
            m_deepSeedingLimitEnabled = !m_deepSeedingLimitEnabled;
            DeepSeedingTextBox.Enabled = m_deepSeedingLimitEnabled;
            DeepSeedingDisabledLabel.Visible = !m_deepSeedingLimitEnabled;
            //EnforceLimitRules();
        }

        private void CancelC_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                FormConfirmDecision dlg = new FormConfirmDecision();
                dlg.Text = "Confirm Cancel";
                dlg.messageString = "Cancel Changes To Rate Model?";
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

        private void OkButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                m_speMdl.shoreFollowDepth = m_shoreFollowingValue;
                m_speMdl.seedMinDepth = m_shallowSeedingLimit;
                m_speMdl.seedingDepthLimitEnabled = m_deepSeedingLimitEnabled;
                m_speMdl.seedingDepthLimit = m_deepSeedingLimitValue;
            }
            Dispose();
        }
    }
}