using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using MMMBSLib; // c# code code and data types.


namespace MBSGUI
{
    public partial class FormAcstcAvrsn: Form
    {
        CAcousticAversion m_aa;
        double m_beachingDepth;
        Boolean m_podBreaksOnAE;
        C3mbSpeciesModel m_speMdl;
        Boolean m_initializing;
        Boolean m_modified;
        string m_szTitle = "3MB Acoustic Aversion Behavior Model - Biomimetica";
        string m_behName = "Acoustic Aversion";

        public Boolean Modified { get { return m_modified; } }

        public FormAcstcAvrsn(C3mbSpeciesModel SpeciesMdl)
        {
            m_initializing = true;
            m_speMdl = SpeciesMdl;

            // Added.
            m_beachingDepth = m_speMdl.beachingDepth;
            m_podBreaksOnAE = m_speMdl.podBreaksOnAE;


            m_aa = SpeciesMdl.AcousticAversion.GetCopy();
            InitializeComponent();
            UpdateEntireForm();
            this.Text = m_szTitle;
            AcousticAversionLabel.Text = m_behName;
            m_initializing = false;
        }

        // Returns true if initializing
        private Boolean UpdateModifiedStatus()
        {
            if(m_initializing == true)
                return true;

            this.Text = m_szTitle + "*";
            AcousticAversionLabel.Text = m_behName + "*";
            m_modified = true;

            return false;
        }


        private void UpdateEntireForm()
        {
            //ActvnTheshBTextBox.Text = "" + m_aa.actThreshB;
            //ActvnTheshATextBox.Text = "" + m_aa.actThreshA;
            //Deact1TextBox.Text = "" + m_aa.deactThreshB;
            //Deact2TextBox.Text = "" + m_aa.deactThreshA;

            BeachDepthTextBox.Text = "" + m_beachingDepth;
            UpdateBeaching();
            UpdatePodBreakUp();
            UpdateDirection();
            UpdateTravelRate();
            UpdateAscentRate();
            UpdateDescentRate();
            UpdateDepth();
            UpdateBottomFollowing();
            UpdateReversals();
            UpdateSurfInterval();
            UpdateExposureUnitsBButtons();
            UpdateExposureUnitsAButtons();
            UpdateDecayFncEditButton();
            UpdateLevelBTypeButton();
            UpdateDoseBSelectButton();
         }

        private void UpdateBeaching()
        {
            if(m_aa.beachingEnabled == true)
            {
                BeachingEnableButton.Text = "Yes";
                BeachDepthTextBox.Text = "" + m_beachingDepth;
                BeachDepthTextBox.Enabled = true;
            }
            else
            {
                BeachingEnableButton.Text = "No";
                BeachDepthTextBox.Text = "---";
                BeachDepthTextBox.Enabled = false;
            }
        }

        private void UpdatePodBreakUp()
        {
            if(m_podBreaksOnAE == true)
                StimPodBreakupButton.Text = "Yes";
            else
                StimPodBreakupButton.Text = "No";
        }

        public void UpdateDirection()
        {
            if(m_aa.directnEnabled == true)
            {
                DirectionSelectButton.Text = m_aa.direction.szType;
                DirectionDescriptionButton.Text = "pert: " + m_aa.direction.pert + "\nbias:" 
                    + m_aa.direction.bias +
                    "\narc step: " + m_aa.direction.arcStep + "\nterm coeff: " + m_aa.direction.coeff;

                DirectionDescriptionButton.Enabled = true;
            }
            else
            {
                DirectionSelectButton.Text = "Disabled";
                DirectionDescriptionButton.Text = "No influence";
                DirectionDescriptionButton.Enabled = false;
            }
            UpdateBottomFollowing();
        }

        public void UpdateTravelRate()
        {
            if(m_aa.rateEnabled == true)
            {
                TravelRateSelectButton.Text = m_aa.rate.szType;
                TravelRateDescriptionButton.Text = m_aa.rate.sz;
                TravelRateDescriptionButton.Enabled = true;
            }
            else
            {
                TravelRateSelectButton.Text = "Disabled";
                TravelRateDescriptionButton.Text = "No influence";
                TravelRateDescriptionButton.Enabled = false;
            }
            UpdateBottomFollowing();
        }

        public void UpdateAscentRate()
        {
            if(m_aa.ascentEnabled == true)
            {
                AscentSelectButton.Text = m_aa.ascent.szType;
                AscentDescriptionButton.Text = m_aa.ascent.sz;
                AscentDescriptionButton.Enabled = true;
            }
            else
            {
                AscentSelectButton.Text = "Disabled";
                AscentDescriptionButton.Text = "No influence";
                AscentDescriptionButton.Enabled = false;
            }
            UpdateBottomFollowing();
        }

        public void UpdateDescentRate()
        {
            if(m_aa.descentEnabled == true)
            {
                DescentSelectButton.Text = m_aa.descent.szType;
                DescentDescriptionButton.Text = m_aa.descent.sz;
                DescentDescriptionButton.Enabled = true;
            }
            else
            {
                DescentSelectButton.Text = "Disabled";
                DescentDescriptionButton.Text = "No influence";
                DescentDescriptionButton.Enabled = false;
            }
            UpdateBottomFollowing();
        }

        public void UpdateDepth()
        {
            if(m_aa.depthEnabled == true)
            {
                DepthSelectButton.Text = m_aa.depth.szType;
                DepthDescriptionButton.Text = m_aa.depth.sz;
                DepthDescriptionButton.Enabled = true;
            }
            else
            {
                DepthSelectButton.Text = "Disabled";
                DepthDescriptionButton.Text = "No influence";
                DepthDescriptionButton.Enabled = false;
            }
            UpdateBottomFollowing();
        }


        public void UpdateBottomFollowing()
        {
            Boolean anyAe = false;

            anyAe = m_aa.ascentEnabled || m_aa.descentEnabled || m_aa.depthEnabled || m_aa.directnEnabled || 
                m_aa.rateEnabled || m_aa.reverseEnabled || m_aa.srfIntvlEnabled;

            if(anyAe == true)
                m_aa.flatBottomDivingEnabled = true;
            else
                m_aa.flatBottomDivingEnabled = false;

            if(m_aa.flatBottomDivingEnabled == true)
            {
                BttmFollowSelectButton.Text = "Disabled";
                BttmFollowDescriptionButton.Enabled = true;

                if(m_aa.flatBottomDives == true)
                    BttmFollowDescriptionButton.Text = "Flat bottom diving allowed";
                else
                    BttmFollowDescriptionButton.Text = "Flat bottom diving not allowed";

            }
            else
            {
                BttmFollowSelectButton.Text = "Disabled";
                BttmFollowDescriptionButton.Text = "Flat bottom diving not influenced";
                BttmFollowDescriptionButton.Enabled = false;
            }
        }


        public void UpdateReversals()
        {
            if(m_aa.reverseEnabled == true)
            {
                ReversalsSelectButton.Text = m_aa.reversal.szType;
                ReversalsDescriptionButton.Text = m_aa.reversal.sz;
                ReversalsDescriptionButton.Enabled = true;
            }
            else
            {
                ReversalsSelectButton.Text = "Disabled";
                ReversalsDescriptionButton.Text = "No influence";
                ReversalsDescriptionButton.Enabled = false;
            }
            UpdateBottomFollowing();
        }


        public void UpdateSurfInterval()
        {
            if(m_aa.srfIntvlEnabled == true)
            {
                SrfIntrvlSelectButton.Text = m_aa.surfaceInterval.szType;
                SrfIntrvlDescriptionButton.Text = m_aa.surfaceInterval.sz;
                SrfIntrvlDescriptionButton.Enabled = true;
            }
            else
            {
                SrfIntrvlSelectButton.Text = "Disabled";
                SrfIntrvlDescriptionButton.Text = "No influence";
                SrfIntrvlDescriptionButton.Enabled = false;
            }
            UpdateBottomFollowing();
        }


        private void ActvnTheshTextBox_TextChanged(object sender, EventArgs e)
        {
            //ActvnTheshBTextBox.Text = CStringUtil.SzEnforceDoubleFmt(ActvnTheshBTextBox.Text);
            //m_aa.actThreshB = CStringUtil.SzToDouble(ActvnTheshBTextBox.Text);
        }


        private void ActvnTheshATextBox_TextChanged(object sender, EventArgs e)
        {
            //ActvnTheshATextBox.Text = CStringUtil.SzEnforceDoubleFmt(ActvnTheshATextBox.Text);
            //m_aa.actThreshA = CStringUtil.SzToDouble(ActvnTheshATextBox.Text);

        }

        private void BeachingEnableButton_Click(object sender, EventArgs e)
        {
            m_aa.beachingEnabled = !m_aa.beachingEnabled;
            UpdateModifiedStatus();
            UpdateBeaching();
        }

        private void StimPodBreakupButton_Click(object sender, EventArgs e)
        {
            m_podBreaksOnAE = !m_podBreaksOnAE;
            UpdateModifiedStatus();
            UpdatePodBreakUp();
        }

        private void DirectionSelectButton_Click(object sender, EventArgs e)
        {
            m_aa.directnEnabled = !m_aa.directnEnabled;
            UpdateModifiedStatus();
            UpdateDirection();
        }

        private void DirectionDescriptionButton_Click(object sender, EventArgs e)
        {
            FormTravelGauss dlg = new FormTravelGauss(m_aa.direction);
            dlg.ShowDialog(this);
            if(dlg.modified == true)
            {
                UpdateModifiedStatus();
                UpdateDirection();
            }
            this.BringToFront();
        }

        private void AscentSelectButton_Click(object sender, EventArgs e)
        {
            m_aa.ascentEnabled = !m_aa.ascentEnabled;
            UpdateModifiedStatus();
            UpdateAscentRate();
        }

        private void AscentDescriptionButton_Click(object sender, EventArgs e)
        {
            GAUSSPARAM gp;
            gp.mean = m_aa.ascent.mean;
            gp.std = m_aa.ascent.std;
            gp.termCoeff = m_aa.ascent.coeff;
            gp.termCoeffEnabled = true;
            FormGaussParams dlg = new FormGaussParams(gp);
            dlg.ShowDialog(this);
            if(dlg.modified == true)
            {
                UpdateModifiedStatus();
                m_aa.ascent.mean = dlg.res.mean;
                m_aa.ascent.std = dlg.res.std;
                m_aa.ascent.coeff = dlg.res.termCoeff;
                UpdateAscentRate();
            }
            this.BringToFront();
        }

        private void DescentSelectButton_Click(object sender, EventArgs e)
        {
            m_aa.descentEnabled = !m_aa.descentEnabled;
            UpdateModifiedStatus();
            UpdateDescentRate();
        }

        private void DescentDescriptionButton_Click(object sender, EventArgs e)
        {
            GAUSSPARAM gp;
            gp.mean = m_aa.descent.mean;
            gp.std = m_aa.descent.std;
            gp.termCoeff = m_aa.descent.coeff;
            gp.termCoeffEnabled = true;
            FormGaussParams dlg = new FormGaussParams(gp);
            dlg.ShowDialog(this);
            if(dlg.modified == true)
            {
                UpdateModifiedStatus();
                m_aa.descent.mean = dlg.res.mean;
                m_aa.descent.std = dlg.res.std;
                m_aa.descent.coeff = dlg.res.termCoeff;
                UpdateDescentRate();
            }
            this.BringToFront();
        }

        private void DepthSelectButton_Click(object sender, EventArgs e)
        {
            m_aa.depthEnabled = !m_aa.depthEnabled;
            UpdateModifiedStatus();
            UpdateDepth();
        }

        private void DepthDescriptionButton_Click(object sender, EventArgs e)
        {
            GAUSSPARAM gp;
            gp.mean = m_aa.depth.mean;
            gp.std = m_aa.depth.std;
            gp.termCoeff = 0;
            gp.termCoeffEnabled = false;

            FormGaussParams dlg = new FormGaussParams(gp);
            dlg.ShowDialog(this);
            if(dlg.modified == true)
            {
                UpdateModifiedStatus();
                m_aa.depth.mean = dlg.res.mean;
                m_aa.depth.std = dlg.res.std;
                UpdateDepth();
            }
            this.BringToFront();
        }

        private void TravelRateSelectButton_Click(object sender, EventArgs e)
        {
            m_aa.rateEnabled = !m_aa.rateEnabled;
            UpdateModifiedStatus();
            UpdateTravelRate();
        }

        private void TravelRateDescriptionButton_Click(object sender, EventArgs e)
        {
            GAUSSPARAM gp;
            gp.mean = m_aa.rate.mean;
            gp.std = m_aa.rate.std;
            gp.termCoeff = m_aa.rate.coeff;
            gp.termCoeffEnabled = true;
            FormGaussParams dlg = new FormGaussParams(gp);
            dlg.ShowDialog(this);
            if(dlg.modified == true)
            {
                UpdateModifiedStatus();
                m_aa.rate.mean = dlg.res.mean;
                m_aa.rate.std = dlg.res.std;
                m_aa.rate.coeff = dlg.res.termCoeff;
                UpdateTravelRate();
            }
            this.BringToFront();

        }

        private void BttmFollowSelectButton_Click(object sender, EventArgs e)
        {
            // Comment back in if user is to be allowed to set flat bottom diving policy.
            //m_aa.flatBottomDivingEnabled = !m_aa.flatBottomDivingEnabled;
            //UpdateBottomFollowing();
        }

        private void BttmFollowDescriptionButton_Click(object sender, EventArgs e)
        {
            // Work here if user is to be allowed to set flat bottom diving policy.

        }

        private void ReversalsSelectButton_Click(object sender, EventArgs e)
        {
            m_aa.reverseEnabled = !m_aa.reverseEnabled;
            UpdateModifiedStatus();
            UpdateReversals();
        }

        private void ReversalsDescriptionButton_Click(object sender, EventArgs e)
        {
            FormReversalGauss dlg = new FormReversalGauss(m_aa.reversal);
            dlg.ShowDialog(this);
            if(dlg.modified == true)
            {
                UpdateModifiedStatus();
                UpdateReversals();
            }
            this.BringToFront();
        }

        private void SrfIntrvlSelectButton_Click(object sender, EventArgs e)
        {
            m_aa.srfIntvlEnabled = !m_aa.srfIntvlEnabled;
            UpdateModifiedStatus();
            UpdateSurfInterval();
        }

        private void SrfIntrvlDescriptionButton_Click(object sender, EventArgs e)
        {
            GAUSSPARAM gp;
            gp.mean = m_aa.surfaceInterval.mean;
            gp.std = m_aa.surfaceInterval.std;
            gp.termCoeff = 0;
            gp.termCoeffEnabled = false;

            FormGaussParams dlg = new FormGaussParams(gp);
            dlg.ShowDialog(this);
            if(dlg.modified == true)
            {
                UpdateModifiedStatus();
                m_aa.surfaceInterval.mean = dlg.res.mean;
                m_aa.surfaceInterval.std = dlg.res.std;
                UpdateSurfInterval();
            }
            this.BringToFront();
        }


        private void UpdateExposureUnitsBButtons()
        {
#if false
            UpdateModifiedStatus();
            switch(m_aa.unitsB)
            {
            case SOUNDPRESSUREUNIT.MICRO_PA:
                ExposureUnitsBButton.Text = ExposureUnits3Button.Text = "" + (char)181 + "Pa";
                break;
            case SOUNDPRESSUREUNIT.MICRO_PA2S:
                ExposureUnitsBButton.Text = ExposureUnits3Button.Text = "" + (char)181 + "Pa^2s";
                break;
            }
#endif
        }

        private void UpdateExposureUnitsAButtons()
        {
#if false
            UpdateModifiedStatus();
            switch(m_aa.unitsA)
            {
            case SOUNDPRESSUREUNIT.MICRO_PA:
                ExposureUnitsAButton.Text = ExposureUnits4Button.Text = "" + (char)181 + "Pa";
                break;
            case SOUNDPRESSUREUNIT.MICRO_PA2S:
                ExposureUnitsAButton.Text = ExposureUnits4Button.Text = "" + (char)181 + "Pa^2s";
                break;
            }
#endif
        }

        private void ExposureUnitsBButton_Click(object sender, EventArgs e)
        {
#if false
            UpdateModifiedStatus();
            switch(m_aa.unitsB)
            {
            case SOUNDPRESSUREUNIT.MICRO_PA:
                m_aa.unitsB = SOUNDPRESSUREUNIT.MICRO_PA2S;
                break;
            case SOUNDPRESSUREUNIT.MICRO_PA2S:
                m_aa.unitsB = SOUNDPRESSUREUNIT.MICRO_PA;
                break;
            }
            UpdateExposureUnitsBButtons();
#endif
        }

        private void ExposureUnitsAButton_Click(object sender, EventArgs e)
        {
#if false
            UpdateModifiedStatus();
            switch(m_aa.unitsA)
            {
            case SOUNDPRESSUREUNIT.MICRO_PA:
                m_aa.unitsA = SOUNDPRESSUREUNIT.MICRO_PA2S;
                break;
            case SOUNDPRESSUREUNIT.MICRO_PA2S:
                m_aa.unitsA = SOUNDPRESSUREUNIT.MICRO_PA;
                break;
            }
            UpdateExposureUnitsAButtons();
#endif
        }

        private void UpdateDecayFncEditButton()
        {
#if false
            UpdateModifiedStatus();
            switch(m_aa.decayFncB.decayFunctionType)
            {
            case DECAYFUNCTIONS.DISABLED:
                DecayFncEditButton.Text = "Disabled";
                break;
            case DECAYFUNCTIONS.DECAYFNC1:
                DecayFncEditButton.Text = "Decay Fnc 1";
                break;
            case DECAYFUNCTIONS.DECAYFNC2:
                DecayFncEditButton.Text = "F(t) = 1/t";
                break;
            case DECAYFUNCTIONS.DECAYFNC3:
                DecayFncEditButton.Text = "a/(b*t)";
                break;
            }

            if(m_aa.decayFncB.decayFunctionType == DECAYFUNCTIONS.DISABLED)
                DecayFncEditButton.Enabled = false;
            else
                DecayFncEditButton.Enabled = true;
#endif
        }

        private void DecayFncSelButton_Click(object sender, EventArgs e)
        {
#if false
            UpdateModifiedStatus();
            switch(m_aa.decayFncB.decayFunctionType)
            {
            case DECAYFUNCTIONS.DISABLED:
                m_aa.decayFncB.decayFunctionType = DECAYFUNCTIONS.DECAYFNC1;
                break;
            case DECAYFUNCTIONS.DECAYFNC1:
                m_aa.decayFncB.decayFunctionType = DECAYFUNCTIONS.DECAYFNC2;
                break;
            case DECAYFUNCTIONS.DECAYFNC2:
                m_aa.decayFncB.decayFunctionType = DECAYFUNCTIONS.DECAYFNC3;
                break;
            case DECAYFUNCTIONS.DECAYFNC3:
                m_aa.decayFncB.decayFunctionType = DECAYFUNCTIONS.DISABLED;
                break;
            }
            UpdateDecayFncEditButton();
#endif
        }

        private void DecayFncEditButton_Click(object sender, EventArgs e)
        {
#if false
            UpdateModifiedStatus();
            FormAcstcAvrsnDecayFnc dlg = new FormAcstcAvrsnDecayFnc(m_aa.decayFncB);
            dlg.ShowDialog(this);
#endif
        }

        private void LevelBTypeButton_Click(object sender, EventArgs e)
        {
#if false
            UpdateModifiedStatus();
            switch(m_aa.aeTypeB)
            {
            case AETYPE.THRESHOLD:
                m_aa.aeTypeB = AETYPE.DOSE;
                break;

            case AETYPE.DOSE:
                m_aa.aeTypeB = AETYPE.THRESHOLD;
                break;
            }
            UpdateLevelBTypeButton();
#endif
        }

        private void DoseBSelectButton_Click(object sender, EventArgs e)
        {
#if false
            UpdateModifiedStatus();
            switch(m_aa.levelBDose)
            {
            case DOSERESPONSE.ODONTOCETE:
                m_aa.levelBDose = DOSERESPONSE.MYSTICETE;
                break;
            case DOSERESPONSE.MYSTICETE:
                m_aa.levelBDose = DOSERESPONSE.OTARIID;
                break;
            case DOSERESPONSE.OTARIID:
                m_aa.levelBDose = DOSERESPONSE.PHOCID;
                break;
            case DOSERESPONSE.PHOCID:
                m_aa.levelBDose = DOSERESPONSE.ODONTOCETE;
                break;
            }
            UpdateDoseBSelectButton();
#endif
        }

        private void UpdateDoseBSelectButton()
        {
#if false
            UpdateModifiedStatus();
            switch(m_aa.levelBDose)
            {
            case DOSERESPONSE.ODONTOCETE:
                DoseBSelectButton.Text = "Odontocete";
                break;
            case DOSERESPONSE.MYSTICETE:
                DoseBSelectButton.Text = "Mysticete";
                break;
            case DOSERESPONSE.OTARIID:
                DoseBSelectButton.Text = "Otariid";
                break;
            case DOSERESPONSE.PHOCID:
                DoseBSelectButton.Text = "Phocid";
                break;
            }
#endif
        }

        private void UpdateLevelBTypeButton()
        {
#if false
            UpdateModifiedStatus();
            switch(m_aa.aeTypeB)
            {
            case AETYPE.THRESHOLD:
                LevelBTypeButton.Text = "Level B Threshold (dB):";
                ActvnTheshBTextBox.Visible = true;
                reBLabel.Visible = true;
                ExposureUnitsBButton.Visible = true;

                DoseBSelectButton.Visible = false;
                break;

            case AETYPE.DOSE:
                LevelBTypeButton.Text = "Level B Dose Function:";
                ActvnTheshBTextBox.Visible = false;
                reBLabel.Visible = false;
                ExposureUnitsBButton.Visible = false;

                DoseBSelectButton.Visible = true;
                break;
            }
            UpdateDoseBSelectButton();
#endif
        }

        private void ModelOKButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                //m_speMdl.beachingDepth = m_beachingDepth; cannot be modified 
                m_speMdl.podBreaksOnAE = m_podBreaksOnAE;

                m_speMdl.AcousticAversion = m_aa; ;
            }
            Dispose();
        }

        private void CancelButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                FormConfirmDecision dlg = new FormConfirmDecision();
                dlg.Text = "Confirm Cancel";
                dlg.messageString = "Cancel Changes To Rate Model?";
                dlg.button1String = "Confirm Cancel";
                dlg.button2String = "Do Not Cancel";
                dlg.SetLocation(ModelOKButton);
                //dlg.SetNearbyLocation(this.Location.X + ModelOKButton.Location.X + ModelOKButton.Width - dlg.Width, this.Location.Y + ModelOKButton.Location.Y-dlg.Height - 5);
                dlg.ShowDialog(this);
                if(dlg.buttonSelected == 2)
                    return;
                m_modified = false;
            }
            Dispose();
        }
     }
}