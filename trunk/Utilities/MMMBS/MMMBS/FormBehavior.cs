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
    public partial class FormBehavior: Form
    {
        //-----------------//
        // Member Variables
        //-----------------//
        CBehavior m_beh;
        C3mbSpeciesModel m_spe;
        int m_myIndex;
        Boolean m_initializing = true;
        Boolean m_modified = false;
        string m_szTitleBar1 = "3MB Behavior Model ";
        string m_szTitleBar2 = " - Biomimetica";



        public FormBehavior(C3mbSpeciesModel Species, int ThisIndex)
        {
            InitializeComponent();
            m_beh = Species.GetBehaviorCopy(ThisIndex);
            m_spe = Species;
            m_myIndex = ThisIndex;
            BehNameButton.Text = m_beh.name;
            this.Text = m_szTitleBar1 + "(" + (m_myIndex+1) + " of " + m_spe.BehaviorCount + ")" + m_szTitleBar2;
            UpdatePanel();
            ModelOKButton.Text = "Done";
            m_initializing = false;
        }

        private Boolean UpdateModifiedStatus()
        {
            if(m_initializing == true)
                return true;

            if(m_modified == true)
                return false;

            m_modified = true;

            // Title Bar
            this.Text = m_szTitleBar1 + "(" + (m_myIndex+1) + " of " + m_spe.BehaviorCount + ")" + m_szTitleBar2 + "*";
            BehNameButton.Text = BehNameButton.Text + "*";
            ModelOKButton.Text = "Accept\nAnd\nDone";
            return false;
        }

        private void UpdatePanel()
        {
            //-------------------------------------------------------//
            // Environmental Attractors: Behavior Transition Matrices
            //-------------------------------------------------------//
            SetEnvAttBehTransitionMatrixState();

            //--------------------------------//
            // Environmental Attractors: Depth
            //--------------------------------//
            // Shelf
            if(m_beh.envAttrDepth.shelfEnabled == true)
            {
                //
                DepthShelfButton.Enabled = true;
                DepthShelfButton.BackColor = System.Drawing.Color.Chartreuse;
            }
            else
            {
                DepthShelfButton.Enabled = false;
                DepthShelfButton.BackColor = System.Drawing.SystemColors.ControlDark;
            }
 
            // Basin
            if(m_beh.envAttrDepth.basinEnabled == true)
            {
                DepthBasinButton.Enabled = true;
                DepthBasinButton.BackColor = System.Drawing.Color.Chartreuse;
            }
            else
            {
                DepthBasinButton.Enabled = false;
                DepthBasinButton.BackColor = System.Drawing.SystemColors.ControlDark;
            }

            // Slope
            if(m_beh.envAttrDepth.slopeEnabled == true)
            {
                DepthSlopeButton.Enabled = true;
                DepthSlopeButton.BackColor = System.Drawing.Color.Chartreuse;
                //DepthSlopeButton.Text = "DEEPER than " + m_beh.envAttrDepth.slopeDepth + ", GREATER than " + m_beh.envAttrDepth.slopeSlope;
            }
            else
            {
                DepthSlopeButton.Enabled = false;
                DepthSlopeButton.BackColor = System.Drawing.SystemColors.ControlDark;
            }


            //--------------------------------------//
            // Environmental Attractors: Temperature
            //--------------------------------------//
            if(m_beh.envAttrTemp.coldEnabled == true)
            {
                TempColdValueTextBox.Enabled = true;
                TempColdValueTextBox.Text = "" + m_beh.envAttrTemp.cold;
            }
            else
            {
                TempColdValueTextBox.Enabled = false;
                TempColdValueTextBox.Text = "----";
            }
            if(m_beh.envAttrTemp.warmEnabled == true)
            {
                TempWarmValueTextBox.Enabled = true;
                TempWarmValueTextBox.Text = "" + m_beh.envAttrTemp.warm;
            }
            else
            {
                TempWarmValueTextBox.Enabled = false;
                TempWarmValueTextBox.Text = "----";
            }
            if(m_beh.envAttrTemp.frontEnabled == true)
            {
                TempFrontValueTextBox.Enabled = true;
                TempFrontValueTextBox.Text = "" + m_beh.envAttrTemp.front;
            }
            else
            {
                TempFrontValueTextBox.Enabled = false;
                TempFrontValueTextBox.Text = "----";
            }

            
            //-----------------------------//
            // Environmental Attractors
            // Radio Priority Boxes
            //-----------------------------//
            SetEnvAttPriorityRadioBox();


            //-----------------//
            // Submodel Updates
            //-----------------//
            DirectionControlsUpdate();
            TravelRateControlsUpdate();
            AscentRateControlsUpdate();
            DescentRateControlsUpdate();
            DiveDepthRateControlsUpdate();
            DiveBottomFollowingControlsUpdate();
            DiveReversalsControlsUpdate();
            DiveSurfaceIntervalControlsUpdate();
            SetEnvAttPriorityRadioBox();
            SetEnvAttBehTransitionMatrixState();
         }

        private void SetEnvAttBehTransitionMatrixState()
        {
            // Depth
            if(m_beh.envAttrDepth.enabled == true)
            {
                DepthEnvBehTransAttractorButton.Enabled = false;
            }
            else
            {
                DepthEnvBehTransAttractorButton.Enabled = false;
            }

            // Temperature
            if(m_beh.envAttrTemp.enabled == true)
            {
                TempEnvBehTransAttractorButton.Enabled = false;
            }
            else
            {
                TempEnvBehTransAttractorButton.Enabled = false;
            }

        }

        private void SetEnvAttPriorityRadioBox()
        {
            if(m_beh.envAttrDepth.enabled == true & m_beh.envAttrTemp.enabled == true)
            {
                DepthPriorityRadioButton.Enabled = TemperaturePriorityRadioButton.Enabled = true;
                if(m_beh.envInfPriority == ENVATTRACTORPRIORITY.DIVE)
                {
                    DepthPriorityRadioButton.Checked = true;
                    TemperaturePriorityRadioButton.Checked = false;
                }
                else
                {
                    DepthPriorityRadioButton.Checked = false;
                    TemperaturePriorityRadioButton.Checked = true;
                }
            }
            else if(m_beh.envAttrDepth.enabled == true)
            {
                DepthPriorityRadioButton.Enabled = true;
                TemperaturePriorityRadioButton.Enabled = false;
                DepthPriorityRadioButton.Checked = true;
                TemperaturePriorityRadioButton.Checked = false;
            }
            else if(m_beh.envAttrTemp.enabled == true)
            {
                DepthPriorityRadioButton.Enabled = false;
                TemperaturePriorityRadioButton.Enabled = true;
                DepthPriorityRadioButton.Checked = false;
                TemperaturePriorityRadioButton.Checked = true;
            }
            else
            {
                DepthPriorityRadioButton.Enabled = TemperaturePriorityRadioButton.Enabled = false;
                DepthPriorityRadioButton.Checked = TemperaturePriorityRadioButton.Checked = false;
            }
        }

        //--------------------------//
        // Travel Direction Controls
        //--------------------------//
        void DirectionControlsUpdate()
        {
            DirectionSelectButton.Text = m_beh.direction.szType;
            DirectionDescriptionButton.Text = m_beh.direction.sz;
        }

        // Travel Direction State.
        void DirectionSelectButton_Click(object sender, EventArgs e)
        {
            switch(m_beh.direction.type)
            {
            case DIRECTIONMODELTYPE.VECTOR_DIRBIAS:
                m_beh.direction.type = DIRECTIONMODELTYPE.VECTOR;
                break;
            case DIRECTIONMODELTYPE.VECTOR:
                m_beh.direction.type = DIRECTIONMODELTYPE.RANDOM_WALK;
                break;
            case DIRECTIONMODELTYPE.RANDOM_WALK:
                m_beh.direction.type = DIRECTIONMODELTYPE.CORR_RAND_WALK;
                break;
            case DIRECTIONMODELTYPE.CORR_RAND_WALK:
                m_beh.direction.type = DIRECTIONMODELTYPE.CORR_RAND_WALK_DIR_BIAS;
                break;
            case DIRECTIONMODELTYPE.CORR_RAND_WALK_DIR_BIAS:
                m_beh.direction.type = DIRECTIONMODELTYPE.VECTOR_DIRBIAS;
                break;
            }
            UpdateModifiedStatus();
            DirectionControlsUpdate();
        }

        private void DirectionDescriptionButton_Click(object sender, EventArgs e)
        {
            FormTravel dlg = new FormTravel(m_beh.direction);
            dlg.ShowDialog(this);
            this.BringToFront();
            if(dlg.modified == true)
            {
                UpdateModifiedStatus(); 
                DirectionControlsUpdate();
            }
        }

        //---------------------//
        // Travel Rate Controls
        //---------------------//
        void TravelRateControlsUpdate()
        {
            TravelRateSelectButton.Text = m_beh.rate.szType;
            TravelRateDescriptionButton.Text = m_beh.rate.sz;
        }

        private void TravelRateSelectButton_Click(object sender, EventArgs e)
        {
            switch(m_beh.rate.modelType)
            {
            case MODELTYPE.GAUSSIAN:
                m_beh.rate.modelType = MODELTYPE.RANDOM;
                break;
            case MODELTYPE.RANDOM:
                m_beh.rate.modelType = MODELTYPE.MATRIX;
                break;
            case MODELTYPE.MATRIX:
                m_beh.rate.modelType = MODELTYPE.GAUSSIAN;
                break;
            }
            UpdateModifiedStatus();
            TravelRateControlsUpdate();
        }

        private void TravelRateDescriptionButton_Click(object sender, EventArgs e)
        {
            Boolean modified = m_modified;

            FormRate dlg = new FormRate(m_beh.rate, "Travel");
            dlg.ShowDialog(this);
            this.BringToFront();
            if(dlg.modified == true)
            {
                UpdateModifiedStatus();
                TravelRateControlsUpdate();
            }
        }


        //--------------------------//
        // Dive Ascent Rate Controls
        //--------------------------//
        void AscentRateControlsUpdate()
        {
            AscentSelectButton.Text = m_beh.ascent.szType;
            AscentDescriptionButton.Text = m_beh.ascent.sz;
        }

        private void AscentDescriptionButton_Click(object sender, EventArgs e)
        {
            FormRate dlg = new FormRate(m_beh.ascent, "Ascent");
            dlg.ShowDialog(this);
            this.BringToFront();
            if(dlg.modified == true)
            {
                UpdateModifiedStatus();
                AscentRateControlsUpdate();
            }
        }


        private void AscentSelectButton_Click(object sender, EventArgs e)
        {
            switch(m_beh.ascent.modelType)
            {
            case MODELTYPE.GAUSSIAN:
                m_beh.ascent.modelType = MODELTYPE.RANDOM;
                break;
            case MODELTYPE.RANDOM:
                m_beh.ascent.modelType = MODELTYPE.MATRIX;
                break;
            case MODELTYPE.MATRIX:
                m_beh.ascent.modelType = MODELTYPE.GAUSSIAN;
                break;
            }
            UpdateModifiedStatus();
            AscentRateControlsUpdate();
        }

        //---------------------------//
        // Dive Descent Rate Controls
        //---------------------------//
        void DescentRateControlsUpdate()
        {
            DescentSelectButton.Text = m_beh.descent.szType;
            DescentDescriptionButton.Text = m_beh.descent.sz;
        }

        private void DescentDescriptionButton_Click(object sender, EventArgs e)
        {
            FormRate dlg = new FormRate(m_beh.descent, "Descent");
            dlg.ShowDialog(this);
            this.BringToFront();
            if(dlg.modified == true)
            {
                UpdateModifiedStatus();
                DescentRateControlsUpdate();
            }
        }

        private void DescentSelectButton_Click(object sender, EventArgs e)
        {
            switch(m_beh.descent.modelType)
            {
            case MODELTYPE.GAUSSIAN:
                m_beh.descent.modelType = MODELTYPE.RANDOM;
                break;
            case MODELTYPE.RANDOM:
                m_beh.descent.modelType = MODELTYPE.MATRIX;
                break;
            case MODELTYPE.MATRIX:
                m_beh.descent.modelType = MODELTYPE.GAUSSIAN;
                break;
            }
            DescentRateControlsUpdate();
            UpdateModifiedStatus();
        }

        //---------------------------//
        // Dive Depth Rate Controls
        //---------------------------//
        void DiveDepthRateControlsUpdate()
        {
            DepthSelectButton.Text = m_beh.depth.szType;
            DepthDescriptionButton.Text = m_beh.depth.sz;
        }
        private void DepthSelectButton_Click(object sender, EventArgs e)
        {
            switch(m_beh.depth.type)
            {
            case MODELTYPE.GAUSSIAN:
                m_beh.depth.type = MODELTYPE.RANDOM;
                break;
            case MODELTYPE.RANDOM:
                m_beh.depth.type = MODELTYPE.MATRIX;
                break;
            case MODELTYPE.MATRIX:
                m_beh.depth.type = MODELTYPE.GAUSSIAN;
                break;
            }
            DiveDepthRateControlsUpdate();
            UpdateModifiedStatus();
        }

        private void DepthDescriptionButton_Click(object sender, EventArgs e)
        {
            FormDepth dlg = new FormDepth(m_beh.depth);
            dlg.ShowDialog(this);
            this.BringToFront();
            if(dlg.modified == true)
            {
                UpdateModifiedStatus();
                DiveDepthRateControlsUpdate();
            }
        }


        //-------------------------------//
        // Dive Bottom Following Controls
        //-------------------------------//
        void DiveBottomFollowingControlsUpdate()
        {
            switch (m_beh.flatBottomDive.modelType)
            {
            case MODELTYPE.DISABLED:
                BttmFollowSelectButton.Text = "No Bottom Following";
                BttmFollowDescriptionButton.Text = m_beh.flatBottomDive.sz;
                BttmFollowDescriptionButton.Enabled = false;
                break;
            case MODELTYPE.EXTERNALLYMODELED:
                BttmFollowSelectButton.Text = "Bottom Follows";
                BttmFollowDescriptionButton.Text = "Bottom Following Enabled";
                BttmFollowDescriptionButton.Enabled = false;
                break;
            case MODELTYPE.GAUSSIAN:
                BttmFollowSelectButton.Text = "Bottom Follows W/Travel Mdl";
                BttmFollowDescriptionButton.Text = "Gaussian Travel Rate Model:\n" + m_beh.flatBottomDive.sz;
                BttmFollowDescriptionButton.Enabled = true;
                break;
            case MODELTYPE.RANDOM:
                BttmFollowSelectButton.Text = "Bottom Follows W/Travel Mdl";
                BttmFollowDescriptionButton.Text = "Random Travel Rate Model:\n" + m_beh.flatBottomDive.sz;
                BttmFollowDescriptionButton.Enabled = true;
                break;
            }
        }
        private void BttmFollowSelectButton_Click(object sender, EventArgs e)
        {
            UpdateModifiedStatus();
            //m_beh.flatBottomDivingEnabled = !m_beh.flatBottomDivingEnabled;
            switch (m_beh.flatBottomDive.modelType)
            {
            case MODELTYPE.DISABLED:
                m_beh.flatBottomDive.modelType = MODELTYPE.EXTERNALLYMODELED;
                break;
            case MODELTYPE.EXTERNALLYMODELED:
                m_beh.flatBottomDive.modelType = MODELTYPE.GAUSSIAN;
                break;
            case MODELTYPE.GAUSSIAN:
                m_beh.flatBottomDive.modelType = MODELTYPE.RANDOM;
                break;
            case MODELTYPE.RANDOM:
                m_beh.flatBottomDive.modelType = MODELTYPE.DISABLED;
                break;
            }
            DiveBottomFollowingControlsUpdate();
        }

        private void BttmFollowDescriptionButton_Click(object sender, EventArgs e)
        {
            FormRate dlg = new FormRate(m_beh.flatBottomDive, "Bottom Following");
            dlg.ShowDialog(this);
            this.BringToFront();
            if(dlg.modified == true)
            {
                UpdateModifiedStatus();
                DiveBottomFollowingControlsUpdate();
            }

        }


        //------------------------//
        // Dive Reversals Controls
        //------------------------//
        void DiveReversalsControlsUpdate()
        {
            if(m_beh.reversal.enabled == false)
            {
                ReversalsEnableDiveRateButton.Enabled = false;
                ReversalsSelectButton.Text = "Disabled.";

                ReversalsDescriptionButton.Enabled = false;
                ReversalsDescriptionButton.Text = "No reversals.";
            }
            else
            {
                ReversalsEnableDiveRateButton.Enabled = true;
                ReversalsSelectButton.Text = m_beh.reversal.szType;

                ReversalsDescriptionButton.Enabled = true;
                ReversalsDescriptionButton.Text = m_beh.reversal.sz;
            }
        }

        private void ReversalsSelectButton_Click(object sender, EventArgs e)
        {
            if(m_beh.reversal.enabled == false)
            {
                m_beh.reversal.enabled = true;
                DiveReversalsControlsUpdate();
                UpdateModifiedStatus();
                return;
            }

            switch(m_beh.reversal.type)
            {
            case MODELTYPE.GAUSSIAN:
                m_beh.reversal.type = MODELTYPE.RANDOM;
                break;
            case MODELTYPE.RANDOM:
                m_beh.reversal.type = MODELTYPE.MATRIX;
                break;
            case MODELTYPE.MATRIX:
                m_beh.reversal.type = MODELTYPE.GAUSSIAN;
                m_beh.reversal.enabled = false;
                break;
            }
            DiveReversalsControlsUpdate();
            UpdateModifiedStatus();
        }

        private void ReversalsEnableDiveRateButton_Click(object sender, EventArgs e)
        {
            //m_beh.reversal.diveRateEnabled = !m_beh.reversal.diveRateEnabled;
            switch(m_beh.reversal.reversalDiveRateType)
            {
                case REVERSAL_DIVE_RATE_TYPE.NO_INDEPENDENT:
                    ReversalsEnableDiveRateButton.Enabled = false;
                    m_beh.reversal.reversalDiveRateType = REVERSAL_DIVE_RATE_TYPE.INDEPENDENT;
                    break;
                case REVERSAL_DIVE_RATE_TYPE.INDEPENDENT:
                    ReversalsEnableDiveRateButton.Enabled = true;
                    m_beh.reversal.reversalDiveRateType = REVERSAL_DIVE_RATE_TYPE.INDEPENDENT_DIVE_AND_ASCENT;
                    break;
                case REVERSAL_DIVE_RATE_TYPE.INDEPENDENT_DIVE_AND_ASCENT:
                    ReversalsEnableDiveRateButton.Enabled = true;
                    m_beh.reversal.reversalDiveRateType = REVERSAL_DIVE_RATE_TYPE.NO_INDEPENDENT;
                    break;
            }
            ReversalsSelectButton.Text = m_beh.reversal.szType;
            DiveReversalsControlsUpdate();
        }

        private void ReversalsDescriptionButton_Click(object sender, EventArgs e)
        {
            FormReversal dlg = new FormReversal(m_beh.reversal);
            dlg.ShowDialog(this);
            this.BringToFront();
            if(dlg.modified == true)
            {
                UpdateModifiedStatus();
                DiveReversalsControlsUpdate();
            }
        }

        //-------------------------------//
        // Dive Surface Interval Controls
        //-------------------------------//
        void DiveSurfaceIntervalControlsUpdate()
        {
            SrfIntrvlSelectButton.Text = m_beh.surfaceInterval.szType;
            SrfIntrvlDescriptionButton.Text = m_beh.surfaceInterval.sz;
        }

        private void SrfIntrvlSelectButton_Click(object sender, EventArgs e)
        {
            switch(m_beh.surfaceInterval.type)
            {
            case SRFINVMODELTYPE.MATRIX:
                m_beh.surfaceInterval.type = SRFINVMODELTYPE.GAUSSIAN;
                break;
            case SRFINVMODELTYPE.GAUSSIAN:
            case SRFINVMODELTYPE.DONNOTUSERANDOM:
                m_beh.surfaceInterval.type = SRFINVMODELTYPE.MATRIX;
                break;
            }

            DiveSurfaceIntervalControlsUpdate();
            UpdateModifiedStatus();
        }

        private void SrfIntrvlDescriptionButton_Click(object sender, EventArgs e)
        {
            FormSurfIntvl dlg = new FormSurfIntvl(m_beh.surfaceInterval);
            dlg.ShowDialog(this);
            this.BringToFront();
            if(dlg.modified == true)
            {
                UpdateModifiedStatus();
                DiveSurfaceIntervalControlsUpdate();
            }
        }

        private void BehNameButton_Click(object sender, EventArgs e)
        {
            Form1Input dlg = new Form1Input("Behavior Name", BehNameButton.Text);
            dlg.ShowDialog(this);
            this.BringToFront();
            if(dlg.result == RESLT.OK && dlg.modified == true)
            {
                m_beh.name = BehNameButton.Text = dlg.dataString;
                UpdateModifiedStatus();
            }
        }

        private void CopyBehaviorButton_Click(object sender, EventArgs e)
        {
            FormNormalModelSelect dlg = new FormNormalModelSelect(m_spe, m_myIndex);
            dlg.ShowDialog(this);
            this.BringToFront();
            if(dlg.result == RESLT.CANCEL)
                return;

            m_beh = m_spe.GetBehaviorCopy(dlg.selectedIndex);
            m_spe.SetBehavior(m_myIndex, m_beh);
            UpdateModifiedStatus();
            UpdatePanel();
        }

        private void BehaviorTransVectorButton_Click(object sender, EventArgs e)
        {
            FormBehTrans dlg = new FormBehTrans(
                m_beh.SpanManager.GetCopy(),
                m_beh.BehaviorTerminationModel,
                m_spe.BehaviorCount,
                m_spe.NameArray,
                m_myIndex
                );
            dlg.ShowDialog(this);
            this.BringToFront();
            if(dlg.modified == true)
            {
                m_beh.SpanManager = (CBehaviorTransitionSpanMgr)dlg.NrmlBehTransMdl;
                m_beh.BehaviorTerminationModel = dlg.BehaviorTerminationModel;
                UpdateModifiedStatus();
            }
        }

        private void DepthEnvBehTransAttractorButton_Click(object sender, EventArgs e)
        {
            RmBehTransVec dlg =
                new RmBehTransVec(TRANSITIONALMODELTYPE.DEPTHENV, m_spe, m_beh, m_myIndex, "Depth Environmental Attractor");
            dlg.ShowDialog(this);
            if(dlg.modified == true)
                UpdateModifiedStatus();
        }

        private void TempEnvBehTransAttractorButton_Click(object sender, EventArgs e)
        {
            RmBehTransVec dlg =
                new RmBehTransVec(TRANSITIONALMODELTYPE.TEMPERATUREENV, m_spe, m_beh, m_myIndex, "Temperature Environmental Attractor");
            dlg.ShowDialog(this);
            if(dlg.modified == true)
                UpdateModifiedStatus();
        }

        private void DepthShelfWaterEnableButton_Click(object sender, EventArgs e)
        {
            m_beh.envAttrDepth.shelfEnabled = !m_beh.envAttrDepth.shelfEnabled;
            if(m_beh.envAttrDepth.shelfEnabled == true)
            {
                m_beh.envAttrDepth.basinEnabled = false;
                m_beh.envAttrDepth.slopeEnabled  = false;
            }
            UpdateModifiedStatus();
            UpdatePanel();
        }


        private void DepthShallowWaterEnableButton_Click(object sender, EventArgs e)
        {
            m_beh.envAttrDepth.basinEnabled = !m_beh.envAttrDepth.basinEnabled;
            if(m_beh.envAttrDepth.basinEnabled == true)
            {
                m_beh.envAttrDepth.shelfEnabled  = false;
                m_beh.envAttrDepth.slopeEnabled = false;
            }
            UpdateModifiedStatus();
            UpdatePanel();
        }

        private void DepthDeepWaterEnableButton_Click(object sender, EventArgs e)
        {
            m_beh.envAttrDepth.slopeEnabled = !m_beh.envAttrDepth.slopeEnabled;
            if(m_beh.envAttrDepth.slopeEnabled == true)
            {
                m_beh.envAttrDepth.shelfEnabled  = false;
                m_beh.envAttrDepth.basinEnabled = false;
            }
            UpdateModifiedStatus();
            UpdatePanel();
        }


        private void TempColdWaterEnableButton_Click(object sender, EventArgs e)
        {
            m_beh.envAttrTemp.coldEnabled = !m_beh.envAttrTemp.coldEnabled;
            UpdateModifiedStatus();
            UpdatePanel();
        }

        private void TempWarmWaterEnableButton_Click(object sender, EventArgs e)
        {
            m_beh.envAttrTemp.warmEnabled = !m_beh.envAttrTemp.warmEnabled;
            UpdateModifiedStatus();
            UpdatePanel();
        }

        private void TempFrontWaterEnableButton_Click(object sender, EventArgs e)
        {
            m_beh.envAttrTemp.frontEnabled = !m_beh.envAttrTemp.frontEnabled;
            UpdateModifiedStatus();
            UpdatePanel();
        }

        private void DepthPriorityRadioButton_CheckedChanged(object sender, EventArgs e)
        {
            UpdateModifiedStatus();
        }

        private void TemperaturePriorityRadioButton_CheckedChanged(object sender, EventArgs e)
        {
            UpdateModifiedStatus();
        }


        private void DepthPriorityRadioButton_Clicked(object sender, EventArgs e)
        {
            UpdateModifiedStatus();
            m_beh.envInfPriority = ENVATTRACTORPRIORITY.DIVE;
            DepthPriorityRadioButton.Checked = true;
            TemperaturePriorityRadioButton.Checked = false;
        }

        private void TemperaturePriorityRadioButton_Clicked(object sender, EventArgs e)
        {
            UpdateModifiedStatus();
            m_beh.envInfPriority = ENVATTRACTORPRIORITY.TEMPERATURE;
            DepthPriorityRadioButton.Checked = false;
            TemperaturePriorityRadioButton.Checked = true;

        }

        private void TempColdValueTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == true)
                return;
            if(TempColdValueTextBox.Enabled == false)
                return;

            TempColdValueTextBox.Text = CStringUtil.SzEnforceDoubleFmt(TempColdValueTextBox.Text);
            m_beh.envAttrTemp.cold = CStringUtil.SzToDouble(TempColdValueTextBox.Text);
        }

        private void TempWarmValueTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == true)
                return;
            if(TempWarmValueTextBox.Enabled == false)
                return;
            TempWarmValueTextBox.Text = CStringUtil.SzEnforceDoubleFmt(TempWarmValueTextBox.Text);
            m_beh.envAttrTemp.warm = CStringUtil.SzToDouble(TempWarmValueTextBox.Text);
        }

        private void TempFrontValueTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == true)
                return;
            if(TempFrontValueTextBox.Enabled == false)
                return;

            TempFrontValueTextBox.Text = CStringUtil.SzEnforceDoubleFmt(TempFrontValueTextBox.Text);
            m_beh.envAttrTemp.front = CStringUtil.SzToDouble(TempFrontValueTextBox.Text);
        }

        private void ModelOKButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
                m_spe.SetBehavior(m_myIndex, m_beh);
            Dispose();
        }

        private void CancelButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                FormConfirmDecision dlg = new FormConfirmDecision();
                dlg.Text = "Confirm Cancel";
                dlg.messageString = "Changes have not been updated!";
                dlg.button1String = "Confirm Cancel";
                dlg.button2String = "Do Not Cancel";
                dlg.SetLocation(ModelOKButton);
                dlg.ShowDialog(this);


                if(dlg.buttonSelected == 2)
                    return;
                m_modified = false;
            }
            Dispose();
        }

        private void DepthShelfButton_Click(object sender, EventArgs e)
        {
            /*
            if(UpdateModifiedStatus() == true)
                return;
            if(DepthShelfTextBox.Enabled == false)
                return;

            DepthShelfTextBox.Text = CStringUtil.SzEnforceDoubleFmt(DepthShelfTextBox.Text);
            m_beh.envAttrDepth.shelfDepth = CStringUtil.SzToDouble(DepthShelfTextBox.Text);

             * */

        }

        private void DepthBasinButton_Click(object sender, EventArgs e)
        {

        }

        private void DepthSlopeButton_Click(object sender, EventArgs e)
        {

        }
    }
}