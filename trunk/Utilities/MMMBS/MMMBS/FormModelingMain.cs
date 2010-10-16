using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Drawing.Imaging;
using System.Text;
using System.Windows.Forms;
using System.Collections;
using System.Threading;
using System.Diagnostics;
using System.IO;
using MMMBSLib; // C# code code and data types.
using mbs; // C++ wrapper class code and data types

namespace MBSGUI
{
    public partial class FormSpeciesDefinition: Form
    {

        //-----------------//
        // Member Variables
        //-----------------//
        private uint INITIALDURATION = (uint)(3600);//3599; // for 1 hour of saved iterations.
        public static double DEFAULT_BATHYMETRY_DEPTH = /*-532.203390*/ -5000;


        private uint m_duration;
        private double m_bathyDepth;
        private C3mbs m_mmmbs = new C3mbs();

        private ANIMATSTATEDATABITMAP[] m_data;
        private DATA_EXTREMES_SPE_BITMAP m_dataExtremes;
        private mbsANIMAT_STATE[] m_stateArray;


        //private Point[] m_points_dive;
        //private Point[] m_points_move;


        // Color for front rectangle
        //private Color frontColor = Color.FromArgb(100, 0, 0, 255);

        C3mbSpeciesModel m_speMdl = new C3mbSpeciesModel();
        // put this somewhere else.
        static int NUM_NRML_BUTTONS = 8;
        //Button[] m_addBehButton = new Button[NUM_NRML_BUTTONS];
        Button[] m_editBehButton = new Button[NUM_NRML_BUTTONS];
        Button[] m_delBehButton = new Button[NUM_NRML_BUTTONS];
        Button[] m_displayBehColorButton = new Button[NUM_NRML_BUTTONS];
        Color[] m_behaviorColor = new Color[NUM_NRML_BUTTONS];
        Color[] m_currentBehaviorColor = new Color[NUM_NRML_BUTTONS];
        Button[] m_shallowWaterButton = new Button[NUM_NRML_BUTTONS];
        Button[] m_deepWaterButton = new Button[NUM_NRML_BUTTONS];
        Button[] m_breakWaterButton = new Button[NUM_NRML_BUTTONS];
        Button[] m_coldWaterButton = new Button[NUM_NRML_BUTTONS];
        Button[] m_warmWaterButton = new Button[NUM_NRML_BUTTONS];
        Button[] m_frontWaterButton = new Button[NUM_NRML_BUTTONS];
        Label[] m_behLabel = new Label[NUM_NRML_BUTTONS];

        ToolStripMenuItem[] m_speciesSelTooStrip;// = new ToolStripMenuItem[C3mbsSpeciesModel.GetSpeciesNameListCount()];

        string m_szFileName;
        bool m_fileNamed;
        bool m_fileEverWasNamed;
        //Boolean m_initializing;
        //Boolean m_modified = false;
        string m_szTitleBar;

        private uint m_additionalSimHours = 0;

        //CDiveTimeScaleBitMap m_diveTimeScaleBitmap;
        CBitmapModelingMain m_bitmapMgr;

        private void AdditionalHoursButton_Click(object sender, EventArgs e)
        {
            m_additionalSimHours = (m_additionalSimHours + 1)%40;
            AdditionalHoursButton.Text = "Additional Hours " + m_additionalSimHours;
            Run3MB(false, false);
        }

        private void ResetHoursButton_Click(object sender, EventArgs e)
        {
            m_additionalSimHours = 0;
            AdditionalHoursButton.Text = "Additional Hours " + m_additionalSimHours;
            Run3MB(false, false);
        }

        public FormSpeciesDefinition()
        {
            int speciesNameListCount = m_speMdl.GetSpeciesNameListCount();
            m_speciesSelTooStrip = new ToolStripMenuItem[speciesNameListCount];
            // bitmap related initializations
            InitializeComponent();
            CopyNormalButtons();
            SetSpeciesToolStripMenuItemNames();
            SetUpFresh();
        }

        void SetUpFresh()
        {
            //CDepthSpan spanRef;
            m_mmmbs = new C3mbs();

            //--------------------------------------------------------------------------//
            // Set up the intial species model.
            //---------------------------------//
            m_speMdl = new C3mbSpeciesModel();

            m_szFileName = "NewSpeciesModel.spe";
            FileNameLabel.Text = m_szFileName;
            m_fileNamed = false;
            m_fileEverWasNamed = false;

            m_szTitleBar = "3MB Species Builder Version " + m_speMdl.speciesCurrentSuperVer +
                "." + String.Format("{0:00}", m_speMdl.speciesCurrentSubVer) + " - Biomimetica";


            m_additionalSimHours = 0;
            AdditionalHoursButton.Text = "Additional Hours " + m_additionalSimHours;


            // A new species model is being created.  A default normal behavior will be
            // automatically added.  Change its name.
            //beh = m_speMdl.GetBehaviorDuplicate(0);

            Text = m_szTitleBar;
            SpeciesVersopmLabel.Text = "species version:";
            SaveVerLabel.Text = "" + m_speMdl.speciesCurrentSuperVer + "." + String.Format("{0:00}", m_speMdl.speciesCurrentSubVer);


            UpdateEntireForm();

            // Bitmap related
            m_duration = INITIALDURATION;
            m_bathyDepth = DEFAULT_BATHYMETRY_DEPTH;

            //m_numIterations = (int)INITIALDURATION;
            InitializeBitMapVars();
            ToggleTargetDepthDisplayButton.BackColor = m_bitmapMgr.TargetDepthColor;
            ToggleBathymetryDisplayButton.BackColor = m_bitmapMgr.BathyColor;

            DepthInputButton.Text = "Bathy Depth:" + String.Format("{0:00}", m_bathyDepth);


            //Save();
            Run3MB(false, false);
        }

        public void InitializeBitMapVars()
        {
            m_data = new ANIMATSTATEDATABITMAP[m_duration+1];

            Rectangle[] r = new Rectangle[4];

            r[0].X = DiveProfileGroupBox.Location.X;
            r[0].Y = DiveProfileGroupBox.Location.Y;
            r[0].Width = DiveProfileGroupBox.Width;
            r[0].Height = DiveProfileGroupBox.Height;

            r[1].X = DiveTimeScaleGroupBox.Location.X;
            r[1].Y = DiveTimeScaleGroupBox.Location.Y;
            r[1].Width = DiveTimeScaleGroupBox.Width;
            r[1].Height = DiveTimeScaleGroupBox.Height;

            r[2].X = DiveDepthScaleGroupBox.Location.X;
            r[2].Y = DiveDepthScaleGroupBox.Location.Y;
            r[2].Width = DiveDepthScaleGroupBox.Width;
            r[2].Height = DiveDepthScaleGroupBox.Height;

            r[3].X = MovementGoupBox.Location.X;
            r[3].Y = MovementGoupBox.Location.Y;
            r[3].Width = MovementGoupBox.Width;
            r[3].Height = MovementGoupBox.Height;

       
            //m_diveTimeScaleBitmap = new CDiveTimeScaleBitMap(r);
            m_bitmapMgr = new CBitmapModelingMain(this, r[0], r[1], r[2], r[3]);
        }

        void Run3MB(Boolean MaintainDataExtremes, Boolean MaintainScaling)
        {
            // Record if, prior to running the model, the model needed to be saved.
            Boolean neededSave = m_speMdl.NeedSave;
            string speFileName = Directory.GetCurrentDirectory() + "\\sztemp.spe";
            int i;
            mbsPosition position;
            mbsPosition[] positionArray = new mbsPosition[1];
            mbsRESULT result;

            position.latitude = 0;
            position.longitude = 0;
            position.depth = 0;// (initial animat depth cannot be set but must be done so
                               // here to avoid compiler error.

            // eventually, instead of saving to file, directly transfer the model to the
            // 3mb libraries
            m_speMdl.SaveToBinFile(speFileName, false);

            // If the user had made changes to the model before it auto ran (to update the
            // display) "touch" the species model so its status changes to needing to be
            // saved.
            if(neededSave == true)
            {
                m_speMdl.Touch();
                UpdateModifiedStatus();
            }

            m_mmmbs.ClearScenario();
            m_duration = (uint)INITIALDURATION * ((uint)m_speMdl.BehaviorCount) + m_additionalSimHours * 3600;
            m_stateArray = new mbsANIMAT_STATE[m_duration + 1];

            if(mbsRESULT.OK != (result = m_mmmbs.AddSpecies(speFileName)))
                return;

            // The zero value in AddIndividualAnimat refers to the specie's index.  There
            // is only a single species, thus, index 0.
            position.latitude = 0;
            position.longitude = 0;
            if(mbsRESULT.OK != (result = m_mmmbs.AddIndividualAnimat(0, position)))
                return;

            m_mmmbs.SetDuration((int)m_duration);

            mbsCONFIG config = m_mmmbs.GetConfiguration();
            config.enabled = false; // binary ouput disengaged.
            config.maintainFirstAnimatState = true;
            //config.distCalcMethod = mbsDISTCALC.PLANAR_GEOMETRY;
            //config.diveRate = true;
            m_mmmbs.SetConfiguration(config);
            m_mmmbs.SetBaythyConstantDepth(m_bathyDepth);

            //----------------------------------------------//
            // Testing and Debugging
            // m_mmmbs.ClearScenario();
            //return;
            //----------------------------------------------//

            // Run the entire scenario.  
            if(mbsRESULT.OK != (result = m_mmmbs.RunScenarioEntireDuration())) // 3mbsWrapper.cpp
            {
                m_mmmbs.ClearScenario();
                return;
            }

            //Thread.Sleep(3);
            while(mbsRUNSTATE.FINISHED != m_mmmbs.GetRunState())
            {
                Thread.Sleep(3);
                if(mbsRESULT.OK != (result = m_mmmbs.GetErrorStatus()))
                {
                    m_mmmbs.ClearScenario();
                    return;
                }
            }


            // retrieve the animats preserved states
            for(i=0; i<m_duration+1; i++)
            {
                m_stateArray[i] = m_mmmbs.RetrieveFirstAnimatStateAtIndex((uint)i);
            }
            // Display the generated data
            SetBitmapDisplayData(MaintainDataExtremes, MaintainScaling);
            if(File.Exists(speFileName))
                File.Delete(speFileName);
        }

        // Override Form OnPaint method
        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);
            Graphics graphicsObject = e.Graphics; // get graphics
            m_bitmapMgr.MyOnPaint(graphicsObject);
        }


        // Determine the bearing of 2 relative to 1
        DISTANGL DetermineBearing(double lat1, double lon1, double lat2, double lon2)
        {
            // The sign convention used in the following equations is that latitude is
            // negative in the South, positive in the North, and that longitude is positive in 
            // the West and negative in the East.  This is opposite of normal convention and 
            // requires some temporary sign manipulation.

            // The following labeling conventions are used: lat1 and lon1 are the latitude and
            // longitude of the sound source, lat2 and lon2 are the latitude and longitude of the 
            // animat.

            double bearing; 	// bearing from animat to sound source
            double d_rads;			// distance from animat to sound source in radians
            double rad_lon1, rad_lon2, rad_lat1, rad_lat2;	// radian conversions of latitude and longitude
            DISTANGL distBear;

            // Signs are temporarily changed for longitude to be used in the following equations which 
            // follow the convention of W being a positive value and E being a negative value
            rad_lon1 = (Math.PI / 180) * (-lon1);
            rad_lon2 = (Math.PI / 180) * (-lon2);
            rad_lat1 = (Math.PI / 180) * (lat1);
            rad_lat2 = (Math.PI / 180) * (lat2);

            // Calculate distance between points
            d_rads =
                2 * Math.Asin(
                        Math.Sqrt(Math.Pow((Math.Sin((rad_lat1 - rad_lat2) / 2)), 2)
                        + 	
		                Math.Cos(rad_lat1) * Math.Cos(rad_lat2) * Math.Pow((Math.Sin((rad_lon1 - rad_lon2) / 2)), 2)
                        )
                    );

            // Determine the bearing in radians
            if(Math.Sin(rad_lon2 - rad_lon1) == 0)
            {
                if(rad_lat1 == rad_lat2 || rad_lat1 < rad_lat2)
                {
                    distBear.angle = 0;
                    distBear.distance = d_rads * 1852 * (180 * 60)/Math.PI; // convert to meters.
                    return distBear;
                }
                else
                {
                    distBear.angle = 180;
                    distBear.distance = d_rads * 1852 * (180 * 60)/Math.PI; // convert to meters.
                    return distBear;
                }
            }
            else if(Math.Sin(rad_lon2 - rad_lon1) < 0)
                bearing = Math.Acos((Math.Sin(rad_lat2) - Math.Sin(rad_lat1) * Math.Cos(d_rads)) / (Math.Sin(d_rads) * Math.Cos(rad_lat1)));
            else
                bearing = 2 * Math.PI - Math.Acos((Math.Sin(rad_lat2) - Math.Sin(rad_lat1) * Math.Cos(d_rads)) / (Math.Sin(d_rads) * Math.Cos(rad_lat1)));

            // Convert bearing back to degrees
            bearing = (180 / Math.PI) * bearing;

            distBear.angle = bearing;
            distBear.distance = d_rads * 1852 * (180 * 60)/Math.PI; // convert to meters.

            return distBear;
        }

        private DATA_EXTREMES_SPE_BITMAP DetermineDataExtremes(ANIMATSTATEDATABITMAP[] StateData)
        {
            int i;
            DATA_EXTREMES_SPE_BITMAP ex;

            ex.xMax = ex.xMin = StateData[0].x;
            ex.yMax = ex.yMin = StateData[0].y;
            ex.zMax = ex.zMin = StateData[0].z;
            if(ex.zMax < StateData[0].targetDepth)
                ex.zMax = StateData[0].targetDepth;
            if(ex.zMin > StateData[0].targetDepth)
                ex.zMin = StateData[0].targetDepth;

            for(i = 0; i < StateData.Length; i++)
            {
                if(ex.xMax < StateData[i].x)
                    ex.xMax = StateData[i].x;
                if(ex.xMin > StateData[i].x)
                    ex.xMin = StateData[i].x;
                if(ex.yMax < StateData[i].y)
                    ex.yMax = StateData[i].y;
                if(ex.yMin > StateData[i].y)
                    ex.yMin = StateData[i].y;

                if(ex.zMax < StateData[i].z)
                    ex.zMax = StateData[i].z;
                if(ex.zMin > StateData[i].z)
                    ex.zMin = StateData[i].z;

                if(ex.zMax < StateData[i].targetDepth)
                    ex.zMax = StateData[i].targetDepth;
                if(ex.zMin > StateData[i].targetDepth)
                    ex.zMin = StateData[i].targetDepth;
            }

            // Give some margin at the top and bottom of the dive profile.
            ex.zMin -= 20;
            ex.zMax += 20;
            return ex;
        }



        private void SetBitmapDisplayData(Boolean MaintainDataExtremes, Boolean MaintainScaling)
        {
            int i;
            DATA_EXTREMES_SPE_BITMAP currentDataExtremes;

            m_data = new ANIMATSTATEDATABITMAP[m_stateArray.Length];

            //////////////////////
            // convert lat/lon to meters
            // converstion process...
            for(i = 0; i< m_stateArray.Length; i++)
            {
                m_data[i].animatState = m_stateArray[i];
                m_data[i].y = (float)(m_stateArray[i].xDistance);
                m_data[i].x = (float)(m_stateArray[i].yDistance);
                m_data[i].z = -(float)m_stateArray[i].depth;
                m_data[i].targetDepth = -(float)m_stateArray[i].targetDepth;
            }

            currentDataExtremes = DetermineDataExtremes(m_data);
            if(MaintainDataExtremes == false)
                m_dataExtremes = currentDataExtremes;
            m_bitmapMgr.SetDisplayData(m_data, m_dataExtremes, currentDataExtremes, MaintainScaling);
        }


        // Returns true if initializing or false otherwise.
        private void UpdateModifiedStatus()
        {
            if(m_speMdl.NeedSave == false)
            {
                Text = m_szTitleBar;
                FileNameLabel.Text = m_szFileName;
            }
            else
            {
                Text = m_szTitleBar + "*";
                FileNameLabel.Text = m_szFileName + "*";
            }
        }



        private void FormSpeciesDesigner_Load(object sender, EventArgs e)
        {
            UpdateNormalBehaviorStatesEntire();
        }

        private void SetSpeciesToolStripMenuItemNames()
        {
            int i;
            for(i=0; i<m_speciesSelTooStrip.Length; i++)
            {
                if(m_speciesSelTooStrip[i] == null)
                    continue;

                m_speciesSelTooStrip[i].Text = 
                    m_speMdl.GetSpeciesLatinNameString(i) + " (" +
                    m_speMdl.GetSpeciesEnglishNameString(i) + ")";
            }
        }


        private void AddBehaviorButton_Click(object sender, EventArgs e)
        {
            int index; // The index of the behavior in the behavior array.
            CBehavior behCpy;

            if(m_speMdl.BehaviorCount == NUM_NRML_BUTTONS)
                return; // Don't add more than allowed.

            behCpy = m_speMdl.GetBehaviorCopy(index = m_speMdl.AddBehavior());
            UpdateModifiedStatus();
            if(m_speMdl.BehaviorCount == NUM_NRML_BUTTONS)
            {
                AddBehaviorButton.Enabled = false;
                AddBehaviorButton.BackColor = System.Drawing.SystemColors.GradientInactiveCaption;
            }
            UpdateNormalBehaviorStatesEntire();
            Run3MB(false, false);
        }

        private void DeleteBehavior(object sender, EventArgs e)
        {
            int index = GuiUtils.MatchIndex(m_delBehButton, (Button)sender);
            if(index >= m_speMdl.BehaviorCount || m_speMdl.BehaviorCount == 1)
                return;
            m_speMdl.DeleteBehavior(index);
            UpdateModifiedStatus();
            UpdateNormalBehaviorStatesEntire();
            Run3MB(false, false);
        }


        private void EditBehavior(object sender, EventArgs e)
        {
            FormBehavior dlg;
            int index = GuiUtils.MatchIndex(m_editBehButton, (Button)sender);
            if(index >= m_speMdl.BehaviorCount)
                return;

            dlg = new FormBehavior(m_speMdl, index);
            dlg.ShowDialog(); // modal
            this.BringToFront();

            UpdateModifiedStatus();
            UpdateNormalBehaviorStatesEntire();
            Run3MB(false, true);
        }

        private void ToggleDisplayColorState(object sender, EventArgs e)
        {
            int index = GuiUtils.MatchIndex(m_displayBehColorButton, (Button)sender);
            if(index < NUM_NRML_BUTTONS && index < m_speMdl.BehaviorCount)
                m_displayBehColorButton[index].BackColor = m_bitmapMgr.ToggleDisplayColor(index);
        }


        private void ToggleShallowWaterState(object sender, EventArgs e)
        {
            int index = GuiUtils.MatchIndex(m_shallowWaterButton, (Button)sender);
            CBehavior beh = m_speMdl.GetBehaviorCopy(index);
            beh.envAttrDepth.slopeEnabled = !beh.envAttrDepth.slopeEnabled;
            m_speMdl.SetBehavior(index, beh);
            UpdateEnvAttState(true, beh.envAttrDepth.slopeEnabled, m_shallowWaterButton[index]);
            UpdateModifiedStatus();
            Run3MB(false, false);
        }

        private void ToggleDeepWaterState(object sender, EventArgs e)
        {
            int index = GuiUtils.MatchIndex(m_deepWaterButton, (Button)sender);
            CBehavior beh = m_speMdl.GetBehaviorCopy(index);
            beh.envAttrDepth.basinEnabled = !beh.envAttrDepth.basinEnabled;
            UpdateEnvAttState(true, beh.envAttrDepth.basinEnabled, m_deepWaterButton[index]);
            m_speMdl.SetBehavior(index, beh);
            UpdateModifiedStatus();
            Run3MB(false, false);
        }

        private void ToggleBreakWaterState(object sender, EventArgs e)
        {
            int index = GuiUtils.MatchIndex(m_breakWaterButton, (Button)sender);
            CBehavior beh = m_speMdl.GetBehaviorCopy(index);
            beh.envAttrDepth.shelfEnabled = !beh.envAttrDepth.shelfEnabled;
            UpdateEnvAttState(true, beh.envAttrDepth.shelfEnabled, m_breakWaterButton[index]);
            m_speMdl.SetBehavior(index, beh);
            UpdateModifiedStatus();
            Run3MB(false, false);
        }


        private void ToggleColdWaterState(object sender, EventArgs e)
        {
            //GuiUtils.MatchIndex
            int index = GuiUtils.MatchIndex(m_coldWaterButton, (Button)sender);
            CBehavior beh = m_speMdl.GetBehaviorCopy(index);
            beh.envAttrTemp.coldEnabled = !beh.envAttrTemp.coldEnabled;
            UpdateEnvAttState(true, beh.envAttrTemp.coldEnabled, m_coldWaterButton[index]);
            m_speMdl.SetBehavior(index, beh);
            UpdateModifiedStatus();
            Run3MB(false, false);
        }

        private void ToggleWarmWaterState(object sender, EventArgs e)
        {
            int index = GuiUtils.MatchIndex(m_warmWaterButton, (Button)sender);
            CBehavior beh = m_speMdl.GetBehaviorCopy(index);
            beh.envAttrTemp.warmEnabled = !beh.envAttrTemp.warmEnabled;
            UpdateEnvAttState(true, beh.envAttrTemp.warmEnabled, m_warmWaterButton[index]);
            m_speMdl.SetBehavior(index, beh);
            UpdateModifiedStatus();
            Run3MB(false, false);
        }

        private void ToggleFrontWaterState(object sender, EventArgs e)
        {
            int index = GuiUtils.MatchIndex(m_frontWaterButton, (Button)sender);
            CBehavior beh = m_speMdl.GetBehaviorCopy(index);
            beh.envAttrTemp.frontEnabled = !beh.envAttrTemp.frontEnabled;
            UpdateEnvAttState(true, beh.envAttrTemp.frontEnabled, m_frontWaterButton[index]);
            m_speMdl.SetBehavior(index, beh);
            UpdateModifiedStatus();
            Run3MB(false, false);
        }

        private void UpdateEnvAttState(Boolean BehaviorEnabled, Boolean AbilityEnabled, Button But)
        {
            But.Enabled = BehaviorEnabled;
            
            if(BehaviorEnabled == false)
                But.BackColor = System.Drawing.SystemColors.GradientInactiveCaption;
            else if(AbilityEnabled == true)
                But.BackColor = System.Drawing.Color.Chartreuse;
            else
                But.BackColor = System.Drawing.SystemColors.Control;
        }


        private void UpdateEntireForm()
        {
            UpdateModifiedStatus();            
            UpdateNormalBehaviorStatesEntire();

            GroupLabel.Text = m_speMdl.GroupString;

            if(m_speMdl.szLatinSpeciesName.Length > 0)
            {
                SpeciesLatinNameLabel.Text = m_speMdl.szLatinSpeciesName;
                SpeciesEnglishNameLabel.Text = "(" + m_speMdl.szEnglishSpeciesName + ")";
            }
            else
            {
                SpeciesLatinNameLabel.Text = "";
                SpeciesEnglishNameLabel.Text = "";
            }


            if(m_fileEverWasNamed == false)
            {
                DateLabel.Text = "";
                TimeLabel.Text = "";
                UniqueIDLabel.Text = "";
                SaveVerLabel.Text = "" + m_speMdl.speciesCurrentSuperVer + "." + String.Format("{0:00}", m_speMdl.speciesCurrentSubVer);
            }
            else
            {
                DateLabel.Text = string.Format("{0:00}/{1:00}/{2:00}", m_speMdl.monthSaved, m_speMdl.daySaved, m_speMdl.yearSaved%1000);
                TimeLabel.Text = string.Format("{0:00}:{1:00}:{2:00}", m_speMdl.hourSaved, m_speMdl.minSaved, m_speMdl.secSaved);
                UniqueIDLabel.Text = "" + m_speMdl.semiUniqueId;

                if(m_speMdl.speciesSavedSuperVer != m_speMdl.speciesCurrentSuperVer &&
                    m_speMdl.speciesSavedSubVer != m_speMdl.speciesCurrentSubVer)
                    SpeciesVersopmLabel.Text = "*previous species version:";
                else
                    SpeciesVersopmLabel.Text = "species version:";
                SaveVerLabel.Text = "" + m_speMdl.speciesSavedSuperVer + "." + String.Format("{0:00}", m_speMdl.speciesSavedSubVer);
            }
        }

        private void UpdateNormalBehaviorStatesEntire()
        {
            int i;
            CBehavior mdl;

            // Set the state of normal behavior buttons that have a normal behavior
            // associated with them.
            if(m_speMdl.BehaviorCount < NUM_NRML_BUTTONS)
            {
                AddBehaviorButton.Enabled = true;
                AddBehaviorButton.BackColor = System.Drawing.SystemColors.Control;
            }
            else
            {
                AddBehaviorButton.Enabled = false;
                AddBehaviorButton.BackColor = System.Drawing.SystemColors.GradientInactiveCaption;
            }

            for(i=0; i<NUM_NRML_BUTTONS; i++)
            {
                if(i<m_speMdl.BehaviorCount)
                {
                    mdl = m_speMdl.GetBehaviorCopy(i);

                    // Populated slots
                    m_editBehButton[i].Enabled = true;
                    m_editBehButton[i].BackColor = System.Drawing.SystemColors.Control;

                    if(m_speMdl.BehaviorCount > 1)
                    {
                        m_delBehButton[i].Enabled = true;
                        m_delBehButton[i].BackColor = System.Drawing.Color.SandyBrown;
                    }
                    else
                    {
                        m_delBehButton[i].Enabled = false;
                        m_delBehButton[i].BackColor = System.Drawing.SystemColors.Control;
                    }
                   
                    m_displayBehColorButton[i].Enabled = true;
                    m_displayBehColorButton[i].BackColor = m_currentBehaviorColor[i];

                    // Environmental Atractor Buttons
                    UpdateEnvAttState(true, mdl.envAttrDepth.slopeEnabled, m_shallowWaterButton[i]);
                    UpdateEnvAttState(true, mdl.envAttrDepth.basinEnabled, m_deepWaterButton[i]);
                    UpdateEnvAttState(true, mdl.envAttrDepth.shelfEnabled, m_breakWaterButton[i]);
                    UpdateEnvAttState(true, mdl.envAttrTemp.coldEnabled, m_coldWaterButton[i]);
                    UpdateEnvAttState(true, mdl.envAttrTemp.warmEnabled, m_warmWaterButton[i]);
                    UpdateEnvAttState(true, mdl.envAttrTemp.frontEnabled, m_frontWaterButton[i]);

                    m_behLabel[i].Text = mdl.name;
                    m_behLabel[i].Font =
                        new System.Drawing.Font("Microsoft Sans Serif", 8.25F,
                        System.Drawing.FontStyle.Regular,
                        System.Drawing.GraphicsUnit.Point, ((byte)(0)));
                    m_behLabel[i].Enabled = true;

                }
                else
                {
                    // The unpopulated slot immediatly after populated slots
                    m_editBehButton[i].Enabled = false;
                    m_editBehButton[i].BackColor = System.Drawing.SystemColors.GradientInactiveCaption;

                    m_delBehButton[i].Enabled = false;
                    m_delBehButton[i].BackColor = System.Drawing.SystemColors.GradientInactiveCaption;

                    m_displayBehColorButton[i].Enabled = false;
                    m_displayBehColorButton[i].BackColor = System.Drawing.SystemColors.GradientInactiveCaption;

                    // Environmental Atractor Buttons
                    UpdateEnvAttState(false, false, m_shallowWaterButton[i]);
                    UpdateEnvAttState(false, false, m_deepWaterButton[i]);
                    UpdateEnvAttState(false, false, m_breakWaterButton[i]);
                    UpdateEnvAttState(false, false, m_coldWaterButton[i]);
                    UpdateEnvAttState(false, false, m_warmWaterButton[i]);
                    UpdateEnvAttState(false, false, m_frontWaterButton[i]);

                    m_behLabel[i].Text = "open...";
                    m_behLabel[i].Font =
                        new System.Drawing.Font("Microsoft Sans Serif", 8.25F, 
                        System.Drawing.FontStyle.Italic,
                        System.Drawing.GraphicsUnit.Point, ((byte)(0)));
                    m_behLabel[i].Enabled = false;
                }
            }
        }

        private void InitialBehaviorButton_Click(object sender, EventArgs e)
        {
            CInitialBehaviorSpanMgr copy = m_speMdl.InitBehSpanMgr.GetCopy();

            FormBehTrans dlg = new FormBehTrans(
                copy,
                m_speMdl.BehaviorCount,
                m_speMdl.NameArray);

            dlg.ShowDialog(this);
            if(dlg.modified == true)
            {
                m_speMdl.InitBehSpanMgr = (CInitialBehaviorSpanMgr)dlg.NrmlBehTransMdl;
                UpdateModifiedStatus();
                Run3MB(false, false);
                //Run3MB(true, true);
            }
            this.BringToFront();
        }

        private void AcousticAversionButton_Click(object sender, EventArgs e)
        {
            FormAcstcAvrsn dlg = new FormAcstcAvrsn(m_speMdl);
            dlg.ShowDialog(this);
            this.BringToFront();
            if(dlg.Modified == true)
                UpdateModifiedStatus();
        }
 
        private void speciesToTextToolStripMenuItem_Click(object sender, EventArgs e)
        {
            string szTemp = m_szFileName;
            if(szTemp.Length > 4 && szTemp[szTemp.Length-4] == '.')
            {
                szTemp = szTemp.Remove(szTemp.Length-4, 4);
                szTemp = szTemp + ".txt";
            }

            if(m_fileNamed == true)
            {
                m_speMdl.SaveToBinFile(szTemp, true);
            }
            else
            {
                SaveFileDialog dlg = new SaveFileDialog();
                dlg.CheckFileExists = false;
                dlg.Filter = "3MBS Species Models Text (*.txt)|*.txt";
                if(m_fileEverWasNamed == true)
                    dlg.FileName = szTemp; // use whatever the last name was.
                
                DialogResult result = dlg.ShowDialog();
                this.BringToFront();

                if(result == DialogResult.Cancel)
                    return;
                m_speMdl.SaveToBinFile(dlg.FileName, true);
            }
        }

        private void SpeciesDescription_Click(object sender, EventArgs e)
        {
            FormSpeciesDescription dlg = new FormSpeciesDescription(m_speMdl);
            dlg.ShowDialog();
            this.BringToFront();
            UpdateModifiedStatus();
            //Run3MB();
        }


        private void DepthValuesButton_Click(object sender, EventArgs e)
        {
            FormDepthValues dlg = new FormDepthValues(m_speMdl);
            dlg.ShowDialog();
            UpdateModifiedStatus();
            this.BringToFront();
            Run3MB(false, false);
        }


        private void SetSpeMembrshp(object sender, EventArgs e)
        {
            m_speMdl.SpeciesIndex = GuiUtils.MatchIndex(m_speciesSelTooStrip, (ToolStripMenuItem)sender); ;
            UpdateEntireForm();
        }


        private int VerifySaveOldBeforeOpenNew()
        {
            if(m_speMdl.NeedSave == true)
            {
                FormConfirmDecision dlg = new FormConfirmDecision();
                dlg.button3Visible = true;
                dlg.Text = "Save Species Model?";
                dlg.messageString = "Current Model Has Been Modified But Not Saved!";
                dlg.button1String = "Do Not Save";
                dlg.button2String = "Save First";
                dlg.button3String = "Cancel";
                dlg.SetLocation(DoneButton);
                dlg.ShowDialog(this);
                return dlg.buttonSelected;
            }
            return 0;
        }

        private void toolStripButtonNew_Click(object sender, EventArgs e)
        {
            switch(VerifySaveOldBeforeOpenNew())
            {
            case 1: // Do Not Save
                break;
            case 2: // Save and continue
                Save();
                break;
            case 3:// Cancel
                return;
            default:
                break;
            }
            SetUpFresh();
        }


        // Returns true if (attempted) saved.
        private Boolean Save()
        {
            bool ret = false;
            if(m_fileNamed == true)
            {
                m_speMdl.SaveToBinFile(m_szFileName, false);
                ret = true;
            }
            else
            {
                SaveFileDialog dlg = new SaveFileDialog();
                dlg.CheckFileExists = false;
                dlg.Filter = "3MBS Species Models (*.spe)|*.spe";
                if(m_fileEverWasNamed == true)
                    dlg.FileName = m_szFileName;

                DialogResult result = dlg.ShowDialog();

                if(result == DialogResult.Cancel)
                    return false;

                m_fileNamed = true;
                m_fileEverWasNamed = true;
                ret = true;
                m_szFileName = dlg.FileName;
                m_speMdl.SaveToBinFile(m_szFileName, false);
            }
            this.BringToFront();
            UpdateEntireForm();
            return ret;
        }

        private void Open()
        {
            C3mbSpeciesModel mdl = new C3mbSpeciesModel(); ;
            mbsRESULT res;
            switch(VerifySaveOldBeforeOpenNew())
            {
            case 1: // Do Not Save
                break;
            case 2: // Save and continue
                Save();
                break;
            case 3:// Cancel
                return;
            default:
                break;
            }

            OpenFileDialog dlg = new OpenFileDialog();
            dlg.CheckFileExists = true;
            dlg.Filter = "3MBS Species Models (*.spe)|*.spe";

            if(m_fileNamed == true)
                dlg.FileName = m_szFileName;

            DialogResult result = dlg.ShowDialog();
            this.BringToFront();
            if(result == DialogResult.Cancel)
                return;

            m_fileNamed = true;
            m_fileEverWasNamed = true;
            m_szFileName = dlg.FileName;
            if(mbsRESULT.OK != (res = mdl.LoadFromBinFile(m_szFileName)))
            {
                // message box.
                FmMsgBox msgBox = new FmMsgBox();
                msgBox.Text = "Load Species File Error";
                msgBox.labelText = m_mmmbs.MbsResultToString(res);

                if(res == mbsRESULT.OBSOLETE_SPECIES_VERSION)
                {
                    msgBox.labelText = msgBox.labelText +
                        "\n\nPlease contact support.";
                }
                else if(res == mbsRESULT.OBSOLETE_3MBS_VERSION)
                {
                    msgBox.labelText = msgBox.labelText +
                        "\n\nPlease download the latest 3MB package to load this species.";
                }
                msgBox.ShowDialog(this);
                return;
            }
            m_additionalSimHours = 0;
            m_bathyDepth = DEFAULT_BATHYMETRY_DEPTH;
            AdditionalHoursButton.Text = "Additional Hours " + m_additionalSimHours;
            m_speMdl = mdl;
            UpdateEntireForm();
            Run3MB(false, false);
        }


        private void DoneButton_Click(object sender, EventArgs e)
        {
            if(m_speMdl.NeedSave == true)
            {
                FormConfirmDecision dlg = new FormConfirmDecision();
                dlg.button3Visible = true;
                dlg.Text = "File Not Saved!";
                dlg.messageString = "Save Species Model Before Exiting?";
                dlg.button3String = "Save And Exit";
                dlg.button1String = "Cancel";
                dlg.button2String = "Exit Without\nSaving";
                dlg.SetLocation(DoneButton);
                dlg.ShowDialog(this);
                if(dlg.buttonSelected == 1) // cancel
                {
                    this.BringToFront();
                    return;
                }
                else if(dlg.buttonSelected == 2) // Exit without saving
                {
                    Dispose();
                }
                else if(dlg.buttonSelected == 3) // Save and exit.
                {
                    if(Save() == false)
                        return;
                    Dispose();
                }
            }
            Dispose();
        }
        private void DoneToolStripMenuItem_Click(object sender, EventArgs e)
        {
            DoneButton_Click(sender, e);
        }

        // Bitmap related
        private void FormSpeciesDefinition_MouseDown(object sender, MouseEventArgs e)
        {

            if(m_bitmapMgr.MouseEventWithinDiveProfileDataBitmap(e) == true)
            {
                if(e.Button == MouseButtons.Left)
                    m_bathyDepth = -m_bitmapMgr.MouseEventToDepthValue(e);
                else if(e.Button == MouseButtons.Right)
                    m_bathyDepth = DEFAULT_BATHYMETRY_DEPTH;

                DepthInputButton.Text = "Bathy Depth:" + string.Format("{0:0.000000}", m_bathyDepth);

                m_mmmbs.SetBaythyConstantDepth(m_bathyDepth);
                Run3MB(true, true);
            }
            else
            {
                m_bitmapMgr.HandleMouseDown(e);
            }
        }

        private void FormSpeciesDefinition_MouseMove(object sender, MouseEventArgs e)
        {
            m_bitmapMgr.HandleMouseMove(e);
        }

        private void FormSpeciesDefinition_MouseUp(object sender, MouseEventArgs e)
        {
            m_bitmapMgr.HandleMouseUp(e);
        }

        private void saveToolStripMenuItem_Click(object sender, EventArgs e) { Save(); }
        private void SaveButton_Click(object sender, EventArgs e) { Save(); }
        private void saveAsToolStripMenuItem_Click(object sender, EventArgs e) { m_fileNamed = false; Save(); }
        private void toolStripButtonSave_Click(object sender, EventArgs e) { Save(); }

        private void toolStripButtonOpen_Click(object sender, EventArgs e) { Open(); }
        private void OpenButton_Click(object sender, EventArgs e) { Open(); }
        private void openToolStripMenuItem_Click(object sender, EventArgs e) { Open(); }


        private void CopyNormalButtons()
        {
            int i;
            // To make accessing easier...
            m_editBehButton[0] = EditBehButton00;
            m_editBehButton[1] = EditBehButton01;
            m_editBehButton[2] = EditBehButton02;
            m_editBehButton[3] = EditBehButton03;
            m_editBehButton[4] = EditBehButton04;
            m_editBehButton[5] = EditBehButton05;
            m_editBehButton[6] = EditBehButton06;
            m_editBehButton[7] = EditBehButton07;

            m_delBehButton[0] = DeleteBehButton00;
            m_delBehButton[1] = DeleteBehButton01;
            m_delBehButton[2] = DeleteBehButton02;
            m_delBehButton[3] = DeleteBehButton03;
            m_delBehButton[4] = DeleteBehButton04;
            m_delBehButton[5] = DeleteBehButton05;
            m_delBehButton[6] = DeleteBehButton06;
            m_delBehButton[7] = DeleteBehButton07;

            m_displayBehColorButton[0] = DisplayColorButton00;
            m_displayBehColorButton[1] = DisplayColorButton01;
            m_displayBehColorButton[2] = DisplayColorButton02;
            m_displayBehColorButton[3] = DisplayColorButton03;
            m_displayBehColorButton[4] = DisplayColorButton04;
            m_displayBehColorButton[5] = DisplayColorButton05;
            m_displayBehColorButton[6] = DisplayColorButton06;
            m_displayBehColorButton[7] = DisplayColorButton07;

            m_behaviorColor[0] = BMPCONSTS.BEHAVIOR_COLOR[0];;
            m_behaviorColor[1] = Color.SteelBlue;
            m_behaviorColor[2] = Color.Olive;
            m_behaviorColor[3] = Color.Brown;
            m_behaviorColor[4] = Color.Aqua;
            m_behaviorColor[5] = Color.Black;
            m_behaviorColor[6] = Color.Magenta;

            m_behaviorColor[7] = Color.Salmon;
            //m_behaviorColor[8] = Color.Aquamarine;
            //m_behaviorColor[9] = Color.Yellow;
            //m_behaviorColor[10] = Color.RosyBrown;
            //m_behaviorColor[11] = Color.Chartreuse;
            //m_behaviorColor[12] = Color.Gray;
            //m_behaviorColor[13] = Color.Teal;
            //m_behaviorColor[14] = Color.Orange;


            // use Color.OrangeRed for acoustic aversion.

            m_currentBehaviorColor[0] = m_behaviorColor[0];
            m_currentBehaviorColor[1] = m_behaviorColor[1];
            m_currentBehaviorColor[2] = m_behaviorColor[2];
            m_currentBehaviorColor[3] = m_behaviorColor[3];
            m_currentBehaviorColor[4] = m_behaviorColor[4];
            m_currentBehaviorColor[5] = m_behaviorColor[5];
            m_currentBehaviorColor[6] = m_behaviorColor[6];
            m_currentBehaviorColor[7] = m_behaviorColor[7];

            m_shallowWaterButton[0] = ShallowEnvAttButton00;
            m_shallowWaterButton[1] = ShallowEnvAttButton01;
            m_shallowWaterButton[2] = ShallowEnvAttButton02;
            m_shallowWaterButton[3] = ShallowEnvAttButton03;
            m_shallowWaterButton[4] = ShallowEnvAttButton04;
            m_shallowWaterButton[5] = ShallowEnvAttButton05;
            m_shallowWaterButton[6] = ShallowEnvAttButton06;
            m_shallowWaterButton[7] = ShallowEnvAttButton07;

            m_deepWaterButton[0] = DeepEnvAttButton00;
            m_deepWaterButton[1] = DeepEnvAttButton01;
            m_deepWaterButton[2] = DeepEnvAttButton02;
            m_deepWaterButton[3] = DeepEnvAttButton03;
            m_deepWaterButton[4] = DeepEnvAttButton04;
            m_deepWaterButton[5] = DeepEnvAttButton05;
            m_deepWaterButton[6] = DeepEnvAttButton06;
            m_deepWaterButton[7] = DeepEnvAttButton07;

            m_breakWaterButton[0] = BreakEnvAttButton00;
            m_breakWaterButton[1] = BreakEnvAttButton01;
            m_breakWaterButton[2] = BreakEnvAttButton02;
            m_breakWaterButton[3] = BreakEnvAttButton03;
            m_breakWaterButton[4] = BreakEnvAttButton04;
            m_breakWaterButton[5] = BreakEnvAttButton05;
            m_breakWaterButton[6] = BreakEnvAttButton06;
            m_breakWaterButton[7] = BreakEnvAttButton07;

            m_coldWaterButton[0] = ColdEnvAttButton00;
            m_coldWaterButton[1] = ColdEnvAttButton01;
            m_coldWaterButton[2] = ColdEnvAttButton02;
            m_coldWaterButton[3] = ColdEnvAttButton03;
            m_coldWaterButton[4] = ColdEnvAttButton04;
            m_coldWaterButton[5] = ColdEnvAttButton05;
            m_coldWaterButton[6] = ColdEnvAttButton06;
            m_coldWaterButton[7] = ColdEnvAttButton07;

            m_warmWaterButton[0] = WarmEnvAttButton00;
            m_warmWaterButton[1] = WarmEnvAttButton01;
            m_warmWaterButton[2] = WarmEnvAttButton02;
            m_warmWaterButton[3] = WarmEnvAttButton03;
            m_warmWaterButton[4] = WarmEnvAttButton04;
            m_warmWaterButton[5] = WarmEnvAttButton05;
            m_warmWaterButton[6] = WarmEnvAttButton06;
            m_warmWaterButton[7] = WarmEnvAttButton07;

            m_frontWaterButton[0] = FrontEnvAttButton00;
            m_frontWaterButton[1] = FrontEnvAttButton01;
            m_frontWaterButton[2] = FrontEnvAttButton02;
            m_frontWaterButton[3] = FrontEnvAttButton03;
            m_frontWaterButton[4] = FrontEnvAttButton04;
            m_frontWaterButton[5] = FrontEnvAttButton05;
            m_frontWaterButton[6] = FrontEnvAttButton06;
            m_frontWaterButton[7] = FrontEnvAttButton07;

            m_behLabel[0] = NmlBehLabel00;
            m_behLabel[1] = NmlBehLabel01;
            m_behLabel[2] = NmlBehLabel02;
            m_behLabel[3] = NmlBehLabel03;
            m_behLabel[4] = NmlBehLabel04;
            m_behLabel[5] = NmlBehLabel05;
            m_behLabel[6] = NmlBehLabel06;
            m_behLabel[7] = NmlBehLabel07;

            for(i = 0; i < m_speciesSelTooStrip.Length; i++)
                m_speciesSelTooStrip[i] = null;


            //m_speciesSelTooStrip[0] = spe000ToolStripMenuItem; // Non existant

            //----------------------------------//
            // ODONTOCETES (HF SPECIALIST) begin
            //----------------------------------//
            m_speciesSelTooStrip[1] = spe001ToolStripMenuItem;
            m_speciesSelTooStrip[2] = spe002ToolStripMenuItem;
            m_speciesSelTooStrip[3] = spe003ToolStripMenuItem;
            m_speciesSelTooStrip[4] = spe004ToolStripMenuItem;
            m_speciesSelTooStrip[5] = spe005ToolStripMenuItem;
            //m_speciesSelTooStrip[6] = spe006ToolStripMenuItem;
            //m_speciesSelTooStrip[7] = spe007ToolStripMenuItem;
            //m_speciesSelTooStrip[8] = spe008ToolStripMenuItem;
            m_speciesSelTooStrip[9] = spe009ToolStripMenuItem;

            m_speciesSelTooStrip[10] = spe010ToolStripMenuItem;
            m_speciesSelTooStrip[11] = spe011ToolStripMenuItem;
            m_speciesSelTooStrip[12] = spe012ToolStripMenuItem;
            //m_speciesSelTooStrip[13] = spe013ToolStripMenuItem;
            //m_speciesSelTooStrip[14] = spe014ToolStripMenuItem;
            //m_speciesSelTooStrip[15] = spe015ToolStripMenuItem;
            m_speciesSelTooStrip[16] = spe016ToolStripMenuItem;
            m_speciesSelTooStrip[17] = spe017ToolStripMenuItem;
            //m_speciesSelTooStrip[18] = spe018ToolStripMenuItem;
            //m_speciesSelTooStrip[19] = spe019ToolStripMenuItem;

            m_speciesSelTooStrip[20] = spe020ToolStripMenuItem;
            m_speciesSelTooStrip[21] = spe021ToolStripMenuItem;
            //m_speciesSelTooStrip[22] = spe022ToolStripMenuItem;
            //m_speciesSelTooStrip[23] = spe023ToolStripMenuItem;
            m_speciesSelTooStrip[24] = spe024ToolStripMenuItem;
            m_speciesSelTooStrip[25] = spe025ToolStripMenuItem;
            //m_speciesSelTooStrip[26] = spe026ToolStripMenuItem;
            //m_speciesSelTooStrip[27] = spe027ToolStripMenuItem;
            m_speciesSelTooStrip[28] = spe028ToolStripMenuItem;
            //m_speciesSelTooStrip[29] = spe029ToolStripMenuItem;

            //m_speciesSelTooStrip[30] = spe030ToolStripMenuItem;

            //-----------------------------------//
            //	ODONTOCETES (MF SPECIALIST) begin
            //-----------------------------------//
            m_speciesSelTooStrip[31] = spe031ToolStripMenuItem;
            m_speciesSelTooStrip[32] = spe032ToolStripMenuItem;
            m_speciesSelTooStrip[33] = spe033ToolStripMenuItem;
            m_speciesSelTooStrip[34] = spe034ToolStripMenuItem;
            m_speciesSelTooStrip[35] = spe035ToolStripMenuItem;
            m_speciesSelTooStrip[36] = spe036ToolStripMenuItem;
            m_speciesSelTooStrip[37] = spe037ToolStripMenuItem;
            m_speciesSelTooStrip[38] = spe038ToolStripMenuItem;
            m_speciesSelTooStrip[39] = spe039ToolStripMenuItem;

            m_speciesSelTooStrip[40] = spe040ToolStripMenuItem;
            m_speciesSelTooStrip[41] = spe041ToolStripMenuItem;
            m_speciesSelTooStrip[42] = spe042ToolStripMenuItem;
            m_speciesSelTooStrip[43] = spe043ToolStripMenuItem;
            m_speciesSelTooStrip[44] = spe044ToolStripMenuItem;
            m_speciesSelTooStrip[45] = spe045ToolStripMenuItem;
            m_speciesSelTooStrip[46] = spe046ToolStripMenuItem;
            m_speciesSelTooStrip[47] = spe047ToolStripMenuItem;
            m_speciesSelTooStrip[48] = spe048ToolStripMenuItem;
            m_speciesSelTooStrip[49] = spe049ToolStripMenuItem;

            m_speciesSelTooStrip[50] = spe050ToolStripMenuItem;
            m_speciesSelTooStrip[51] = spe051ToolStripMenuItem;
            m_speciesSelTooStrip[52] = spe052ToolStripMenuItem;
            m_speciesSelTooStrip[53] = spe053ToolStripMenuItem;
            m_speciesSelTooStrip[54] = spe054ToolStripMenuItem;
            m_speciesSelTooStrip[55] = spe055ToolStripMenuItem;
            m_speciesSelTooStrip[56] = spe056ToolStripMenuItem;
            m_speciesSelTooStrip[57] = spe057ToolStripMenuItem;
            m_speciesSelTooStrip[58] = spe058ToolStripMenuItem;
            m_speciesSelTooStrip[59] = spe059ToolStripMenuItem;

            //m_speciesSelTooStrip[60] = spe060ToolStripMenuItem;
            //m_speciesSelTooStrip[61] = spe061ToolStripMenuItem;
            //m_speciesSelTooStrip[62] = spe062ToolStripMenuItem;
            //m_speciesSelTooStrip[63] = spe063ToolStripMenuItem;
            //m_speciesSelTooStrip[64] = spe064ToolStripMenuItem;
            //m_speciesSelTooStrip[65] = spe065ToolStripMenuItem;
            m_speciesSelTooStrip[66] = spe066ToolStripMenuItem;
            m_speciesSelTooStrip[67] = spe067ToolStripMenuItem;
            m_speciesSelTooStrip[68] = spe068ToolStripMenuItem;
            m_speciesSelTooStrip[69] = spe069ToolStripMenuItem;

            m_speciesSelTooStrip[70] = spe070ToolStripMenuItem;
            m_speciesSelTooStrip[71] = spe071ToolStripMenuItem;
            m_speciesSelTooStrip[72] = spe072ToolStripMenuItem;
            m_speciesSelTooStrip[73] = spe073ToolStripMenuItem;
            m_speciesSelTooStrip[74] = spe074ToolStripMenuItem;
            m_speciesSelTooStrip[75] = spe075ToolStripMenuItem;
            m_speciesSelTooStrip[76] = spe076ToolStripMenuItem;
            m_speciesSelTooStrip[77] = spe077ToolStripMenuItem;
            m_speciesSelTooStrip[78] = spe078ToolStripMenuItem;
            m_speciesSelTooStrip[79] = spe079ToolStripMenuItem;

            m_speciesSelTooStrip[80] = spe080ToolStripMenuItem;
            m_speciesSelTooStrip[81] = spe081ToolStripMenuItem;
            m_speciesSelTooStrip[82] = spe082ToolStripMenuItem;
            m_speciesSelTooStrip[83] = spe083ToolStripMenuItem;
            m_speciesSelTooStrip[84] = spe084ToolStripMenuItem;
            //m_speciesSelTooStrip[85] = spe085ToolStripMenuItem;
            //m_speciesSelTooStrip[86] = spe086ToolStripMenuItem;
            //m_speciesSelTooStrip[87] = spe087ToolStripMenuItem;
            //m_speciesSelTooStrip[88] = spe088ToolStripMenuItem;
            //m_speciesSelTooStrip[89] = spe089ToolStripMenuItem;

            m_speciesSelTooStrip[90] = spe090ToolStripMenuItem;
            //m_speciesSelTooStrip[91] = spe091ToolStripMenuItem;
            //m_speciesSelTooStrip[92] = spe092ToolStripMenuItem;
            m_speciesSelTooStrip[93] = spe093ToolStripMenuItem;
            m_speciesSelTooStrip[94] = spe094ToolStripMenuItem;
            m_speciesSelTooStrip[95] = spe095ToolStripMenuItem;
            //m_speciesSelTooStrip[96] = spe096ToolStripMenuItem;
            //m_speciesSelTooStrip[97] = spe097ToolStripMenuItem;

            //-----------------------------------//
            //	MYSTICETES (LF SPECIALISTS) begin
            //-----------------------------------//
            m_speciesSelTooStrip[98] = spe098ToolStripMenuItem;
            m_speciesSelTooStrip[99] = spe099ToolStripMenuItem;

            m_speciesSelTooStrip[100] = spe100ToolStripMenuItem;
            //m_speciesSelTooStrip[101] = spe101ToolStripMenuItem;
            //m_speciesSelTooStrip[102] = spe102ToolStripMenuItem;
            m_speciesSelTooStrip[103] = spe103ToolStripMenuItem;
            m_speciesSelTooStrip[104] = spe104ToolStripMenuItem;
            m_speciesSelTooStrip[105] = spe105ToolStripMenuItem;
            m_speciesSelTooStrip[106] = spe106ToolStripMenuItem;
            m_speciesSelTooStrip[107] = spe107ToolStripMenuItem;
            m_speciesSelTooStrip[108] = spe108ToolStripMenuItem;
            //m_speciesSelTooStrip[109] = spe109ToolStripMenuItem;

            //m_speciesSelTooStrip[110] = spe110ToolStripMenuItem;
            m_speciesSelTooStrip[111] = spe111ToolStripMenuItem;
            //m_speciesSelTooStrip[112] = spe112ToolStripMenuItem;
            //m_speciesSelTooStrip[113] = spe113ToolStripMenuItem;
            m_speciesSelTooStrip[114] = spe114ToolStripMenuItem;
            //m_speciesSelTooStrip[115] = spe115ToolStripMenuItem;
            //m_speciesSelTooStrip[116] = spe116ToolStripMenuItem;

            //------------------//
            // PINNIPED (PHOCID)
            //------------------//
            m_speciesSelTooStrip[117] = spe117ToolStripMenuItem;
            m_speciesSelTooStrip[118] = spe118ToolStripMenuItem;
            m_speciesSelTooStrip[119] = spe119ToolStripMenuItem;

            m_speciesSelTooStrip[120] = spe120ToolStripMenuItem;
            m_speciesSelTooStrip[121] = spe121ToolStripMenuItem;
            m_speciesSelTooStrip[122] = spe122ToolStripMenuItem;
            m_speciesSelTooStrip[123] = spe123ToolStripMenuItem;
            m_speciesSelTooStrip[124] = spe124ToolStripMenuItem;
            m_speciesSelTooStrip[125] = spe125ToolStripMenuItem;
            m_speciesSelTooStrip[126] = spe126ToolStripMenuItem;
            //m_speciesSelTooStrip[127] = spe127ToolStripMenuItem;
            //m_speciesSelTooStrip[128] = spe128ToolStripMenuItem;
            //m_speciesSelTooStrip[129] = spe129ToolStripMenuItem;

            //m_speciesSelTooStrip[130] = spe130ToolStripMenuItem;
            m_speciesSelTooStrip[131] = spe131ToolStripMenuItem;
            m_speciesSelTooStrip[132] = spe132ToolStripMenuItem;
            m_speciesSelTooStrip[133] = spe133ToolStripMenuItem;
            m_speciesSelTooStrip[134] = spe134ToolStripMenuItem;
            m_speciesSelTooStrip[135] = spe135ToolStripMenuItem;
            m_speciesSelTooStrip[136] = spe136ToolStripMenuItem;
            m_speciesSelTooStrip[137] = spe137ToolStripMenuItem;
            m_speciesSelTooStrip[138] = spe138ToolStripMenuItem;
            //m_speciesSelTooStrip[139] = spe139ToolStripMenuItem;

            //m_speciesSelTooStrip[140] = spe140ToolStripMenuItem;
            //m_speciesSelTooStrip[141] = spe141ToolStripMenuItem;

            //-------------------------//
            // PINNIPED (OTARRID) begin
            //-------------------------//
            m_speciesSelTooStrip[142] = spe142ToolStripMenuItem;
            m_speciesSelTooStrip[143] = spe143ToolStripMenuItem;
            m_speciesSelTooStrip[144] = spe144ToolStripMenuItem;
            m_speciesSelTooStrip[145] = spe145ToolStripMenuItem;
            m_speciesSelTooStrip[146] = spe146ToolStripMenuItem;
            //m_speciesSelTooStrip[147] = spe147ToolStripMenuItem;
            //m_speciesSelTooStrip[148] = spe148ToolStripMenuItem;
            //m_speciesSelTooStrip[149] = spe149ToolStripMenuItem;

            m_speciesSelTooStrip[150] = spe150ToolStripMenuItem;
            m_speciesSelTooStrip[151] = spe151ToolStripMenuItem;
            m_speciesSelTooStrip[152] = spe152ToolStripMenuItem;
            m_speciesSelTooStrip[153] = spe153ToolStripMenuItem;
            m_speciesSelTooStrip[154] = spe154ToolStripMenuItem;
            m_speciesSelTooStrip[155] = spe155ToolStripMenuItem;
            m_speciesSelTooStrip[156] = spe156ToolStripMenuItem;
            m_speciesSelTooStrip[157] = spe157ToolStripMenuItem;
            m_speciesSelTooStrip[158] = spe158ToolStripMenuItem;
            //m_speciesSelTooStrip[159] = spe159ToolStripMenuItem;

            //m_speciesSelTooStrip[160] = spe160ToolStripMenuItem;
            //m_speciesSelTooStrip[161] = spe161ToolStripMenuItem;
            //m_speciesSelTooStrip[162] = spe162ToolStripMenuItem;

            //-----------------------------//
            // Special Considerations begin
            //-----------------------------//
            m_speciesSelTooStrip[163] = spe163ToolStripMenuItem;
            //m_speciesSelTooStrip[164] = spe164ToolStripMenuItem;
            //m_speciesSelTooStrip[165] = spe165ToolStripMenuItem;

            //---------------------------//
            // Other Marine Mammals Begin
            //---------------------------//
            m_speciesSelTooStrip[166] = spe166ToolStripMenuItem;
            //m_speciesSelTooStrip[167] = spe167ToolStripMenuItem;
            //m_speciesSelTooStrip[168] = spe168ToolStripMenuItem;
            m_speciesSelTooStrip[169] = spe169ToolStripMenuItem;

            //m_speciesSelTooStrip[170] = spe170ToolStripMenuItem;
            //m_speciesSelTooStrip[171] = spe171ToolStripMenuItem;
            m_speciesSelTooStrip[172] = spe172ToolStripMenuItem;
            m_speciesSelTooStrip[173] = spe173ToolStripMenuItem;
            m_speciesSelTooStrip[174] = spe174ToolStripMenuItem;
            //m_speciesSelTooStrip[175] = spe175ToolStripMenuItem;
            //m_speciesSelTooStrip[176] = spe176ToolStripMenuItem;
            m_speciesSelTooStrip[177] = spe177ToolStripMenuItem;
            m_speciesSelTooStrip[178] = spe178ToolStripMenuItem;
            //m_speciesSelTooStrip[179] = spe179ToolStripMenuItem;

            //m_speciesSelTooStrip[180] = spe180ToolStripMenuItem;
            m_speciesSelTooStrip[181] = spe181ToolStripMenuItem;
            //m_speciesSelTooStrip[182] = spe182ToolStripMenuItem;
            //m_speciesSelTooStrip[183] = spe183ToolStripMenuItem;

            //------------------//
            // Sea Turtles Begin
            //------------------//
            m_speciesSelTooStrip[184] = spe184ToolStripMenuItem;
            m_speciesSelTooStrip[185] = spe185ToolStripMenuItem;
            m_speciesSelTooStrip[186] = spe186ToolStripMenuItem;
            m_speciesSelTooStrip[187] = spe187ToolStripMenuItem;
            m_speciesSelTooStrip[188] = spe188ToolStripMenuItem;
            m_speciesSelTooStrip[189] = spe189ToolStripMenuItem;

            //m_speciesSelTooStrip[190] = spe190ToolStripMenuItem;
            //m_speciesSelTooStrip[191] = spe191ToolStripMenuItem;
            //m_speciesSelTooStrip[192] = spe192ToolStripMenuItem;

            // Future Use
            //m_speciesSelTooStrip[193] = spe193ToolStripMenuItem;
            //m_speciesSelTooStrip[194] = spe194ToolStripMenuItem;
            //m_speciesSelTooStrip[195] = spe195ToolStripMenuItem;
            //m_speciesSelTooStrip[196] = spe196ToolStripMenuItem;
            //m_speciesSelTooStrip[197] = spe197ToolStripMenuItem;
            //m_speciesSelTooStrip[198] = spe198ToolStripMenuItem;
            //m_speciesSelTooStrip[199] = spe199ToolStripMenuItem;
            //if((C3mbsSpeciesModel.GetSpeciesLatinNameString(i)).Length > 0)
        }

        private void DepthInputButton_Click(object sender, EventArgs e)
        {
            //FormSpanInt frm;
            //frm = new FormSpanInt("Transition Span", "Enter depth span:", 9, 8, 9, 8, "< Animat Depth <=");
            //frm.ShowDialog();
            //this.BringToFront();
        }

        private void ToggleTargetDepthDisplayButton_Click(object sender, EventArgs e)
        {
            m_bitmapMgr.DisplayTargetDepth = !m_bitmapMgr.DisplayTargetDepth;
            Invalidate();
        }

        private void ToggleBathymetryDisplayButton_Click(object sender, EventArgs e)
        {
            m_bitmapMgr.DisplayBathy = !m_bitmapMgr.DisplayBathy;
            Invalidate();
        }

    }
}