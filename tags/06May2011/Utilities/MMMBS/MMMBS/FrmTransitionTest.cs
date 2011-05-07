using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using MMMBSLib;  // c# code code and data types.
using mbs; // C++ wrapper code and data types
using System.Diagnostics;



namespace MBSGUI
{
    public partial class FrmTransitionTest : Form
    {
        private CBitmapSingleBehaviorTimeTransitionManager m_bitmapMgr;
        private CMatrix m_matrix;
        MBSDEFAULTS.BITMAPDISPLAYTYPE m_displayType;
        int m_behaviorIndex;
        string[] m_behaviorNameArray;
        int m_numBehaviors;
        int m_numTrials;
        GroupBox[] m_transitionGroupBoxArray = new GroupBox[MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS];
        Label[] m_transitionLabelArray = new Label[MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS];
        Label[] m_transitionLabelNumberArray = new Label[MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS];
        mbsSNGLBEHTRANSTRUCT m_displayData;


        static int NORMAL_TRIALS_FACTOR = 200;


        public FrmTransitionTest(MBSDEFAULTS.BITMAPDISPLAYTYPE Type, CMatrix Matrix, string[] BehNameArray)
        {
            InitializeComponent();

            Text = "Intitial Behavior Trials";
            BehaviorNumberLabel.Visible = false;
            BehaviorNameLabel.Visible = false;
            label6.Visible = false;
            DurationTextBoxMax.Visible = false;
            DurationTextBoxAve.Visible = false;
            DurationTextBoxMin.Visible = false;

            m_displayType = Type;
            m_behaviorIndex = -1;
            m_matrix = Matrix;
            m_behaviorNameArray = BehNameArray;
            m_numBehaviors = Matrix.ColumnCount - 3;
            Construct();
        }
        public FrmTransitionTest(MBSDEFAULTS.BITMAPDISPLAYTYPE Type, CMatrix Matrix, string[] BehNameArray, int BehIndex)
        {
            InitializeComponent();

            if(Type == MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION)
                Text = BehNameArray[BehIndex] + " (Behavior " + (BehIndex + 1) + ") Transition Trials";
            else
                Text = "Initial Behavior Trials";

            m_displayType = Type;
            m_behaviorIndex = BehIndex;
            m_matrix = Matrix;
            m_behaviorNameArray = BehNameArray;
            m_numBehaviors = Matrix.ColumnCount - 5;
            Construct();
        }
        void Construct()
        {
            int i;
            Rectangle transitionRect;
            Rectangle maxTransTimeRect;
            Rectangle aveTransTimeRect;
            Rectangle minTransTimeRect;
            Rectangle[] behColorRectArray;

            // This assert could go away if this form were to ever need to be used for a matrix
            // without colums but is left in for now because it isn't being used that
            // way currently.  For now at the very least there will always be a need
            // for 1+ the number of behavior number of columns.
            CopyControls();

            m_numTrials = NORMAL_TRIALS_FACTOR/m_matrix.RowCount;
            NumTrialsButton.Text = "" + m_numTrials;

            //--------------------------------------------------------------------------//
            // Set up the results bitmap display (the main display on the form).
            //-----------------------------------------------------------------//
            // Copy the attributes from the group box that defines the display area into
            // the rectangle passed into the bitmap manager
            transitionRect = new Rectangle();
            transitionRect.X = DisplayGroupBox.Location.X;
            transitionRect.Y = DisplayGroupBox.Location.Y;
            transitionRect.Width = DisplayGroupBox.Width;
            transitionRect.Height = DisplayGroupBox.Height;


            //--------------------------------------------------------------------------//
            // Allocate and and format the behavior key on the form
            //-----------------------------------------------------//
            if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.INITIAL_BEHAVIOR)
            {
                // Initial behavior display
                // Do not display a transition-from-behavior behavior name.
                BehaviorNameLabel.Text = "";
                BehaviorNumberLabel.Visible = false;
            }
            else
            {
                // Behavior transition display
                // Display a transition-from-behavior behavior name.
                BehaviorNameLabel.Text = m_behaviorNameArray[m_behaviorIndex];
                BehaviorNumberLabel.Visible = true;
                BehaviorNumberLabel.Text = "" + "(" + (m_behaviorIndex+1) + ")";
            }

            // Fill in names on the behavior key.  Put empty strings on unused behaviors.
            for(i=0; i<MBGUICONSTANTS.MAXNUMBEHAVIORS; i++)
            {
                if(i<m_numBehaviors)
                {
                    m_transitionLabelArray[i].Text = m_behaviorNameArray[i];
                    m_transitionLabelNumberArray[i].Visible = true;
                }
                else
                {
                    m_transitionLabelArray[i].Text = "";
                    m_transitionLabelNumberArray[i].Visible = false;
                }
            }

            // Copy the attributes from the group box that defines the size and placement
            // of the maximum transition color-code key display area into a rectangle
            // passed into the bitmap manager
            maxTransTimeRect = new Rectangle();
            maxTransTimeRect.X = DurationGroupBoxMax.Location.X;
            maxTransTimeRect.Y = DurationGroupBoxMax.Location.Y;
            maxTransTimeRect.Width = DurationGroupBoxMax.Width;
            maxTransTimeRect.Height = DurationGroupBoxMax.Height;

            // Copy the attributes from the group box that defines the size and placement
            // of the ave transition color-code key display area into a rectangle
            // passed into the bitmap manager
            aveTransTimeRect = new Rectangle();
            aveTransTimeRect.X = DurationGroupBoxAve.Location.X;
            aveTransTimeRect.Y = DurationGroupBoxAve.Location.Y;
            aveTransTimeRect.Width = DurationGroupBoxAve.Width;
            aveTransTimeRect.Height = DurationGroupBoxAve.Height;

            // Copy the attributes from the group box that defines the size and placement
            // of the minimum transition color-code key display area into a rectangle
            // passed into the bitmap manager
            minTransTimeRect = new Rectangle();
            minTransTimeRect.X = DurationGroupBoxMin.Location.X;
            minTransTimeRect.Y = DurationGroupBoxMin.Location.Y;
            minTransTimeRect.Width = DurationGroupBoxMin.Width;
            minTransTimeRect.Height = DurationGroupBoxMin.Height;

            // Copy the attributes from the group box that defines the size and placement
            // of the individual behavior color-code key display area into a rectangle
            // passed into the bitmap manager
            behColorRectArray = new Rectangle[m_numBehaviors];
            for(i=0; i<m_numBehaviors; i++)
            {
                behColorRectArray[i].X = m_transitionGroupBoxArray[i].Location.X;
                behColorRectArray[i].Y = m_transitionGroupBoxArray[i].Location.Y;
                behColorRectArray[i].Width = m_transitionGroupBoxArray[i].Width;
                behColorRectArray[i].Height = m_transitionGroupBoxArray[i].Height;
            }

            m_bitmapMgr = new CBitmapSingleBehaviorTimeTransitionManager(this,
                m_displayType, // display type, either initial behavor or behavior transition
                transitionRect, // bitmap setup of the main (results) display
                maxTransTimeRect, // bitmap setup of the maximum time rectangle key
                aveTransTimeRect, // bitmap setup of the average time rectangles key
                minTransTimeRect, // bitmap setup of the minimum time rectangle key
                behColorRectArray // bitmaps setup of the behavior rectangles key
                );

            RunTransitionOverTimeTest(CUtil.CopyMatrix(m_matrix)); // Generate and display initial data
        }

        private void RunTransitionOverTimeTest(mbsMATRIX M)
        {
            C3mbsWrapperSpeciesModel wrapper = new C3mbsWrapperSpeciesModel();
            // Generate and display data.
            if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION)
                m_displayData = wrapper.RunBehaviorTransitionControlTest(M, m_behaviorIndex, m_numTrials);
            else
                m_displayData = wrapper.InitialBehaviorControlTest(M, m_numTrials);

            m_bitmapMgr.SetDisplayData(m_numBehaviors, m_displayData);

        }

        // Overrdie Form OnPaint method
        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);
            Graphics graphicsObject = e.Graphics; // get graphics
            m_bitmapMgr.MyOnPaint(graphicsObject);
        }


        private void CopyControls()
        {
            m_transitionGroupBoxArray[0] = TransitionGroupBoxBeh0;
            m_transitionGroupBoxArray[1] = TransitionGroupBoxBeh1;
            m_transitionGroupBoxArray[2] = TransitionGroupBoxBeh2;
            m_transitionGroupBoxArray[3] = TransitionGroupBoxBeh3;
            m_transitionGroupBoxArray[4] = TransitionGroupBoxBeh4;
            m_transitionGroupBoxArray[5] = TransitionGroupBoxBeh5;
            m_transitionGroupBoxArray[6] = TransitionGroupBoxBeh6;
            m_transitionGroupBoxArray[7] = TransitionGroupBoxBeh7;

            m_transitionLabelArray[0] = TransitionLabelBeh0;
            m_transitionLabelArray[1] = TransitionLabelBeh1;
            m_transitionLabelArray[2] = TransitionLabelBeh2;
            m_transitionLabelArray[3] = TransitionLabelBeh3;
            m_transitionLabelArray[4] = TransitionLabelBeh4;
            m_transitionLabelArray[5] = TransitionLabelBeh5;
            m_transitionLabelArray[6] = TransitionLabelBeh6;
            m_transitionLabelArray[7] = TransitionLabelBeh7;

            m_transitionLabelNumberArray[0] = TransitionLabelBehNumber0;
            m_transitionLabelNumberArray[1] = TransitionLabelBehNumber1;
            m_transitionLabelNumberArray[2] = TransitionLabelBehNumber2;
            m_transitionLabelNumberArray[3] = TransitionLabelBehNumber3;
            m_transitionLabelNumberArray[4] = TransitionLabelBehNumber4;
            m_transitionLabelNumberArray[5] = TransitionLabelBehNumber5;
            m_transitionLabelNumberArray[6] = TransitionLabelBehNumber6;
            m_transitionLabelNumberArray[7] = TransitionLabelBehNumber7;
        }

        private void NumTrialsButton_Click(object sender, EventArgs e)
        {
            Form1Input dlg = new Form1Input("Set Number Number Of Trials Per Transition Period", ""+m_numTrials);

            dlg.ShowDialog(this);
            this.BringToFront();
            if(dlg.result == RESLT.OK && dlg.modified == true)
            {
                m_numTrials = CStringUtil.SzToIntOrMin1(dlg.dataString);
                NumTrialsButton.Text = "" + m_numTrials;
                RunTransitionOverTimeTest(CUtil.CopyMatrix(m_matrix));
            }

        }

        private void DoneButton_Click(object sender, EventArgs e)
        {
            Dispose();
        }

        private void FrmTransitionTest_Resize(object sender, EventArgs e)
        {
            FrmTransitionTest_ResizeEnd(sender, e);
        }

        private void FrmTransitionTest_ResizeEnd(object sender, EventArgs e)
        {
            int i;
            Rectangle transitionRect;
            Rectangle maxTransTimeRect;
            Rectangle aveTransTimeRect;
            Rectangle minTransTimeRect;
            Rectangle[] behColorRectArray;

            transitionRect = new Rectangle();
            transitionRect.X = DisplayGroupBox.Location.X;
            transitionRect.Y = DisplayGroupBox.Location.Y;
            transitionRect.Width = DisplayGroupBox.Width;
            transitionRect.Height = DisplayGroupBox.Height;

            maxTransTimeRect = new Rectangle();
            maxTransTimeRect.X = DurationGroupBoxMax.Location.X;
            maxTransTimeRect.Y = DurationGroupBoxMax.Location.Y;
            maxTransTimeRect.Width = DurationGroupBoxMax.Width;
            maxTransTimeRect.Height = DurationGroupBoxMax.Height;

            aveTransTimeRect = new Rectangle();
            aveTransTimeRect.X = DurationGroupBoxAve.Location.X;
            aveTransTimeRect.Y = DurationGroupBoxAve.Location.Y;
            aveTransTimeRect.Width = DurationGroupBoxAve.Width;
            aveTransTimeRect.Height = DurationGroupBoxAve.Height;

            minTransTimeRect = new Rectangle();
            minTransTimeRect.X = DurationGroupBoxMin.Location.X;
            minTransTimeRect.Y = DurationGroupBoxMin.Location.Y;
            minTransTimeRect.Width = DurationGroupBoxMin.Width;
            minTransTimeRect.Height = DurationGroupBoxMin.Height;

            behColorRectArray = new Rectangle[m_numBehaviors];
            for(i=0; i<m_numBehaviors; i++)
            {
                behColorRectArray[i].X = m_transitionGroupBoxArray[i].Location.X;
                behColorRectArray[i].Y = m_transitionGroupBoxArray[i].Location.Y;
                behColorRectArray[i].Width = m_transitionGroupBoxArray[i].Width;
                behColorRectArray[i].Height = m_transitionGroupBoxArray[i].Height;
            }

            m_bitmapMgr.SetWindowRectanglesForm(transitionRect,
                maxTransTimeRect,
                aveTransTimeRect,
                minTransTimeRect,
                behColorRectArray);
        }
    }
}