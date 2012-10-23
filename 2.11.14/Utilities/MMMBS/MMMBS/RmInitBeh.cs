using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Diagnostics;
using MMMBSLib;  // c# code code and data types.
using mbs; // C++ wrapper code and data types


namespace MBSGUI
{
    public partial class RmInitBeh: Form
    {
        //------------------//
        // Member Variables
        //------------------//
        int m_numBehaviorsDefined;
        string[] m_behaviorNameArray;
        MBSDEFAULTS.BITMAPDISPLAYTYPE m_displayType;
        int m_behaviorIndex = 0;
        CMatrix m_matrix;
        private Boolean m_modified = false;
        private Boolean m_initializing;
        private string m_szTitle = "3MB Initial Behavior Vector Model - Biomimetica";
        private Boolean m_szFormatOK = true;
        CBitmapSingleBehaviorTimeTransitionManager m_bitmapMgr;
        //MULTIBEHTRANSSTRUCT[] m_displayData;
        mbsSNGLBEHTRANSTRUCT m_displayData;
        static int NUM_NRML_BUTTONS = 8;
        static int NORMAL_TRIALS_FACTOR = 200;

        int m_numTrials;

        
        GroupBox[] m_transitionGroupBoxArray = new GroupBox[NUM_NRML_BUTTONS];
        Label[] m_transitionLabelArray = new Label[NUM_NRML_BUTTONS];
        Label[] m_transitionLabelNumberArray = new Label[NUM_NRML_BUTTONS];


        //-----------------//
        // Class Properties
        //-----------------//
        public CMatrix matrix { get { return m_matrix; } }
        public Boolean modified { get { return m_modified; } }

        //------------//
        // Constructor
        //------------//
        // 'NumAdditionalColumns' is the column count above the number of behaviors so
        // must include the first column that represents the starting range of zero of a
        // behavior and therefore must be 1 or greater.  For the initial behavior matrix
        // 'NumAdditionalColumns' then must be 3 (for the two additional columns for start
        // time and end time plus the 0 probablility starting column) and for the
        // behavior translation as a function of time it must be 5 for the same reason
        // plus the slope and ave time in behavior... or something...
        public RmInitBeh(CMatrix M, int NumBehaviorsDefined, string[] BehaviorNames)
        {
            // The 3 entered as a param for Construct() is due to this form being used to
            // show initial behavior model.  The initial behavior matrix has 3 more
            // columns in it than the number of behaviors it models: start time, end time,
            // zero probability + one for each behavior.
            InitializeComponent();
            m_displayType = MBSDEFAULTS.BITMAPDISPLAYTYPE.INITIAL_BEHAVIOR;
            BehaviorNumberLabel.Visible = false;
            BehaviorNameLabel.Visible = false;
            label6.Visible = false;
            DurationTextBoxMax.Visible = false;
            DurationTextBoxAve.Visible = false;
            DurationTextBoxMin.Visible = false;
            Construct(M, NumBehaviorsDefined, BehaviorNames, 3);
        }

        public RmInitBeh(CMatrix M, int NumBehaviorsDefined, string[] BehaviorNames, int BehaviorIndex)
        {
            // 5 is entered as a param for Construct() is due to this form being used to
            // show the behavior transition as a function of time and current behavior
            // the associated matrix model of which has 5 more columns in it than the
            // number of behaviors it models: start time, end time, zero probability + one
            // for each behavior + T50 and k (slope)).
            InitializeComponent();
            m_behaviorIndex = BehaviorIndex;
            m_displayType = MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION;
            Construct(M, NumBehaviorsDefined, BehaviorNames, 5);
        }

        void Construct(CMatrix M, int NumBehaviorsDefined, string[] BehaviorNames, int NumAdditionalColumns)
        {
            int i;
            // This assert could go away if this form were to ever need to be used for a matrix
            // without colums but is left in for now because it isn't being used that
            // way currently.  For now at the very least there will always be a need
            // for 1+ the number of behavior number of columns.
            Debug.Assert(NumAdditionalColumns >= 1);

            CopyControls();

            m_matrix = M;


            m_initializing = true;
            this.Text = m_szTitle;


            m_numTrials = NORMAL_TRIALS_FACTOR/m_matrix.RowCount;
            NumTrialsButton.Text = "" + m_numTrials;

            m_behaviorNameArray = BehaviorNames;
            m_numBehaviorsDefined = NumBehaviorsDefined;
            BehaviorCountLabel.Text = "Species Defined Behavior Count: " + NumBehaviorsDefined;

            MatrixTextBox.Text = M.ConvertToStringB();
            MatrixDimensionsLabel.Text = "Matrix Dimensions: " + M.RowCount + "x" + M.ColumnCount;

            m_szFormatOK = UpdateModel();
            RefreshButton.Enabled = false;
            DoneButton.Enabled = true;
            NumTrialsButton.Enabled = true;
            m_initializing = false;

            //---------------------------//
            // Display the behavior names
            //---------------------------//
            if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.INITIAL_BEHAVIOR)
            {
                BehaviorNameLabel.Text = "";
                BehaviorNumberLabel.Visible = false;
            }
            else
            {
                BehaviorNameLabel.Text = m_behaviorNameArray[m_behaviorIndex];
                BehaviorNumberLabel.Visible = true;
                BehaviorNumberLabel.Text = "" + "(" + (m_behaviorIndex+1) + ")";
            }

            InitializeBehaviorTransitionAsAFunctionOfTimeBitMapVars();
            RunTransitionOverTimeTest(CUtil.CopyMatrix(m_matrix)); // Generate and display initial data


            for(i=0; i<m_behaviorNameArray.Length; i++)
            {
                m_transitionLabelArray[i].Text = m_behaviorNameArray[i];
                m_transitionLabelNumberArray[i].Visible = true;

            }

            for(; i<NUM_NRML_BUTTONS; i++)
            {
                m_transitionLabelArray[i].Text = "";
                m_transitionLabelNumberArray[i].Visible = false;
            }
        }


        void RunTransitionOverTimeTest(mbsMATRIX M)
        {
            C3mbsWrapperSpeciesModel wrapper = new C3mbsWrapperSpeciesModel();
            if(m_szFormatOK == false)
                return;

            // Generate and display data.
            if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION)
                m_displayData = wrapper.RunBehaviorTransitionControlTest(M, m_behaviorIndex, m_numTrials);
            else
                m_displayData = wrapper.InitialBehaviorControlTest(M, m_numTrials);

            m_bitmapMgr.SetDisplayData(m_numBehaviorsDefined, m_displayData);
        }


        public void InitializeBehaviorTransitionAsAFunctionOfTimeBitMapVars()
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

            behColorRectArray = new Rectangle[m_numBehaviorsDefined];
            for(i=0; i<m_numBehaviorsDefined; i++)
            {
                behColorRectArray[i].X = m_transitionGroupBoxArray[i].Location.X;
                behColorRectArray[i].Y = m_transitionGroupBoxArray[i].Location.Y;
                behColorRectArray[i].Width = m_transitionGroupBoxArray[i].Width;
                behColorRectArray[i].Height = m_transitionGroupBoxArray[i].Height;
            }

            m_bitmapMgr = new CBitmapSingleBehaviorTimeTransitionManager(this,
                m_displayType,
                transitionRect,
                maxTransTimeRect,
                aveTransTimeRect,
                minTransTimeRect,
                behColorRectArray);
        }

        private void FormInitialBehavior_Resize(object sender, EventArgs e)
        {
            FormInitialBehavior_ResizeEnd(sender, e);
        }
 
        private void FormInitialBehavior_ResizeEnd(object sender, EventArgs e)
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

            behColorRectArray = new Rectangle[m_numBehaviorsDefined];
            for(i=0; i<m_numBehaviorsDefined; i++)
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


        // Overrdie Form OnPaint method
        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);
            Graphics graphicsObject = e.Graphics; // get graphics
            m_bitmapMgr.MyOnPaint(graphicsObject);
        }


        //-----------------//
        // Member Functions
        //-----------------//
        // Returns true if initializing
        private Boolean UpdateModifiedStatus()
        {
            if(m_initializing == true)
                return true;

            this.Text = m_szTitle + "*";
            m_modified = true;

            return false;
        }


        private Boolean UpdateModel()
        {
            MATRIXDIMENSIONS md;

            md = CStringUtil.SzMatrixDimensions(MatrixTextBox.Text);

            // Check for proper characters
            if(CStringUtil.SzVerifyMatrixFormatDouble(MatrixTextBox.Text) == true)
            {
                BiasFormatErrorLabel.Text = "";
                BiasFormatErrorLabel.BackColor = System.Drawing.SystemColors.Control;

                MatrixDimensionsLabel.Text = "Matrix Dimensions: [" + md.rowCnt + "x" + md.maxCols + "]";
                return true;
            }
            else
            {
                BiasFormatErrorLabel.Text = "Input Warning";
                BiasFormatErrorLabel.BackColor = System.Drawing.Color.Tomato;

                MatrixDimensionsLabel.Text = "Matrix Dimensions: [" + "???" + "x" + "???" + "]";
                return false;
            }
            //return true;
        }

        private void InitialBehaviorTextBox_TextChanged(object sender, EventArgs e)
        {
            if(UpdateModifiedStatus() == true)
                return;
            DoneButton.Enabled = false;
            NumTrialsButton.Enabled = false;
            RefreshButton.Enabled = true;
            m_szFormatOK = UpdateModel();
        }



        private void RefreshButton_Click(object sender, EventArgs e)
        {
            int numAdditionalColumns;
            CMatrix m = new CMatrix();
            string sz = CStringUtil.SzForceIntoMatrixDoubleFormat(MatrixTextBox.Text);

            if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.INITIAL_BEHAVIOR)
                numAdditionalColumns = 3;
            else
                numAdditionalColumns = 5;


            m.a = CStringUtil.SzMatrixToDouble2DArray(MatrixTextBox.Text);
            while(m.ColumnCount > m_numBehaviorsDefined + numAdditionalColumns)
                m.DeleteColumn(m.ColumnCount-1);
            while(m.ColumnCount < m_numBehaviorsDefined + numAdditionalColumns)
                m.AddColumn();

            MatrixTextBox.Text = m.ConvertToStringB();
            DoneButton.Enabled = true;
            NumTrialsButton.Enabled = true;
            RefreshButton.Enabled = false;
            if(true == (m_szFormatOK = UpdateModel()))
                RunTransitionOverTimeTest(CUtil.CopyMatrix(m));
        }

        private void DoneButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                MatrixTextBox.Text = CStringUtil.SzForceIntoMatrixDoubleFormat(MatrixTextBox.Text);
                m_matrix.a = CStringUtil.SzMatrixToDouble2DArray(MatrixTextBox.Text);
            }
            Dispose();
        }
        private void CancelButton_Click(object sender, EventArgs e)
        {
            if(m_modified == true)
            {
                FormConfirmDecision dlg = new FormConfirmDecision();
                dlg.Text = "Confirm Cancel";
                dlg.messageString = "Abandon changes to model?";
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

        private void NumTrialsButton_Click(object sender, EventArgs e)
        {
            CMatrix m;
            string sz;
            Form1Input dlg = new Form1Input("Set Number Number Of Trials Per Transition Period", ""+m_numTrials);

            dlg.ShowDialog(this);
            this.BringToFront();
            if(dlg.result == RESLT.OK && dlg.modified == true)
            {
                m_numTrials = CStringUtil.SzToIntOrMin1(dlg.dataString);
                NumTrialsButton.Text = "" + m_numTrials;
                m = new CMatrix();
                sz = CStringUtil.SzForceIntoMatrixDoubleFormat(MatrixTextBox.Text);
                m.a = CStringUtil.SzMatrixToDouble2DArray(MatrixTextBox.Text);
                RunTransitionOverTimeTest(CUtil.CopyMatrix(m));
            }
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

    }
}