using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Diagnostics;
using MMMBSLib;  // c# code code and data types.


namespace MBSGUI
{
    public  struct TRANSPER{ public int start; public int end;}; // Transition period
    public struct GUICONTROLPLACEMENTADJUST
    {
        public Point upperLeft; // The upperleft-most location to place controls.
    }; // Transition period


    public partial class FormBehTrans:Form
    {
        private const int PIXELSBETWEENBEHCOLUMNS = 6; // space between columns in pixels.
        private const int PIXELSBETWEENBEHCOLANDT50COL = 20;
        private const double MATRIXELEMENTWARNING = 9999.9999;
        private const double MATRIXELEMENTMAX = 1.0;
        private const int NUMSECONDSPERDAY = 24*60*60;
        private const int RIGHTMARGIN = 22;

        // Variables for determining if the OK button is enabled or disabled.
        private Boolean m_matrixCheckPasses = true; // matrix values considerations
        private Boolean m_T50SlopeVarsPass = true; // T50 and k considerations.
        private Boolean m_simpleProbabilityDisplay = false; // Simple (true) or cumulative (false).
        private String m_simpleProbabilityString = "Simple\nProbability Display";
        private String m_cumulativeProbabilityString = "Cumulative\nProbability Display:";
        private Boolean m_bDepthSpanActiveLocked; // Mitigates mulitple clicks of the same btton.
        private GUICONTROLPLACEMENTADJUST m_cntrlPlacement;

        // Passed into the class via the constructor
        private int m_numBehaviorsDefined;
        private string[] m_behaviorNameArray;

        private int m_behaviorIndex;

        //CMatrix m_matrixParam = null;
        private CSpanMgr m_spanMgr;
        private BEHTRANS_TERM_MODEL m_behTermMdl; // Behavior termination model.
        private int m_activeSpanIndex;
        private int m_activeMatrixRowIndex;


        // Determined and set based upon constructor type called.
        private MBSDEFAULTS.BITMAPDISPLAYTYPE m_displayType = MBSDEFAULTS.BITMAPDISPLAYTYPE.INITIAL_BEHAVIOR;

        // Set true when the matrix is altered by the user.
        private Boolean m_bModified;


        // BehTitleTextBox has a +1 in the array size for the zero probability column (not
        // displayed).
        TextBox[] m_behTitleTextBox = new TextBox[MBGUICONSTANTS.MAXNUMBEHAVIORS+1];
        Button[] m_spanButton = new Button[MBGUICONSTANTS.MAXNUMSPANS];
        TextBox[][] m_matrixTextBox = new TextBox[MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS][];
        TextBox[] m_T50TextBox = new TextBox[MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS];
        TextBox[] m_slopeTextBox = new TextBox[MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS];
        Button[] m_clkSpanButton = new Button[MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS];
        Boolean[][] m_bMatrixError = new Boolean[MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS][];

        // Class Properties
        public BEHTRANS_TERM_MODEL BehaviorTerminationModel { get { return m_behTermMdl; } }

        private CMatrix Matrix
        {
            get
            {
                if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.INITIAL_BEHAVIOR)
                    return m_spanMgr.GetSpan(m_activeSpanIndex).behTrans.matrix;
                else if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION)
                    return m_spanMgr.GetSpan(m_activeSpanIndex).behTrans.matrix;
                else
                    return null;

            }
        }
        public Boolean modified { get { return m_bModified; } }
        public CSpanMgr NrmlBehTransMdl { get { return m_spanMgr; } }

        public Boolean modifed { get {return m_bModified;}}

        //------------------------------------------------------------------------------//
        // Class Constructors
        //------------------------------------------------------------------------------//
        public FormBehTrans(CSpanMgr SpanListMgr, BEHTRANS_TERM_MODEL BehavorTerminationMdl, int BehCount,
            string[] BehaviorNames, int BehIndex)
        {
            // This class constructor is for behavior transition matrices.
            m_displayType = MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION;
            m_spanMgr = SpanListMgr;

            m_behTermMdl = BehavorTerminationMdl;

            // Set intial indicies
            m_behaviorIndex = BehIndex;
            m_activeSpanIndex = 0; // start at span index 0
            m_activeMatrixRowIndex = 0;// start at row index 0

            m_numBehaviorsDefined = BehCount;
            m_behaviorNameArray = BehaviorNames;
            ConstructForm();
            SetTransitionFormulaText();
        }

        public FormBehTrans(CSpanMgr SpanlistMgr, int BehCount, string[] BehaviorNames)
        {
            // This class constructor is for the intitial behavior matrix.
            m_displayType = MBSDEFAULTS.BITMAPDISPLAYTYPE.INITIAL_BEHAVIOR;
            m_spanMgr = SpanlistMgr;
            //m_matrixParam = SpanlistMgr;

            // Set intial indicies
            m_behaviorIndex = -1;// no behavior indices for the initial behavior matrix.
            m_activeSpanIndex = 0;
            m_activeMatrixRowIndex = 0; // start at row index 0

            m_numBehaviorsDefined = BehCount;
            m_behaviorNameArray = BehaviorNames;
            ConstructForm();
        }

        //------------------------------------------------------------------------------//
        // Class Methods
        //------------------------------------------------------------------------------//
        private void ConstructForm()
        {
            InitializeComponent();
            m_bDepthSpanActiveLocked = false;

            // Copy controls into member variables arrays for easy reference via indexing.
            // Copying is placed in its own function because it is so very long.
            CopyControls();

            // Run debug assertions on parameters passed in.
            try
            {
                RunParamDebugAssertions();
            }
            catch
            {
                MessageBox.Show("badd goat");// mn;// = new MessageBox();
                //mn = new MessageBox;
            }
            
            // Set dialog control item locations and visibility based on if the dialog box
            // is being used to edit the initial behavior matrix or the behavior transition
            // matrix.
            SetInitialGuiState();

            SetControlPlacement();

            SetTabOrder();
        }


        private Boolean RunParamDebugAssertions()
        {
            int i;
            CSpan ds;

            // Verify there is at least 1 but not more than the maximum allowed behaviors defined.
            Debug.Assert(m_numBehaviorsDefined > 0);
            Debug.Assert(m_numBehaviorsDefined <= MBGUICONSTANTS.MAXNUMBEHAVIORS);
            Debug.Assert(m_numBehaviorsDefined == m_behaviorNameArray.Length);
            if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION)
            {
                // Magic number 5 because there is a columns for:
                // [start time] [end time] [zero probability] [T50] [slope].
                Debug.Assert(Matrix.ColumnCount == m_numBehaviorsDefined + 5);

                Debug.Assert(m_spanMgr != null);
                Debug.Assert(m_spanMgr.SpanCount > 0 && m_spanMgr.SpanCount <= MBGUICONSTANTS.MAXNUMSPANS);

                for(i=0; i<m_spanMgr.SpanCount; i++)
                {
                    ds = m_spanMgr.GetSpan(i);
                    Debug.Assert(ds.shallow <=0 && ds.deep <= 0);
                    Debug.Assert(ds.behaviorTransCount  == m_numBehaviorsDefined);
                    Debug.Assert(ds.timeTransCount >= 1 && ds.timeTransCount < MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS);
                }
            }
            else if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.INITIAL_BEHAVIOR)
            {
                // Magic number 3 because Columns for start time, end time, and zero
                // probability column.  Zero probability column not displayed.
                Debug.Assert(Matrix.RowCount >= 1); // At least one transition period is defined.
                Debug.Assert(Matrix.RowCount <= MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS);
                Debug.Assert(Matrix.ColumnCount == m_numBehaviorsDefined + 3);
            }
            else
            {
                Debug.Assert(false);
            }
            //--------------------------------------------------------------------------//
            return true;
        }

        private void MatrixModelToGUIControls()
        {
            for(int i=0; i<MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS; i++)
                MatrixModelRowToGuiControl(i);
        }


        /*
         * Transfer the information contained in the GUI controls into a matrix.
         * */
        private void GuiControlsToMatrixModel()
        {
            int i;
            int tp = 9;
            double displayVal, val, cumulativeVal;

            //--------------------------------------------------------------------------//
            // Programming debug assertions
            //-----------------------------//
            Debug.Assert(Matrix.RowCount >= 1); // At least one transition period is defined.
            Debug.Assert(Matrix.RowCount <= MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS);

            if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION)
            {
                // Magic number 5 because Columns for start time, end time, zero
                // probability column, T50, and slope.  Zero probability column not displayed.
                Debug.Assert(Matrix.ColumnCount == m_numBehaviorsDefined + 5);
            }
            else if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.INITIAL_BEHAVIOR)
            {
                // Magic number 3 because Columns for 
                // [start time] [end time] and [zero probability] columns.
                Debug.Assert(Matrix.ColumnCount == m_numBehaviorsDefined + 3);
            }
            //--------------------------------------------------------------------------//

            for(tp=0; tp <Matrix.RowCount && tp < MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS; tp++)
            {
                // Zero probability column
                Matrix.a[tp][2] = 0.0;

                // Matrix elements
                // The number of matrix elements are 1 + the number of behaviors defined
                // because of the zero probability column.
                cumulativeVal = 0;
                for(i=0; i<=m_numBehaviorsDefined; i++)
                {
                    //-------------------------------------------------------------------------------------------------
                    // Set the displayed value based on if display is for simple probability or cumulative probability
                    displayVal = CStringUtil.SzToDouble(m_matrixTextBox[tp][i].Text);
                    if(m_simpleProbabilityDisplay == true)
                    {
                        val = cumulativeVal + displayVal;
                        cumulativeVal = val;
                    }
                    else // cumulative probability
                    {
                        val = displayVal;
                    }

                    // clock start, clock end, zero matrix m_simpleProbabilityDisplay
                    Matrix.a[tp][i+2] = val;
                }

                if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION)
                {
                    // T50 termination coefficient
                    Matrix.a[tp][3 + m_numBehaviorsDefined] = CStringUtil.SzToDouble(m_T50TextBox[tp].Text);

                    // Slope
                    Matrix.a[tp][4 + m_numBehaviorsDefined] = CStringUtil.SzToDouble(m_slopeTextBox[tp].Text);
                }
            }
        }



        private void SetModified()
        {
            if(m_bModified == true)
                return;
            m_bModified = true;
            Text = Text + "*";
        }

        private void ManageDepthSpanControls()
        {
            int i;
            Color black = Color.FromArgb(0, 0, 0, 0);;
            Color mouseOver = Color.FromArgb(0, 0, 255, 0);;

            Debug.Assert(m_spanMgr != null);
            Debug.Assert(m_spanMgr.SpanCount > 0);

            // Manage the  Add Depth Span Button
            if(m_spanMgr.SpanCount >= MBGUICONSTANTS.MAXNUMSPANS)
                AddSpanButton.Enabled = false;
            else if(m_spanMgr.SpanCount < MBGUICONSTANTS.MAXNUMSPANS)
                AddSpanButton.Enabled = true;

            // Manage the  Delete Depth Span Button
            if(m_spanMgr.SpanCount <= 1)
                DeleteSpanButton.Enabled = false;
            else if(m_spanMgr.SpanCount >=1)
                DeleteSpanButton.Enabled = true;

            // Manage the Promote Depth Span Button
            if(m_activeSpanIndex != 0)
                PromoteButton.Enabled = true;
            else
                PromoteButton.Enabled = false;

            // Manage the Demote Depth Span Button
            if(m_activeSpanIndex != m_spanMgr.SpanCount - 1)
                DemoteButton.Enabled = true;
            else
                DemoteButton.Enabled = false;

            // Manage the Duplicate Depth Span Button
            if(m_spanMgr.SpanCount < MBGUICONSTANTS.MAXNUMSPANS)
                DuplicateButton.Enabled = true;
            else if(m_spanMgr.SpanCount == MBGUICONSTANTS.MAXNUMSPANS)
                DuplicateButton.Enabled = false;

            // Manage the depth span buttons.
            for(i=0; i< MBGUICONSTANTS.MAXNUMSPANS; i++)
                ManageSingleDepthSpanButton(i);

            // Manage the depth span group box.
            UpdateDepthSpanGroupBox();


        }

        private void ManageSingleDepthSpanButton(int Index)
        {
            CSpan span;
            Debug.Assert(Index < MBGUICONSTANTS.MAXNUMSPANS);

            if(Index < m_spanMgr.SpanCount && null != (span = m_spanMgr.GetSpan(Index)))
            {
                m_spanButton[Index].Enabled = true;
                m_spanButton[Index].Visible = true;

                if(Index == m_activeSpanIndex)
                    m_spanButton[Index].BackColor = Color.GreenYellow;
                else
                    m_spanButton[Index].BackColor = Color.Aquamarine;

                m_spanButton[Index].Text = "" + -((int)span.shallow) + " to " + -((int)span.deep) + " meters";

                if(span.timeTransCount == 1)
                    m_spanButton[Index].Text += "\n 1 clock period";
                else
                    m_spanButton[Index].Text += "\n" + span.timeTransCount + " clock periods";
            }
            else
            {
                m_spanButton[Index].BackColor = SystemColors.Control;
                m_spanButton[Index].Enabled = false;
                m_spanButton[Index].Text = "-----------";
                m_spanButton[Index].Visible = true;
            }
        }

        private void UpdateDepthSpanGroupBox()
        {
            CSpan span;
            Debug.Assert(m_activeSpanIndex >= 0 && m_activeSpanIndex < MBGUICONSTANTS.MAXNUMSPANS);

            span = m_spanMgr.GetSpan(m_activeSpanIndex);

            SpanGroupBox.Text = "Transition Depth Span " + (m_activeSpanIndex+1) + "  (" + -((int)span.shallow);
            if(m_activeSpanIndex == 0)
                SpanGroupBox.Text += " meters <= Animat <= " + -((int)span.deep) + " meters)";
            else
                SpanGroupBox.Text += " meters < Animat <= " + -((int)span.deep) + " meters)";
        }

        private void ManageMatrixControls()
        {
            // The Add Matrix Row button.
            if(Matrix.RowCount == MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS-1)
                AddClkButton.Enabled = false;
            else
                AddClkButton.Enabled = true;

            // The Delete Matrix Row Button
            if(Matrix.RowCount == 1)
                DeleteClkButton.Enabled = false;
            else
                DeleteClkButton.Enabled = true;

            // The Promote Matrix Row button
            if(m_activeMatrixRowIndex == 0)
                PromoteClkButton.Enabled = false;
            else
                PromoteClkButton.Enabled = true;

            // The Demote Matrix Row button
            if(m_activeMatrixRowIndex == Matrix.RowCount-1)
                DemoteClkButton.Enabled = false;
            else
                DemoteClkButton.Enabled = true;  

            // The Matrix Rows
            MatrixModelToGUIControls();
        }

        private void MatrixModelRowToGuiControl(int TransitionRow)
        {
            HHMMSS hhmmss;
            int i;
            double displayVal, val, preVal = 0;

            CMatrix matrix = null;

            if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.INITIAL_BEHAVIOR)
            {
                matrix = m_spanMgr.GetSpan(m_activeSpanIndex).behTrans.matrix;
            }
            else if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION)
            {
                matrix = m_spanMgr.GetSpan(m_activeSpanIndex).behTrans.matrix;
            }
            else
            {
                Debug.Assert(false);
                return;
            }


            // TransitionRow is the row who's state, composed of clocks, matrix, T50, and
            // k, that is to be set. Row count is the number of rows in the matrix.
            // row count is the number of transitional periods.
            Debug.Assert(TransitionRow < MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS);
            if(TransitionRow < matrix.RowCount)
            {
                // Grab the start time stored in the first column (at index [Index][0]).
                hhmmss = CUtil.TwentyHrFloatClockToHHMMSSOrZeroInclusive(matrix.a[TransitionRow][0]);
                m_clkSpanButton[TransitionRow].Text =
                    String.Format("{0:00}:{1:00}:{2:00}", hhmmss.hh, hhmmss.mm, hhmmss.ss);

                // Grab the end time stored in the second column (at index [Index][1]).
                hhmmss = CUtil.TwentyHrFloatClockToHHMMSSOrZeroInclusive(matrix.a[TransitionRow][1]);
                m_clkSpanButton[TransitionRow].Text +=
                    String.Format(" to {0:00}:{1:00}:{2:00}", hhmmss.hh, hhmmss.mm, hhmmss.ss);

                if(TransitionRow == m_activeMatrixRowIndex)
                    m_clkSpanButton[TransitionRow].BackColor = Color.GreenYellow;
                else
                    m_clkSpanButton[TransitionRow].BackColor = Color.Aquamarine;


                //----------------------------------------------------------------------//
                // Check programming assertions
                //-----------------------------//
                // TODO: Provide some feedback here if the model changes anything.
                // If this isn't the first row check that the transition end time of the
                // previous row equals the transtion start time of the current row.
                if(TransitionRow > 0)
                {
                    if(matrix.a[TransitionRow-1][1] != matrix.a[TransitionRow][0])
                        Debug.Assert(matrix.a[TransitionRow-1][1] == matrix.a[TransitionRow][0]);
                }

                // If this isn't the last row check that the transition end time of the
                // current row equals the transtion start time of the next row.
                if(TransitionRow < matrix.a.Length-1)
                {
                    if(matrix.a[TransitionRow][1] != matrix.a[TransitionRow+1][0])
                        Debug.Assert(matrix.a[TransitionRow][1] == matrix.a[TransitionRow+1][0]);
                }
                //----------------------------------------------------------------------//

                m_clkSpanButton[TransitionRow].Enabled = true;

                // matrix elements
                // The number of matrix elements are 1 + the number of behaviors defined
                // because of the zero probability column.
                // Note that the zero probablity column is acounted for but not displayed
                preVal = 0;
                for(i=0; i<=m_numBehaviorsDefined; i++)
                {
                    // zero matrix and behaviors
                    if(i == 0 || i == m_numBehaviorsDefined)
                    {
                        m_matrixTextBox[TransitionRow][i].Enabled = false; // start time, end time
                        m_matrixTextBox[TransitionRow][i].ReadOnly = true; // start time, end time
                    }
                    else
                    {
                        m_matrixTextBox[TransitionRow][i].Enabled = true; // zero matrix
                        m_matrixTextBox[TransitionRow][i].ReadOnly = false; // start time, end time
                    }


                    //-------------------------------------------------------------------------------------------------
                    // Set the displayed value based on if display is for simple probability or cumulative probability
                    val = matrix.a[TransitionRow][i + 2];
                    if(m_simpleProbabilityDisplay == true)
                    {
                        displayVal = val - preVal;
                        preVal = val; ;
                    }
                    else // cumulative probability
                    {
                        displayVal = val; //, val, 
                    }
                    m_matrixTextBox[TransitionRow][i].Text = String.Format("{0:0.0000}", displayVal);
                    m_matrixTextBox[TransitionRow][i].BackColor = SystemColors.Window;
                    //-------------------------------------------------------------------------------------------------
                }

                if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION)
                {
                    // T50 termination coefficient
                    m_T50TextBox[TransitionRow].Enabled = true;
                    m_T50TextBox[TransitionRow].Text = "" + matrix.a[TransitionRow][3 + m_numBehaviorsDefined];
                    m_T50TextBox[TransitionRow].BackColor = SystemColors.Window;

                    // Slope
                    m_slopeTextBox[TransitionRow].Enabled = true;
                    m_slopeTextBox[TransitionRow].Text = "" + matrix.a[TransitionRow][4 + m_numBehaviorsDefined];
                    m_slopeTextBox[TransitionRow].BackColor = SystemColors.Window;
                }
                else
                {
                    m_T50TextBox[TransitionRow].Text = "";
                    m_T50TextBox[TransitionRow].Enabled = false;
                    m_T50TextBox[TransitionRow].BackColor = SystemColors.Control;

                    m_slopeTextBox[TransitionRow].Text = "";
                    m_slopeTextBox[TransitionRow].Enabled = false;
                    m_slopeTextBox[TransitionRow].BackColor = SystemColors.Control;
                }
            }
            else if(TransitionRow < MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS)
            {
                m_clkSpanButton[TransitionRow].Text = "----------- to ------------";
                m_clkSpanButton[TransitionRow].Enabled = false;
                m_clkSpanButton[TransitionRow].BackColor = SystemColors.Control;
   

                // matrix elements
                // The number of matrix elements are 1 + the number of behaviors defined
                // because of the zero probability column.
                // Note that the zero probablity column is acounted for but not displayed
                for(i=0; i<=m_numBehaviorsDefined; i++)
                {
                    m_matrixTextBox[TransitionRow][i].Enabled = false;
                    m_matrixTextBox[TransitionRow][i].Text = "";
                    m_matrixTextBox[TransitionRow][i].BackColor = SystemColors.Control;

                }
                m_T50TextBox[TransitionRow].Text = "";
                m_T50TextBox[TransitionRow].Enabled = false;
                m_T50TextBox[TransitionRow].BackColor = SystemColors.Control;

                m_slopeTextBox[TransitionRow].Text = "";
                m_slopeTextBox[TransitionRow].Enabled = false;
                m_slopeTextBox[TransitionRow].BackColor = SystemColors.Control;
            }
        }

        private void MatrixTextChangeCheckTransitionRow(int RowIndex, int ColIndex)
        {
            int i, j;
            double vi, vj;
            //double displayVal, val, cumulativeVal;

            Boolean errorFound = false;

            // Initially reset the colors of each text box associated with the time period
            // being considered of the transion matrix to either normal color or error
            // color.
            for(i=0; i<=m_numBehaviorsDefined; i++)
            {
                if(m_bMatrixError[RowIndex][i] == false)
                    m_matrixTextBox[RowIndex][i].BackColor = SystemColors.Window;
                else
                    m_matrixTextBox[RowIndex][i].BackColor = Color.Tomato;
            }

            // Compare each element
            if(m_simpleProbabilityDisplay == false)
            {
                for(i = 0; i <= m_numBehaviorsDefined; i++)
                {
                    vi = CStringUtil.SzToDouble(m_matrixTextBox[RowIndex][i].Text);
                    for(j = i + 1; j <= m_numBehaviorsDefined; j++)
                    {
                        // The value at index i must not be greater than the value at index j.
                        // Otherwise change the text box's background color to yellow
                        vj = CStringUtil.SzToDouble(m_matrixTextBox[RowIndex][j].Text);

                        if(vi > vj)
                        {
                            if(m_bMatrixError[RowIndex][i] == false)
                            {
                                m_matrixTextBox[RowIndex][i].BackColor = Color.Tomato;
                                m_matrixCheckPasses = false;
                                CheckEnableOKButtonStatus();
                                errorFound = true;
                            }
                            if(m_bMatrixError[RowIndex][j] == false)
                            {
                                m_matrixTextBox[RowIndex][j].BackColor = Color.Tomato;
                                m_matrixCheckPasses = false;
                                CheckEnableOKButtonStatus();
                                errorFound = true;
                            }
                        }
                    }
                }
            }
            else //(m_simpleProbabilityDisplay == true)
            {
                decimal cumulativeVal;
                decimal delta;
                decimal[] dArray = new decimal[m_matrixTextBox[RowIndex].Length];

                for(cumulativeVal=0, i=0; i <= m_numBehaviorsDefined; i++)
                {
                    // Grab the currently displayed element value in value, add it to the
                    // cumulated value, then store the cumulated value into the temporary
                    // double array.
                    dArray[i] = (decimal)CStringUtil.SzToDouble(m_matrixTextBox[RowIndex][i].Text);
                    cumulativeVal += dArray[i];
                    //dArray[i] = cumulativeVal;
                }

                // delta needs to be zero.
                delta = (decimal)1.0-cumulativeVal;

                if(delta > 0)
                {
                    // Total cumulative probability was less than 1.
                    dArray[m_numBehaviorsDefined] += delta; // makes the total cumulative propability 1.
                }
                else if(delta < 0)
                {
                    delta = -delta; // set postive.
                    // Total cumulative probability was greater than 1.
                    for(i=m_numBehaviorsDefined; i>0 && delta != 0 ; i--)
                    {
                        // Don't modify box currently being edited by user.
                        if(i == ColIndex)
                            continue;
                    
                        if(dArray[i] - delta < 0)
                        {
                            delta -= dArray[i];
                            dArray[i] = 0;
                        }
                        else
                        {
                            dArray[i] -= delta;
                            delta = 0;
                        }
                    }
                }

                for(i=m_numBehaviorsDefined; i>0; i--)
                {
                    // Don't modify box currently being edited by user.
                    if(i == ColIndex)
                        continue;

                    m_matrixTextBox[RowIndex][i].Text = String.Format("{0:0.0000}", (double)dArray[i]);
                }


                // Verify cumulative value is coming out to 1.0
                for(cumulativeVal=0, i=0; i <= m_numBehaviorsDefined; i++)
                    cumulativeVal += dArray[i];

                Debug.Assert(cumulativeVal == (decimal)1.0);
                if(cumulativeVal != (decimal)1.0)
                {
                    CheckEnableOKButtonStatus();
                    errorFound = true;
                }
            }

            if(errorFound == true)
                return;

            m_matrixCheckPasses = true;
            CheckEnableOKButtonStatus();
        }

        private void RunErrorCheck()
        {
            int i, j;
            for(i = 0; i < m_bMatrixError.Length; i++)
            {
                for(j = 0; j < m_bMatrixError[i].Length; j++)
                {
                    if(m_bMatrixError[i][j] == true)
                    {
                        m_matrixCheckPasses = false;
                        CheckEnableOKButtonStatus();

                        // Added 12-19-09 because didn't make sense without
                        return;
                    }
                }
            }
            m_matrixCheckPasses = true;
            CheckEnableOKButtonStatus();
        }

        // Sets the status of the OK enabled button and returns that status.
        private Boolean CheckEnableOKButtonStatus()
        {

            if(m_matrixCheckPasses == true && m_T50SlopeVarsPass == true)
                MyOKButton.Enabled = true;
            else
                MyOKButton.Enabled = false;

            if(m_T50SlopeVarsPass == true)
                ProbDispButton.Enabled = true;
            else
                ProbDispButton.Enabled = false;

            return MyOKButton.Enabled;
        }



        // Called only when this form is launched.
        private void SetInitialGuiState()
        {
            int i, j;

            // Quick Exit Button
            if(CUtil.DebugDefined == true)
                QuickExitButton.Visible = true;

            // Set the string displayed on the Probability Display Button.
            if(m_simpleProbabilityDisplay == true)
                ProbDispButton.Text = m_simpleProbabilityString;
            else
                ProbDispButton.Text = m_cumulativeProbabilityString;


           if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.INITIAL_BEHAVIOR)
           {
               // Dialog box title string              
               Text = "Intial Behavior Matrix -Biomimetica";

               ManageDepthSpanControls();
               // Transition Depths
               TransitionDepthRangeLabel.Text = "";
               TransitionDepthRangeUnderlineLabel.Text = "";


               TransitionDepthRangeLabel.Visible = false;
               TransitionDepthRangeUnderlineLabel.Visible = false;


               // Transition Clock
               //TransitionClockLabel.Text = "Clock";

               // Transition Matrix
               TMTitleLabel.Text = "Initial Behavior Matrix";

               // T50 labels
               T50Label.Enabled = false;
               minLabel.Enabled = false;
               SlopeLabel.Enabled = false;

               TermCoeffLabel.Enabled = false; // K slope label             
               TermFormButton.Enabled = false;
               TermFormLineLabel.Enabled = false;
           }
           else
           {
               Debug.Assert(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION);

               // Dialog box title string
               Text = " Behavior " + (m_behaviorIndex+1) + " Transition: (" + m_behaviorNameArray[m_behaviorIndex] + ")   -Biomimetica";

               // Span Controls
               ManageDepthSpanControls();

               // Transition Clock
               //TransitionClockLabel.Text = "Transition Clock";

               // Transition Matrix
               TMTitleLabel.Text = "Transition Matrix";
           }


            // Behavior Labels (located above behavior columns).
            // Locations are fixed so only need to hide titles of non-defined behaviors.
            for(i=m_numBehaviorsDefined+1; i<=MBGUICONSTANTS.MAXNUMBEHAVIORS; i++)
            {
                m_behTitleTextBox[i].Text = "";
                m_behTitleTextBox[i].Visible = false;
                m_behTitleTextBox[i].Enabled = false;
            }

            // Disable and hide matrix elements for non-defined behaviors and place the
            // T50 and slope k text boxes
            for(i=0; i<MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS; i++)
            {
                // Matrix elements for non-defined behaviors
                for(j=m_numBehaviorsDefined+1; j<=MBGUICONSTANTS.MAXNUMBEHAVIORS; j++)
                {
                    m_matrixTextBox[i][j].Enabled = false;
                    m_matrixTextBox[i][j].Visible = false;
                }

                if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.INITIAL_BEHAVIOR)
                {
                    //m_T50TextBox[i].Visible = false;
                    m_T50TextBox[i].Enabled = false;

                    //m_slopeTextBox[i].Visible = false;
                    m_slopeTextBox[i].Enabled = false;
                }
            }

            // The controls associated with the matrix model.
            ManageMatrixControls();
        }


        private void DetermineControlPlacementAndSize()
        {
            m_cntrlPlacement.upperLeft = TransitionDepthRangeLabel.Location;
        }


        private void AdjustLocation(Control C, int X, int Y) { C.Location=new Point(C.Location.X+X, C.Location.Y+Y); }
        private void SetLocation(Control C, int X, int Y) { C.Location = new Point(X, Y); }
        private int GetXPlusWidth(Control C) { return C.Location.X + C.Width; }
        private int GetYPlusHeight(Control C) { return C.Location.Y + C.Height; }

        private void SetControlPlacement()
        {
            int i;
            int dxm = 0, dym = 0; // deltas of the objects in the matrix.

            // T50 termination coefficient and slope placement calculation
            dxm = m_T50TextBox[0].Location.X - (GetXPlusWidth(m_matrixTextBox[0][m_numBehaviorsDefined]) + PIXELSBETWEENBEHCOLANDT50COL);

            // Disable and hide matrix elements for non-defined behaviors and place the
            // T50 and slope k text boxes
            for(i=0; i<MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS; i++)
            {
                // Place T50 termination coefficient and slope k
                AdjustLocation(m_T50TextBox[i], -dxm, 0);
                AdjustLocation(m_slopeTextBox[i], -dxm, 0);
            }

            //--------------------------------------------------------------------------//
            // Form Size and Control Placement
            // Size and placement depend on the number of behaviors defined.
            //--------------------------------------------------------------//
            AdjustLocation(TermCoeffLabel, -dxm, 0);
            AdjustLocation(T50Label, -dxm, 0);
            AdjustLocation(minLabel, -dxm, 0);
            AdjustLocation(SlopeLabel, -dxm, 0);
            AdjustLocation(TermFormButton, -dxm, 0);
            AdjustLocation(TermFormLineLabel, -dxm, 0);

            //SpanGroupBox.Width = SlopeLabel.Location.X + SlopeLabel.Width + 20;
            SpanGroupBox.Width -= dxm;

            TMLineLabel.Width -= dxm;
            TMTitleLabel.Width -= dxm;

            // Handle the cases where there are too few behaviors (set to 4 or less)
            // to allow the Bheavior Transition Label to be wide enough so that it may
            // nicely display its string.
            TMTitleLabel.Text = "Behavior Transition Matrix";
            if(m_numBehaviorsDefined < 4)
            {
                // Add the current height of the transition matrix label into variable
                // dym.  It will be subtracted to get the change in height that
                // results from the label expanding by setting it to auto size then
                // placing mulitple lines into i.
                dym = TMTitleLabel.Height;
                //dym = 0;
                TMTitleLabel.AutoSize = true;
                if(m_numBehaviorsDefined == 1)
                    TMTitleLabel.Text = "Behavior\nTransition\nMatrix";
                else
                    TMTitleLabel.Text = "Behavior\nTransition Matrix";
                dym -= TMTitleLabel.Height; // determines the change in height.

                TMTitleLabel.Location = new Point(TMTitleLabel.Location.X, TMTitleLabel.Location.Y + dym);

                SpanGroupBox.Location = new Point(SpanGroupBox.Location.X, SpanGroupBox.Location.Y + dym);
                SpanGroupBox.Height += (-dym);
            }

            // Recenter the Behavior Transition Label with respect to the line under it.
            TMTitleLabel.Location = new Point(TMLineLabel.Location.X + (TMLineLabel.Width-TMTitleLabel.Width)/2,
                                              TMTitleLabel.Location.Y);

                //m_behTitleTextBox[m_numBehaviorsDefined].Location.X - ZeroProb.Location.X + ZeroProb.Width + 50;

            this.Width = SpanGroupBox.Location.X + SpanGroupBox.Width + 50;
        }


        //-----------------------------------------------------------------------------//
        // GUI Event Responses
        //-----------------------------------------------------------------------------//
        private void SetTransitionFormulaText()
        {
            switch(m_behTermMdl)
            {
                case BEHTRANS_TERM_MODEL.GAUSSIAN_TERM:
                    TermCoeffLabel.Visible = true;
                    TermCoeffLabel.Width = 35;
                    T50Label.Visible = false;
                    TermCoeffLabel.Text = "Mean";
                    minLabel.Text = "(min)";

//                    SlopeLabel.Font = new Font("Microsoft Sans Serif", 6.25F);
                    SlopeLabel.Text = "Std Dev (min)";
                    break;
                case BEHTRANS_TERM_MODEL.T50_K_TERM:
                default:
                    TermCoeffLabel.Width = 15;
                    TermCoeffLabel.Visible = true; // T

                    T50Label.Visible = true; // 50
                    minLabel.Text = "(min)";
                    SlopeLabel.Text = "k";
                    TermCoeffLabel.Text = "T";
                    break;
            }
        }
        private void TermFormButton_Click(object sender, EventArgs e)
        {
            switch(m_behTermMdl)
            {
                case BEHTRANS_TERM_MODEL.T50_K_TERM:
                    m_behTermMdl = BEHTRANS_TERM_MODEL.GAUSSIAN_TERM;
                    break;
                case BEHTRANS_TERM_MODEL.GAUSSIAN_TERM:
                    m_behTermMdl = BEHTRANS_TERM_MODEL.T50_K_TERM;
                    break;
                default:
                    m_behTermMdl = BEHTRANS_TERM_MODEL.T50_K_TERM;
                    break;
            }
            SetTransitionFormulaText();

            // Indicate a modification.  
            SetModified();

        }

        private void CheckClockStartAndEndTimes(CMatrix M)
        {

#if false
            int lastRow;
            if(M == null || M.RowCount == 0)
                return;

            if(CUtil.TimeIsZeroOr24(M[0][0]) == true && CUtil.TimeIsZeroOr24(M[0][1]) == true)
            {
                M[0][0] = 0.0;
                M[0][1] = 24.0;
            }
#endif  
        }


        private void ManageClocksAfterModifiedSpan()
        {
            double[] prevRow = null, thisRow = null, nextRow = null;
            int i;

            // The GUI is (or must be) programmed to not allow the user to modify a time
            // span if there's only a single time span (row).  There is simply no need to.
            Debug.Assert(Matrix.RowCount > 1);

            prevRow = Matrix.a[CUtil.IndexToCircularIndex(m_activeMatrixRowIndex-1, Matrix.RowCount)];
            thisRow = Matrix.a[m_activeMatrixRowIndex]; // The promoted row
            nextRow = Matrix.a[CUtil.IndexToCircularIndex(m_activeMatrixRowIndex+1, Matrix.RowCount)];

            // If the modification did not cross over midnight it is a simple modification.
            i = CUtil.IndexToCircularIndex(m_activeMatrixRowIndex+1, Matrix.RowCount);
            while(i != m_activeMatrixRowIndex)
            {
                if(nextRow[0] < thisRow[1])
                    nextRow[0] = thisRow[1];
                else
                    break;

                //if(nextRow[1] < thisRow[1]
                 //   nextRow[1] = thisRow[1];
            }

            if(thisRow[0] < thisRow[1])
            {
                prevRow[1] = thisRow[0];
                nextRow[0] = thisRow[1];
                return;
            }

        }

        private void ClkSpanButton_Click(object sender, EventArgs e)
        {
            int index;
            FromSpanClock frm;
            FORMSPANINPUT input = new FORMSPANINPUT();
            double[] prevRow = null, thisRow = null, nextRow = null;

            if(m_bDepthSpanActiveLocked == true)
                return;
            m_bDepthSpanActiveLocked = true;

            

            // Copy the current GUI control values into the matrix.
            GuiControlsToMatrixModel();

            // If the button clicked is the same as the previously clicked button then
            // open up the time span dialog box.
            if(m_activeMatrixRowIndex == (index = GuiUtils.MatchIndex(m_clkSpanButton, (Button)sender)))
            {
                Debug.Assert(index < Matrix.RowCount);
                // Initialize the FORMSPANINPUT struct as needed that is passed into the dialog box.

                input.szRange = "<= time of day <";
                input.szCaption = "Behavior Transition Time Span";
                input.szInstruction = "Set a clock span (HH:MM):";

                // Modify the FORM_SPAN_INT struct as needded.              
                thisRow = Matrix.a[index];
                input.minInput = thisRow[0]; // start of clock span
                input.maxInput = thisRow[1]; // end of clock span

                input.lowerLimit = 0;
                input.uppperLimit = 24.0;

                //if(index == CUtil.IndexToCircularBufferIndex(index + 1, Matrix.RowCount))
                  //  index = index;

                input.minInputIsReadOnly = input.maxInputIsReadOnly = false;
                if(Matrix.RowCount == 1)
                {
                    // If there is only a single row in the behavior transition matrix
                    // there is no need to be able to set the time span so make the time
                    // span read only
                    input.minInputIsReadOnly = input.maxInputIsReadOnly = true;
                }
                else
                {
                    // If there are only two rows then the previous row will be the same
                    // as the next row.  This is allowed.
                    prevRow = Matrix.a[CUtil.IndexToCircularIndex(index - 1, Matrix.RowCount)];
                    nextRow = Matrix.a[CUtil.IndexToCircularIndex(index + 1, Matrix.RowCount)];
                }

                frm = new FromSpanClock(input);
                frm.Location = 
                    new Point(this.Location.X + ((Button)sender).Location.X + ((Button)sender).Width + 10,
                              this.Location.Y + ((Button)sender).Location.Y + ((Button)sender).Height/2- frm.Height/2);

                frm.ShowDialog();

                if(frm.Modified == true)
                {
                    m_bModified = true;
                    thisRow[0] = frm.MinResult;
                    thisRow[1] = frm.MaxResult;
                    if(prevRow != null)
                        prevRow[1] = thisRow[0];
                    if(nextRow != null)
                        nextRow[0] = thisRow[1];
                }
            }
            m_activeMatrixRowIndex = index;
            ManageMatrixControls();
            m_bDepthSpanActiveLocked = false;

        }

        private void MangeClockOnPromotedRow()
        {
            double[] prevRow = null, thisRow = null, nextRow = null;

            // The GUI is (or must be) programmed to not allow the very a row to deleted
            // when there is only one remaining.  Therefore when this funciton is called
            // there should be at least two rows in the matrix.
            Debug.Assert(Matrix.RowCount >= 1);

            prevRow = Matrix.a[CUtil.IndexToCircularIndex(m_activeMatrixRowIndex-1, Matrix.RowCount)];
            thisRow = Matrix.a[m_activeMatrixRowIndex]; // The promoted row
            nextRow = Matrix.a[CUtil.IndexToCircularIndex(m_activeMatrixRowIndex+1, Matrix.RowCount)];

            // If this is the last remaining row simply set the start and end times to 0
            // and 24.
            if(Matrix.RowCount == 1)
            {
                thisRow[0] = 0.0; // start time is set to 0
                thisRow[1] = 24.0; // end time is set to 24
                return;
            }

            // The promoted row maintains its current end time but its start time is set
            // to the preceeding row's end time.
            thisRow[0] = prevRow[1];

            if(m_activeMatrixRowIndex == Matrix.RowCount-1 && m_activeMatrixRowIndex != 0)
            {
                // If this row is at the bottom of the matrix and it is not the only row
                // in the model then for asthestic reasons if its clock span's begining
                // clock and end clock are both set to 0 or 24 (both at midnight) change
                // both to 24.
                if(CUtil.TimeIsZeroOr24(thisRow[0]) == true && CUtil.TimeIsZeroOr24(thisRow[1]) == true &&
                    Matrix.RowCount >= 2)
                {
                    thisRow[0] = CUtil.HHMMSSToTwentyHrFloatClockInclusive(24, 0, 0); // start time is set to 24:00:00
                    thisRow[1] = CUtil.HHMMSSToTwentyHrFloatClockInclusive(24, 0, 0); // end time is set to 24:00:00
                }
            }
        }


        private void ManageClockOnAddedSpan()
        {
            double[] prevRow = null, thisRow = null, nextRow = null;
            int index = Matrix.RowCount-1;

            // The GUI is (or must be) programmed to not allow the very a row to deleted
            // when there is only one remaining.  Therefore when this funciton is called
            // there should be at least two rows in the matrix.
            Debug.Assert(Matrix.RowCount >= 2);

            prevRow = Matrix.a[CUtil.IndexToCircularIndex(index-1, Matrix.RowCount)];
            thisRow = Matrix.a[index]; // The newly added row.
            nextRow = Matrix.a[CUtil.IndexToCircularIndex(index+1, Matrix.RowCount)];

            // Set the start time to the end time of the previous clock span, set the end
            // time to the start time of the next clock time span, and initialize the zero
            // column to zero.
            thisRow[0] = prevRow[1]; // start time is set to 24:00:00
            thisRow[1] = nextRow[0]; // end time is set to 24:00:00
            thisRow[2] = 0; // zero transition matrix


            if(index == Matrix.RowCount-1 && index != 0)
            {
                // If this row is at the bottom of the matrix and it is not the only row
                // in the model then for asthestic reasons if its clock span's begining
                // clock and end clock are both set to 0 or 24 (both at midnight) change
                // both to 24.
                if(CUtil.TimeIsZeroOr24(thisRow[0]) == true && CUtil.TimeIsZeroOr24(thisRow[1]) == true &&
                    Matrix.RowCount >= 2)
                {
                    thisRow[0] = CUtil.HHMMSSToTwentyHrFloatClockInclusive(24, 0, 0); // start time is set to 24:00:00
                    thisRow[1] = CUtil.HHMMSSToTwentyHrFloatClockInclusive(24, 0, 0); // end time is set to 24:00:00
                }
            }

            if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION)
            {
                // Matrix.RowCount-1 is the newly added row's index.
                thisRow[Matrix.ColumnCount-2] = 60;
                thisRow[Matrix.ColumnCount-1] = 7;
            }

        }

        // User is adding a row 
        private void AddClockRowButton_Click(object sender, EventArgs e)
        {
            // Prevent user from repeated clicks.
            if(m_bDepthSpanActiveLocked == true || Matrix.RowCount == MBGUICONSTANTS.MAXNUMTRANSITIONPERIODS)
                return;           
            m_bDepthSpanActiveLocked = true;

            // The GUI must have prevented user from deleting all rows.  The Matrix can
            // handle having no rows but for ease of use best no to since at least one row
            // is needed for a valid model.
            Debug.Assert(Matrix.RowCount >= 1);

            // Indicate a modification.  
            SetModified();

            // Copy the current GUI control values into the matrix.
            GuiControlsToMatrixModel();

            // Add the row.  AddRow() automatically contains the correct number of
            // initialized columns.
            Matrix.AddRow();
            ManageClockOnAddedSpan();


            // Place the matrix values into the GUI and update GUI controls to reflect the changes made.
            ManageMatrixControls();

            m_bDepthSpanActiveLocked = false;
        }

        // User is deleting the active row.
        private void DeleteClkButton_Click(object sender, EventArgs e)
        {
            // Prevent user from repeated clicks.
            if(m_bDepthSpanActiveLocked == true || Matrix.RowCount == 1)
                return;
            m_bDepthSpanActiveLocked = true;

            // The GUI must prevent user from deleting all rows.  The Matrix can handle
            // having no rows but for ease of use best no to since at least one row is
            // needed for a valid model.
            Debug.Assert(Matrix.RowCount >= 2);

            // The user is adding a transition row so indicate a modification.  
            SetModified();

            // The Matrix Rows
            GuiControlsToMatrixModel();

            Matrix.DeleteRow(m_activeMatrixRowIndex);

            // If this was the last row then decrement the value stored in
            // m_activeMatrixRowIndex so that the active matrix row becomes the previous
            // row
            if(m_activeMatrixRowIndex == Matrix.RowCount)
                m_activeMatrixRowIndex--;

            MangeClockOnPromotedRow();

            // Call ManageDepthSpanControls() to update the displayed clock period count
            // on the span button.
            ManageDepthSpanControls();

            // Reset the text box controls on the dialog box matrix
            ManageMatrixControls();

            m_bDepthSpanActiveLocked = false;
        }

        private void DuplicateClkButton_Click(object sender, EventArgs e)
        {
            if(m_bDepthSpanActiveLocked == true || Matrix.RowCount == 0)
                return;
            m_bDepthSpanActiveLocked = true;

            GuiControlsToMatrixModel();
            SetModified();

            // Add a duplicate row to the end of the matrix.  Manually set the start time and end time, however.
            Matrix.AddRow(Matrix.a[m_activeMatrixRowIndex]);
            Matrix.a[Matrix.RowCount-1][0] = CUtil.HHMMSSToTwentyHrFloatClockInclusive(24, 0, 0); // start time is set to 24:00:00
            Matrix.a[Matrix.RowCount-1][1] = CUtil.HHMMSSToTwentyHrFloatClockInclusive(24, 0, 0); // end time is set to 24:00:00


            // Call ManageDepthSpanControls() to update the displayed clock period count
            // on the span button.
            ManageDepthSpanControls();

            // Reset the text box controls on the dialog box matrix
            ManageMatrixControls();

            m_bDepthSpanActiveLocked = false;
        }

        private void PromoteClkButton_Click(object sender, EventArgs e)
        {
            double[] upperRow, lowerRow;
            int upper, lower;
            double fval;

            if(m_bDepthSpanActiveLocked == true || m_activeMatrixRowIndex == 0)
                return;

            m_bDepthSpanActiveLocked = true;

            GuiControlsToMatrixModel();
            SetModified();

            upper = m_activeMatrixRowIndex-1;
            lower = m_activeMatrixRowIndex;

            upperRow = Matrix.a[upper];
            lowerRow = Matrix.a[lower];

            Matrix.a[upper] = lowerRow;
            Matrix.a[lower] = upperRow;

            // Keep the clock values in their original row
            fval = Matrix.a[upper][0];
            Matrix.a[upper][0] = Matrix.a[lower][0];
            Matrix.a[lower][0] = fval;

            fval = Matrix.a[upper][1];
            Matrix.a[upper][1] = Matrix.a[lower][1];
            Matrix.a[lower][1] = fval;

            m_activeMatrixRowIndex--;

            // Call ManageDepthSpanControls() to update the displayed clock period count
            // on the span button.
            //ManageDepthSpanControls();

            ManageMatrixControls();

            m_bDepthSpanActiveLocked = false;
        }

        private void DemoteClkButton_Click(object sender, EventArgs e)
        {
            double[] upperRow, lowerRow;
            int upper, lower;
            double fval;

            if(m_bDepthSpanActiveLocked == true || m_activeMatrixRowIndex == Matrix.ColumnCount-1)
                return;
            m_bDepthSpanActiveLocked = true;

            GuiControlsToMatrixModel();
            SetModified();

            upper = m_activeMatrixRowIndex;
            lower = m_activeMatrixRowIndex+1;

            upperRow = Matrix.a[upper];
            lowerRow = Matrix.a[lower];

            Matrix.a[upper] = lowerRow;
            Matrix.a[lower] = upperRow;

            // Keep the clock values in their original row
            fval = Matrix.a[upper][0];
            Matrix.a[upper][0] = Matrix.a[lower][0];
            Matrix.a[lower][0] = fval;

            fval = Matrix.a[upper][1];
            Matrix.a[upper][1] = Matrix.a[lower][1];
            Matrix.a[lower][1] = fval;


            m_activeMatrixRowIndex++;


            // Call ManageDepthSpanControls() to update the displayed clock period count
            // on the span button.
            //ManageDepthSpanControls();

            ManageMatrixControls();

            m_bDepthSpanActiveLocked = false;
        }

        private void DepthSpanButton_Click(object sender, EventArgs e)
        {
            int index;
            FormSpanInt frm;
            FORMSPANINPUT input = new FORMSPANINPUT();
            CSpan prev = null, current = null, next = null;

            if(m_bDepthSpanActiveLocked == true)
                return;
            m_bDepthSpanActiveLocked = true;

            // Transfer information from the GUI controls into the matrix
            GuiControlsToMatrixModel();

            if(m_activeSpanIndex == (index = GuiUtils.MatchIndex(m_spanButton, (Button)sender)))
            {
                // Initialize the FORM_SPAN_INT struct as needded.
                input.minInputIsReadOnly = input.maxInputIsReadOnly = true;
                input.szRange = "< Animat Depth <=";
                input.szCaption = "Behavior Transition Depth Span";
                input.szInstruction = "Set a depth span (meters):";

                // Modify the FORM_SPAN_INT struct as needded.
                current = m_spanMgr.GetSpan(index);
                input.minInput = -(int)current.shallow;
                input.maxInput = -(int)current.deep;

                input.lowerLimit = 0;
                input.uppperLimit = 3500;

                if(index > 0 && null != (prev = m_spanMgr.GetSpan(index-1)))
                {
                    input.lowerLimit = -(int)prev.shallow;
                    input.minInputIsReadOnly = false;
                }

                if(index+1 < m_spanMgr.SpanCount && null != (next = m_spanMgr.GetSpan(index+1)))
                {
                    input.uppperLimit = -(int)next.deep;
                    input.maxInputIsReadOnly = false;
                }

                if(input.lowerLimit == 0)
                    input.szRange = "<= Animat Depth <=";

                frm = new FormSpanInt(input);

                frm.Location =
                    new Point(this.Location.X + ((Button)sender).Location.X + ((Button)sender).Width + 10,
                              this.Location.Y + ((Button)sender).Location.Y + ((Button)sender).Height/2- frm.Height/2);


                frm.ShowDialog();
                if(frm.Modified == true)
                {
                    m_bModified = true;
                    current.shallow = -(double)frm.MinResult;
                    current.deep = -(double)frm.MaxResult;
                    if(prev != null)
                        prev.deep = current.shallow;
                    if(next != null)
                        next.shallow = current.deep;
                }
            }



            // Mangage depth span buttons
            m_activeSpanIndex = index;
            ManageDepthSpanControls();

            // Transfer the matrix into the GUI.
            m_activeMatrixRowIndex = 0;
            ManageMatrixControls();

            m_bDepthSpanActiveLocked = false;
        }


        private void AddSpanButton_Click(object sender, EventArgs e)
        {
            CSpan newSpan;

            // Initial do not have T50 or k's.
            //Debug.Assert(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION);

            // Prevent user from repeated clicks.
            if(m_bDepthSpanActiveLocked == true || m_spanMgr.SpanCount == MBGUICONSTANTS.MAXNUMSPANS)
                return;           
            m_bDepthSpanActiveLocked = true;

            // Transfer information from the GUI controls into the matrix
            GuiControlsToMatrixModel();


            // Indicate a modification.  
            SetModified();

            /* Have the span manager add a span and adjust the number of behavior
             * transition columns to match the number of behaviors curretnly modeled. */
            newSpan = m_spanMgr.AddSpan();
            while(newSpan.behaviorTransCount < m_numBehaviorsDefined)
                newSpan.IncrementSpanBehaviorTransitionVectorElement();
            newSpan.deep = newSpan.shallow = -3500.0;

            ManageDepthSpanControls();

            // Transfer the matrix into the GUI.
            m_activeMatrixRowIndex = 0;
            ManageMatrixControls();

            m_bDepthSpanActiveLocked = false;
        }


        // Deletes the span at the active span index.  Since an array list contains the
        // spans all spans move up one index allowing the active span index to remain
        // valid unless the active index was at the end of the array.
        private void DeleteSpanButton_Click(object sender, EventArgs e)
        {
            // At least two spans must exist in the array list for the delete button to
            // have been enabled.
            Debug.Assert(m_spanMgr.SpanCount >= 2);
            if(m_bDepthSpanActiveLocked == true || m_spanMgr.SpanCount <= 1)
                return;

            // Lock the ability to edit spans while this function is running and set this
            // form to 'modified'.
            m_bDepthSpanActiveLocked = true;
            SetModified();

            // Transfer information from the GUI controls into the matrix
            GuiControlsToMatrixModel();

            // Delete the span at the active index.
            m_spanMgr.DeleteSpan(m_activeSpanIndex);

            if(m_activeSpanIndex == 0)
            {
                // The first nd most shallow span was deleted so set the new shallow value
                // of the new most shallow span (newly promoted to index 0) to zero
                // meters.
                m_spanMgr.GetSpan(m_activeSpanIndex).shallow = 0;
            }
            else if(m_activeSpanIndex == m_spanMgr.SpanCount)
            {
                // The last and deepest span was deleted.  The span at the index one above
                // the deleted span's is now the deepest.  Decrement the active span index
                // then set the newly deepst span deep value to the maximum depth.
                m_activeSpanIndex--;
                m_spanMgr.GetSpan(m_activeSpanIndex).deep = -3500.0;
            }
            else
            {
                // Deleting a span in the middle somewhere.  All spans below the delete
                // span move up one index.  Set the deep value of the previous span to the
                // promoted span's deep value.
                m_spanMgr.GetSpan(m_activeSpanIndex-1).deep =
                    m_spanMgr.GetSpan(m_activeSpanIndex).shallow;
            }

            // Set up the depth range button controls.
            ManageDepthSpanControls();

            // Reset the text box controls on the dialog box matrix
            m_activeMatrixRowIndex = 0; 
            ManageMatrixControls();

            m_bDepthSpanActiveLocked = false;

        }



        private void DuplicateSpanButton_Click(object sender, EventArgs e)
        {
            CSpan s;
            if(m_bDepthSpanActiveLocked == true || m_spanMgr.SpanCount == 0)
                return;

            // Prevent user from mulitple clicks.
            m_bDepthSpanActiveLocked = true;

            // Indicate the model has been modified.
            m_bModified = true;

            // Transfer information from the GUI controls into the matrix
            GuiControlsToMatrixModel();

            // Get a copy of the current span.
            s = m_spanMgr.GetSpan(m_activeSpanIndex).GetCopy();

            // Duplicate spans are added to the bottom (end) of the span, so set an added
            // span's range to the maximum depth.
            s.shallow = s.deep = -3500.0;
            m_spanMgr.AddSpan(s);

            // Set up the depth range button controls.
            ManageDepthSpanControls();
            m_bDepthSpanActiveLocked = false;

            // Transfer the matrix into the GUI.
            m_activeMatrixRowIndex = 0; 
            ManageMatrixControls();
        }


        private void PromoteSpanButton_Click(object sender, EventArgs e)
        {
            CSpan a, b;
            double shallow, deep;

            Debug.Assert(m_activeSpanIndex != 0);

            if(m_bDepthSpanActiveLocked == true || m_spanMgr.SpanCount == 0)
                return;
            m_bDepthSpanActiveLocked = true;

            // Transfer information from the GUI controls into the matrix
            GuiControlsToMatrixModel();

            m_bModified = true;

            a = m_spanMgr.GetSpan(m_activeSpanIndex);
            b = m_spanMgr.GetSpan(m_activeSpanIndex-1);

            shallow = a.shallow;
            deep = a.deep;

            a.shallow = b.shallow;
            a.deep = b.deep;

            b.shallow = shallow;
            b.deep = deep;

            m_spanMgr.SwapSpans(m_activeSpanIndex, m_activeSpanIndex - 1);

            m_activeSpanIndex--;

            // Set up the depth range button controls.
            ManageDepthSpanControls();

            // Transfer the matrix into the GUI.
            m_activeMatrixRowIndex = 0;
            ManageMatrixControls();

            m_bDepthSpanActiveLocked = false;

        }


        private void DemoteSpanButton_Click(object sender, EventArgs e)
        {
            CSpan a, b;
            double shallow, deep;

            Debug.Assert(m_activeSpanIndex != m_spanMgr.SpanCount-1);

            if(m_bDepthSpanActiveLocked == true || m_activeSpanIndex == m_spanMgr.SpanCount-1)
                return;
            m_bDepthSpanActiveLocked = true;

            // Transfer information from the GUI controls into the matrix
            GuiControlsToMatrixModel();

            m_bModified = true;

            a = m_spanMgr.GetSpan(m_activeSpanIndex);
            b = m_spanMgr.GetSpan(m_activeSpanIndex + 1);

            shallow = a.shallow;
            deep = a.deep;

            a.shallow = b.shallow;
            a.deep = b.deep;

            b.shallow = shallow;
            b.deep = deep;

            m_spanMgr.SwapSpans(m_activeSpanIndex, m_activeSpanIndex+1);

            m_activeSpanIndex++;

            // Set up the depth range button controls.
            ManageDepthSpanControls();
            //ManageTransitionDepthRangeDisplayState();

            // Transfer the matrix into the GUI.
            m_activeMatrixRowIndex = 0;
            ManageMatrixControls();

            m_bDepthSpanActiveLocked = false;

        }

         // Called whenever text of any of the matrix elements change.  Only interested,
        // however, in changes made by the user so this will return if the change is non-user made.
        private ROWCOL m_matrixLastIndex;  // Just a variable to make matix index matching faster.  Serves no other purpose.
        private void MatrixText_Changed(object sender, EventArgs e)
        {
            double val;
            String sz;
            int row, col;


            m_matrixLastIndex = GuiUtils.MatchIndex(m_matrixTextBox, (TextBox)sender, m_matrixLastIndex);
            col = m_matrixLastIndex.col;
            row = m_matrixLastIndex.row;

            // Only interested in changes to the text made by the user.  If entered by
            // user this text box will be focused.
            if(m_matrixTextBox[row][col].Focused == false)
                return;

            //-----------------//
            // Debug assertions
            //-----------------//
            // Assert valid rows and columns.  Column is checked against 1 instead of
            // zero because the zero-th matrix isn't currently used or modified.
            Debug.Assert(row >= 0 && row <m_matrixTextBox.Length);
            Debug.Assert(col >= 1 && col <m_matrixTextBox[row].Length);
            // TODO: Add error handling.

            // User has modified the value in the text box so indicate a change that
            // prevents user from exiting the associated dialog box without changing
            // without confirmation.
            SetModified();

            //--------------------------------------------------------------------------//
            // Verify that a valid floating point number was entered.
            //------------------------------------------------------//
            // Verify string entered by user is a valid float by grabbing a copy of the
            // text box's sting and forcing it into a valid float format then compare
            // to original string.  If strings don't match the value entered by the user
            // was not valid.  If not valid set the text box string to the valid value
            // then return.
            // Important here is that by setting the string to the new value causes this
            // function to be called again so the process starts over again.
            sz = CStringUtil.SzEnforceDoubleFmt(m_matrixTextBox[row][col].Text);
            if(m_matrixTextBox[row][col].Text != sz)
            {
                m_matrixTextBox[row][col].Text = sz;
                return;
            }

            // Verify the float string is within limits by comparing against allowed
            // warning limit and maximum limit.  While editing the user is allowed to
            // momentarily have floating point value held in the edit box higher than the
            // actual maximum allowed when done editing (so the warning value will be
            // higher than the maximum value).
            sz = m_matrixTextBox[row][col].Text;
            if((val = CStringUtil.SzToDouble(sz)) > MATRIXELEMENTWARNING)
            {
                // The value entered is higher than the maximum warning value.  Reset the
                // entered value to the maximum warning value (which will cause this
                // callback function to be called again) and return.
                m_matrixTextBox[row][col].Text = String.Format("{0:0.0000}", MATRIXELEMENTWARNING);
                return;
            }

            // If the user has entered a value exceeding the maximum value, an empty
            // string, or a ".", set and display error conditions.
            if(val > MATRIXELEMENTMAX || sz == "" || sz == ".")
            {
                m_matrixTextBox[row][col].BackColor = Color.Salmon;
                m_bMatrixError[row][col] = true;
                m_matrixCheckPasses = false;

                // CheckEnableOKButtonStatus() with the matrix not passing
                // (m_matrixCheckPasses set to false) disables the OK (return) button for
                // the associated dialog box.
                CheckEnableOKButtonStatus();
                return;
            }

            //------------------------------------------------------------------
            // At this point the user entry is a valid floating point number within
            // acceptable limits.

            // If previously a bad value was associate with the matrix element update
            // its status to show the error is clear then run an error check to see
            // if OK button can be enabled.
            if(m_bMatrixError[row][col] == true) // set during a previous call
            {                                             // to this function.
                m_bMatrixError[row][col] = false;
                RunErrorCheck();
            }
            MatrixTextChangeCheckTransitionRow(row, col);
            return; // String is properly modified
        }

        private void MatrixText_Leave(object sender, EventArgs e)
        {
            //double MAXVALUE = 1.0;
            int row, col;

            m_matrixLastIndex = GuiUtils.MatchIndex(m_matrixTextBox, (TextBox)sender, m_matrixLastIndex);
            col = m_matrixLastIndex.col;
            row = m_matrixLastIndex.row;

            //-----------------//
            // Debug assertions
            //-----------------//
            // Assert valid rows and columns.  Column is checked against 1 instead of
            // zero because the zero-th matrix isn't currently used or modified.
            Debug.Assert(row >= 0 && row <m_matrixTextBox.Length);
            Debug.Assert(col >= 1 && col <m_matrixTextBox[row].Length);
            // TODO: Add error handling.

                
            // Nothing to do if no error was detected.
            if(m_bMatrixError[row][col] == false || m_matrixCheckPasses == false)
                return;

            // The following are the only conditions this callback function handles and
            // each of these conditions must have set the matrix error flag to true.
            Debug.Assert(m_matrixTextBox[row][col].Text == "" ||
                m_matrixTextBox[row][col].Text == "." ||
                CStringUtil.SzToDouble(m_matrixTextBox[row][col].Text) > MATRIXELEMENTMAX);

            // If any matrix element error flag is set to false the OK button must have
            // been set to false.
            Debug.Assert(m_matrixCheckPasses == false);
            Debug.Assert(MyOKButton.Enabled == false);

            // This function will correct the errors it handles so set the error flag
            // associated with this matrix element to false.
            m_bMatrixError[row][col] = false;

            // Handle the errors.
            if(CStringUtil.SzToDouble(m_matrixTextBox[row][col].Text) > MATRIXELEMENTMAX)
                m_matrixTextBox[row][col].Text = String.Format("{0:0.0000}", MATRIXELEMENTMAX);
            else if(m_matrixTextBox[row][col].Text == "" || m_matrixTextBox[row][col].Text == ".")
                m_matrixTextBox[row][col].Text = String.Format("{0:0.0000}", 0);
        }


        private void T50SlopeTextBox_Changed(object sender, EventArgs e)
        {
            double doubleVal;
            TextBox TB = (TextBox)sender;
            double MAXVALUE = 99999; // Highest value allowed to be entered/displayed in the
            // T50 or k slope text box.

            // Only interested in changes made by the application user.
            if(TB.Focused == false)
                return;

            // The user is modifying the model.  Indicate it as such.
            SetModified();

            // An empty string is invalid.  Change the box's background color as in
            // indication of the invalid input then disable the OK button.
            if(TB.Text == "")
            {
                TB.BackColor = Color.Tomato;
                m_T50SlopeVarsPass = false;
                CheckEnableOKButtonStatus();
                return;
            }

            // Verify it is a valid double string
            if(TB.Text != CStringUtil.SzEnforceDoubleFmt(TB.Text))
            {
                TB.Text = "";
                return; // will loop back into this event handler
            }

            // Don't allow negative values
            if(TB.Text.Length > 0 && TB.Text[0] == '-')
            {
                TB.Text = TB.Text.Remove(0, 1);
                return;
            }

            // Verify the integral string is wihtin limits
            doubleVal = CStringUtil.SzToDouble(TB.Text);
            if(doubleVal > MAXVALUE)
            {
                TB.Text = "" + MAXVALUE;
                return; // will loop back into this event handler
            }
            m_T50SlopeVarsPass = true;
            CheckEnableOKButtonStatus();
            TB.BackColor = SystemColors.Window;
        }

        private void T50SlopeTextBox_Leave(object sender, EventArgs e)
        {
            TextBox Tb = (TextBox)sender;
            if(Tb.Text == "" || Tb.Text != CStringUtil.SzEnforceDoubleFmt(Tb.Text))
                Tb.Text = "0";

            m_T50SlopeVarsPass = true;
            CheckEnableOKButtonStatus();
            Tb.BackColor = SystemColors.Window;
        }


        // Toggels the displayed probability in the matrices between simple and cumulative.
        private void ProbDispButton_Click(object sender, EventArgs e)
        {
            int i, row;
            decimal val, preVal, dispVal, cumVal;

            m_simpleProbabilityDisplay = !m_simpleProbabilityDisplay;
            if(m_simpleProbabilityDisplay == true)
                ProbDispButton.Text = m_simpleProbabilityString;
            else
                ProbDispButton.Text = m_cumulativeProbabilityString;


            for(row = 0; row < Matrix.RowCount; row++)
            {
                // Matrix elements
                // The number of matrix elements are 1 + the number of behaviors defined
                // because of the zero probability column.
                cumVal = preVal = 0;
                for(i = 0; i <= m_numBehaviorsDefined; i++)
                {

                    //val = m_matrix.a[tp][i + 2];
                    val = (decimal)CStringUtil.SzToDouble(m_matrixTextBox[row][i].Text);

                    //-------------------------------------------------------------------------------------------------
                    // Set the displayed value based on if display is for simple probability or cumulative probability
                    if(m_simpleProbabilityDisplay == true)
                    {
                        dispVal = val - preVal;
                        preVal = val;
                    }
                    else // cumulative probability
                    {
                        cumVal += val;
                        dispVal = cumVal;
                    }
                    m_matrixTextBox[row][i].Text = String.Format("{0:0.0000}", (double)dispVal);
                }
            }
        }


        private void MyOKButton_Click(object sender, EventArgs e)
        {
            GuiControlsToMatrixModel();
            Dispose();
        }

        private void MyCancelButton_Click(object sender, EventArgs e)
        {
            if(m_bModified == true)
            {
                FormConfirmDecision dlg = new FormConfirmDecision();
                dlg.Text = "Confirm Cancel";
                dlg.messageString = "Cancel Changes To Rate Model?";
                dlg.button1String = "Confirm Cancel";
                dlg.button2String = "Do Not Cancel";
                dlg.SetLocation(MyOKButton);
                dlg.ShowDialog(this);
                if(dlg.buttonSelected == 2)
                    return;
                m_bModified = false;
            }
            Dispose();
        }

        private void TestButton_Click(object sender, EventArgs e)
        {
            FrmTransitionTest dlg;
            GuiControlsToMatrixModel();
            if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION)
                dlg = new FrmTransitionTest(m_displayType, Matrix, m_behaviorNameArray, m_behaviorIndex);
            else
                dlg = new FrmTransitionTest(m_displayType, Matrix, m_behaviorNameArray);
            dlg.Show(this);
            this.BringToFront();
        }

        private void QuickExitButton_Click(object sender, EventArgs e)
        {
            Process.GetCurrentProcess().Kill();
        }


        private void SetTabOrder()
        {
            int i, j, tab = 0;
            for(i=0; i<m_matrixTextBox.Length; i++)
            {
                for(j=0; j<m_matrixTextBox[i].Length; j++)
                    m_matrixTextBox[i][j].TabIndex = tab++;

                m_T50TextBox[i].TabIndex = tab++;
                m_slopeTextBox[i].TabIndex = tab++;
            }
            MyOKButton.TabIndex = tab++;
            MyCancelButton.TabIndex = tab++;
        }

        private void CopyControls()
        {
            // Allocate memory to hold references to the dialog box controls
            for(int i=0; i<m_matrixTextBox.Length; i++)
            {
                m_matrixTextBox[i] = new TextBox[MBGUICONSTANTS.MAXNUMBEHAVIORS+1]; // +1 for the zero-probablity column.
                m_bMatrixError[i] = new Boolean[MBGUICONSTANTS.MAXNUMBEHAVIORS+1];
            }

            // Copy references
            m_clkSpanButton[0] = ClkButton00;
            m_clkSpanButton[1] = ClkButton01;
            m_clkSpanButton[2] = ClkButton02;
            m_clkSpanButton[3] = ClkButton03;
            m_clkSpanButton[4] = ClkButton04;
            m_clkSpanButton[5] = ClkButton05;
            m_clkSpanButton[6] = ClkButton06;
            m_clkSpanButton[7] = ClkButton07;
            m_clkSpanButton[8] = ClkButton08;
            m_clkSpanButton[9] = ClkButton09;
            m_clkSpanButton[10] = ClkButton10;
            m_clkSpanButton[11] = ClkButton11;
            m_clkSpanButton[12] = ClkButton12;
            m_clkSpanButton[13] = ClkButton13;
            m_clkSpanButton[14] = ClkButton14;
            m_clkSpanButton[15] = ClkButton15;
            m_clkSpanButton[16] = ClkButton16;
            m_clkSpanButton[17] = ClkButton17;
            m_clkSpanButton[18] = ClkButton18;
            m_clkSpanButton[19] = ClkButton19;
            m_clkSpanButton[20] = ClkButton20;
            m_clkSpanButton[21] = ClkButton21;
            m_clkSpanButton[22] = ClkButton22;
            m_clkSpanButton[23] = ClkButton23;

            m_behTitleTextBox[0] = ZeroProb;
            m_behTitleTextBox[1] = BehLabel1;
            m_behTitleTextBox[2] = BehLabel2;
            m_behTitleTextBox[3] = BehLabel3;
            m_behTitleTextBox[4] = BehLabel4;
            m_behTitleTextBox[5] = BehLabel5;
            m_behTitleTextBox[6] = BehLabel6;
            m_behTitleTextBox[7] = BehLabel7;
            m_behTitleTextBox[8] = BehLabel8;


            //m_matrixElement
            m_matrixTextBox[0][0] = MatrixTextBox00_00;
            m_matrixTextBox[0][1] = MatrixTextBox00_01;
            m_matrixTextBox[0][2] = MatrixTextBox00_02;
            m_matrixTextBox[0][3] = MatrixTextBox00_03;
            m_matrixTextBox[0][4] = MatrixTextBox00_04;
            m_matrixTextBox[0][5] = MatrixTextBox00_05;
            m_matrixTextBox[0][6] = MatrixTextBox00_06;
            m_matrixTextBox[0][7] = MatrixTextBox00_07;
            m_matrixTextBox[0][8] = MatrixTextBox00_08;

            m_matrixTextBox[1][0] = MatrixTextBox01_00;
            m_matrixTextBox[1][1] = MatrixTextBox01_01;
            m_matrixTextBox[1][2] = MatrixTextBox01_02;
            m_matrixTextBox[1][3] = MatrixTextBox01_03;
            m_matrixTextBox[1][4] = MatrixTextBox01_04;
            m_matrixTextBox[1][5] = MatrixTextBox01_05;
            m_matrixTextBox[1][6] = MatrixTextBox01_06;
            m_matrixTextBox[1][7] = MatrixTextBox01_07;
            m_matrixTextBox[1][8] = MatrixTextBox01_08;

            m_matrixTextBox[2][0] = MatrixTextBox02_00;
            m_matrixTextBox[2][1] = MatrixTextBox02_01;
            m_matrixTextBox[2][2] = MatrixTextBox02_02;
            m_matrixTextBox[2][3] = MatrixTextBox02_03;
            m_matrixTextBox[2][4] = MatrixTextBox02_04;
            m_matrixTextBox[2][5] = MatrixTextBox02_05;
            m_matrixTextBox[2][6] = MatrixTextBox02_06;
            m_matrixTextBox[2][7] = MatrixTextBox02_07;
            m_matrixTextBox[2][8] = MatrixTextBox02_08;

            m_matrixTextBox[3][0] = MatrixTextBox03_00;
            m_matrixTextBox[3][1] = MatrixTextBox03_01;
            m_matrixTextBox[3][2] = MatrixTextBox03_02;
            m_matrixTextBox[3][3] = MatrixTextBox03_03;
            m_matrixTextBox[3][4] = MatrixTextBox03_04;
            m_matrixTextBox[3][5] = MatrixTextBox03_05;
            m_matrixTextBox[3][6] = MatrixTextBox03_06;
            m_matrixTextBox[3][7] = MatrixTextBox03_07;
            m_matrixTextBox[3][8] = MatrixTextBox03_08;

            m_matrixTextBox[4][0] = MatrixTextBox04_00;
            m_matrixTextBox[4][1] = MatrixTextBox04_01;
            m_matrixTextBox[4][2] = MatrixTextBox04_02;
            m_matrixTextBox[4][3] = MatrixTextBox04_03;
            m_matrixTextBox[4][4] = MatrixTextBox04_04;
            m_matrixTextBox[4][5] = MatrixTextBox04_05;
            m_matrixTextBox[4][6] = MatrixTextBox04_06;
            m_matrixTextBox[4][7] = MatrixTextBox04_07;
            m_matrixTextBox[4][8] = MatrixTextBox04_08;

            m_matrixTextBox[5][0] = MatrixTextBox05_00;
            m_matrixTextBox[5][1] = MatrixTextBox05_01;
            m_matrixTextBox[5][2] = MatrixTextBox05_02;
            m_matrixTextBox[5][3] = MatrixTextBox05_03;
            m_matrixTextBox[5][4] = MatrixTextBox05_04;
            m_matrixTextBox[5][5] = MatrixTextBox05_05;
            m_matrixTextBox[5][6] = MatrixTextBox05_06;
            m_matrixTextBox[5][7] = MatrixTextBox05_07;
            m_matrixTextBox[5][8] = MatrixTextBox05_09;

            m_matrixTextBox[6][0] = MatrixTextBox60;
            m_matrixTextBox[6][1] = MatrixTextBox06_01;
            m_matrixTextBox[6][2] = MatrixTextBox06_02;
            m_matrixTextBox[6][3] = MatrixTextBox06_03;
            m_matrixTextBox[6][4] = MatrixTextBox06_04;
            m_matrixTextBox[6][5] = MatrixTextBox06_05;
            m_matrixTextBox[6][6] = MatrixTextBox06_06;
            m_matrixTextBox[6][7] = MatrixTextBox06_07;
            m_matrixTextBox[6][8] = MatrixTextBox06_08;

            m_matrixTextBox[7][0] = MatrixTextBox07_00;
            m_matrixTextBox[7][1] = MatrixTextBox07_01;
            m_matrixTextBox[7][2] = MatrixTextBox07_02;
            m_matrixTextBox[7][3] = MatrixTextBox07_03;
            m_matrixTextBox[7][4] = MatrixTextBox07_04;
            m_matrixTextBox[7][5] = MatrixTextBox07_05;
            m_matrixTextBox[7][6] = MatrixTextBox07_06;
            m_matrixTextBox[7][7] = MatrixTextBox07_07;
            m_matrixTextBox[7][8] = MatrixTextBox07_08;

            m_matrixTextBox[8][0] = MatrixTextBox08_00;
            m_matrixTextBox[8][1] = MatrixTextBox08_01;
            m_matrixTextBox[8][2] = MatrixTextBox08_02;
            m_matrixTextBox[8][3] = MatrixTextBox08_03;
            m_matrixTextBox[8][4] = MatrixTextBox08_04;
            m_matrixTextBox[8][5] = MatrixTextBox08_05;
            m_matrixTextBox[8][6] = MatrixTextBox08_06;
            m_matrixTextBox[8][7] = MatrixTextBox08_07;
            m_matrixTextBox[8][8] = MatrixTextBox08_08;

            m_matrixTextBox[9][0] = MatrixTextBox09_00;
            m_matrixTextBox[9][1] = MatrixTextBox09_01;
            m_matrixTextBox[9][2] = MatrixTextBox09_02;
            m_matrixTextBox[9][3] = MatrixTextBox09_03;
            m_matrixTextBox[9][4] = MatrixTextBox09_04;
            m_matrixTextBox[9][5] = MatrixTextBox09_05;
            m_matrixTextBox[9][6] = MatrixTextBox09_06;
            m_matrixTextBox[9][7] = MatrixTextBox09_07;
            m_matrixTextBox[9][8] = MatrixTextBox09_08;

            m_matrixTextBox[10][0] = MatrixTextBox10_00;
            m_matrixTextBox[10][1] = MatrixTextBox10_01;
            m_matrixTextBox[10][2] = MatrixTextBox10_02;
            m_matrixTextBox[10][3] = MatrixTextBox10_03;
            m_matrixTextBox[10][4] = MatrixTextBox10_04;
            m_matrixTextBox[10][5] = MatrixTextBox10_05;
            m_matrixTextBox[10][6] = MatrixTextBox10_06;
            m_matrixTextBox[10][7] = MatrixTextBox10_07;
            m_matrixTextBox[10][8] = MatrixTextBox10_08;

            m_matrixTextBox[11][0] = MatrixTextBox11_00;
            m_matrixTextBox[11][1] = MatrixTextBox11_01;
            m_matrixTextBox[11][2] = MatrixTextBox11_02;
            m_matrixTextBox[11][3] = MatrixTextBox11_03;
            m_matrixTextBox[11][4] = MatrixTextBox11_04;
            m_matrixTextBox[11][5] = MatrixTextBox11_05;
            m_matrixTextBox[11][6] = MatrixTextBox11_06;
            m_matrixTextBox[11][7] = MatrixTextBox11_07;
            m_matrixTextBox[11][8] = MatrixTextBox11_08;

            m_matrixTextBox[12][0] = MatrixTextBox12_00;
            m_matrixTextBox[12][1] = MatrixTextBox12_01;
            m_matrixTextBox[12][2] = MatrixTextBox12_02;
            m_matrixTextBox[12][3] = MatrixTextBox12_03;
            m_matrixTextBox[12][4] = MatrixTextBox12_04;
            m_matrixTextBox[12][5] = MatrixTextBox12_05;
            m_matrixTextBox[12][6] = MatrixTextBox12_06;
            m_matrixTextBox[12][7] = MatrixTextBox12_07;
            m_matrixTextBox[12][8] = MatrixTextBox12_08;

            m_matrixTextBox[13][0] = MatrixTextBox13_00;
            m_matrixTextBox[13][1] = MatrixTextBox13_01;
            m_matrixTextBox[13][2] = MatrixTextBox13_02;
            m_matrixTextBox[13][3] = MatrixTextBox13_03;
            m_matrixTextBox[13][4] = MatrixTextBox13_04;
            m_matrixTextBox[13][5] = MatrixTextBox13_05;
            m_matrixTextBox[13][6] = MatrixTextBox13_06;
            m_matrixTextBox[13][7] = MatrixTextBox13_07;
            m_matrixTextBox[13][8] = MatrixTextBox13_08;

            m_matrixTextBox[14][0] = MatrixTextBox14_00;
            m_matrixTextBox[14][1] = MatrixTextBox14_01;
            m_matrixTextBox[14][2] = MatrixTextBox14_02;
            m_matrixTextBox[14][3] = MatrixTextBox14_03;
            m_matrixTextBox[14][4] = MatrixTextBox14_04;
            m_matrixTextBox[14][5] = MatrixTextBox14_05;
            m_matrixTextBox[14][6] = MatrixTextBox14_06;
            m_matrixTextBox[14][7] = MatrixTextBox14_07;
            m_matrixTextBox[14][8] = MatrixTextBox14_08;

            m_matrixTextBox[15][0] = MatrixTextBox15_00;
            m_matrixTextBox[15][1] = MatrixTextBox15_01;
            m_matrixTextBox[15][2] = MatrixTextBox15_02;
            m_matrixTextBox[15][3] = MatrixTextBox15_03;
            m_matrixTextBox[15][4] = MatrixTextBox15_04;
            m_matrixTextBox[15][5] = MatrixTextBox15_05;
            m_matrixTextBox[15][6] = MatrixTextBox15_06;
            m_matrixTextBox[15][7] = MatrixTextBox15_07;
            m_matrixTextBox[15][8] = MatrixTextBox15_08;

            m_matrixTextBox[16][0] = MatrixTextBox16_00;
            m_matrixTextBox[16][1] = MatrixTextBox16_01;
            m_matrixTextBox[16][2] = MatrixTextBox16_02;
            m_matrixTextBox[16][3] = MatrixTextBox16_03;
            m_matrixTextBox[16][4] = MatrixTextBox16_04;
            m_matrixTextBox[16][5] = MatrixTextBox16_05;
            m_matrixTextBox[16][6] = MatrixTextBox16_06;
            m_matrixTextBox[16][7] = MatrixTextBox16_07;
            m_matrixTextBox[16][8] = MatrixTextBox16_08;

            m_matrixTextBox[17][0] = MatrixTextBox17_00;
            m_matrixTextBox[17][1] = MatrixTextBox17_01;
            m_matrixTextBox[17][2] = MatrixTextBox17_02;
            m_matrixTextBox[17][3] = MatrixTextBox17_03;
            m_matrixTextBox[17][4] = MatrixTextBox17_04;
            m_matrixTextBox[17][5] = MatrixTextBox17_05;
            m_matrixTextBox[17][6] = MatrixTextBox17_06;
            m_matrixTextBox[17][7] = MatrixTextBox17_07;
            m_matrixTextBox[17][8] = MatrixTextBox17_08;

            m_matrixTextBox[18][0] = MatrixTextBox18_00;
            m_matrixTextBox[18][1] = MatrixTextBox18_01;
            m_matrixTextBox[18][2] = MatrixTextBox18_02;
            m_matrixTextBox[18][3] = MatrixTextBox18_03;
            m_matrixTextBox[18][4] = MatrixTextBox18_04;
            m_matrixTextBox[18][5] = MatrixTextBox18_05;
            m_matrixTextBox[18][6] = MatrixTextBox18_06;
            m_matrixTextBox[18][7] = MatrixTextBox18_07;
            m_matrixTextBox[18][8] = MatrixTextBox18_08;

            m_matrixTextBox[19][0] = MatrixTextBox19_00;
            m_matrixTextBox[19][1] = MatrixTextBox19_01;
            m_matrixTextBox[19][2] = MatrixTextBox19_02;
            m_matrixTextBox[19][3] = MatrixTextBox19_03;
            m_matrixTextBox[19][4] = MatrixTextBox19_04;
            m_matrixTextBox[19][5] = MatrixTextBox19_05;
            m_matrixTextBox[19][6] = MatrixTextBox19_06;
            m_matrixTextBox[19][7] = MatrixTextBox19_07;
            m_matrixTextBox[19][8] = MatrixTextBox19_08;

            m_matrixTextBox[20][0] = MatrixTextBox20_00;
            m_matrixTextBox[20][1] = MatrixTextBox20_01;
            m_matrixTextBox[20][2] = MatrixTextBox20_02;
            m_matrixTextBox[20][3] = MatrixTextBox20_03;
            m_matrixTextBox[20][4] = MatrixTextBox20_04;
            m_matrixTextBox[20][5] = MatrixTextBox20_05;
            m_matrixTextBox[20][6] = MatrixTextBox20_06;
            m_matrixTextBox[20][7] = MatrixTextBox20_07;
            m_matrixTextBox[20][8] = MatrixTextBox20_08;

            m_matrixTextBox[21][0] = MatrixTextBox21_00;
            m_matrixTextBox[21][1] = MatrixTextBox21_01;
            m_matrixTextBox[21][2] = MatrixTextBox21_02;
            m_matrixTextBox[21][3] = MatrixTextBox21_03;
            m_matrixTextBox[21][4] = MatrixTextBox21_04;
            m_matrixTextBox[21][5] = MatrixTextBox21_05;
            m_matrixTextBox[21][6] = MatrixTextBox21_06;
            m_matrixTextBox[21][7] = MatrixTextBox21_07;
            m_matrixTextBox[21][8] = MatrixTextBox21_08;

            m_matrixTextBox[22][0] = MatrixTextBox22_00;
            m_matrixTextBox[22][1] = MatrixTextBox22_01;
            m_matrixTextBox[22][2] = MatrixTextBox22_02;
            m_matrixTextBox[22][3] = MatrixTextBox22_03;
            m_matrixTextBox[22][4] = MatrixTextBox22_04;
            m_matrixTextBox[22][5] = MatrixTextBox22_05;
            m_matrixTextBox[22][6] = MatrixTextBox22_06;
            m_matrixTextBox[22][7] = MatrixTextBox22_07;
            m_matrixTextBox[22][8] = MatrixTextBox22_08;

            m_matrixTextBox[23][0] = MatrixTextBox23_00;
            m_matrixTextBox[23][1] = MatrixTextBox23_01;
            m_matrixTextBox[23][2] = MatrixTextBox23_02;
            m_matrixTextBox[23][3] = MatrixTextBox23_03;
            m_matrixTextBox[23][4] = MatrixTextBox23_04;
            m_matrixTextBox[23][5] = MatrixTextBox23_05;
            m_matrixTextBox[23][6] = MatrixTextBox23_06;
            m_matrixTextBox[23][7] = MatrixTextBox23_07;
            m_matrixTextBox[23][8] = MatrixTextBox23_08;

            m_T50TextBox[0] = T50TextBox00;
            m_T50TextBox[1] = T50TextBox01;
            m_T50TextBox[2] = T50TextBox02;
            m_T50TextBox[3] = T50TextBox03;
            m_T50TextBox[4] = T50TextBox04;
            m_T50TextBox[5] = T50TextBox05;
            m_T50TextBox[6] = T50TextBox06;
            m_T50TextBox[7] = T50TextBox07;
            m_T50TextBox[8] = T50TextBox08;
            m_T50TextBox[9] = T50TextBox09;
            m_T50TextBox[10] = T50TextBox10;
            m_T50TextBox[11] = T50TextBox11;
            m_T50TextBox[12] = T50TextBox12;
            m_T50TextBox[13] = T50TextBox13;
            m_T50TextBox[14] = T50TextBox14;
            m_T50TextBox[15] = T50TextBox15;
            m_T50TextBox[16] = T50TextBox16;
            m_T50TextBox[17] = T50TextBox17;
            m_T50TextBox[18] = T50TextBox18;
            m_T50TextBox[19] = T50TextBox19;
            m_T50TextBox[20] = T50TextBox20;
            m_T50TextBox[21] = T50TextBox21;
            m_T50TextBox[22] = T50TextBox22;
            m_T50TextBox[23] = T50TextBox23;

            m_slopeTextBox[0] = SlopeTextBox00;
            m_slopeTextBox[1] = SlopeTextBox01;
            m_slopeTextBox[2] = SlopeTextBox02;
            m_slopeTextBox[3] = SlopeTextBox03;
            m_slopeTextBox[4] = SlopeTextBox04;
            m_slopeTextBox[5] = SlopeTextBox05;
            m_slopeTextBox[6] = SlopeTextBox06;
            m_slopeTextBox[7] = SlopeTextBox07;
            m_slopeTextBox[8] = SlopeTextBox08;
            m_slopeTextBox[9] = SlopeTextBox09;
            m_slopeTextBox[10] = SlopeTextBox10;
            m_slopeTextBox[11] = SlopeTextBox11;
            m_slopeTextBox[12] = SlopeTextBox12;
            m_slopeTextBox[13] = SlopeTextBox13;
            m_slopeTextBox[14] = SlopeTextBox14;
            m_slopeTextBox[15] = SlopeTextBox15;
            m_slopeTextBox[16] = SlopeTextBox16;
            m_slopeTextBox[17] = SlopeTextBox17;
            m_slopeTextBox[18] = SlopeTextBox18;
            m_slopeTextBox[19] = SlopeTextBox19;
            m_slopeTextBox[20] = SlopeTextBox20;
            m_slopeTextBox[21] = SlopeTextBox21;
            m_slopeTextBox[22] = SlopeTextBox22;
            m_slopeTextBox[23] = SlopeTextBox23;

            m_spanButton[0] = SpanButton00;
            m_spanButton[1] = SpanButton01;
            m_spanButton[2] = SpanButton02;
            m_spanButton[3] = SpanButton03;
            m_spanButton[4] = SpanButton04;
            m_spanButton[5] = SpanButton05;
            m_spanButton[6] = SpanButton06;
            m_spanButton[7] = SpanButton07;
            m_spanButton[8] = SpanButton08;
            m_spanButton[9] = SpanButton09;
            m_spanButton[10] = SpanButton10;
            m_spanButton[11] = SpanButton11;
            m_spanButton[12] = SpanButton12;
            m_spanButton[13] = SpanButton13;
        }


        private void MatrixTextBox_MouseEnter(object sender, MouseEventArgs e)
        {
            int row = m_matrixLastIndex.row;
            m_matrixLastIndex = GuiUtils.MatchIndex(m_matrixTextBox, (TextBox)sender, m_matrixLastIndex);
            if(m_matrixLastIndex.row == -1 || m_matrixLastIndex.col == -1)
            {
                m_matrixLastIndex.col = 0;
                m_matrixLastIndex.row = GuiUtils.MatchIndex(m_T50TextBox, (TextBox)sender);

                if(m_matrixLastIndex.row == -1)
                {
                    m_matrixLastIndex.row = GuiUtils.MatchIndex(m_slopeTextBox, (TextBox)sender);

                    if(m_matrixLastIndex.row == -1)
                    {
                        m_matrixLastIndex.row = 0;
                        return;
                    }
                }
            }

            if(row == m_matrixLastIndex.row)
                return;
            GuiControlsToMatrixModel();
            m_activeMatrixRowIndex = m_matrixLastIndex.row;
            ManageMatrixControls();

        }


    } // End of class 
}