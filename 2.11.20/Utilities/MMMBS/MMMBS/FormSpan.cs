using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Diagnostics;
using MMMBSLib;

namespace MBSGUI
{
    //static readonly int[] ROWINITIALYPLACEMENT = new int[] { 9, 31, 55 };

    public struct FORMSPANINPUT
    {
        public string szCaption;
        public string szInstruction;
        public double minInput;
        public Boolean minInputIsReadOnly;
        public double maxInput;
        public Boolean maxInputIsReadOnly;
        public double lowerLimit;
        public double uppperLimit;
        public string szRange;
    }


    public partial class FormSpan : Form
    {

        //--------------------//
        // Class Variables
        //--------------------//
        protected Boolean m_bModified = false;
        protected const int LEFTMARGIN = 13;
        protected const int RIGHTMARGIN = 20;
        protected const int SPACEBETWEENCONTROLS = 10;

        //--------------------//
        // Class Properties
        //--------------------//
        public Boolean Modified { get { return m_bModified; } }
        public int InputWidth
        {
            set
            {
                I1TextBox.Width = I2TextBox.Width = I3TextBox.Width = I4TextBox.Width = I5TextBox.Width =
                    I6TextBox.Width = value;
            }
        }

        //--------------------//
        // Class Constructors
        //--------------------//
        // Empty constructor for initial developement.
        private FormSpan() { InitializeComponent(); }
        protected FormSpan(string Caption) : this() { this.Text = Caption; }

        //-----------------------------//
        // Event Handler Class Methods
        //-----------------------------//
        protected void AdjustCntrlLoc(Control C, int Amt) { C.Location = new Point(C.Location.X + Amt, C.Location.Y); }
        protected virtual void MyOKButton_Click(object sender, EventArgs e) { }
        protected virtual void MyCancelButton_Click(object sender, EventArgs e) { m_bModified = false; }
        protected virtual void TextBox_TextChanged(object sender, EventArgs e)
        {
            CheckInputFocused(sender);
        }

        //--------------//
        // Class Methods
        //--------------//
        // Returns 'true' if passes format and input needs processing.
        protected Boolean CheckInputFocused(object sender)
        {
            // If the changed text box isn't focused then it wasn't a user modification
            // so don't bother to check.  Otherwise, indicate a modification
            if(((TextBox)sender).Focused == false)
                return false;

            // Indicate the user has changed a value.
            m_bModified = true;
            return true;
        }

        protected Boolean CheckInputFormatDouble(TextBox TB)
        {
            return true;
        }
        protected Boolean CheckInputFormatInt(TextBox TB)
        {
            return true;
        }
    }

    public class FormSpanDouble : FormSpan
    {
        //--------------------//
        // Class Variables
        //--------------------//
        protected double m_upperLim; // Upper limit user may enter
        protected double m_lowerLim; // Lower limit user may enter.
        protected double m_minOrgnl; // Orginal minimum value inputed
        protected double m_maxOrgnl; // Orginal maximum value inputed
        protected double m_minReslt; // Minimum result value
        protected double m_maxReslt; // Maximum result value

        //--------------------//
        // Class Properties
        //--------------------//
        public double MinResult { get { return m_minReslt; } }
        public double MaxResult { get { return m_maxReslt; } }

        //--------------------//
        // Class Constructors
        //--------------------//
        protected FormSpanDouble(string Caption) : base(Caption) { }
        public FormSpanDouble(FORMSPANINPUT FSI)
            : base(FSI.szCaption)
        {
            int adjust;

            // Instruction String
            InstrctnLabel.Visible = InstrctnLabel.Enabled = true;
            InstrctnLabel.Text = FSI.szInstruction;

            // Input 1 (Minimum)
            I1TextBox.Visible = I1TextBox.Enabled = true;
            I1TextBox.Text = "" + FSI.minInput;
            I1TextBox.Enabled = !FSI.minInputIsReadOnly;
            m_minReslt = m_minOrgnl = FSI.minInput;
            

            // Input 2 (Maximum)
            I2TextBox.Visible = I2TextBox.Enabled = true;
            I2TextBox.Text = "" + FSI.maxInput;
            I2TextBox.Enabled = !FSI.maxInputIsReadOnly;
            m_maxReslt = m_maxOrgnl = FSI.maxInput;

            // Low Limit
            m_lowerLim = FSI.lowerLimit; // Save the orginal input
            Input1Label.Visible = Input1Label.Enabled = true;
            Input1Label.Text = "Lower Limit: " + m_lowerLim;

            // High Limit
            m_upperLim = FSI.uppperLimit;
            Input2Label.Visible = Input2Label.Enabled = true;
            Input2Label.Text = "Upper Limit: " + m_upperLim;

            // Range Span String
            RangeLabel.Visible = RangeLabel.Enabled = true;
            RangeLabel.Text = FSI.szRange;

            // Place the control locations and dialog box size.
            RangeLabel.Location = new Point(I1TextBox.Location.X + I1TextBox.Width, RangeLabel.Location.Y);
            I2TextBox.Location = new Point(RangeLabel.Location.X + RangeLabel.Width, I2TextBox.Location.Y);
            Input1Label.Location = new Point(I1TextBox.Location.X, Input1Label.Location.Y);
            Input2Label.Location = new Point(I2TextBox.Location.X, Input2Label.Location.Y);

            // Set the width of this control box and adjust the position of the input text
            // boxes if needed.
            if(Input2Label.Location.X + Input2Label.Width > I2TextBox.Location.X + I2TextBox.Width)
            {
                this.Width = Input2Label.Location.X + Input2Label.Width + RIGHTMARGIN;
                adjust = ((Input2Label.Location.X + Input2Label.Width) - (I2TextBox.Location.X + I2TextBox.Width))/2;
                AdjustCntrlLoc(I1TextBox, adjust);
                AdjustCntrlLoc(I2TextBox, adjust);
                AdjustCntrlLoc(RangeLabel, adjust);
             }
            else
            {
                this.Width = I2TextBox.Location.X + I2TextBox.Width + LEFTMARGIN + RIGHTMARGIN;
            }

            if(m_minOrgnl > m_maxOrgnl || m_minOrgnl < m_lowerLim || m_maxOrgnl > m_upperLim || m_lowerLim > m_upperLim)
                MyOKButton.Enabled = false;
        }

        protected override void TextBox_TextChanged(object sender, EventArgs e)
        {
            String sz;
            TextBox tb = (TextBox)sender;

            // If the changed text box isn't focused then it wasn't a user modification
            // so don't bother to check.
            if(tb.Focused == false)
                return;

            // Indicate the user has changed a value.
            m_bModified = true;

            // Run the numerical string the user entered into the text box through the
            // routine that returns a string correctly fomrated as an integral value by
            // removing non-numerical (and decimal) characters then compare the returned
            // string with the orginal.  If it comes back identical then the string meets
            // integer format rules and continue.  If not, set the text box to the
            // returned string.  Doing so will forces this routine to be called again.
            if(tb.Text != (sz = CStringUtil.SzEnforceDoubleFmt(tb.Text)))
            {
                // Force the entered string back to what it was.  Doing so will regenerate
                // the TextChanged event and cause this function to be called again.
                tb.Text = sz;
                return;
            }

            // A empty string is accepted as a valid intregal value for allowing user to
            // edit without hassle.  If empty string is entered disable the OK button to
            // prevent user from entering a bad value.
            if(sz == "")
            {
                MyOKButton.Enabled = false;
                return;
            }

            m_minReslt = Convert.ToDouble(I1TextBox.Text);
            m_maxReslt = Convert.ToDouble(I2TextBox.Text);
            if(m_minReslt > m_maxReslt || m_minReslt < m_lowerLim || m_maxReslt > m_upperLim)
            {
                MyOKButton.Enabled = false;
                return;
            }
            // Everything checks out so set the OK button enabled.
            MyOKButton.Enabled = true;
        }

        protected override void MyCancelButton_Click(object sender, EventArgs e)
        {
            m_bModified = false;
            m_minReslt = m_minOrgnl;
            m_maxReslt = m_maxOrgnl;
        }

    }

    public class FromSpanClock : FormSpanDouble
    {
        public FromSpanClock(FORMSPANINPUT FSI)
            : base(FSI.szCaption)
        {
            HHMMSS HMS = new HHMMSS();
            int adjust;

            InputWidth = 24;

            // Instruction String
            InstrctnLabel.Visible = InstrctnLabel.Enabled = true;
            InstrctnLabel.Text = FSI.szInstruction;

            //--------------------------------------------------------------------------//
            // Minimum Clock
            //--------------//
            m_minReslt = m_minOrgnl = FSI.minInput;
            HMS = CUtil.TwentyHrFloatClockToHHMMSSOrZeroInclusive(FSI.minInput);

            // Input 1 (Minimum HH)
            I1TextBox.Visible = I1TextBox.Enabled = true;
            I1TextBox.Enabled = !FSI.minInputIsReadOnly;
            I1TextBox.Text = String.Format("{0:00}", HMS.hh);

            Colon1Label.Visible = Colon1Label.Enabled = true;
            Colon1Label.Text = ":";

            // Input 2 (Minimum MM)
            I2TextBox.Visible = I2TextBox.Enabled = true;
            I2TextBox.Enabled = !FSI.minInputIsReadOnly;
            I2TextBox.Text = String.Format("{0:00}", HMS.mm);

            Colon2Label.Visible = Colon1Label.Enabled = true;
            Colon2Label.Text = ":";

            I3TextBox.Visible = true;
            I3TextBox.Text = String.Format("{0:00}", 0);
            //--------------------------------------------------------------------------//


            //--------------------------------------------------------------------------//
            // Maximum Clock
            //--------------//
            m_maxReslt = m_maxOrgnl = FSI.maxInput;
            HMS = CUtil.TwentyHrFloatClockToHHMMSSOrZeroInclusive(FSI.maxInput);

            // Input 2 (Maximum HH)
            I4TextBox.Visible = I4TextBox.Enabled = true;
            I4TextBox.Enabled = !FSI.maxInputIsReadOnly;
            I4TextBox.Text = String.Format("{0:00}", HMS.hh);

            Colon3Label.Visible = Colon2Label.Enabled = true;
            Colon3Label.Text = ":";

            // Input 2 (Maximum MM)
            I5TextBox.Visible = I5TextBox.Enabled = true;
            I5TextBox.Enabled = !FSI.maxInputIsReadOnly;
            I5TextBox.Text = String.Format("{0:00}", HMS.mm);

            Colon3Label.Visible = Colon2Label.Enabled = true;
            Colon3Label.Text = ":";

            I6TextBox.Visible = true;
            I6TextBox.Text = String.Format("{0:00}", 0);
            //--------------------------------------------------------------------------//

            //--------------------------------------------------------------------------//
            // Low Clock Limit
            //----------------//
            HMS = CUtil.TwentyHrFloatClockToHHMMSSOrZeroInclusive(FSI.lowerLimit);
            m_lowerLim = FSI.lowerLimit; // Save the orginal input
            Input1Label.Visible = Input1Label.Enabled = true;
            Input1Label.Text = String.Format("Earliest Settable: {0:00}:{1:00}:00", HMS.hh, HMS.mm);
            //--------------------------------------------------------------------------//

            //--------------------------------------------------------------------------//
            // High Clock Limit
            //-----------------//
            HMS = CUtil.TwentyHrFloatClockToHHMMSSOrZeroInclusive(FSI.uppperLimit);
            m_upperLim = FSI.uppperLimit;
            Input2Label.Visible = Input2Label.Enabled = true;
            Input2Label.Text = String.Format("Latest Settable: {0:00}:{1:00}:00", HMS.hh, HMS.mm);
            //--------------------------------------------------------------------------//

            //--------------------------------------------------------------------------//
            // Range Span String
            //-------------------//
            RangeLabel.Visible = RangeLabel.Enabled = true;
            RangeLabel.Text = FSI.szRange;
            //--------------------------------------------------------------------------//

            //--------------------------------------------------------------------------//
            // Control Placement
            //-------------------//
            Colon1Label.Location = new Point(I1TextBox.Location.X + I1TextBox.Width-3, Colon1Label.Location.Y);
            I2TextBox.Location = new Point(Colon1Label.Location.X + Colon1Label.Width-3, I2TextBox.Location.Y);
            Colon2Label.Location = new Point(I2TextBox.Location.X + I2TextBox.Width-3, Colon2Label.Location.Y);
            I3TextBox.Location = new Point(Colon2Label.Location.X + Colon2Label.Width-3, I3TextBox.Location.Y);
            RangeLabel.Location = new Point(I3TextBox.Location.X + I3TextBox.Width, RangeLabel.Location.Y);
            I4TextBox.Location = new Point(RangeLabel.Location.X + RangeLabel.Width, I4TextBox.Location.Y);
            Colon3Label.Location = new Point(I4TextBox.Location.X + I4TextBox.Width-3, Colon3Label.Location.Y);
            I5TextBox.Location = new Point(Colon3Label.Location.X + Colon3Label.Width-3, I5TextBox.Location.Y);
            Colon4Label.Location = new Point(I5TextBox.Location.X + I5TextBox.Width-3, Colon4Label.Location.Y);
            I6TextBox.Location = new Point(Colon4Label.Location.X + Colon4Label.Width-3, I6TextBox.Location.Y);


            Input1Label.Location = new Point(I1TextBox.Location.X, Input1Label.Location.Y);
            Input2Label.Location = new Point(I4TextBox.Location.X, Input2Label.Location.Y);

            if(Input2Label.Location.X + Input2Label.Width > I6TextBox.Location.X + I6TextBox.Width)
            {
                this.Width = Input2Label.Location.X + Input2Label.Width + RIGHTMARGIN;
                adjust = ((Input2Label.Location.X + Input2Label.Width) - (I5TextBox.Location.X + I5TextBox.Width))/2;

                AdjustCntrlLoc(I1TextBox, adjust);
                AdjustCntrlLoc(Colon1Label, adjust);
                AdjustCntrlLoc(I2TextBox, adjust);
                AdjustCntrlLoc(Colon2Label, adjust);
                AdjustCntrlLoc(I3TextBox, adjust);
                AdjustCntrlLoc(RangeLabel, adjust);
                AdjustCntrlLoc(I4TextBox, adjust);
                AdjustCntrlLoc(Colon3Label, adjust);
                AdjustCntrlLoc(I5TextBox, adjust);
                AdjustCntrlLoc(Colon4Label, adjust);
                AdjustCntrlLoc(I6TextBox, adjust);
            }
            else
            {
                this.Width = I6TextBox.Location.X + I6TextBox.Width + RIGHTMARGIN;
            }
            //--------------------------------------------------------------------------//

            // Commented out until a better limit system is established.
            //if(m_minOrgnl > m_maxOrgnl || m_minOrgnl < m_lowerLim || m_maxOrgnl > m_upperLim || m_lowerLim > m_upperLim)
              //   MyOKButton.Enabled = false;
        }

        protected override void TextBox_TextChanged(object sender, EventArgs e)
        {
            String sz;
            TextBox tb = (TextBox)sender;
            HHMMSS min = new HHMMSS();
            HHMMSS max = new HHMMSS();

            // If the changed text box isn't focused then it wasn't a user modification
            // so don't bother to check.
            if(tb.Focused == false)
                return;

            // Indicate the user has changed a value.
            m_bModified = true;

            // Run the numerical string the user entered into the text box through the
            // routine that returns a string correctly fomrated as an integral value by
            // removing non-numerical (and decimal) characters then compare the returned
            // string with the orginal.  If it comes back identical then the string meets
            // integer format rules and continue.  If not, set the text box to the
            // returned string.  Doing so will forces this routine to be called again.
            if(tb.Text != (sz = CStringUtil.SzEnforceIntFmt(tb.Text)))
            {
                // Force the entered string back to what it was.  Doing so will regenerate
                // the TextChanged event and cause this function to be called again.
                tb.Text = sz;
                return;
            }

            // A empty string is accepted as a valid intregal value for allowing user to
            // edit without hassle.  If empty string is entered disable the OK button to
            // prevent user from entering a bad value.
            if(sz == "")
            {
                MyOKButton.Enabled = false;
                return;
            }

            min.hh = (uint)CStringUtil.SzToIntOrMin0(I1TextBox.Text);
            min.mm = (uint)CStringUtil.SzToIntOrMin0(I2TextBox.Text);
            min.ss = 0;

            max.hh = (uint)CStringUtil.SzToIntOrMin0(I4TextBox.Text);
            max.mm = (uint)CStringUtil.SzToIntOrMin0(I5TextBox.Text);
            max.ss = 0;

            m_minReslt = CUtil.HHMMSSTo3MBClock(min);
            m_maxReslt = CUtil.HHMMSSTo3MBClock(max);

#if false
            if(m_minReslt > m_maxReslt || m_minReslt < m_lowerLim || m_maxReslt > m_upperLim || m_minReslt > 24.0 || m_maxReslt > 24.0)
            {
                MyOKButton.Enabled = false;
                return;
            }
#endif
            // Everything checks out so set the OK button enabled.
            MyOKButton.Enabled = true;
        }
    }


    public class FormSpanInt : FormSpanDouble
    {

        //--------------------//
        // Class Properties
        //--------------------//
        public new int MinResult { get { return (int)base.MinResult; } }
        public new int MaxResult { get { return (int)base.MaxResult; } }

        //--------------------//
        // Class Constructors
        //--------------------//
        //public FormSpanInt(string Caption, string Instruction, int Min, int Max,
          //  int LowLimit, int HighLimit, string Range):
            //base(Caption, Instruction, Min, Max, LowLimit, HighLimit, Range) {}
        public FormSpanInt(FORMSPANINPUT FSI) : base(FSI) { }
    }
}





#if false
    public partial class FormInput : Form
    {
        protected int m_leftMargin = 12; // margine from the left side of form
        protected int m_rightMargin = 18; // margine from the right side of form 
        protected int m_topMargin = 10; // margin from the top of the form
        protected int m_bttmMargin = 11; // margin from the top of the OK/Cancel buttons at the bottom of the form.
        protected int m_cntrlHorzSpacing = 5;
        protected int m_cntrlVertSpacing = 5;

        private Boolean m_bModified = false;
        public Boolean Modified { get { return m_bModified; } }


        protected TextBox[][] m_input;
        public int TextBoxRowCount { get { return m_input.Length; } }
        public int TextBoxColCount { get { return m_input[0].Length; } }


        // Class Constructors.
        protected FormInput() { InitializeComponent(); }
        protected virtual void MyOKButton_Click(object sender, EventArgs e) { }
        protected virtual void MyCancelButton_Click(object sender, EventArgs e) { m_bModified = false; }

        protected virtual void TextBox_TextChanged(object sender, EventArgs e)
        {
            //this.siz
            // If the changed text box isn't focused then it wasn't a user modification
            // so don't beother toe check.
            if(((TextBox)sender).Focused == false)
                return;

            // A empty string is accepted as a valid intregal value for allowing user to
            // edit without hassle.  If empty string is entered disable the OK button to
            // prevent user from entering a bad value.
            if(((TextBox)sender).Text == "")
            {
                MyOKButton.Enabled = false;
                return;
            }
            m_bModified = true;
            MyOKButton.Enabled = true;
        }

        // Initializes components to a default setup that the programmer may modifiy
        // through accessor methods and class properties.
        private void InitializeDlg(int TextBoxRowCount, int TextBoxColumnCount)
        {
            int i, j, tab;

            m_input = new TextBox[TextBoxRowCount][];
            for(i=0; i < m_input.Length; i++)
            {
                m_input[i] = new TextBox[TextBoxColumnCount];
                for(j=0; j<TextBoxColumnCount; j++)
                {
                    tab = i*m_input.Length + j;
                    m_input[i][j].Location = new System.Drawing.Point(295, 8);
                    m_input[i][j].Name = "Input" + tab;
                    m_input[i][j].Size = new System.Drawing.Size(54, 20);
                    m_input[i][j].TabIndex = tab;
                    m_input[i][j].Text = "";
                    m_input[i][j].TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
                    m_input[i][j].Visible = true;
                    m_input[i][j].TextChanged += new System.EventHandler(this.TextBox_TextChanged);
                }
            }
        }

    }


    public class FormSimpleSingleStringInput : FormInput
    {
        public string SzTitle { set { this.Text = value; } }
        public string SzUserInput { get { return Input1TextBox.Text; } }

        public FormSimpleSingleStringInput(string Value, string FormTitle, string ValueLimit, string Instruction)
        {
            Point p = new Point(m_leftMargin, m_topMargin);
            int addedHeight = 0;
            int wider;
            InstrctnLabel.Location = Input1TextBox.Location = Input1Label.Location = new Point(0, 0);

            if(FormTitle == null)
                this.Text = "Enter Value";
            else
                this.Text = FormTitle;

            this.Height += m_bttmMargin-MyOKButton.Location.Y;

            if(Instruction != null)
            {
                InstrctnLabel.Visible = true;
                InstrctnLabel.Text = Instruction;
                InstrctnLabel.Location = new Point(p.X, p.Y + addedHeight);
                addedHeight += (InstrctnLabel.Height + m_cntrlVertSpacing); // consider height of control being added to dispaly
            }

            Input1TextBox.TextAlign = HorizontalAlignment.Left;
            Input1TextBox.Visible = true;
            Input1TextBox.Text = Value;
            Input1TextBox.Location = new Point(p.X, p.Y + addedHeight);
            addedHeight += Input1TextBox.Height;

            if(ValueLimit != null)
            {
                addedHeight += m_cntrlVertSpacing;

                Input1Label.Visible = true;
                Input1Label.Text = ValueLimit;
                Input1Label.Location = new Point(p.X, p.Y + addedHeight);

                addedHeight += Input1Label.Height;
            }

            addedHeight += m_bttmMargin;

            this.Height += addedHeight;

            wider = InstrctnLabel.Location.X + InstrctnLabel.Width;
            if(wider < Input1Label.Location.X + Input1Label.Width)
                wider = Input1Label.Location.X + Input1Label.Width;

            if(this.Width <  wider + m_leftMargin)
                this.Width = wider + m_leftMargin;
        }


    }

    public class FormDoubleSpan : FormInput
    {
        private double m_min;
        private double m_max;

#if false
        // These go away
        private int m_max; // also deep value entered
        private int m_min; // also shallow value
        private Boolean m_absMinSet = false; // Flag that indicates an absolute minimum value is set
        private Boolean m_absMaxSet = false; // Flag that indicates an absolute maximum value is set

        // Results
        public Boolean Modified { get { return m_bModified; } }
        public int MinSetPoint { get { return m_min; } }
        public int MaxSetPoint { get { return m_max; } }

        // Calling routine's set points.
        public Boolean EditMinEnabled { set { Input1TextBox.Enabled = value; } }
        public Boolean EditMaxEnabled { set { Input2TextBox.Enabled = value; } }
#endif
        public double Min
        {
            set
            {
                m_min = value;
                Input1Label.Text = "Limit: " + value;
                Input1Label.Visible = true;
            }
        }

        public double Max
        {
            set
            {
                m_max = value;
                Input2Label.Text = "Limit: " + value;
                Input2Label.Visible = true;
            }
        }

        public FormDoubleSpan()
        {

#if false
            TextBox dog = new TextBox();
            //this.Controls.Add(dog);
            this.InstructionString = "laskdjflsajd";
            //    this.MyOKButton.Click += new System.EventHandler(this.MyyOKButton_Click);
#endif
        }

        public FormDoubleSpan(int Input1, int Input2)
        {
#if false
            InitializeComponent();
            this.StartPosition = FormStartPosition.WindowsDefaultLocation;
            m_min = Input1;
            m_max = Input2;
            Input1TextBox.Text = "" + Input1;
            Input2TextBox.Text = "" + Input2;

#if false
            MoreFormSpanInt dlg = new MoreFormSpanInt();
            //dlg.Show();
            dlg.ShowDialog();
            dlg.Dispose();

            dlg.m_absMaxSet = true;
#endif
#endif
        }

        protected override void TextBox_TextChanged(object sender, EventArgs e)
        {
#if false
            String sz;
            int min, max;
            TextBox tb = (TextBox)sender;

            // If the changed text box isn't focused then it wasn't a user modification
            // so don't beother toe check.
            if(tb.Focused == false)
                return;

            // Indicate the user has changed a value.
            m_bModified = true;

            // Run the numerical string the user entered into the text box through the
            // routine that returns a string correctly fomrated as an integral value by
            // removing non-numerical (and decimal) characters then compare the returned
            // string with the orginal.  If it comes back identical then the string meets
            // integer format rules and continue.  If not, set the text box to the
            // returned string.  Doing so will forces this routine to be called again.
            if(tb.Text != (sz = CStringUtil.SzEnforceIntFmt(tb.Text)))
            {
                // Force the entered string back to what it was.  Doing so will regenerate
                // the TextChanged event and cause this function to be called again.
                tb.Text = sz;
                return;
            }

            // A empty string is accepted as a valid intregal value for allowing user to
            // edit without hassle.  If empty string is entered disable the OK button to
            // prevent user from entering a bad value.
            if(sz == "")
            {
                MyOKButton.Enabled = false;
                return;
            }

            min = (int)CStringUtil.SzToIntOrMin0(Input1TextBox.Text);
            max = (int)CStringUtil.SzToIntOrMin0(Input2TextBox.Text);
            if(min > max || (m_absMinSet == true && min < m_absMin) || (m_absMaxSet == true && max > m_absMax))
            {
                MyOKButton.Enabled = false;
                return;
            }
            // Everything checks out so set the OK button enabled.
            MyOKButton.Enabled = true;
#endif
        }


        protected virtual void MyOKButton_Click(object sender, EventArgs e) { }
#if false
        {
            if(m_bModified == false)
                return;
            m_min = CStringUtil.SzToIntOrMin0(Input1TextBox.Text);
            m_max = CStringUtil.SzToIntOrMin0(Input2TextBox.Text);
        }
#endif
    }
}
#endif