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
    public partial class FormAcstcAvrsnDecayFnc : Form
    {
        CAcousticExposureDecayFncParams m_f;
        public FormAcstcAvrsnDecayFnc(CAcousticExposureDecayFncParams F)
        {
            m_f = F;
            InitializeComponent();

            a1TextBox.Text = "" + m_f.A1;

            a2TextBox.Text = "" + m_f.B1;
            b2TextBox.Text = "" + m_f.B2;

            a3TextBox.Text = "" + m_f.C1;
            b3TextBox.Text = "" + m_f.C2;
            c3TextBox.Text = "" + m_f.C3;

            Fnc1GroupBox.Enabled = Fnc2GroupBox.Enabled = Fnc3GroupBox.Enabled = false;

            if(m_f.decayFunctionType == DECAYFUNCTIONS.DECAYFNC1)
                Fnc1GroupBox.Enabled = true;
            else if(m_f.decayFunctionType == DECAYFUNCTIONS.DECAYFNC2)
                Fnc2GroupBox.Enabled = true;
            else if(m_f.decayFunctionType == DECAYFUNCTIONS.DECAYFNC3)
                Fnc3GroupBox.Enabled = true;
        }
    }
}