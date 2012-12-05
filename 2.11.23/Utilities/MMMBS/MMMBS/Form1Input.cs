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
    public partial class Form1Input: Form
    {
        RESLT m_result = RESLT.CANCEL;
        Boolean m_enforceDouble = false;
        string m_szOutput;
        Boolean m_initializing;
        Boolean m_modified = false;

        public Boolean modified { get { return m_modified; } }

        public Form1Input(string SzTitle, string SzInput)
        {
            InitializeComponent();
            m_initializing = true;
            Text = "3MB - " + SzTitle;
            InputLabel.Text = SzTitle;
            InputTextBox.Text = SzInput;
            m_initializing = false;
        }

        public Form1Input(string SzTitle, double Input)
        {
            InitializeComponent();
            m_initializing = true;
            Text = "3MB - " + SzTitle;
            InputLabel.Text = SzTitle;
            InputTextBox.Text = "" + Input;
            m_enforceDouble = true;
            m_initializing = false;
        }


        public RESLT result { get { return m_result; } }
        public string dataString { get { return m_szOutput; } }

        private void OkButton_Click(object sender, EventArgs e)
        {
            m_result = RESLT.OK;
            m_szOutput = InputTextBox.Text;
            Dispose();
        }

        private void CancelC_Click(object sender, EventArgs e)
        {
            m_modified = false;
            m_result = RESLT.CANCEL;
            Dispose();
        }

        private void InputTextBox_TextChanged(object sender, EventArgs e)
        {
            if(m_initializing == true)
                return;
            if(m_enforceDouble == true)
                InputTextBox.Text = CStringUtil.SzEnforceDoubleFmt(InputTextBox.Text);
            m_modified = true;
        }
    }
}