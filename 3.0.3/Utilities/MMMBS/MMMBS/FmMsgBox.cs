using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace MBSGUI
{
    public partial class FmMsgBox : Form
    {
        public FmMsgBox()
        {
            InitializeComponent();
        }

        public string labelText { set { this.label1.Text = value; } get { return label1.Text; } }

    }
}