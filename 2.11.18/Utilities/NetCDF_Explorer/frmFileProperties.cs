using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;
using NetCDF;

namespace NetCDF_Explorer
{
    public partial class frmFileProperties : Form
    {
        private NcFile myFile;

        public frmFileProperties()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            openFileDialog1.FileName = Properties.Settings.Default.LastFilename;
            if (openFileDialog1.ShowDialog() == DialogResult.OK)
            {
                Properties.Settings.Default.LastFilename = openFileDialog1.FileName;
                textBox1.Text = openFileDialog1.FileName;
            }
        }

        private void textBox1_TextChanged(object sender, EventArgs e)
        {
            if (File.Exists(textBox1.Text))
            {
                StatusText("Reading file...");
                myFile = new NcFile(textBox1.Text);
                ncFileExplorer1.NcFile = myFile;
                StatusText("Ready");
            }
            else
                ErrorText("File not found");
        }

        private void ErrorText(string text)
        {
            tsStatus.Text = text;
            tsStatus.ForeColor = Color.Red;
        }

        private void StatusText(string text)
        {
            tsStatus.Text = text;
            tsStatus.ForeColor = SystemColors.ControlText;
        }
    }
}