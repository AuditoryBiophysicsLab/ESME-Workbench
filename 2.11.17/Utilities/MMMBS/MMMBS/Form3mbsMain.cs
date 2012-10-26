using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace MBSGUI
{
    public partial class Form3mbsMain: Form
    {
        public Form3mbsMain()
        {
            InitializeComponent();
        }

        private void MainForm_Load(object sender, EventArgs e)
        {

        }

        private void SpeciesModelingButton_Click(object sender, EventArgs e)
        {
            FormSpeciesDefinition dlg = new FormSpeciesDefinition();
            dlg.ShowDialog(); // modal
            //dlg.Show(); // non-modal

        }

        private void ExperimentButton_Click(object sender, EventArgs e)
        {
            FormExperimental dlg = new FormExperimental();
            dlg.ShowDialog(); // modal

        }

        private void ExitButton_Click(object sender, EventArgs e)
        {
            Dispose();
        }
    }
}