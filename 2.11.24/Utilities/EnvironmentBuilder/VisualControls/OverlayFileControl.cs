using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using EnvironmentBuilder.ClassControls;
using ESME.Overlay;

namespace EnvironmentBuilder.VisualControls
{
    public partial class OverlayFileControl : UserControl
    {
        public OverlayFileControl()
        {
            InitializeComponent();
        }

        private OverlayFiles mOverlayFiles = new OverlayFiles();

        public OverlayFiles OverlayFiles
        {
            get { return mOverlayFiles; }
            set
            {
                mOverlayFiles = value;
                lstOverlayFiles.Items.Clear();
                foreach (OverlayFile f in mOverlayFiles)
                    lstOverlayFiles.Items.Add(Path.GetFileName(f.FileName));
            }
        }

        public event GenericEventHandler<OverlayFileControl, EventArgs> OverlaysChanged;
        private void OnOverlaysChanged()
        {
            if (OverlaysChanged != null)
                OverlaysChanged(this, new EventArgs());
        }

        private void ctxAddFile_Click(object sender, EventArgs e)
        {
            if (openFileDialog1.ShowDialog() == DialogResult.OK)
            {
                mOverlayFiles.Add(openFileDialog1.FileName);
                OnOverlaysChanged();
            }
        }

        private void ctxDeleteFile_Click(object sender, EventArgs e)
        {
            if (MessageBox.Show("Are you sure you want to delete overlay file" + lstOverlayFiles.SelectedItem.ToString() + "?", "Delete overlay file", MessageBoxButtons.YesNo, MessageBoxIcon.Exclamation) == DialogResult.Yes)
            {
                mOverlayFiles.Delete(lstOverlayFiles.SelectedItem.ToString());
                OnOverlaysChanged();
            }
        }

        private void ctxSetColor_Click(object sender, EventArgs e)
        {

        }

        private void ctxDisplay_Click(object sender, EventArgs e)
        {

        }
    }
    public delegate void GenericEventHandler<Sender, TEventArgs>(Sender sender, TEventArgs e) where TEventArgs : EventArgs;
    public class FileEventArgs : EventArgs
    {
        public FileEventArgs(string Filename)
        {
            this.Filename = Filename;
        }

        public string Filename;
    }

}
