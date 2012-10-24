using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using ESME.Environment;

namespace EnvironmentBuilder.VisualControls
{
    public partial class EnvironmentFileDropdown : ComboBox
    {
        private string mSearchDirectory, mLayerName;
        private List<DataFile> mMatchingFiles = new List<DataFile>();

        public EnvironmentFileDropdown()
        {
            InitializeComponent();
            this.DropDownStyle = ComboBoxStyle.DropDownList;
        }

        public string SearchDirectory
        {
            get { return mSearchDirectory; }
            set
            {
                mSearchDirectory = value;
                FindFiles();
            }
        }

        public string LayerName
        {
            get { return mLayerName; }
            set
            {
                mLayerName = value;
                FindFiles();
            }
        }

        private void FindFiles()
        {
            DataFile curDataFile;
            string[] directory;

            mMatchingFiles.Clear();

            if ((mSearchDirectory == null) || (mSearchDirectory == string.Empty) || (!Directory.Exists(mSearchDirectory)))
                return;

            if ((mLayerName == null) || (mLayerName == string.Empty))
                return;

            directory = Directory.GetFiles(mSearchDirectory, "*.eeb");
            foreach (string curFileName in directory)
            {
                curDataFile = DataFile.Open(curFileName);
                if (curDataFile[mLayerName] != null)
                    mMatchingFiles.Add(curDataFile);
            }
            curDataFile = null;

            this.Items.Clear();
            if (mMatchingFiles.Count() != 0)
            {
                foreach (DataFile cur in mMatchingFiles)
                {
                    if (cur[mLayerName].TimePeriod.ToLower() != "n/a")
                        this.Items.Add(Path.GetFileNameWithoutExtension(cur.FileName) + " [" + cur[mLayerName].TimePeriod + "]");
                    else
                        this.Items.Add(Path.GetFileNameWithoutExtension(cur.FileName));
                }
            }
            if (Items.Count == 0)
            {
                this.Items.Add("No matching files found");
                this.Enabled = false;
            }
            if (Items.Count == 1)
                this.SelectedIndex = 0;
            else
            {
                this.Items.Add("Please select...");
                this.SelectedIndex = Items.Count - 1;
            }
        }

        public DataFile SelectedFile
        {
            get
            {
                if ((mMatchingFiles.Count > 0) && (SelectedIndex < mMatchingFiles.Count))
                    return mMatchingFiles[SelectedIndex];
                return null;
            }
        }

        public DataLayer SelectedLayer
        {
            get
            {
                if ((mMatchingFiles.Count > 0) && (SelectedIndex < mMatchingFiles.Count))
                    return mMatchingFiles[SelectedIndex][LayerName];
                return null;
            }
        }

        public bool LayerAvailable
        {
            get 
            {
                return (mMatchingFiles.Count > 0);
            }
        }
    }
}
