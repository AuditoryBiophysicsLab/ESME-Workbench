using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using ESME.Environment;

namespace NetCDF_Explorer
{
    public partial class DatabaseKeyComboBox : ComboBox
    {
        #region Private data members
        private ESME.Environment.Database db;
        private string tableName, foreignKeyName;
        private int foreignKeyValue, selectedKey;
        private bool inOnLeave;
        #endregion

        #region Constructors
        public DatabaseKeyComboBox()
        {
            InitializeComponent();
        }
        #endregion

        #region Public properties
        public ESME.Environment.Database Database
        {
            set
            {
                db = new Database(value);
                UpdateContents();
            }
        }

        /// <summary>
        /// Name of the table in the environmental database to use to populate this control
        /// </summary>
        public string TableName
        {
            set
            {
                tableName = value;
                UpdateContents();
            }

            get { return tableName; }
        }

        /// <summary>
        /// Name of the foreign key field in the specified table
        /// </summary>
        public string ForeignKeyName
        {
            set
            {
                foreignKeyName = value;
                UpdateContents();
            }

            get { return foreignKeyName; }
        }

        /// <summary>
        /// Value of the foreign key to use to populate this control
        /// </summary>
        public int ForeignKeyValue 
        { 
            set 
            {
                foreignKeyValue = value;
                UpdateContents();
            }

            get { return foreignKeyValue; } 
        }

        /// <summary>
        /// Returns the key for the currently selected row
        /// </summary>
        public int SelectedKey { get { return selectedKey; } }
        #endregion

        #region Private utility methods
        private void UpdateContents()
        {
            string[] items;
            if (!IsInitialized())
                return;
            this.Items.Clear();
            items = db.GetNameList("Name", tableName, foreignKeyName, foreignKeyValue);
            if (items != null)
            this.Items.AddRange(items);
        }

        private bool IsInitialized()
        {
            if (db == null)
            {
                this.Items.Clear();
                return false;
            }
            if ((tableName == null) || (tableName.Length == 0))
            {
                this.Items.Clear();
                return false;
            }
            if ((foreignKeyName == null) || (foreignKeyName.Length == 0))
            {
                this.Items.Clear();
                return false;
            }
            if (foreignKeyValue <= 0)
            {
                this.Items.Clear();
                return false;
            }
            return true;
        }
        #endregion

        #region Protected Event Handler Overrides
        protected override void OnLeave(EventArgs e)
        {
            base.OnLeave(e);

            if (!IsInitialized())
                return;

            if (inOnLeave)
                return;
            inOnLeave = true;

            selectedKey = db.GetIDField(tableName, this.Text, foreignKeyName, foreignKeyValue);
            if (selectedKey == 0)
            {
                string Message = String.Format("\"{0}\" is not an existing {1}.  Do you want to add it?", this.Text, tableName);
                string Caption = String.Format("{0} does not exist", this.Text);
                if (MessageBox.Show(this.Parent, Message, Caption, MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
                {
                    switch (tableName.ToLower())
                    {
                        case "dataset":
                            db.AddDataSet(foreignKeyValue, this.Text);
                            break;
                        case "datasubset":
                            db.AddDataSubset(foreignKeyValue, this.Text);
                            break;
                        default:
                            MessageBox.Show("Unknown field!  Bug!!!");
                            break;
                    }
                    selectedKey = db.GetIDField(tableName, this.Text, foreignKeyName, foreignKeyValue); 
                    UpdateContents();
                    for (int i = 0; i < this.Items.Count; i++)
                    {
                        if (((string)this.Items[i]) == this.Text)
                        {
                            this.SelectedIndex = i;
                            break;
                        }
                    }
                }
            }
            inOnLeave = false;
        }

        protected override void OnSelectedIndexChanged(EventArgs e)
        {
            base.OnSelectedIndexChanged(e);
            selectedKey = db.GetIDField(tableName, this.Text, foreignKeyName, foreignKeyValue);
        }
        #endregion
    }
}
