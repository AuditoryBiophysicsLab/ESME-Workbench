namespace NetCDF_Explorer
{
    partial class frmAddToDatabase
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.label1 = new System.Windows.Forms.Label();
            this.statusStrip1 = new System.Windows.Forms.StatusStrip();
            this.tsStatus = new System.Windows.Forms.ToolStripStatusLabel();
            this.openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
            this.ncFileExplorer1 = new NetCDF.NcFileExplorer();
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.openNetCDFFileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem1 = new System.Windows.Forms.ToolStripSeparator();
            this.exitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.databaseToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.connectToEnvironmentalDatabaseToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.createNewEnvironmentalDatabaseToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.gbEnvironmentalDatabase = new System.Windows.Forms.GroupBox();
            this.dbcDataSubset = new NetCDF_Explorer.DatabaseKeyComboBox();
            this.dbcDataSet = new NetCDF_Explorer.DatabaseKeyComboBox();
            this.label4 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.cboDataType = new System.Windows.Forms.ComboBox();
            this.label2 = new System.Windows.Forms.Label();
            this.gbNetCDFVariables = new System.Windows.Forms.GroupBox();
            this.cboDepthName = new System.Windows.Forms.ComboBox();
            this.label9 = new System.Windows.Forms.Label();
            this.btnImport = new System.Windows.Forms.Button();
            this.gbAttributes = new System.Windows.Forms.GroupBox();
            this.cboMissingValue = new System.Windows.Forms.ComboBox();
            this.label10 = new System.Windows.Forms.Label();
            this.cboOffsetName = new System.Windows.Forms.ComboBox();
            this.label8 = new System.Windows.Forms.Label();
            this.cboScalingName = new System.Windows.Forms.ComboBox();
            this.label7 = new System.Windows.Forms.Label();
            this.cboDataName = new System.Windows.Forms.ComboBox();
            this.lblDataVariable = new System.Windows.Forms.Label();
            this.cboLatitudeName = new System.Windows.Forms.ComboBox();
            this.label6 = new System.Windows.Forms.Label();
            this.cboLongitudeName = new System.Windows.Forms.ComboBox();
            this.label5 = new System.Windows.Forms.Label();
            this.statusStrip1.SuspendLayout();
            this.menuStrip1.SuspendLayout();
            this.gbEnvironmentalDatabase.SuspendLayout();
            this.gbNetCDFVariables.SuspendLayout();
            this.gbAttributes.SuspendLayout();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(13, 13);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(90, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "NetCDF Filename";
            // 
            // statusStrip1
            // 
            this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.tsStatus});
            this.statusStrip1.Location = new System.Drawing.Point(0, 606);
            this.statusStrip1.Name = "statusStrip1";
            this.statusStrip1.Size = new System.Drawing.Size(482, 22);
            this.statusStrip1.TabIndex = 3;
            this.statusStrip1.Text = "statusStrip1";
            // 
            // tsStatus
            // 
            this.tsStatus.Name = "tsStatus";
            this.tsStatus.Size = new System.Drawing.Size(39, 17);
            this.tsStatus.Text = "Ready";
            // 
            // openFileDialog1
            // 
            this.openFileDialog1.FileName = "openFileDialog1";
            this.openFileDialog1.Filter = "NetCDF files (*.nc)|*.nc|All files (*.*)|*.*";
            // 
            // ncFileExplorer1
            // 
            this.ncFileExplorer1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.ncFileExplorer1.Location = new System.Drawing.Point(12, 29);
            this.ncFileExplorer1.Name = "ncFileExplorer1";
            this.ncFileExplorer1.NcFile = null;
            this.ncFileExplorer1.Size = new System.Drawing.Size(249, 574);
            this.ncFileExplorer1.TabIndex = 5;
            // 
            // menuStrip1
            // 
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem,
            this.databaseToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(482, 24);
            this.menuStrip1.TabIndex = 6;
            this.menuStrip1.Text = "menuStrip1";
            // 
            // fileToolStripMenuItem
            // 
            this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.openNetCDFFileToolStripMenuItem,
            this.toolStripMenuItem1,
            this.exitToolStripMenuItem});
            this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
            this.fileToolStripMenuItem.Size = new System.Drawing.Size(37, 20);
            this.fileToolStripMenuItem.Text = "&File";
            // 
            // openNetCDFFileToolStripMenuItem
            // 
            this.openNetCDFFileToolStripMenuItem.Name = "openNetCDFFileToolStripMenuItem";
            this.openNetCDFFileToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.O)));
            this.openNetCDFFileToolStripMenuItem.Size = new System.Drawing.Size(220, 22);
            this.openNetCDFFileToolStripMenuItem.Text = "&Open NetCDF File...";
            this.openNetCDFFileToolStripMenuItem.Click += new System.EventHandler(this.openNetCDFFileToolStripMenuItem_Click);
            // 
            // toolStripMenuItem1
            // 
            this.toolStripMenuItem1.Name = "toolStripMenuItem1";
            this.toolStripMenuItem1.Size = new System.Drawing.Size(217, 6);
            // 
            // exitToolStripMenuItem
            // 
            this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
            this.exitToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Alt | System.Windows.Forms.Keys.F4)));
            this.exitToolStripMenuItem.Size = new System.Drawing.Size(220, 22);
            this.exitToolStripMenuItem.Text = "E&xit";
            // 
            // databaseToolStripMenuItem
            // 
            this.databaseToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.connectToEnvironmentalDatabaseToolStripMenuItem,
            this.createNewEnvironmentalDatabaseToolStripMenuItem});
            this.databaseToolStripMenuItem.Name = "databaseToolStripMenuItem";
            this.databaseToolStripMenuItem.Size = new System.Drawing.Size(67, 20);
            this.databaseToolStripMenuItem.Text = "&Database";
            // 
            // connectToEnvironmentalDatabaseToolStripMenuItem
            // 
            this.connectToEnvironmentalDatabaseToolStripMenuItem.Name = "connectToEnvironmentalDatabaseToolStripMenuItem";
            this.connectToEnvironmentalDatabaseToolStripMenuItem.Size = new System.Drawing.Size(273, 22);
            this.connectToEnvironmentalDatabaseToolStripMenuItem.Text = "Connect to Environmental Database...";
            this.connectToEnvironmentalDatabaseToolStripMenuItem.Click += new System.EventHandler(this.connectToEnvironmentalDatabaseToolStripMenuItem_Click);
            // 
            // createNewEnvironmentalDatabaseToolStripMenuItem
            // 
            this.createNewEnvironmentalDatabaseToolStripMenuItem.Name = "createNewEnvironmentalDatabaseToolStripMenuItem";
            this.createNewEnvironmentalDatabaseToolStripMenuItem.Size = new System.Drawing.Size(273, 22);
            this.createNewEnvironmentalDatabaseToolStripMenuItem.Text = "Create new Environmental Database...";
            this.createNewEnvironmentalDatabaseToolStripMenuItem.Click += new System.EventHandler(this.createNewEnvironmentalDatabaseToolStripMenuItem_Click);
            // 
            // gbEnvironmentalDatabase
            // 
            this.gbEnvironmentalDatabase.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.gbEnvironmentalDatabase.Controls.Add(this.dbcDataSubset);
            this.gbEnvironmentalDatabase.Controls.Add(this.dbcDataSet);
            this.gbEnvironmentalDatabase.Controls.Add(this.label4);
            this.gbEnvironmentalDatabase.Controls.Add(this.label3);
            this.gbEnvironmentalDatabase.Controls.Add(this.cboDataType);
            this.gbEnvironmentalDatabase.Controls.Add(this.label2);
            this.gbEnvironmentalDatabase.Enabled = false;
            this.gbEnvironmentalDatabase.Location = new System.Drawing.Point(268, 29);
            this.gbEnvironmentalDatabase.Name = "gbEnvironmentalDatabase";
            this.gbEnvironmentalDatabase.Size = new System.Drawing.Size(202, 159);
            this.gbEnvironmentalDatabase.TabIndex = 9;
            this.gbEnvironmentalDatabase.TabStop = false;
            this.gbEnvironmentalDatabase.Text = "Environmental Database";
            // 
            // dbcDataSubset
            // 
            this.dbcDataSubset.ForeignKeyName = "idDataSet";
            this.dbcDataSubset.ForeignKeyValue = 0;
            this.dbcDataSubset.FormattingEnabled = true;
            this.dbcDataSubset.Location = new System.Drawing.Point(9, 123);
            this.dbcDataSubset.Name = "dbcDataSubset";
            this.dbcDataSubset.Size = new System.Drawing.Size(187, 21);
            this.dbcDataSubset.TabIndex = 17;
            this.dbcDataSubset.TableName = "DataSubSet";
            this.dbcDataSubset.SelectedIndexChanged += new System.EventHandler(this.dbcDataSubset_SelectedIndexChanged);
            // 
            // dbcDataSet
            // 
            this.dbcDataSet.ForeignKeyName = "idDataType";
            this.dbcDataSet.ForeignKeyValue = 0;
            this.dbcDataSet.FormattingEnabled = true;
            this.dbcDataSet.Location = new System.Drawing.Point(9, 83);
            this.dbcDataSet.Name = "dbcDataSet";
            this.dbcDataSet.Size = new System.Drawing.Size(187, 21);
            this.dbcDataSet.TabIndex = 16;
            this.dbcDataSet.TableName = "DataSet";
            this.dbcDataSet.SelectedIndexChanged += new System.EventHandler(this.dbcDataSet_SelectedIndexChanged);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(6, 107);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(66, 13);
            this.label4.TabIndex = 14;
            this.label4.Text = "Data Subset";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(6, 66);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(49, 13);
            this.label3.TabIndex = 12;
            this.label3.Text = "Data Set";
            // 
            // cboDataType
            // 
            this.cboDataType.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.cboDataType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboDataType.FormattingEnabled = true;
            this.cboDataType.Location = new System.Drawing.Point(11, 42);
            this.cboDataType.Name = "cboDataType";
            this.cboDataType.Size = new System.Drawing.Size(185, 21);
            this.cboDataType.TabIndex = 11;
            this.cboDataType.SelectedIndexChanged += new System.EventHandler(this.cboDataType_SelectedIndexChanged);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(6, 25);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(57, 13);
            this.label2.TabIndex = 10;
            this.label2.Text = "Data Type";
            // 
            // gbNetCDFVariables
            // 
            this.gbNetCDFVariables.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.gbNetCDFVariables.Controls.Add(this.cboDepthName);
            this.gbNetCDFVariables.Controls.Add(this.label9);
            this.gbNetCDFVariables.Controls.Add(this.btnImport);
            this.gbNetCDFVariables.Controls.Add(this.gbAttributes);
            this.gbNetCDFVariables.Controls.Add(this.cboDataName);
            this.gbNetCDFVariables.Controls.Add(this.lblDataVariable);
            this.gbNetCDFVariables.Controls.Add(this.cboLatitudeName);
            this.gbNetCDFVariables.Controls.Add(this.label6);
            this.gbNetCDFVariables.Controls.Add(this.cboLongitudeName);
            this.gbNetCDFVariables.Controls.Add(this.label5);
            this.gbNetCDFVariables.Enabled = false;
            this.gbNetCDFVariables.Location = new System.Drawing.Point(268, 194);
            this.gbNetCDFVariables.Name = "gbNetCDFVariables";
            this.gbNetCDFVariables.Size = new System.Drawing.Size(202, 410);
            this.gbNetCDFVariables.TabIndex = 10;
            this.gbNetCDFVariables.TabStop = false;
            this.gbNetCDFVariables.Text = "NetCDF Dataset";
            // 
            // cboDepthName
            // 
            this.cboDepthName.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.cboDepthName.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboDepthName.FormattingEnabled = true;
            this.cboDepthName.Location = new System.Drawing.Point(11, 124);
            this.cboDepthName.Name = "cboDepthName";
            this.cboDepthName.Size = new System.Drawing.Size(185, 21);
            this.cboDepthName.TabIndex = 23;
            this.cboDepthName.SelectedIndexChanged += new System.EventHandler(this.cboDepthName_SelectedIndexChanged);
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(6, 107);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(76, 13);
            this.label9.TabIndex = 22;
            this.label9.Text = "Depth variable";
            // 
            // btnImport
            // 
            this.btnImport.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.btnImport.Location = new System.Drawing.Point(6, 375);
            this.btnImport.Name = "btnImport";
            this.btnImport.Size = new System.Drawing.Size(190, 23);
            this.btnImport.TabIndex = 21;
            this.btnImport.Text = "Queue this file for import";
            this.btnImport.UseVisualStyleBackColor = true;
            this.btnImport.Click += new System.EventHandler(this.btnImport_Click);
            // 
            // gbAttributes
            // 
            this.gbAttributes.Controls.Add(this.cboMissingValue);
            this.gbAttributes.Controls.Add(this.label10);
            this.gbAttributes.Controls.Add(this.cboOffsetName);
            this.gbAttributes.Controls.Add(this.label8);
            this.gbAttributes.Controls.Add(this.cboScalingName);
            this.gbAttributes.Controls.Add(this.label7);
            this.gbAttributes.Location = new System.Drawing.Point(6, 192);
            this.gbAttributes.Name = "gbAttributes";
            this.gbAttributes.Size = new System.Drawing.Size(187, 156);
            this.gbAttributes.TabIndex = 20;
            this.gbAttributes.TabStop = false;
            this.gbAttributes.Text = "Data Attributes";
            // 
            // cboMissingValue
            // 
            this.cboMissingValue.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.cboMissingValue.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboMissingValue.FormattingEnabled = true;
            this.cboMissingValue.Location = new System.Drawing.Point(11, 43);
            this.cboMissingValue.Name = "cboMissingValue";
            this.cboMissingValue.Size = new System.Drawing.Size(169, 21);
            this.cboMissingValue.TabIndex = 25;
            this.cboMissingValue.SelectedIndexChanged += new System.EventHandler(this.cboMissingValue_SelectedIndexChanged);
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(6, 26);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(71, 13);
            this.label10.TabIndex = 24;
            this.label10.Text = "Missing value";
            // 
            // cboOffsetName
            // 
            this.cboOffsetName.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.cboOffsetName.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboOffsetName.FormattingEnabled = true;
            this.cboOffsetName.Location = new System.Drawing.Point(11, 125);
            this.cboOffsetName.Name = "cboOffsetName";
            this.cboOffsetName.Size = new System.Drawing.Size(169, 21);
            this.cboOffsetName.TabIndex = 21;
            this.cboOffsetName.SelectedIndexChanged += new System.EventHandler(this.cboOffsetName_SelectedIndexChanged);
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(6, 108);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(35, 13);
            this.label8.TabIndex = 20;
            this.label8.Text = "Offset";
            // 
            // cboScalingName
            // 
            this.cboScalingName.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.cboScalingName.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboScalingName.FormattingEnabled = true;
            this.cboScalingName.Location = new System.Drawing.Point(11, 84);
            this.cboScalingName.Name = "cboScalingName";
            this.cboScalingName.Size = new System.Drawing.Size(169, 21);
            this.cboScalingName.TabIndex = 19;
            this.cboScalingName.SelectedIndexChanged += new System.EventHandler(this.cboScalingName_SelectedIndexChanged);
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(6, 67);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(64, 13);
            this.label7.TabIndex = 18;
            this.label7.Text = "Scale factor";
            // 
            // cboDataName
            // 
            this.cboDataName.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.cboDataName.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboDataName.FormattingEnabled = true;
            this.cboDataName.Location = new System.Drawing.Point(11, 165);
            this.cboDataName.Name = "cboDataName";
            this.cboDataName.Size = new System.Drawing.Size(185, 21);
            this.cboDataName.TabIndex = 17;
            this.cboDataName.SelectedIndexChanged += new System.EventHandler(this.cboDataName_SelectedIndexChanged);
            // 
            // lblDataVariable
            // 
            this.lblDataVariable.AutoSize = true;
            this.lblDataVariable.Location = new System.Drawing.Point(6, 148);
            this.lblDataVariable.Name = "lblDataVariable";
            this.lblDataVariable.Size = new System.Drawing.Size(70, 13);
            this.lblDataVariable.TabIndex = 16;
            this.lblDataVariable.Text = "Data variable";
            // 
            // cboLatitudeName
            // 
            this.cboLatitudeName.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.cboLatitudeName.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboLatitudeName.FormattingEnabled = true;
            this.cboLatitudeName.Location = new System.Drawing.Point(11, 83);
            this.cboLatitudeName.Name = "cboLatitudeName";
            this.cboLatitudeName.Size = new System.Drawing.Size(185, 21);
            this.cboLatitudeName.TabIndex = 15;
            this.cboLatitudeName.SelectedIndexChanged += new System.EventHandler(this.cboLatitudeName_SelectedIndexChanged);
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(6, 66);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(85, 13);
            this.label6.TabIndex = 14;
            this.label6.Text = "Latitude variable";
            // 
            // cboLongitudeName
            // 
            this.cboLongitudeName.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.cboLongitudeName.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboLongitudeName.FormattingEnabled = true;
            this.cboLongitudeName.Location = new System.Drawing.Point(11, 42);
            this.cboLongitudeName.Name = "cboLongitudeName";
            this.cboLongitudeName.Size = new System.Drawing.Size(185, 21);
            this.cboLongitudeName.TabIndex = 13;
            this.cboLongitudeName.SelectedIndexChanged += new System.EventHandler(this.cboLongitudeName_SelectedIndexChanged);
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(6, 25);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(94, 13);
            this.label5.TabIndex = 12;
            this.label5.Text = "Longitude variable";
            // 
            // frmAddToDatabase
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(482, 628);
            this.Controls.Add(this.gbNetCDFVariables);
            this.Controls.Add(this.gbEnvironmentalDatabase);
            this.Controls.Add(this.ncFileExplorer1);
            this.Controls.Add(this.statusStrip1);
            this.Controls.Add(this.menuStrip1);
            this.Controls.Add(this.label1);
            this.MainMenuStrip = this.menuStrip1;
            this.MinimumSize = new System.Drawing.Size(498, 601);
            this.Name = "frmAddToDatabase";
            this.Text = "Import NetCDF File into Environmental Database";
            this.statusStrip1.ResumeLayout(false);
            this.statusStrip1.PerformLayout();
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.gbEnvironmentalDatabase.ResumeLayout(false);
            this.gbEnvironmentalDatabase.PerformLayout();
            this.gbNetCDFVariables.ResumeLayout(false);
            this.gbNetCDFVariables.PerformLayout();
            this.gbAttributes.ResumeLayout(false);
            this.gbAttributes.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.StatusStrip statusStrip1;
        private System.Windows.Forms.ToolStripStatusLabel tsStatus;
        private System.Windows.Forms.OpenFileDialog openFileDialog1;
        private NetCDF.NcFileExplorer ncFileExplorer1;
        private System.Windows.Forms.MenuStrip menuStrip1;
        private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem openNetCDFFileToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem exitToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem databaseToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem connectToEnvironmentalDatabaseToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem createNewEnvironmentalDatabaseToolStripMenuItem;
        private System.Windows.Forms.GroupBox gbEnvironmentalDatabase;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.ComboBox cboDataType;
        private System.Windows.Forms.GroupBox gbNetCDFVariables;
        private System.Windows.Forms.ComboBox cboDataName;
        private System.Windows.Forms.Label lblDataVariable;
        private System.Windows.Forms.ComboBox cboLatitudeName;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.ComboBox cboLongitudeName;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Button btnImport;
        private System.Windows.Forms.GroupBox gbAttributes;
        private System.Windows.Forms.ComboBox cboOffsetName;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.ComboBox cboScalingName;
        private System.Windows.Forms.Label label7;
        private DatabaseKeyComboBox dbcDataSet;
        private DatabaseKeyComboBox dbcDataSubset;
        private System.Windows.Forms.ComboBox cboDepthName;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.ComboBox cboMissingValue;
        private System.Windows.Forms.Label label10;
    }
}

