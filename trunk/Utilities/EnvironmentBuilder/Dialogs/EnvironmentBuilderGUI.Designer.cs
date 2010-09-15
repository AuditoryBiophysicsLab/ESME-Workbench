namespace EnvironmentBuilder.Dialogs
{
    partial class EnvironmentBuilderGUI
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
            this.components = new System.ComponentModel.Container();
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.chooseEnvironmentDatabaseDirectoryToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.chooseNEMODataFilesDirectoryToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.loadNEMOScenarioFileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.importOverlayFileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem1 = new System.Windows.Forms.ToolStripSeparator();
            this.exitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.helpToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.zoomIn = new System.Windows.Forms.ToolStripMenuItem();
            this.zoomOut = new System.Windows.Forms.ToolStripMenuItem();
            this.resetToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.statusStrip1 = new System.Windows.Forms.StatusStrip();
            this.toolStripStatusLabel1 = new System.Windows.Forms.ToolStripStatusLabel();
            this.panel1 = new System.Windows.Forms.Panel();
            this.selectAreaToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.zoomInToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.contextMenuStrip1 = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.extractCurrentViewToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem3 = new System.Windows.Forms.ToolStripMenuItem();
            this.clearAreaToolStripMenuItem2 = new System.Windows.Forms.ToolStripMenuItem();
            this.zoomInToolStripMenuItem2 = new System.Windows.Forms.ToolStripMenuItem();
            this.zoomOutToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.zoomToActualSizeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.saveFileDialog1 = new System.Windows.Forms.SaveFileDialog();
            this.folderBrowserDialog1 = new System.Windows.Forms.FolderBrowserDialog();
            this.openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
            this.contextMenuStrip2 = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.addOverlayFileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.displayToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.deleteToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.folderBrowserDialog2 = new System.Windows.Forms.FolderBrowserDialog();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.environmentFiles1 = new EnvironmentBuilder.VisualControls.EnvironmentFiles();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            this.fileList_Control1 = new EnvironmentBuilder.VisualControls.FileList_Control();
            this.areaInfo1 = new EnvironmentBuilder.VisualControls.AreaInfo();
            this.map_Panel1 = new EnvironmentBuilder.VisualControls.Map_Panel();
            this.menuStrip1.SuspendLayout();
            this.statusStrip1.SuspendLayout();
            this.contextMenuStrip1.SuspendLayout();
            this.contextMenuStrip2.SuspendLayout();
            this.tabControl1.SuspendLayout();
            this.tabPage1.SuspendLayout();
            this.tabPage2.SuspendLayout();
            this.SuspendLayout();
            // 
            // menuStrip1
            // 
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem,
            this.helpToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(1214, 24);
            this.menuStrip1.TabIndex = 1;
            this.menuStrip1.Text = "menuStrip1";
            // 
            // fileToolStripMenuItem
            // 
            this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.chooseEnvironmentDatabaseDirectoryToolStripMenuItem,
            this.chooseNEMODataFilesDirectoryToolStripMenuItem,
            this.loadNEMOScenarioFileToolStripMenuItem,
            this.importOverlayFileToolStripMenuItem,
            this.toolStripMenuItem1,
            this.exitToolStripMenuItem});
            this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
            this.fileToolStripMenuItem.Size = new System.Drawing.Size(37, 20);
            this.fileToolStripMenuItem.Text = "&File";
            // 
            // chooseEnvironmentDatabaseDirectoryToolStripMenuItem
            // 
            this.chooseEnvironmentDatabaseDirectoryToolStripMenuItem.Name = "chooseEnvironmentDatabaseDirectoryToolStripMenuItem";
            this.chooseEnvironmentDatabaseDirectoryToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)(((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Shift)
                        | System.Windows.Forms.Keys.E)));
            this.chooseEnvironmentDatabaseDirectoryToolStripMenuItem.Size = new System.Drawing.Size(377, 22);
            this.chooseEnvironmentDatabaseDirectoryToolStripMenuItem.Text = "Choose Environmental Database Directory...";
            this.chooseEnvironmentDatabaseDirectoryToolStripMenuItem.Click += new System.EventHandler(this.chooseEnvironmentDatabaseDirectoryToolStripMenuItem_Click);
            // 
            // chooseNEMODataFilesDirectoryToolStripMenuItem
            // 
            this.chooseNEMODataFilesDirectoryToolStripMenuItem.Name = "chooseNEMODataFilesDirectoryToolStripMenuItem";
            this.chooseNEMODataFilesDirectoryToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)(((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Shift)
                        | System.Windows.Forms.Keys.N)));
            this.chooseNEMODataFilesDirectoryToolStripMenuItem.Size = new System.Drawing.Size(377, 22);
            this.chooseNEMODataFilesDirectoryToolStripMenuItem.Text = "Choose NEMO Data Files Directory...";
            this.chooseNEMODataFilesDirectoryToolStripMenuItem.Click += new System.EventHandler(this.chooseNEMODataFilesDirectoryToolStripMenuItem_Click);
            // 
            // toolStripMenuItem1
            // 
            this.toolStripMenuItem1.Name = "toolStripMenuItem1";
            this.toolStripMenuItem1.Size = new System.Drawing.Size(374, 6);
            // 
            // exitToolStripMenuItem
            // 
            this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
            this.exitToolStripMenuItem.Size = new System.Drawing.Size(377, 22);
            this.exitToolStripMenuItem.Text = "E&xit";
            this.exitToolStripMenuItem.Click += new System.EventHandler(this.exitToolStripMenuItem_Click);
            // 
            // helpToolStripMenuItem
            // 
            this.helpToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.zoomIn,
            this.zoomOut,
            this.resetToolStripMenuItem});
            this.helpToolStripMenuItem.Name = "helpToolStripMenuItem";
            this.helpToolStripMenuItem.Size = new System.Drawing.Size(44, 20);
            this.helpToolStripMenuItem.Text = "&View";
            // 
            // zoomIn
            // 
            this.zoomIn.Name = "zoomIn";
            this.zoomIn.Size = new System.Drawing.Size(129, 22);
            this.zoomIn.Text = "Zoom In";
            this.zoomIn.Click += new System.EventHandler(this.zoomIn_Click);
            // 
            // zoomOut
            // 
            this.zoomOut.Name = "zoomOut";
            this.zoomOut.Size = new System.Drawing.Size(129, 22);
            this.zoomOut.Text = "Zoom Out";
            this.zoomOut.Click += new System.EventHandler(this.zoomOut_Click);
            // 
            // resetToolStripMenuItem
            // 
            this.resetToolStripMenuItem.Name = "resetToolStripMenuItem";
            this.resetToolStripMenuItem.Size = new System.Drawing.Size(129, 22);
            this.resetToolStripMenuItem.Text = "Reset";
            this.resetToolStripMenuItem.Click += new System.EventHandler(this.resetToolStripMenuItem_Click);
            // 
            // statusStrip1
            // 
            this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripStatusLabel1});
            this.statusStrip1.Location = new System.Drawing.Point(0, 496);
            this.statusStrip1.Name = "statusStrip1";
            this.statusStrip1.Size = new System.Drawing.Size(1214, 22);
            this.statusStrip1.TabIndex = 2;
            this.statusStrip1.Text = "statusStrip1";
            // 
            // toolStripStatusLabel1
            // 
            this.toolStripStatusLabel1.Name = "toolStripStatusLabel1";
            this.toolStripStatusLabel1.Size = new System.Drawing.Size(247, 17);
            this.toolStripStatusLabel1.Text = "Welcome to ESME Geographical Environment";
            // 
            // panel1
            // 
            this.panel1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.panel1.BackColor = System.Drawing.Color.Transparent;
            this.panel1.BackgroundImage = global::EnvironmentBuilder.Properties.Resources.ESME_LogoNew;
            this.panel1.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch;
            this.panel1.Location = new System.Drawing.Point(21, 356);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(261, 138);
            this.panel1.TabIndex = 5;
            // 
            // selectAreaToolStripMenuItem
            // 
            this.selectAreaToolStripMenuItem.Name = "selectAreaToolStripMenuItem";
            this.selectAreaToolStripMenuItem.Size = new System.Drawing.Size(132, 22);
            this.selectAreaToolStripMenuItem.Text = "Select Area";
            // 
            // zoomInToolStripMenuItem1
            // 
            this.zoomInToolStripMenuItem1.Name = "zoomInToolStripMenuItem1";
            this.zoomInToolStripMenuItem1.Size = new System.Drawing.Size(132, 22);
            this.zoomInToolStripMenuItem1.Text = "Zoom In";
            // 
            // contextMenuStrip1
            // 
            this.contextMenuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.extractCurrentViewToolStripMenuItem,
            this.toolStripMenuItem3,
            this.clearAreaToolStripMenuItem2,
            this.zoomInToolStripMenuItem2,
            this.zoomOutToolStripMenuItem1,
            this.zoomToActualSizeToolStripMenuItem});
            this.contextMenuStrip1.Name = "contextMenuStrip1";
            this.contextMenuStrip1.Size = new System.Drawing.Size(190, 158);
            // 
            // extractCurrentViewToolStripMenuItem
            // 
            this.extractCurrentViewToolStripMenuItem.Enabled = false;
            this.extractCurrentViewToolStripMenuItem.Name = "extractCurrentViewToolStripMenuItem";
            this.extractCurrentViewToolStripMenuItem.Size = new System.Drawing.Size(189, 22);
            this.extractCurrentViewToolStripMenuItem.Text = "Extract Current View...";
            this.extractCurrentViewToolStripMenuItem.Click += new System.EventHandler(this.extractCurrentViewToolStripMenuItem_Click);
            // 
            // toolStripMenuItem3
            // 
            this.toolStripMenuItem3.Enabled = false;
            this.toolStripMenuItem3.Name = "toolStripMenuItem3";
            this.toolStripMenuItem3.Size = new System.Drawing.Size(189, 22);
            this.toolStripMenuItem3.Text = "Extract Area...";
            this.toolStripMenuItem3.Click += new System.EventHandler(this.selectAreaToolStripMenuItem1_Click);
            // 
            // clearAreaToolStripMenuItem2
            // 
            this.clearAreaToolStripMenuItem2.Name = "clearAreaToolStripMenuItem2";
            this.clearAreaToolStripMenuItem2.Size = new System.Drawing.Size(189, 22);
            this.clearAreaToolStripMenuItem2.Text = "Clear Selection";
            this.clearAreaToolStripMenuItem2.Click += new System.EventHandler(this.clearAreaToolStripMenuItem2_Click);
            // 
            // zoomInToolStripMenuItem2
            // 
            this.zoomInToolStripMenuItem2.Name = "zoomInToolStripMenuItem2";
            this.zoomInToolStripMenuItem2.Size = new System.Drawing.Size(189, 22);
            this.zoomInToolStripMenuItem2.Text = "Zoom in...";
            this.zoomInToolStripMenuItem2.ToolTipText = "Click to Zoom Into the center of current view or to zoom to the selected area.";
            this.zoomInToolStripMenuItem2.Click += new System.EventHandler(this.zoomInToolStripMenuItem2_Click);
            // 
            // zoomOutToolStripMenuItem1
            // 
            this.zoomOutToolStripMenuItem1.Name = "zoomOutToolStripMenuItem1";
            this.zoomOutToolStripMenuItem1.Size = new System.Drawing.Size(189, 22);
            this.zoomOutToolStripMenuItem1.Text = "Zoom out";
            this.zoomOutToolStripMenuItem1.Click += new System.EventHandler(this.zoomOutToolStripMenuItem1_Click);
            // 
            // zoomToActualSizeToolStripMenuItem
            // 
            this.zoomToActualSizeToolStripMenuItem.Name = "zoomToActualSizeToolStripMenuItem";
            this.zoomToActualSizeToolStripMenuItem.Size = new System.Drawing.Size(189, 22);
            this.zoomToActualSizeToolStripMenuItem.Text = "Reset view";
            this.zoomToActualSizeToolStripMenuItem.Click += new System.EventHandler(this.zoomToActualSizeToolStripMenuItem_Click);
            // 
            // contextMenuStrip2
            // 
            this.contextMenuStrip2.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.addOverlayFileToolStripMenuItem,
            this.displayToolStripMenuItem,
            this.deleteToolStripMenuItem});
            this.contextMenuStrip2.Name = "contextMenuStrip2";
            this.contextMenuStrip2.Size = new System.Drawing.Size(161, 70);
            // 
            // displayToolStripMenuItem
            // 
            this.displayToolStripMenuItem.Checked = true;
            this.displayToolStripMenuItem.CheckOnClick = true;
            this.displayToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked;
            this.displayToolStripMenuItem.Name = "displayToolStripMenuItem";
            this.displayToolStripMenuItem.Size = new System.Drawing.Size(160, 22);
            this.displayToolStripMenuItem.Text = "Display";
            this.displayToolStripMenuItem.Click += new System.EventHandler(this.displayToolStripMenuItem_Click);
            // 
            // deleteToolStripMenuItem
            // 
            this.deleteToolStripMenuItem.Name = "deleteToolStripMenuItem";
            this.deleteToolStripMenuItem.Size = new System.Drawing.Size(160, 22);
            this.deleteToolStripMenuItem.Text = "Remove";
            this.deleteToolStripMenuItem.MouseDown += new System.Windows.Forms.MouseEventHandler(this.deleteToolStripMenuItem_MouseDown);
            // 
            // tabControl1
            // 
            this.tabControl1.AllowDrop = true;
            this.tabControl1.Controls.Add(this.tabPage1);
            this.tabControl1.Controls.Add(this.tabPage2);
            this.tabControl1.Location = new System.Drawing.Point(-4, 24);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(286, 220);
            this.tabControl1.TabIndex = 0;
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.environmentFiles1);
            this.tabPage1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.tabPage1.ForeColor = System.Drawing.SystemColors.ControlText;
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(278, 194);
            this.tabPage1.TabIndex = 0;
            this.tabPage1.Text = "Environmental Database";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // environmentFiles1
            // 
            this.environmentFiles1.Anchor = System.Windows.Forms.AnchorStyles.None;
            this.environmentFiles1.BackColor = System.Drawing.SystemColors.Control;
            this.environmentFiles1.Location = new System.Drawing.Point(13, 5);
            this.environmentFiles1.Name = "environmentFiles1";
            this.environmentFiles1.SearchDirectory = null;
            this.environmentFiles1.Size = new System.Drawing.Size(251, 184);
            this.environmentFiles1.TabIndex = 8;
            // 
            // tabPage2
            // 
            this.tabPage2.Controls.Add(this.fileList_Control1);
            this.tabPage2.Location = new System.Drawing.Point(4, 22);
            this.tabPage2.Name = "tabPage2";
            this.tabPage2.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage2.Size = new System.Drawing.Size(278, 194);
            this.tabPage2.TabIndex = 1;
            this.tabPage2.Text = "Objects Displayed";
            this.tabPage2.UseVisualStyleBackColor = true;
            // 
            // fileList_Control1
            // 
            this.fileList_Control1.AllowDrop = true;
            this.fileList_Control1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.fileList_Control1.BackColor = System.Drawing.SystemColors.Info;
            this.fileList_Control1.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.fileList_Control1.FormattingEnabled = true;
            this.fileList_Control1.Location = new System.Drawing.Point(25, 9);
            this.fileList_Control1.Name = "fileList_Control1";
            this.fileList_Control1.Size = new System.Drawing.Size(247, 169);
            this.fileList_Control1.TabIndex = 9;
            this.fileList_Control1.MouseDown += new System.Windows.Forms.MouseEventHandler(this.fileList_Control1_MouseDown);
            this.fileList_Control1.DragEnter += new System.Windows.Forms.DragEventHandler(this.fileList_Control1_DragEnter);
            // 
            // areaInfo1
            // 
            this.areaInfo1.BackColor = System.Drawing.SystemColors.Control;
            this.areaInfo1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.areaInfo1.East = 0F;
            this.areaInfo1.Location = new System.Drawing.Point(-2, 241);
            this.areaInfo1.Name = "areaInfo1";
            this.areaInfo1.North = 0F;
            this.areaInfo1.Size = new System.Drawing.Size(284, 215);
            this.areaInfo1.South = 0F;
            this.areaInfo1.TabIndex = 3;
            this.areaInfo1.West = 0F;
            this.areaInfo1.CoordinateChanged += new System.EventHandler<System.EventArgs>(this.areaInfo1_CoordinateChanged);
            // 
            // map_Panel1
            // 
            this.map_Panel1.AllowDrop = true;
            this.map_Panel1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.map_Panel1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.map_Panel1.DrawableItems = null;
            this.map_Panel1.East = 0F;
            this.map_Panel1.Location = new System.Drawing.Point(288, 24);
            this.map_Panel1.Name = "map_Panel1";
            this.map_Panel1.North = 0F;
            this.map_Panel1.Size = new System.Drawing.Size(926, 432);
            this.map_Panel1.South = 0F;
            this.map_Panel1.TabIndex = 7;
            this.map_Panel1.West = 0F;
            this.map_Panel1.CoordinateChanged += new System.EventHandler<System.EventArgs>(this.map_Panel1_CoordinateChanged);
            this.map_Panel1.MouseMove += new System.Windows.Forms.MouseEventHandler(this.MapPanel_MouseMove);
            this.map_Panel1.MouseClick += new System.Windows.Forms.MouseEventHandler(this.map_Panel1_MouseClick);
            this.map_Panel1.MouseDown += new System.Windows.Forms.MouseEventHandler(this.MapPanel_MouseDown);
            this.map_Panel1.MouseUp += new System.Windows.Forms.MouseEventHandler(this.map_Panel1_MouseUp);
            this.map_Panel1.DragEnter += new System.Windows.Forms.DragEventHandler(this.map_Panel1_DragEnter);
            // 
            // EnvironmentBuilderGUI
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackgroundImage = global::EnvironmentBuilder.Properties.Resources.ESME_Background;
            this.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch;
            this.ClientSize = new System.Drawing.Size(1214, 518);
            this.Controls.Add(this.menuStrip1);
            this.Controls.Add(this.tabControl1);
            this.Controls.Add(this.areaInfo1);
            this.Controls.Add(this.panel1);
            this.Controls.Add(this.statusStrip1);
            this.Controls.Add(this.map_Panel1);
            this.DoubleBuffered = true;
            this.MainMenuStrip = this.menuStrip1;
            this.MinimumSize = new System.Drawing.Size(1078, 534);
            this.Name = "EnvironmentBuilderGUI";
            this.Text = "ESME Environment Builder";
            this.Load += new System.EventHandler(this.Form_Load);
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.Form_Closing);
            this.Resize += new System.EventHandler(this.ESME_GE_Resize);
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.statusStrip1.ResumeLayout(false);
            this.statusStrip1.PerformLayout();
            this.contextMenuStrip1.ResumeLayout(false);
            this.contextMenuStrip2.ResumeLayout(false);
            this.tabControl1.ResumeLayout(false);
            this.tabPage1.ResumeLayout(false);
            this.tabPage2.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.MenuStrip menuStrip1;
        private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem helpToolStripMenuItem;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.ToolStripMenuItem exitToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem zoomIn;
        private System.Windows.Forms.ToolStripMenuItem zoomOut;
        protected EnvironmentBuilder.VisualControls.AreaInfo areaInfo1;
        protected System.Windows.Forms.StatusStrip statusStrip1;
        protected System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel1;
        private EnvironmentBuilder.VisualControls.Map_Panel map_Panel1;
        private System.Windows.Forms.ToolStripMenuItem selectAreaToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem zoomInToolStripMenuItem1;
        private System.Windows.Forms.ContextMenuStrip contextMenuStrip1;
        private System.Windows.Forms.ToolStripMenuItem zoomInToolStripMenuItem2;
        private System.Windows.Forms.ToolStripMenuItem clearAreaToolStripMenuItem2;
        private System.Windows.Forms.ToolStripMenuItem zoomOutToolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem zoomToActualSizeToolStripMenuItem;
        private EnvironmentBuilder.VisualControls.EnvironmentFiles environmentFiles1;
        private System.Windows.Forms.SaveFileDialog saveFileDialog1;
        private System.Windows.Forms.ToolStripMenuItem resetToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem chooseEnvironmentDatabaseDirectoryToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripMenuItem1;
        private System.Windows.Forms.FolderBrowserDialog folderBrowserDialog1;
        private System.Windows.Forms.ToolStripMenuItem importOverlayFileToolStripMenuItem;
        private System.Windows.Forms.OpenFileDialog openFileDialog1;
        private System.Windows.Forms.ContextMenuStrip contextMenuStrip2;
        private System.Windows.Forms.ToolStripMenuItem displayToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem deleteToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addOverlayFileToolStripMenuItem;
        private EnvironmentBuilder.VisualControls.FileList_Control fileList_Control1;
        private System.Windows.Forms.ToolStripMenuItem loadNEMOScenarioFileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem extractCurrentViewToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem chooseNEMODataFilesDirectoryToolStripMenuItem;
        private System.Windows.Forms.FolderBrowserDialog folderBrowserDialog2;
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tabPage1;
        private System.Windows.Forms.TabPage tabPage2;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem3;
    }
}

