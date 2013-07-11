namespace EnvironmentBuilder.VisualControls
{
    partial class OverlayFileControl
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

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.lstOverlayFiles = new System.Windows.Forms.ListBox();
            this.contextMenuStrip1 = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.ctxAddFile = new System.Windows.Forms.ToolStripMenuItem();
            this.ctxDeleteFile = new System.Windows.Forms.ToolStripMenuItem();
            this.ctxSetColor = new System.Windows.Forms.ToolStripMenuItem();
            this.ctxDisplay = new System.Windows.Forms.ToolStripMenuItem();
            this.openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
            this.contextMenuStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // lstOverlayFiles
            // 
            this.lstOverlayFiles.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.lstOverlayFiles.FormattingEnabled = true;
            this.lstOverlayFiles.HorizontalScrollbar = true;
            this.lstOverlayFiles.Location = new System.Drawing.Point(0, 2);
            this.lstOverlayFiles.Name = "lstOverlayFiles";
            this.lstOverlayFiles.Size = new System.Drawing.Size(147, 147);
            this.lstOverlayFiles.TabIndex = 0;
            // 
            // contextMenuStrip1
            // 
            this.contextMenuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.ctxAddFile,
            this.ctxDeleteFile,
            this.ctxSetColor,
            this.ctxDisplay});
            this.contextMenuStrip1.Name = "contextMenuStrip1";
            this.contextMenuStrip1.Size = new System.Drawing.Size(181, 92);
            // 
            // ctxAddFile
            // 
            this.ctxAddFile.Name = "ctxAddFile";
            this.ctxAddFile.Size = new System.Drawing.Size(180, 22);
            this.ctxAddFile.Text = "Add Overlay File...";
            this.ctxAddFile.Click += new System.EventHandler(this.ctxAddFile_Click);
            // 
            // ctxDeleteFile
            // 
            this.ctxDeleteFile.Name = "ctxDeleteFile";
            this.ctxDeleteFile.Size = new System.Drawing.Size(180, 22);
            this.ctxDeleteFile.Text = "Delete Overlay File...";
            this.ctxDeleteFile.Click += new System.EventHandler(this.ctxDeleteFile_Click);
            // 
            // ctxSetColor
            // 
            this.ctxSetColor.Name = "ctxSetColor";
            this.ctxSetColor.Size = new System.Drawing.Size(180, 22);
            this.ctxSetColor.Text = "Set Color...";
            this.ctxSetColor.Click += new System.EventHandler(this.ctxSetColor_Click);
            // 
            // ctxDisplay
            // 
            this.ctxDisplay.Checked = true;
            this.ctxDisplay.CheckOnClick = true;
            this.ctxDisplay.CheckState = System.Windows.Forms.CheckState.Checked;
            this.ctxDisplay.Name = "ctxDisplay";
            this.ctxDisplay.Size = new System.Drawing.Size(180, 22);
            this.ctxDisplay.Text = "Display on map";
            this.ctxDisplay.Click += new System.EventHandler(this.ctxDisplay_Click);
            // 
            // openFileDialog1
            // 
            this.openFileDialog1.FileName = "openFileDialog1";
            // 
            // OverlayFileControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.lstOverlayFiles);
            this.Name = "OverlayFileControl";
            this.contextMenuStrip1.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.ListBox lstOverlayFiles;
        private System.Windows.Forms.ContextMenuStrip contextMenuStrip1;
        private System.Windows.Forms.ToolStripMenuItem ctxAddFile;
        private System.Windows.Forms.ToolStripMenuItem ctxDeleteFile;
        private System.Windows.Forms.ToolStripMenuItem ctxSetColor;
        private System.Windows.Forms.ToolStripMenuItem ctxDisplay;
        private System.Windows.Forms.OpenFileDialog openFileDialog1;
    }
}
