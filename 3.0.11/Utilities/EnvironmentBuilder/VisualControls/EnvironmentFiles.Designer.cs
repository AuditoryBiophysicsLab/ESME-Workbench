namespace EnvironmentBuilder.VisualControls
{
    partial class EnvironmentFiles
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
            this.environmentFileWind = new EnvironmentBuilder.VisualControls.EnvironmentFileDropdown();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.environmentFileSoundSpeed = new EnvironmentBuilder.VisualControls.EnvironmentFileDropdown();
            this.label3 = new System.Windows.Forms.Label();
            this.environmentFileBottomType = new EnvironmentBuilder.VisualControls.EnvironmentFileDropdown();
            this.label4 = new System.Windows.Forms.Label();
            this.environmentFileBathymetry = new EnvironmentBuilder.VisualControls.EnvironmentFileDropdown();
            this.SuspendLayout();
            // 
            // environmentFileWind
            // 
            this.environmentFileWind.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.environmentFileWind.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.environmentFileWind.FormattingEnabled = true;
            this.environmentFileWind.LayerName = "windspeed";
            this.environmentFileWind.Location = new System.Drawing.Point(3, 20);
            this.environmentFileWind.Name = "environmentFileWind";
            this.environmentFileWind.SearchDirectory = null;
            this.environmentFileWind.Size = new System.Drawing.Size(274, 21);
            this.environmentFileWind.TabIndex = 0;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(4, 4);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(32, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "Wind";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(3, 49);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(72, 13);
            this.label2.TabIndex = 3;
            this.label2.Text = "Sound Speed";
            // 
            // environmentFileSoundSpeed
            // 
            this.environmentFileSoundSpeed.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.environmentFileSoundSpeed.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.environmentFileSoundSpeed.FormattingEnabled = true;
            this.environmentFileSoundSpeed.LayerName = "soundspeed";
            this.environmentFileSoundSpeed.Location = new System.Drawing.Point(2, 65);
            this.environmentFileSoundSpeed.Name = "environmentFileSoundSpeed";
            this.environmentFileSoundSpeed.SearchDirectory = null;
            this.environmentFileSoundSpeed.Size = new System.Drawing.Size(274, 21);
            this.environmentFileSoundSpeed.TabIndex = 2;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(3, 94);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(67, 13);
            this.label3.TabIndex = 5;
            this.label3.Text = "Bottom Type";
            // 
            // environmentFileBottomType
            // 
            this.environmentFileBottomType.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.environmentFileBottomType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.environmentFileBottomType.FormattingEnabled = true;
            this.environmentFileBottomType.LayerName = "bottomtype";
            this.environmentFileBottomType.Location = new System.Drawing.Point(2, 110);
            this.environmentFileBottomType.Name = "environmentFileBottomType";
            this.environmentFileBottomType.SearchDirectory = null;
            this.environmentFileBottomType.Size = new System.Drawing.Size(274, 21);
            this.environmentFileBottomType.TabIndex = 4;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(3, 139);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(59, 13);
            this.label4.TabIndex = 7;
            this.label4.Text = "Bathymetry";
            // 
            // environmentFileBathymetry
            // 
            this.environmentFileBathymetry.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.environmentFileBathymetry.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.environmentFileBathymetry.FormattingEnabled = true;
            this.environmentFileBathymetry.LayerName = "bathymetry";
            this.environmentFileBathymetry.Location = new System.Drawing.Point(2, 155);
            this.environmentFileBathymetry.Name = "environmentFileBathymetry";
            this.environmentFileBathymetry.SearchDirectory = null;
            this.environmentFileBathymetry.Size = new System.Drawing.Size(274, 21);
            this.environmentFileBathymetry.TabIndex = 6;
            // 
            // EnvironmentFiles
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.label4);
            this.Controls.Add(this.environmentFileBathymetry);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.environmentFileBottomType);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.environmentFileSoundSpeed);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.environmentFileWind);
            this.Name = "EnvironmentFiles";
            this.Size = new System.Drawing.Size(281, 190);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        public EnvironmentFileDropdown environmentFileWind;
        public EnvironmentFileDropdown environmentFileSoundSpeed;
        public EnvironmentFileDropdown environmentFileBottomType;
        public EnvironmentFileDropdown environmentFileBathymetry;
    }
}
