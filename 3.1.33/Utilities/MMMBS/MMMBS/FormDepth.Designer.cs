namespace MBSGUI
{
    partial class FormDepth
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
            if(disposing && (components != null))
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
            this.CancellButton = new System.Windows.Forms.Button();
            this.MaxTextBox = new System.Windows.Forms.TextBox();
            this.MaxLabel = new System.Windows.Forms.Label();
            this.OkButton = new System.Windows.Forms.Button();
            this.MeanTextBox = new System.Windows.Forms.TextBox();
            this.MeanLabel = new System.Windows.Forms.Label();
            this.StdTextBox = new System.Windows.Forms.TextBox();
            this.StdLabel = new System.Windows.Forms.Label();
            this.RndGroupBox = new System.Windows.Forms.GroupBox();
            this.GaussGroupBox = new System.Windows.Forms.GroupBox();
            this.VectorGroupBox = new System.Windows.Forms.GroupBox();
            this.MaxVectorLabel = new System.Windows.Forms.Label();
            this.MaxVctrTextBox = new System.Windows.Forms.TextBox();
            this.VectorTextBox = new System.Windows.Forms.TextBox();
            this.StepLabel = new System.Windows.Forms.Label();
            this.VectorLabel = new System.Windows.Forms.Label();
            this.StepTextBox = new System.Windows.Forms.TextBox();
            this.FrmErrLabel = new System.Windows.Forms.Label();
            this.RefreshButton = new System.Windows.Forms.Button();
            this.RndGroupBox.SuspendLayout();
            this.GaussGroupBox.SuspendLayout();
            this.VectorGroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // CancellButton
            // 
            this.CancellButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.CancellButton.Location = new System.Drawing.Point(87, 250);
            this.CancellButton.Name = "CancellButton";
            this.CancellButton.Size = new System.Drawing.Size(90, 44);
            this.CancellButton.TabIndex = 26;
            this.CancellButton.Text = "Cancel";
            this.CancellButton.UseVisualStyleBackColor = true;
            this.CancellButton.Click += new System.EventHandler(this.CancelButton_Click);
            // 
            // MaxTextBox
            // 
            this.MaxTextBox.Location = new System.Drawing.Point(103, 28);
            this.MaxTextBox.Name = "MaxTextBox";
            this.MaxTextBox.Size = new System.Drawing.Size(55, 20);
            this.MaxTextBox.TabIndex = 4;
            this.MaxTextBox.TextChanged += new System.EventHandler(this.MaxTextBox_TextChanged);
            // 
            // MaxLabel
            // 
            this.MaxLabel.AutoSize = true;
            this.MaxLabel.Location = new System.Drawing.Point(20, 31);
            this.MaxLabel.Name = "MaxLabel";
            this.MaxLabel.Size = new System.Drawing.Size(76, 13);
            this.MaxLabel.TabIndex = 7;
            this.MaxLabel.Text = "max depth (m):";
            // 
            // OkButton
            // 
            this.OkButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.OkButton.Location = new System.Drawing.Point(279, 250);
            this.OkButton.Name = "OkButton";
            this.OkButton.Size = new System.Drawing.Size(90, 44);
            this.OkButton.TabIndex = 25;
            this.OkButton.Text = "OK";
            this.OkButton.UseVisualStyleBackColor = true;
            this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
            // 
            // MeanTextBox
            // 
            this.MeanTextBox.Location = new System.Drawing.Point(101, 28);
            this.MeanTextBox.Name = "MeanTextBox";
            this.MeanTextBox.Size = new System.Drawing.Size(55, 20);
            this.MeanTextBox.TabIndex = 4;
            this.MeanTextBox.TextChanged += new System.EventHandler(this.MeanTextBox_TextChanged);
            // 
            // MeanLabel
            // 
            this.MeanLabel.AutoSize = true;
            this.MeanLabel.Location = new System.Drawing.Point(14, 31);
            this.MeanLabel.Name = "MeanLabel";
            this.MeanLabel.Size = new System.Drawing.Size(83, 13);
            this.MeanLabel.TabIndex = 7;
            this.MeanLabel.Text = "mean depth (m):";
            // 
            // StdTextBox
            // 
            this.StdTextBox.Location = new System.Drawing.Point(101, 54);
            this.StdTextBox.Name = "StdTextBox";
            this.StdTextBox.Size = new System.Drawing.Size(55, 20);
            this.StdTextBox.TabIndex = 5;
            this.StdTextBox.TextChanged += new System.EventHandler(this.StdTextBox_TextChanged);
            // 
            // StdLabel
            // 
            this.StdLabel.AutoSize = true;
            this.StdLabel.Location = new System.Drawing.Point(56, 57);
            this.StdLabel.Name = "StdLabel";
            this.StdLabel.Size = new System.Drawing.Size(41, 13);
            this.StdLabel.TabIndex = 9;
            this.StdLabel.Text = "std (m):";
            // 
            // RndGroupBox
            // 
            this.RndGroupBox.Controls.Add(this.MaxTextBox);
            this.RndGroupBox.Controls.Add(this.MaxLabel);
            this.RndGroupBox.Location = new System.Drawing.Point(198, 12);
            this.RndGroupBox.Name = "RndGroupBox";
            this.RndGroupBox.Size = new System.Drawing.Size(171, 86);
            this.RndGroupBox.TabIndex = 24;
            this.RndGroupBox.TabStop = false;
            this.RndGroupBox.Text = "Random";
            // 
            // GaussGroupBox
            // 
            this.GaussGroupBox.Controls.Add(this.MeanTextBox);
            this.GaussGroupBox.Controls.Add(this.MeanLabel);
            this.GaussGroupBox.Controls.Add(this.StdTextBox);
            this.GaussGroupBox.Controls.Add(this.StdLabel);
            this.GaussGroupBox.Location = new System.Drawing.Point(12, 12);
            this.GaussGroupBox.Name = "GaussGroupBox";
            this.GaussGroupBox.Size = new System.Drawing.Size(170, 86);
            this.GaussGroupBox.TabIndex = 23;
            this.GaussGroupBox.TabStop = false;
            this.GaussGroupBox.Text = "Gaussian";
            // 
            // VectorGroupBox
            // 
            this.VectorGroupBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.VectorGroupBox.Controls.Add(this.MaxVectorLabel);
            this.VectorGroupBox.Controls.Add(this.MaxVctrTextBox);
            this.VectorGroupBox.Controls.Add(this.VectorTextBox);
            this.VectorGroupBox.Controls.Add(this.StepLabel);
            this.VectorGroupBox.Controls.Add(this.VectorLabel);
            this.VectorGroupBox.Controls.Add(this.StepTextBox);
            this.VectorGroupBox.Controls.Add(this.FrmErrLabel);
            this.VectorGroupBox.Location = new System.Drawing.Point(12, 104);
            this.VectorGroupBox.Name = "VectorGroupBox";
            this.VectorGroupBox.Size = new System.Drawing.Size(357, 137);
            this.VectorGroupBox.TabIndex = 27;
            this.VectorGroupBox.TabStop = false;
            this.VectorGroupBox.Text = "Vector Modeling";
            // 
            // MaxVectorLabel
            // 
            this.MaxVectorLabel.AutoSize = true;
            this.MaxVectorLabel.Location = new System.Drawing.Point(183, 104);
            this.MaxVectorLabel.Name = "MaxVectorLabel";
            this.MaxVectorLabel.Size = new System.Drawing.Size(100, 13);
            this.MaxVectorLabel.TabIndex = 22;
            this.MaxVectorLabel.Text = "maximum depth (m):";
            // 
            // MaxVctrTextBox
            // 
            this.MaxVctrTextBox.Location = new System.Drawing.Point(287, 101);
            this.MaxVctrTextBox.Name = "MaxVctrTextBox";
            this.MaxVctrTextBox.ReadOnly = true;
            this.MaxVctrTextBox.Size = new System.Drawing.Size(37, 20);
            this.MaxVctrTextBox.TabIndex = 21;
            // 
            // VectorTextBox
            // 
            this.VectorTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.VectorTextBox.Location = new System.Drawing.Point(7, 39);
            this.VectorTextBox.Name = "VectorTextBox";
            this.VectorTextBox.Size = new System.Drawing.Size(337, 20);
            this.VectorTextBox.TabIndex = 0;
            this.VectorTextBox.TextChanged += new System.EventHandler(this.VectorTextBox_TextChanged);
            // 
            // StepLabel
            // 
            this.StepLabel.AutoSize = true;
            this.StepLabel.Location = new System.Drawing.Point(15, 104);
            this.StepLabel.Name = "StepLabel";
            this.StepLabel.Size = new System.Drawing.Size(68, 13);
            this.StepLabel.TabIndex = 20;
            this.StepLabel.Text = "step size (m):";
            // 
            // VectorLabel
            // 
            this.VectorLabel.AutoSize = true;
            this.VectorLabel.Location = new System.Drawing.Point(6, 23);
            this.VectorLabel.Name = "VectorLabel";
            this.VectorLabel.Size = new System.Drawing.Size(150, 13);
            this.VectorLabel.TabIndex = 15;
            this.VectorLabel.Text = "Probability of Turning,  [1x200]";
            // 
            // StepTextBox
            // 
            this.StepTextBox.Location = new System.Drawing.Point(87, 101);
            this.StepTextBox.Name = "StepTextBox";
            this.StepTextBox.Size = new System.Drawing.Size(37, 20);
            this.StepTextBox.TabIndex = 19;
            this.StepTextBox.TextChanged += new System.EventHandler(this.StepTextBox_TextChanged);
            // 
            // FrmErrLabel
            // 
            this.FrmErrLabel.BackColor = System.Drawing.SystemColors.Control;
            this.FrmErrLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FrmErrLabel.Location = new System.Drawing.Point(7, 61);
            this.FrmErrLabel.Name = "FrmErrLabel";
            this.FrmErrLabel.Size = new System.Drawing.Size(63, 17);
            this.FrmErrLabel.TabIndex = 16;
            this.FrmErrLabel.Text = "Format Error";
            this.FrmErrLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // RefreshButton
            // 
            this.RefreshButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.RefreshButton.Location = new System.Drawing.Point(183, 250);
            this.RefreshButton.Name = "RefreshButton";
            this.RefreshButton.Size = new System.Drawing.Size(90, 44);
            this.RefreshButton.TabIndex = 18;
            this.RefreshButton.Text = "Refresh";
            this.RefreshButton.UseVisualStyleBackColor = true;
            this.RefreshButton.Click += new System.EventHandler(this.RefreshButton_Click);
            // 
            // FormDepth
            // 
            this.AcceptButton = this.OkButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(381, 306);
            this.Controls.Add(this.VectorGroupBox);
            this.Controls.Add(this.CancellButton);
            this.Controls.Add(this.RefreshButton);
            this.Controls.Add(this.OkButton);
            this.Controls.Add(this.RndGroupBox);
            this.Controls.Add(this.GaussGroupBox);
            this.MaximizeBox = false;
            this.MinimumSize = new System.Drawing.Size(389, 333);
            this.Name = "FormDepth";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "3MB Dive Depth Description - Biomimetica";
            this.RndGroupBox.ResumeLayout(false);
            this.RndGroupBox.PerformLayout();
            this.GaussGroupBox.ResumeLayout(false);
            this.GaussGroupBox.PerformLayout();
            this.VectorGroupBox.ResumeLayout(false);
            this.VectorGroupBox.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button CancellButton;
        private System.Windows.Forms.TextBox MaxTextBox;
        private System.Windows.Forms.Label MaxLabel;
        private System.Windows.Forms.Button OkButton;
        private System.Windows.Forms.TextBox MeanTextBox;
        private System.Windows.Forms.Label MeanLabel;
        private System.Windows.Forms.TextBox StdTextBox;
        private System.Windows.Forms.Label StdLabel;
        private System.Windows.Forms.GroupBox RndGroupBox;
        private System.Windows.Forms.GroupBox GaussGroupBox;
        private System.Windows.Forms.GroupBox VectorGroupBox;
        private System.Windows.Forms.Label MaxVectorLabel;
        private System.Windows.Forms.TextBox MaxVctrTextBox;
        private System.Windows.Forms.Button RefreshButton;
        private System.Windows.Forms.TextBox VectorTextBox;
        private System.Windows.Forms.Label StepLabel;
        private System.Windows.Forms.TextBox StepTextBox;
        private System.Windows.Forms.Label FrmErrLabel;
        private System.Windows.Forms.Label VectorLabel;
    }
}