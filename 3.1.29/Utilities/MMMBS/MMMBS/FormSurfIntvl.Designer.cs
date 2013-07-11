namespace MBSGUI
{
    partial class FormSurfIntvl
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
            this.CancllButton = new System.Windows.Forms.Button();
            this.OkButton = new System.Windows.Forms.Button();
            this.MeanTextBox = new System.Windows.Forms.TextBox();
            this.MeanLabel = new System.Windows.Forms.Label();
            this.StdTextBox = new System.Windows.Forms.TextBox();
            this.StdLabel = new System.Windows.Forms.Label();
            this.GaussGroupBox = new System.Windows.Forms.GroupBox();
            this.label1 = new System.Windows.Forms.Label();
            this.VectorGroupBox = new System.Windows.Forms.GroupBox();
            this.MaxVectorLabel = new System.Windows.Forms.Label();
            this.MaxVctrTextBox = new System.Windows.Forms.TextBox();
            this.VectorTextBox = new System.Windows.Forms.TextBox();
            this.VectorLabel = new System.Windows.Forms.Label();
            this.StepLabel = new System.Windows.Forms.Label();
            this.StepTextBox = new System.Windows.Forms.TextBox();
            this.FrmErrLabel = new System.Windows.Forms.Label();
            this.RefreshButton = new System.Windows.Forms.Button();
            this.GaussGroupBox.SuspendLayout();
            this.VectorGroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // CancllButton
            // 
            this.CancllButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.CancllButton.Location = new System.Drawing.Point(134, 272);
            this.CancllButton.Name = "CancllButton";
            this.CancllButton.Size = new System.Drawing.Size(75, 26);
            this.CancllButton.TabIndex = 26;
            this.CancllButton.Text = "Cancel";
            this.CancllButton.UseVisualStyleBackColor = true;
            this.CancllButton.Click += new System.EventHandler(this.CancelButton_Click);
            // 
            // OkButton
            // 
            this.OkButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.OkButton.Location = new System.Drawing.Point(296, 272);
            this.OkButton.Name = "OkButton";
            this.OkButton.Size = new System.Drawing.Size(75, 26);
            this.OkButton.TabIndex = 25;
            this.OkButton.Text = "OK";
            this.OkButton.UseVisualStyleBackColor = true;
            this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
            // 
            // MeanTextBox
            // 
            this.MeanTextBox.Location = new System.Drawing.Point(114, 38);
            this.MeanTextBox.Name = "MeanTextBox";
            this.MeanTextBox.Size = new System.Drawing.Size(61, 20);
            this.MeanTextBox.TabIndex = 4;
            this.MeanTextBox.TextChanged += new System.EventHandler(this.MeanTextBox_TextChanged);
            // 
            // MeanLabel
            // 
            this.MeanLabel.AutoSize = true;
            this.MeanLabel.Location = new System.Drawing.Point(62, 41);
            this.MeanLabel.Name = "MeanLabel";
            this.MeanLabel.Size = new System.Drawing.Size(50, 13);
            this.MeanLabel.TabIndex = 7;
            this.MeanLabel.Text = "mean (s):";
            // 
            // StdTextBox
            // 
            this.StdTextBox.Location = new System.Drawing.Point(114, 64);
            this.StdTextBox.Name = "StdTextBox";
            this.StdTextBox.Size = new System.Drawing.Size(61, 20);
            this.StdTextBox.TabIndex = 5;
            this.StdTextBox.TextChanged += new System.EventHandler(this.StdTextBox_TextChanged);
            // 
            // StdLabel
            // 
            this.StdLabel.AutoSize = true;
            this.StdLabel.Location = new System.Drawing.Point(74, 67);
            this.StdLabel.Name = "StdLabel";
            this.StdLabel.Size = new System.Drawing.Size(38, 13);
            this.StdLabel.TabIndex = 9;
            this.StdLabel.Text = "std (s):";
            // 
            // GaussGroupBox
            // 
            this.GaussGroupBox.Controls.Add(this.label1);
            this.GaussGroupBox.Controls.Add(this.MeanTextBox);
            this.GaussGroupBox.Controls.Add(this.MeanLabel);
            this.GaussGroupBox.Controls.Add(this.StdTextBox);
            this.GaussGroupBox.Controls.Add(this.StdLabel);
            this.GaussGroupBox.Location = new System.Drawing.Point(12, 12);
            this.GaussGroupBox.Name = "GaussGroupBox";
            this.GaussGroupBox.Size = new System.Drawing.Size(193, 93);
            this.GaussGroupBox.TabIndex = 23;
            this.GaussGroupBox.TabStop = false;
            this.GaussGroupBox.Text = "Gaussian";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(9, 16);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(125, 13);
            this.label1.TabIndex = 10;
            this.label1.Text = "Surface Interval Duration";
            // 
            // VectorGroupBox
            // 
            this.VectorGroupBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.VectorGroupBox.Controls.Add(this.MaxVectorLabel);
            this.VectorGroupBox.Controls.Add(this.MaxVctrTextBox);
            this.VectorGroupBox.Controls.Add(this.VectorTextBox);
            this.VectorGroupBox.Controls.Add(this.VectorLabel);
            this.VectorGroupBox.Controls.Add(this.StepLabel);
            this.VectorGroupBox.Controls.Add(this.StepTextBox);
            this.VectorGroupBox.Controls.Add(this.FrmErrLabel);
            this.VectorGroupBox.Location = new System.Drawing.Point(12, 121);
            this.VectorGroupBox.Name = "VectorGroupBox";
            this.VectorGroupBox.Size = new System.Drawing.Size(359, 141);
            this.VectorGroupBox.TabIndex = 28;
            this.VectorGroupBox.TabStop = false;
            this.VectorGroupBox.Text = "Vector Modeling";
            // 
            // MaxVectorLabel
            // 
            this.MaxVectorLabel.AutoSize = true;
            this.MaxVectorLabel.Location = new System.Drawing.Point(156, 109);
            this.MaxVectorLabel.Name = "MaxVectorLabel";
            this.MaxVectorLabel.Size = new System.Drawing.Size(108, 13);
            this.MaxVectorLabel.TabIndex = 22;
            this.MaxVectorLabel.Text = "maximum duration (s):";
            // 
            // MaxVctrTextBox
            // 
            this.MaxVctrTextBox.Location = new System.Drawing.Point(270, 106);
            this.MaxVctrTextBox.Name = "MaxVctrTextBox";
            this.MaxVctrTextBox.ReadOnly = true;
            this.MaxVctrTextBox.Size = new System.Drawing.Size(37, 20);
            this.MaxVctrTextBox.TabIndex = 21;
            // 
            // VectorTextBox
            // 
            this.VectorTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.VectorTextBox.Location = new System.Drawing.Point(7, 44);
            this.VectorTextBox.Name = "VectorTextBox";
            this.VectorTextBox.Size = new System.Drawing.Size(346, 20);
            this.VectorTextBox.TabIndex = 0;
            this.VectorTextBox.TextChanged += new System.EventHandler(this.VectorTextBox_TextChanged);
            // 
            // VectorLabel
            // 
            this.VectorLabel.AutoSize = true;
            this.VectorLabel.Location = new System.Drawing.Point(9, 28);
            this.VectorLabel.Name = "VectorLabel";
            this.VectorLabel.Size = new System.Drawing.Size(150, 13);
            this.VectorLabel.TabIndex = 15;
            this.VectorLabel.Text = "Probability of Turning,  [1x200]";
            // 
            // StepLabel
            // 
            this.StepLabel.AutoSize = true;
            this.StepLabel.Location = new System.Drawing.Point(29, 109);
            this.StepLabel.Name = "StepLabel";
            this.StepLabel.Size = new System.Drawing.Size(65, 13);
            this.StepLabel.TabIndex = 20;
            this.StepLabel.Text = "step size (s):";
            // 
            // StepTextBox
            // 
            this.StepTextBox.Location = new System.Drawing.Point(100, 106);
            this.StepTextBox.Name = "StepTextBox";
            this.StepTextBox.Size = new System.Drawing.Size(37, 20);
            this.StepTextBox.TabIndex = 19;
            this.StepTextBox.TextChanged += new System.EventHandler(this.StepTextBox_TextChanged);
            // 
            // FrmErrLabel
            // 
            this.FrmErrLabel.BackColor = System.Drawing.SystemColors.Control;
            this.FrmErrLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FrmErrLabel.Location = new System.Drawing.Point(6, 67);
            this.FrmErrLabel.Name = "FrmErrLabel";
            this.FrmErrLabel.Size = new System.Drawing.Size(63, 17);
            this.FrmErrLabel.TabIndex = 16;
            this.FrmErrLabel.Text = "Format Error";
            this.FrmErrLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // RefreshButton
            // 
            this.RefreshButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.RefreshButton.Location = new System.Drawing.Point(215, 272);
            this.RefreshButton.Name = "RefreshButton";
            this.RefreshButton.Size = new System.Drawing.Size(75, 26);
            this.RefreshButton.TabIndex = 18;
            this.RefreshButton.Text = "Refresh";
            this.RefreshButton.UseVisualStyleBackColor = true;
            this.RefreshButton.Click += new System.EventHandler(this.RefreshButton_Click);
            // 
            // FormSurfIntvl
            // 
            this.AcceptButton = this.OkButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(381, 310);
            this.Controls.Add(this.VectorGroupBox);
            this.Controls.Add(this.CancllButton);
            this.Controls.Add(this.RefreshButton);
            this.Controls.Add(this.OkButton);
            this.Controls.Add(this.GaussGroupBox);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.MinimumSize = new System.Drawing.Size(387, 335);
            this.Name = "FormSurfIntvl";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "3MB Surface Interval Description - Biomimetica";
            this.GaussGroupBox.ResumeLayout(false);
            this.GaussGroupBox.PerformLayout();
            this.VectorGroupBox.ResumeLayout(false);
            this.VectorGroupBox.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button CancllButton;
        private System.Windows.Forms.Button OkButton;
        private System.Windows.Forms.TextBox MeanTextBox;
        private System.Windows.Forms.Label MeanLabel;
        private System.Windows.Forms.TextBox StdTextBox;
        private System.Windows.Forms.Label StdLabel;
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
        private System.Windows.Forms.Label label1;
    }
}