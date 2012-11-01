namespace MBSGUI
{
    partial class FormRate
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
            this.GaussGroupBox = new System.Windows.Forms.GroupBox();
            this.MeanTextBox = new System.Windows.Forms.TextBox();
            this.MeanLabel = new System.Windows.Forms.Label();
            this.StdTextBox = new System.Windows.Forms.TextBox();
            this.StdLabel = new System.Windows.Forms.Label();
            this.CoeffGTextBox = new System.Windows.Forms.TextBox();
            this.CoefficientGaussLabel = new System.Windows.Forms.Label();
            this.RndGroupBox = new System.Windows.Forms.GroupBox();
            this.MaxTextBox = new System.Windows.Forms.TextBox();
            this.MaxLabel = new System.Windows.Forms.Label();
            this.MinTextBox = new System.Windows.Forms.TextBox();
            this.MinLabel = new System.Windows.Forms.Label();
            this.CoeffRTextBox = new System.Windows.Forms.TextBox();
            this.CoeffRandomLabel = new System.Windows.Forms.Label();
            this.CancllButton = new System.Windows.Forms.Button();
            this.OkButton = new System.Windows.Forms.Button();
            this.VectorGroupBox = new System.Windows.Forms.GroupBox();
            this.MaxRateLabel = new System.Windows.Forms.Label();
            this.MaxVctrTextBox = new System.Windows.Forms.TextBox();
            this.RateVectorTextBox = new System.Windows.Forms.TextBox();
            this.StepLabel = new System.Windows.Forms.Label();
            this.StepTextBox = new System.Windows.Forms.TextBox();
            this.VectorLabel = new System.Windows.Forms.Label();
            this.TermCoeffLabel = new System.Windows.Forms.Label();
            this.TermCoeffTextBox = new System.Windows.Forms.TextBox();
            this.FrmErrLabel = new System.Windows.Forms.Label();
            this.RateRefreshButton = new System.Windows.Forms.Button();
            this.GaussGroupBox.SuspendLayout();
            this.RndGroupBox.SuspendLayout();
            this.VectorGroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // GaussGroupBox
            // 
            this.GaussGroupBox.Controls.Add(this.MeanTextBox);
            this.GaussGroupBox.Controls.Add(this.MeanLabel);
            this.GaussGroupBox.Controls.Add(this.StdTextBox);
            this.GaussGroupBox.Controls.Add(this.StdLabel);
            this.GaussGroupBox.Controls.Add(this.CoeffGTextBox);
            this.GaussGroupBox.Controls.Add(this.CoefficientGaussLabel);
            this.GaussGroupBox.Location = new System.Drawing.Point(12, 12);
            this.GaussGroupBox.Name = "GaussGroupBox";
            this.GaussGroupBox.Size = new System.Drawing.Size(193, 118);
            this.GaussGroupBox.TabIndex = 17;
            this.GaussGroupBox.TabStop = false;
            this.GaussGroupBox.Text = "Gaussian";
            // 
            // MeanTextBox
            // 
            this.MeanTextBox.Location = new System.Drawing.Point(104, 28);
            this.MeanTextBox.Name = "MeanTextBox";
            this.MeanTextBox.Size = new System.Drawing.Size(71, 20);
            this.MeanTextBox.TabIndex = 4;
            this.MeanTextBox.TextChanged += new System.EventHandler(this.MeanTextBox_TextChanged);
            // 
            // MeanLabel
            // 
            this.MeanLabel.AutoSize = true;
            this.MeanLabel.Location = new System.Drawing.Point(36, 31);
            this.MeanLabel.Name = "MeanLabel";
            this.MeanLabel.Size = new System.Drawing.Size(63, 13);
            this.MeanLabel.TabIndex = 7;
            this.MeanLabel.Text = "mean (m/s):";
            // 
            // StdTextBox
            // 
            this.StdTextBox.Location = new System.Drawing.Point(104, 54);
            this.StdTextBox.Name = "StdTextBox";
            this.StdTextBox.Size = new System.Drawing.Size(71, 20);
            this.StdTextBox.TabIndex = 5;
            this.StdTextBox.TextChanged += new System.EventHandler(this.StdTextBox_TextChanged);
            // 
            // StdLabel
            // 
            this.StdLabel.AutoSize = true;
            this.StdLabel.Location = new System.Drawing.Point(46, 57);
            this.StdLabel.Name = "StdLabel";
            this.StdLabel.Size = new System.Drawing.Size(54, 13);
            this.StdLabel.TabIndex = 9;
            this.StdLabel.Text = "std. (m/s):";
            // 
            // CoeffGTextBox
            // 
            this.CoeffGTextBox.Location = new System.Drawing.Point(133, 80);
            this.CoeffGTextBox.Name = "CoeffGTextBox";
            this.CoeffGTextBox.Size = new System.Drawing.Size(42, 20);
            this.CoeffGTextBox.TabIndex = 6;
            this.CoeffGTextBox.TextChanged += new System.EventHandler(this.CoeffGTextBox_TextChanged);
            // 
            // CoefficientGaussLabel
            // 
            this.CoefficientGaussLabel.AutoSize = true;
            this.CoefficientGaussLabel.Location = new System.Drawing.Point(17, 83);
            this.CoefficientGaussLabel.Name = "CoefficientGaussLabel";
            this.CoefficientGaussLabel.Size = new System.Drawing.Size(113, 13);
            this.CoefficientGaussLabel.TabIndex = 11;
            this.CoefficientGaussLabel.Text = "termination coefficient:";
            // 
            // RndGroupBox
            // 
            this.RndGroupBox.Controls.Add(this.MaxTextBox);
            this.RndGroupBox.Controls.Add(this.MaxLabel);
            this.RndGroupBox.Controls.Add(this.MinTextBox);
            this.RndGroupBox.Controls.Add(this.MinLabel);
            this.RndGroupBox.Controls.Add(this.CoeffRTextBox);
            this.RndGroupBox.Controls.Add(this.CoeffRandomLabel);
            this.RndGroupBox.Location = new System.Drawing.Point(228, 12);
            this.RndGroupBox.Name = "RndGroupBox";
            this.RndGroupBox.Size = new System.Drawing.Size(193, 118);
            this.RndGroupBox.TabIndex = 18;
            this.RndGroupBox.TabStop = false;
            this.RndGroupBox.Text = "Random";
            // 
            // MaxTextBox
            // 
            this.MaxTextBox.Location = new System.Drawing.Point(96, 28);
            this.MaxTextBox.Name = "MaxTextBox";
            this.MaxTextBox.Size = new System.Drawing.Size(71, 20);
            this.MaxTextBox.TabIndex = 4;
            this.MaxTextBox.TextChanged += new System.EventHandler(this.MaxTextBox_TextChanged);
            // 
            // MaxLabel
            // 
            this.MaxLabel.AutoSize = true;
            this.MaxLabel.Location = new System.Drawing.Point(16, 31);
            this.MaxLabel.Name = "MaxLabel";
            this.MaxLabel.Size = new System.Drawing.Size(77, 13);
            this.MaxLabel.TabIndex = 7;
            this.MaxLabel.Text = "maximum (m/s)";
            // 
            // MinTextBox
            // 
            this.MinTextBox.Location = new System.Drawing.Point(96, 54);
            this.MinTextBox.Name = "MinTextBox";
            this.MinTextBox.Size = new System.Drawing.Size(71, 20);
            this.MinTextBox.TabIndex = 5;
            this.MinTextBox.TextChanged += new System.EventHandler(this.MinTextBox_TextChanged);
            // 
            // MinLabel
            // 
            this.MinLabel.AutoSize = true;
            this.MinLabel.Location = new System.Drawing.Point(19, 57);
            this.MinLabel.Name = "MinLabel";
            this.MinLabel.Size = new System.Drawing.Size(74, 13);
            this.MinLabel.TabIndex = 9;
            this.MinLabel.Text = "minimum (m/s)";
            // 
            // CoeffRTextBox
            // 
            this.CoeffRTextBox.Location = new System.Drawing.Point(127, 80);
            this.CoeffRTextBox.Name = "CoeffRTextBox";
            this.CoeffRTextBox.Size = new System.Drawing.Size(42, 20);
            this.CoeffRTextBox.TabIndex = 6;
            this.CoeffRTextBox.TextChanged += new System.EventHandler(this.CoeffRTextBox_TextChanged);
            // 
            // CoeffRandomLabel
            // 
            this.CoeffRandomLabel.AutoSize = true;
            this.CoeffRandomLabel.Location = new System.Drawing.Point(10, 83);
            this.CoeffRandomLabel.Name = "CoeffRandomLabel";
            this.CoeffRandomLabel.Size = new System.Drawing.Size(113, 13);
            this.CoeffRandomLabel.TabIndex = 11;
            this.CoeffRandomLabel.Text = "termination coefficient:";
            // 
            // CancllButton
            // 
            this.CancllButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.CancllButton.Location = new System.Drawing.Point(189, 291);
            this.CancllButton.Name = "CancllButton";
            this.CancllButton.Size = new System.Drawing.Size(75, 26);
            this.CancllButton.TabIndex = 22;
            this.CancllButton.Text = "Cancel";
            this.CancllButton.UseVisualStyleBackColor = true;
            this.CancllButton.Click += new System.EventHandler(this.CancelButton_Click);
            // 
            // OkButton
            // 
            this.OkButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.OkButton.Location = new System.Drawing.Point(351, 291);
            this.OkButton.Name = "OkButton";
            this.OkButton.Size = new System.Drawing.Size(75, 26);
            this.OkButton.TabIndex = 21;
            this.OkButton.Text = "OK";
            this.OkButton.UseVisualStyleBackColor = true;
            this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
            // 
            // VectorGroupBox
            // 
            this.VectorGroupBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.VectorGroupBox.Controls.Add(this.MaxRateLabel);
            this.VectorGroupBox.Controls.Add(this.MaxVctrTextBox);
            this.VectorGroupBox.Controls.Add(this.RateVectorTextBox);
            this.VectorGroupBox.Controls.Add(this.StepLabel);
            this.VectorGroupBox.Controls.Add(this.StepTextBox);
            this.VectorGroupBox.Controls.Add(this.VectorLabel);
            this.VectorGroupBox.Controls.Add(this.TermCoeffLabel);
            this.VectorGroupBox.Controls.Add(this.TermCoeffTextBox);
            this.VectorGroupBox.Controls.Add(this.FrmErrLabel);
            this.VectorGroupBox.Location = new System.Drawing.Point(12, 136);
            this.VectorGroupBox.Name = "VectorGroupBox";
            this.VectorGroupBox.Size = new System.Drawing.Size(412, 141);
            this.VectorGroupBox.TabIndex = 24;
            this.VectorGroupBox.TabStop = false;
            this.VectorGroupBox.Text = "Vector Modeling";
            // 
            // MaxRateLabel
            // 
            this.MaxRateLabel.AutoSize = true;
            this.MaxRateLabel.Location = new System.Drawing.Point(230, 112);
            this.MaxRateLabel.Name = "MaxRateLabel";
            this.MaxRateLabel.Size = new System.Drawing.Size(130, 13);
            this.MaxRateLabel.TabIndex = 22;
            this.MaxRateLabel.Text = "maximum travel rate (m/s):";
            // 
            // MaxVctrTextBox
            // 
            this.MaxVctrTextBox.Location = new System.Drawing.Point(367, 109);
            this.MaxVctrTextBox.Name = "MaxVctrTextBox";
            this.MaxVctrTextBox.ReadOnly = true;
            this.MaxVctrTextBox.Size = new System.Drawing.Size(37, 20);
            this.MaxVctrTextBox.TabIndex = 21;
            // 
            // RateVectorTextBox
            // 
            this.RateVectorTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.RateVectorTextBox.Location = new System.Drawing.Point(7, 39);
            this.RateVectorTextBox.Name = "RateVectorTextBox";
            this.RateVectorTextBox.Size = new System.Drawing.Size(392, 20);
            this.RateVectorTextBox.TabIndex = 0;
            this.RateVectorTextBox.TextChanged += new System.EventHandler(this.RateVectorTextBox_TextChanged);
            // 
            // StepLabel
            // 
            this.StepLabel.AutoSize = true;
            this.StepLabel.Location = new System.Drawing.Point(42, 84);
            this.StepLabel.Name = "StepLabel";
            this.StepLabel.Size = new System.Drawing.Size(78, 13);
            this.StepLabel.TabIndex = 20;
            this.StepLabel.Text = "step size (m/s):";
            // 
            // StepTextBox
            // 
            this.StepTextBox.Location = new System.Drawing.Point(130, 81);
            this.StepTextBox.Name = "StepTextBox";
            this.StepTextBox.Size = new System.Drawing.Size(37, 20);
            this.StepTextBox.TabIndex = 19;
            this.StepTextBox.TextChanged += new System.EventHandler(this.StepTextBox_TextChanged);
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
            // TermCoeffLabel
            // 
            this.TermCoeffLabel.AutoSize = true;
            this.TermCoeffLabel.Location = new System.Drawing.Point(6, 112);
            this.TermCoeffLabel.Name = "TermCoeffLabel";
            this.TermCoeffLabel.Size = new System.Drawing.Size(113, 13);
            this.TermCoeffLabel.TabIndex = 17;
            this.TermCoeffLabel.Text = "termination coefficient:";
            // 
            // TermCoeffTextBox
            // 
            this.TermCoeffTextBox.Location = new System.Drawing.Point(130, 109);
            this.TermCoeffTextBox.Name = "TermCoeffTextBox";
            this.TermCoeffTextBox.Size = new System.Drawing.Size(37, 20);
            this.TermCoeffTextBox.TabIndex = 14;
            this.TermCoeffTextBox.TextChanged += new System.EventHandler(this.TermCoeffTextBox_TextChanged);
            // 
            // FrmErrLabel
            // 
            this.FrmErrLabel.BackColor = System.Drawing.SystemColors.Control;
            this.FrmErrLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FrmErrLabel.Location = new System.Drawing.Point(7, 62);
            this.FrmErrLabel.Name = "FrmErrLabel";
            this.FrmErrLabel.Size = new System.Drawing.Size(63, 17);
            this.FrmErrLabel.TabIndex = 16;
            this.FrmErrLabel.Text = "Format Error";
            this.FrmErrLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // RateRefreshButton
            // 
            this.RateRefreshButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.RateRefreshButton.Location = new System.Drawing.Point(270, 291);
            this.RateRefreshButton.Name = "RateRefreshButton";
            this.RateRefreshButton.Size = new System.Drawing.Size(75, 26);
            this.RateRefreshButton.TabIndex = 18;
            this.RateRefreshButton.Text = "Refresh";
            this.RateRefreshButton.UseVisualStyleBackColor = true;
            this.RateRefreshButton.Click += new System.EventHandler(this.RateRefreshButton_Click);
            // 
            // FormRate
            // 
            this.AcceptButton = this.OkButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(436, 327);
            this.Controls.Add(this.VectorGroupBox);
            this.Controls.Add(this.CancllButton);
            this.Controls.Add(this.RateRefreshButton);
            this.Controls.Add(this.OkButton);
            this.Controls.Add(this.RndGroupBox);
            this.Controls.Add(this.GaussGroupBox);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimumSize = new System.Drawing.Size(444, 354);
            this.Name = "FormRate";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.Text = "FormRate";
            this.GaussGroupBox.ResumeLayout(false);
            this.GaussGroupBox.PerformLayout();
            this.RndGroupBox.ResumeLayout(false);
            this.RndGroupBox.PerformLayout();
            this.VectorGroupBox.ResumeLayout(false);
            this.VectorGroupBox.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox GaussGroupBox;
        private System.Windows.Forms.TextBox MeanTextBox;
        private System.Windows.Forms.Label MeanLabel;
        private System.Windows.Forms.TextBox StdTextBox;
        private System.Windows.Forms.Label StdLabel;
        private System.Windows.Forms.TextBox CoeffGTextBox;
        private System.Windows.Forms.Label CoefficientGaussLabel;
        private System.Windows.Forms.GroupBox RndGroupBox;
        private System.Windows.Forms.TextBox MaxTextBox;
        private System.Windows.Forms.Label MaxLabel;
        private System.Windows.Forms.TextBox MinTextBox;
        private System.Windows.Forms.Label MinLabel;
        private System.Windows.Forms.TextBox CoeffRTextBox;
        private System.Windows.Forms.Label CoeffRandomLabel;
        private System.Windows.Forms.Button CancllButton;
        private System.Windows.Forms.Button OkButton;
        private System.Windows.Forms.GroupBox VectorGroupBox;
        private System.Windows.Forms.Button RateRefreshButton;
        private System.Windows.Forms.TextBox RateVectorTextBox;
        private System.Windows.Forms.Label StepLabel;
        private System.Windows.Forms.TextBox StepTextBox;
        private System.Windows.Forms.Label TermCoeffLabel;
        private System.Windows.Forms.TextBox TermCoeffTextBox;
        private System.Windows.Forms.Label FrmErrLabel;
        private System.Windows.Forms.Label VectorLabel;
        private System.Windows.Forms.Label MaxRateLabel;
        private System.Windows.Forms.TextBox MaxVctrTextBox;
    }
}