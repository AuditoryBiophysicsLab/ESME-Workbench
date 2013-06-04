namespace MBSGUI
{
    partial class FormTravel
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
            this.rwCoefflabel = new System.Windows.Forms.Label();
            this.rwCoeffTextBox = new System.Windows.Forms.TextBox();
            this.crwCoeffTextBox = new System.Windows.Forms.TextBox();
            this.crwCoeffLabel = new System.Windows.Forms.Label();
            this.crwPertTextBox = new System.Windows.Forms.TextBox();
            this.crwPertlabel = new System.Windows.Forms.Label();
            this.crwdbPertLabel = new System.Windows.Forms.Label();
            this.crwdbPertTextBox = new System.Windows.Forms.TextBox();
            this.crwdbDirOfBiasLabel = new System.Windows.Forms.Label();
            this.crwdbDirOfBiasTextBox = new System.Windows.Forms.TextBox();
            this.crwdbBiasLabel = new System.Windows.Forms.Label();
            this.crwdbBiasTextBox = new System.Windows.Forms.TextBox();
            this.crwdbArcStepLabel = new System.Windows.Forms.Label();
            this.crwdbArcStepTextBox = new System.Windows.Forms.TextBox();
            this.crwdbCoeffLabel = new System.Windows.Forms.Label();
            this.crwdbCoeffTextBox = new System.Windows.Forms.TextBox();
            this.CrRndWlkDbGroupBox = new System.Windows.Forms.GroupBox();
            this.RndWlkGroupBox = new System.Windows.Forms.GroupBox();
            this.CrRndWlkGroupBox = new System.Windows.Forms.GroupBox();
            this.CanclButton = new System.Windows.Forms.Button();
            this.OkButton = new System.Windows.Forms.Button();
            this.ProbTurnVectorTextBox = new System.Windows.Forms.TextBox();
            this.VectorGroupBox = new System.Windows.Forms.GroupBox();
            this.label1 = new System.Windows.Forms.Label();
            this.TermCoeffTextBox = new System.Windows.Forms.TextBox();
            this.BiasFormatErrorLabel = new System.Windows.Forms.Label();
            this.ProbTurningFrmErrLabel = new System.Windows.Forms.Label();
            this.BiasLabel = new System.Windows.Forms.Label();
            this.BiasTextBox = new System.Windows.Forms.TextBox();
            this.ProbOfTurningLabel = new System.Windows.Forms.Label();
            this.RefreshButton = new System.Windows.Forms.Button();
            this.CrRndWlkDbGroupBox.SuspendLayout();
            this.RndWlkGroupBox.SuspendLayout();
            this.CrRndWlkGroupBox.SuspendLayout();
            this.VectorGroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // rwCoefflabel
            // 
            this.rwCoefflabel.AutoSize = true;
            this.rwCoefflabel.Location = new System.Drawing.Point(41, 28);
            this.rwCoefflabel.Name = "rwCoefflabel";
            this.rwCoefflabel.Size = new System.Drawing.Size(113, 13);
            this.rwCoefflabel.TabIndex = 0;
            this.rwCoefflabel.Text = "termination coefficient:";
            // 
            // rwCoeffTextBox
            // 
            this.rwCoeffTextBox.Location = new System.Drawing.Point(164, 25);
            this.rwCoeffTextBox.Name = "rwCoeffTextBox";
            this.rwCoeffTextBox.Size = new System.Drawing.Size(47, 20);
            this.rwCoeffTextBox.TabIndex = 0;
            this.rwCoeffTextBox.TextChanged += new System.EventHandler(this.rwCoeffTextBox_TextChanged);
            // 
            // crwCoeffTextBox
            // 
            this.crwCoeffTextBox.Location = new System.Drawing.Point(164, 52);
            this.crwCoeffTextBox.Name = "crwCoeffTextBox";
            this.crwCoeffTextBox.Size = new System.Drawing.Size(47, 20);
            this.crwCoeffTextBox.TabIndex = 1;
            this.crwCoeffTextBox.TextChanged += new System.EventHandler(this.crwCoeffTextBox_TextChanged);
            // 
            // crwCoeffLabel
            // 
            this.crwCoeffLabel.AutoSize = true;
            this.crwCoeffLabel.Location = new System.Drawing.Point(45, 56);
            this.crwCoeffLabel.Name = "crwCoeffLabel";
            this.crwCoeffLabel.Size = new System.Drawing.Size(113, 13);
            this.crwCoeffLabel.TabIndex = 2;
            this.crwCoeffLabel.Text = "termination coefficient:";
            // 
            // crwPertTextBox
            // 
            this.crwPertTextBox.Location = new System.Drawing.Point(164, 26);
            this.crwPertTextBox.Name = "crwPertTextBox";
            this.crwPertTextBox.Size = new System.Drawing.Size(47, 20);
            this.crwPertTextBox.TabIndex = 0;
            this.crwPertTextBox.TextChanged += new System.EventHandler(this.crwPertTextBox_TextChanged);
            // 
            // crwPertlabel
            // 
            this.crwPertlabel.AutoSize = true;
            this.crwPertlabel.Location = new System.Drawing.Point(94, 29);
            this.crwPertlabel.Name = "crwPertlabel";
            this.crwPertlabel.Size = new System.Drawing.Size(66, 13);
            this.crwPertlabel.TabIndex = 5;
            this.crwPertlabel.Text = "perturbation:";
            // 
            // crwdbPertLabel
            // 
            this.crwdbPertLabel.AutoSize = true;
            this.crwdbPertLabel.Location = new System.Drawing.Point(85, 31);
            this.crwdbPertLabel.Name = "crwdbPertLabel";
            this.crwdbPertLabel.Size = new System.Drawing.Size(66, 13);
            this.crwdbPertLabel.TabIndex = 7;
            this.crwdbPertLabel.Text = "perturbation:";
            // 
            // crwdbPertTextBox
            // 
            this.crwdbPertTextBox.Location = new System.Drawing.Point(155, 28);
            this.crwdbPertTextBox.Name = "crwdbPertTextBox";
            this.crwdbPertTextBox.Size = new System.Drawing.Size(47, 20);
            this.crwdbPertTextBox.TabIndex = 0;
            this.crwdbPertTextBox.TextChanged += new System.EventHandler(this.crwdbPertTextBox_TextChanged);
            // 
            // crwdbDirOfBiasLabel
            // 
            this.crwdbDirOfBiasLabel.AutoSize = true;
            this.crwdbDirOfBiasLabel.Location = new System.Drawing.Point(71, 57);
            this.crwdbDirOfBiasLabel.Name = "crwdbDirOfBiasLabel";
            this.crwdbDirOfBiasLabel.Size = new System.Drawing.Size(80, 13);
            this.crwdbDirOfBiasLabel.TabIndex = 9;
            this.crwdbDirOfBiasLabel.Text = "directional bias:";
            // 
            // crwdbDirOfBiasTextBox
            // 
            this.crwdbDirOfBiasTextBox.Location = new System.Drawing.Point(155, 54);
            this.crwdbDirOfBiasTextBox.Name = "crwdbDirOfBiasTextBox";
            this.crwdbDirOfBiasTextBox.Size = new System.Drawing.Size(47, 20);
            this.crwdbDirOfBiasTextBox.TabIndex = 1;
            this.crwdbDirOfBiasTextBox.TextChanged += new System.EventHandler(this.crwdbDirOfBiasTextBox_TextChanged);
            // 
            // crwdbBiasLabel
            // 
            this.crwdbBiasLabel.AutoSize = true;
            this.crwdbBiasLabel.Location = new System.Drawing.Point(125, 83);
            this.crwdbBiasLabel.Name = "crwdbBiasLabel";
            this.crwdbBiasLabel.Size = new System.Drawing.Size(26, 13);
            this.crwdbBiasLabel.TabIndex = 11;
            this.crwdbBiasLabel.Text = "bias";
            // 
            // crwdbBiasTextBox
            // 
            this.crwdbBiasTextBox.Location = new System.Drawing.Point(155, 80);
            this.crwdbBiasTextBox.Name = "crwdbBiasTextBox";
            this.crwdbBiasTextBox.Size = new System.Drawing.Size(47, 20);
            this.crwdbBiasTextBox.TabIndex = 2;
            this.crwdbBiasTextBox.TextChanged += new System.EventHandler(this.crwdbBiasTextBox_TextChanged);
            // 
            // crwdbArcStepLabel
            // 
            this.crwdbArcStepLabel.AutoSize = true;
            this.crwdbArcStepLabel.Location = new System.Drawing.Point(106, 109);
            this.crwdbArcStepLabel.Name = "crwdbArcStepLabel";
            this.crwdbArcStepLabel.Size = new System.Drawing.Size(45, 13);
            this.crwdbArcStepLabel.TabIndex = 13;
            this.crwdbArcStepLabel.Text = "arc step";
            // 
            // crwdbArcStepTextBox
            // 
            this.crwdbArcStepTextBox.Location = new System.Drawing.Point(155, 106);
            this.crwdbArcStepTextBox.Name = "crwdbArcStepTextBox";
            this.crwdbArcStepTextBox.Size = new System.Drawing.Size(47, 20);
            this.crwdbArcStepTextBox.TabIndex = 3;
            this.crwdbArcStepTextBox.TextChanged += new System.EventHandler(this.crwdbArcStepTextBox_TextChanged);
            // 
            // crwdbCoeffLabel
            // 
            this.crwdbCoeffLabel.AutoSize = true;
            this.crwdbCoeffLabel.Location = new System.Drawing.Point(38, 135);
            this.crwdbCoeffLabel.Name = "crwdbCoeffLabel";
            this.crwdbCoeffLabel.Size = new System.Drawing.Size(113, 13);
            this.crwdbCoeffLabel.TabIndex = 15;
            this.crwdbCoeffLabel.Text = "termination coefficient:";
            // 
            // crwdbCoeffTextBox
            // 
            this.crwdbCoeffTextBox.Location = new System.Drawing.Point(155, 132);
            this.crwdbCoeffTextBox.Name = "crwdbCoeffTextBox";
            this.crwdbCoeffTextBox.Size = new System.Drawing.Size(47, 20);
            this.crwdbCoeffTextBox.TabIndex = 4;
            this.crwdbCoeffTextBox.TextChanged += new System.EventHandler(this.crwdbCoeffTextBox_TextChanged);
            // 
            // CrRndWlkDbGroupBox
            // 
            this.CrRndWlkDbGroupBox.Controls.Add(this.crwdbPertTextBox);
            this.CrRndWlkDbGroupBox.Controls.Add(this.crwdbCoeffLabel);
            this.CrRndWlkDbGroupBox.Controls.Add(this.crwdbPertLabel);
            this.CrRndWlkDbGroupBox.Controls.Add(this.crwdbCoeffTextBox);
            this.CrRndWlkDbGroupBox.Controls.Add(this.crwdbDirOfBiasTextBox);
            this.CrRndWlkDbGroupBox.Controls.Add(this.crwdbArcStepLabel);
            this.CrRndWlkDbGroupBox.Controls.Add(this.crwdbDirOfBiasLabel);
            this.CrRndWlkDbGroupBox.Controls.Add(this.crwdbArcStepTextBox);
            this.CrRndWlkDbGroupBox.Controls.Add(this.crwdbBiasTextBox);
            this.CrRndWlkDbGroupBox.Controls.Add(this.crwdbBiasLabel);
            this.CrRndWlkDbGroupBox.Location = new System.Drawing.Point(265, 21);
            this.CrRndWlkDbGroupBox.Name = "CrRndWlkDbGroupBox";
            this.CrRndWlkDbGroupBox.Size = new System.Drawing.Size(232, 165);
            this.CrRndWlkDbGroupBox.TabIndex = 18;
            this.CrRndWlkDbGroupBox.TabStop = false;
            this.CrRndWlkDbGroupBox.Text = "Correlated Rand Walk With Directional  Bias";
            // 
            // RndWlkGroupBox
            // 
            this.RndWlkGroupBox.Controls.Add(this.rwCoeffTextBox);
            this.RndWlkGroupBox.Controls.Add(this.rwCoefflabel);
            this.RndWlkGroupBox.Location = new System.Drawing.Point(12, 21);
            this.RndWlkGroupBox.Name = "RndWlkGroupBox";
            this.RndWlkGroupBox.Size = new System.Drawing.Size(232, 63);
            this.RndWlkGroupBox.TabIndex = 16;
            this.RndWlkGroupBox.TabStop = false;
            this.RndWlkGroupBox.Text = "Random Walk";
            // 
            // CrRndWlkGroupBox
            // 
            this.CrRndWlkGroupBox.Controls.Add(this.crwPertTextBox);
            this.CrRndWlkGroupBox.Controls.Add(this.crwCoeffLabel);
            this.CrRndWlkGroupBox.Controls.Add(this.crwCoeffTextBox);
            this.CrRndWlkGroupBox.Controls.Add(this.crwPertlabel);
            this.CrRndWlkGroupBox.Location = new System.Drawing.Point(12, 97);
            this.CrRndWlkGroupBox.Name = "CrRndWlkGroupBox";
            this.CrRndWlkGroupBox.Size = new System.Drawing.Size(232, 89);
            this.CrRndWlkGroupBox.TabIndex = 17;
            this.CrRndWlkGroupBox.TabStop = false;
            this.CrRndWlkGroupBox.Text = "Correlated Random Walk";
            // 
            // CanclButton
            // 
            this.CanclButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.CanclButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.CanclButton.Location = new System.Drawing.Point(365, 534);
            this.CanclButton.Name = "CanclButton";
            this.CanclButton.Size = new System.Drawing.Size(84, 40);
            this.CanclButton.TabIndex = 20;
            this.CanclButton.Text = "Cancel";
            this.CanclButton.UseVisualStyleBackColor = true;
            this.CanclButton.Click += new System.EventHandler(this.CancelButton_Click);
            // 
            // OkButton
            // 
            this.OkButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.OkButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.OkButton.Location = new System.Drawing.Point(545, 534);
            this.OkButton.Name = "OkButton";
            this.OkButton.Size = new System.Drawing.Size(84, 40);
            this.OkButton.TabIndex = 19;
            this.OkButton.Text = "Accept Changes";
            this.OkButton.UseVisualStyleBackColor = true;
            this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
            // 
            // ProbTurnVectorTextBox
            // 
            this.ProbTurnVectorTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.ProbTurnVectorTextBox.Location = new System.Drawing.Point(6, 41);
            this.ProbTurnVectorTextBox.Name = "ProbTurnVectorTextBox";
            this.ProbTurnVectorTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Horizontal;
            this.ProbTurnVectorTextBox.Size = new System.Drawing.Size(593, 20);
            this.ProbTurnVectorTextBox.TabIndex = 0;
            this.ProbTurnVectorTextBox.TextChanged += new System.EventHandler(this.ProbTurnVectorTextBox_TextChanged);
            // 
            // VectorGroupBox
            // 
            this.VectorGroupBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.VectorGroupBox.Controls.Add(this.label1);
            this.VectorGroupBox.Controls.Add(this.TermCoeffTextBox);
            this.VectorGroupBox.Controls.Add(this.BiasFormatErrorLabel);
            this.VectorGroupBox.Controls.Add(this.ProbTurningFrmErrLabel);
            this.VectorGroupBox.Controls.Add(this.BiasLabel);
            this.VectorGroupBox.Controls.Add(this.BiasTextBox);
            this.VectorGroupBox.Controls.Add(this.ProbOfTurningLabel);
            this.VectorGroupBox.Controls.Add(this.ProbTurnVectorTextBox);
            this.VectorGroupBox.Location = new System.Drawing.Point(12, 192);
            this.VectorGroupBox.MinimumSize = new System.Drawing.Size(615, 336);
            this.VectorGroupBox.Name = "VectorGroupBox";
            this.VectorGroupBox.Size = new System.Drawing.Size(617, 336);
            this.VectorGroupBox.TabIndex = 24;
            this.VectorGroupBox.TabStop = false;
            this.VectorGroupBox.Text = "Vector";
            // 
            // label1
            // 
            this.label1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(7, 305);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(113, 13);
            this.label1.TabIndex = 9;
            this.label1.Text = "termination coefficient:";
            // 
            // TermCoeffTextBox
            // 
            this.TermCoeffTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TermCoeffTextBox.Location = new System.Drawing.Point(123, 302);
            this.TermCoeffTextBox.Name = "TermCoeffTextBox";
            this.TermCoeffTextBox.Size = new System.Drawing.Size(37, 20);
            this.TermCoeffTextBox.TabIndex = 1;
            this.TermCoeffTextBox.TextChanged += new System.EventHandler(this.TermCoeffTextBox_TextChanged);
            // 
            // BiasFormatErrorLabel
            // 
            this.BiasFormatErrorLabel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.BiasFormatErrorLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.BiasFormatErrorLabel.Location = new System.Drawing.Point(9, 275);
            this.BiasFormatErrorLabel.Name = "BiasFormatErrorLabel";
            this.BiasFormatErrorLabel.Size = new System.Drawing.Size(63, 17);
            this.BiasFormatErrorLabel.TabIndex = 7;
            this.BiasFormatErrorLabel.Text = "Format Error";
            this.BiasFormatErrorLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // ProbTurningFrmErrLabel
            // 
            this.ProbTurningFrmErrLabel.BackColor = System.Drawing.SystemColors.Control;
            this.ProbTurningFrmErrLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.ProbTurningFrmErrLabel.Location = new System.Drawing.Point(7, 66);
            this.ProbTurningFrmErrLabel.Name = "ProbTurningFrmErrLabel";
            this.ProbTurningFrmErrLabel.Size = new System.Drawing.Size(63, 17);
            this.ProbTurningFrmErrLabel.TabIndex = 6;
            this.ProbTurningFrmErrLabel.Text = "Format Error";
            this.ProbTurningFrmErrLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // BiasLabel
            // 
            this.BiasLabel.AutoSize = true;
            this.BiasLabel.Location = new System.Drawing.Point(6, 101);
            this.BiasLabel.Name = "BiasLabel";
            this.BiasLabel.Size = new System.Drawing.Size(216, 13);
            this.BiasLabel.TabIndex = 3;
            this.BiasLabel.Text = "Probability of Turning Bias Matrix,  [200x200]";
            // 
            // BiasTextBox
            // 
            this.BiasTextBox.AcceptsReturn = true;
            this.BiasTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.BiasTextBox.Location = new System.Drawing.Point(6, 117);
            this.BiasTextBox.Multiline = true;
            this.BiasTextBox.Name = "BiasTextBox";
            this.BiasTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.BiasTextBox.Size = new System.Drawing.Size(593, 144);
            this.BiasTextBox.TabIndex = 2;
            this.BiasTextBox.WordWrap = false;
            this.BiasTextBox.TextChanged += new System.EventHandler(this.BiasTextBox_TextChanged);
            // 
            // ProbOfTurningLabel
            // 
            this.ProbOfTurningLabel.AutoSize = true;
            this.ProbOfTurningLabel.Location = new System.Drawing.Point(6, 23);
            this.ProbOfTurningLabel.Name = "ProbOfTurningLabel";
            this.ProbOfTurningLabel.Size = new System.Drawing.Size(150, 13);
            this.ProbOfTurningLabel.TabIndex = 1;
            this.ProbOfTurningLabel.Text = "Probability of Turning,  [1x200]";
            // 
            // RefreshButton
            // 
            this.RefreshButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.RefreshButton.Location = new System.Drawing.Point(455, 534);
            this.RefreshButton.Name = "RefreshButton";
            this.RefreshButton.Size = new System.Drawing.Size(84, 40);
            this.RefreshButton.TabIndex = 12;
            this.RefreshButton.Text = "Refresh";
            this.RefreshButton.UseVisualStyleBackColor = true;
            this.RefreshButton.Click += new System.EventHandler(this.RefreshButton_Click);
            // 
            // FormTravel
            // 
            this.AcceptButton = this.OkButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(636, 586);
            this.Controls.Add(this.RefreshButton);
            this.Controls.Add(this.VectorGroupBox);
            this.Controls.Add(this.CanclButton);
            this.Controls.Add(this.CrRndWlkGroupBox);
            this.Controls.Add(this.RndWlkGroupBox);
            this.Controls.Add(this.OkButton);
            this.Controls.Add(this.CrRndWlkDbGroupBox);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.MinimumSize = new System.Drawing.Size(642, 599);
            this.Name = "FormTravel";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "3MB Travel Direction Description - Biomimetica";
            this.CrRndWlkDbGroupBox.ResumeLayout(false);
            this.CrRndWlkDbGroupBox.PerformLayout();
            this.RndWlkGroupBox.ResumeLayout(false);
            this.RndWlkGroupBox.PerformLayout();
            this.CrRndWlkGroupBox.ResumeLayout(false);
            this.CrRndWlkGroupBox.PerformLayout();
            this.VectorGroupBox.ResumeLayout(false);
            this.VectorGroupBox.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Label rwCoefflabel;
        private System.Windows.Forms.TextBox rwCoeffTextBox;
        private System.Windows.Forms.TextBox crwCoeffTextBox;
        private System.Windows.Forms.Label crwCoeffLabel;
        private System.Windows.Forms.TextBox crwPertTextBox;
        private System.Windows.Forms.Label crwPertlabel;
        private System.Windows.Forms.Label crwdbPertLabel;
        private System.Windows.Forms.TextBox crwdbPertTextBox;
        private System.Windows.Forms.Label crwdbDirOfBiasLabel;
        private System.Windows.Forms.TextBox crwdbDirOfBiasTextBox;
        private System.Windows.Forms.Label crwdbBiasLabel;
        private System.Windows.Forms.TextBox crwdbBiasTextBox;
        private System.Windows.Forms.Label crwdbArcStepLabel;
        private System.Windows.Forms.TextBox crwdbArcStepTextBox;
        private System.Windows.Forms.Label crwdbCoeffLabel;
        private System.Windows.Forms.TextBox crwdbCoeffTextBox;
        private System.Windows.Forms.GroupBox CrRndWlkDbGroupBox;
        private System.Windows.Forms.GroupBox RndWlkGroupBox;
        private System.Windows.Forms.GroupBox CrRndWlkGroupBox;
        private System.Windows.Forms.Button CanclButton;
        private System.Windows.Forms.Button OkButton;
        private System.Windows.Forms.TextBox ProbTurnVectorTextBox;
        private System.Windows.Forms.GroupBox VectorGroupBox;
        private System.Windows.Forms.Label BiasLabel;
        private System.Windows.Forms.TextBox BiasTextBox;
        private System.Windows.Forms.Label ProbOfTurningLabel;
        private System.Windows.Forms.Label BiasFormatErrorLabel;
        private System.Windows.Forms.Label ProbTurningFrmErrLabel;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox TermCoeffTextBox;
        private System.Windows.Forms.Button RefreshButton;
    }
}