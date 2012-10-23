namespace MBSGUI
{
    partial class FormTravelGauss
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
            this.CanclButton = new System.Windows.Forms.Button();
            this.OkButton = new System.Windows.Forms.Button();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.crwdbPertTextBox = new System.Windows.Forms.TextBox();
            this.crwdbCoeffLabel = new System.Windows.Forms.Label();
            this.crwdbPertLabel = new System.Windows.Forms.Label();
            this.crwdbCoeffTextBox = new System.Windows.Forms.TextBox();
            this.crwdbArcStepLabel = new System.Windows.Forms.Label();
            this.crwdbArcStepTextBox = new System.Windows.Forms.TextBox();
            this.crwdbBiasTextBox = new System.Windows.Forms.TextBox();
            this.crwdbBiasLabel = new System.Windows.Forms.Label();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // CanclButton
            // 
            this.CanclButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.CanclButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.CanclButton.Location = new System.Drawing.Point(89, 168);
            this.CanclButton.Name = "CanclButton";
            this.CanclButton.Size = new System.Drawing.Size(75, 26);
            this.CanclButton.TabIndex = 23;
            this.CanclButton.Text = "Cancel";
            this.CanclButton.UseVisualStyleBackColor = true;
            this.CanclButton.Click += new System.EventHandler(this.CancelButton_Click);
            // 
            // OkButton
            // 
            this.OkButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.OkButton.Location = new System.Drawing.Point(170, 168);
            this.OkButton.Name = "OkButton";
            this.OkButton.Size = new System.Drawing.Size(75, 26);
            this.OkButton.TabIndex = 22;
            this.OkButton.Text = "OK";
            this.OkButton.UseVisualStyleBackColor = true;
            this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.crwdbPertTextBox);
            this.groupBox1.Controls.Add(this.crwdbCoeffLabel);
            this.groupBox1.Controls.Add(this.crwdbPertLabel);
            this.groupBox1.Controls.Add(this.crwdbCoeffTextBox);
            this.groupBox1.Controls.Add(this.crwdbArcStepLabel);
            this.groupBox1.Controls.Add(this.crwdbArcStepTextBox);
            this.groupBox1.Controls.Add(this.crwdbBiasTextBox);
            this.groupBox1.Controls.Add(this.crwdbBiasLabel);
            this.groupBox1.Location = new System.Drawing.Point(12, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(232, 142);
            this.groupBox1.TabIndex = 21;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Correlated Rand Walk With Directional  Bias";
            // 
            // crwdbPertTextBox
            // 
            this.crwdbPertTextBox.Location = new System.Drawing.Point(170, 28);
            this.crwdbPertTextBox.Name = "crwdbPertTextBox";
            this.crwdbPertTextBox.Size = new System.Drawing.Size(46, 20);
            this.crwdbPertTextBox.TabIndex = 0;
            this.crwdbPertTextBox.TextChanged += new System.EventHandler(this.crwdbPertTextBox_TextChanged);
            // 
            // crwdbCoeffLabel
            // 
            this.crwdbCoeffLabel.AutoSize = true;
            this.crwdbCoeffLabel.Location = new System.Drawing.Point(53, 109);
            this.crwdbCoeffLabel.Name = "crwdbCoeffLabel";
            this.crwdbCoeffLabel.Size = new System.Drawing.Size(113, 13);
            this.crwdbCoeffLabel.TabIndex = 15;
            this.crwdbCoeffLabel.Text = "termination coefficient:";
            // 
            // crwdbPertLabel
            // 
            this.crwdbPertLabel.AutoSize = true;
            this.crwdbPertLabel.Location = new System.Drawing.Point(100, 31);
            this.crwdbPertLabel.Name = "crwdbPertLabel";
            this.crwdbPertLabel.Size = new System.Drawing.Size(66, 13);
            this.crwdbPertLabel.TabIndex = 7;
            this.crwdbPertLabel.Text = "perturbation:";
            // 
            // crwdbCoeffTextBox
            // 
            this.crwdbCoeffTextBox.Location = new System.Drawing.Point(170, 106);
            this.crwdbCoeffTextBox.Name = "crwdbCoeffTextBox";
            this.crwdbCoeffTextBox.Size = new System.Drawing.Size(46, 20);
            this.crwdbCoeffTextBox.TabIndex = 4;
            this.crwdbCoeffTextBox.TextChanged += new System.EventHandler(this.crwdbCoeffTextBox_TextChanged);
            // 
            // crwdbArcStepLabel
            // 
            this.crwdbArcStepLabel.AutoSize = true;
            this.crwdbArcStepLabel.Location = new System.Drawing.Point(118, 83);
            this.crwdbArcStepLabel.Name = "crwdbArcStepLabel";
            this.crwdbArcStepLabel.Size = new System.Drawing.Size(48, 13);
            this.crwdbArcStepLabel.TabIndex = 13;
            this.crwdbArcStepLabel.Text = "arc step:";
            // 
            // crwdbArcStepTextBox
            // 
            this.crwdbArcStepTextBox.Location = new System.Drawing.Point(170, 80);
            this.crwdbArcStepTextBox.Name = "crwdbArcStepTextBox";
            this.crwdbArcStepTextBox.Size = new System.Drawing.Size(46, 20);
            this.crwdbArcStepTextBox.TabIndex = 3;
            this.crwdbArcStepTextBox.TextChanged += new System.EventHandler(this.crwdbArcStepTextBox_TextChanged);
            // 
            // crwdbBiasTextBox
            // 
            this.crwdbBiasTextBox.Location = new System.Drawing.Point(170, 54);
            this.crwdbBiasTextBox.Name = "crwdbBiasTextBox";
            this.crwdbBiasTextBox.Size = new System.Drawing.Size(46, 20);
            this.crwdbBiasTextBox.TabIndex = 2;
            this.crwdbBiasTextBox.TextChanged += new System.EventHandler(this.crwdbBiasTextBox_TextChanged);
            // 
            // crwdbBiasLabel
            // 
            this.crwdbBiasLabel.AutoSize = true;
            this.crwdbBiasLabel.Location = new System.Drawing.Point(137, 57);
            this.crwdbBiasLabel.Name = "crwdbBiasLabel";
            this.crwdbBiasLabel.Size = new System.Drawing.Size(29, 13);
            this.crwdbBiasLabel.TabIndex = 11;
            this.crwdbBiasLabel.Text = "bias:";
            // 
            // FormTravelGauss
            // 
            this.AcceptButton = this.OkButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(257, 206);
            this.Controls.Add(this.CanclButton);
            this.Controls.Add(this.OkButton);
            this.Controls.Add(this.groupBox1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "FormTravelGauss";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "AE Gaussian Travel Direction Description*";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button CanclButton;
        private System.Windows.Forms.Button OkButton;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.TextBox crwdbPertTextBox;
        private System.Windows.Forms.Label crwdbCoeffLabel;
        private System.Windows.Forms.Label crwdbPertLabel;
        private System.Windows.Forms.TextBox crwdbCoeffTextBox;
        private System.Windows.Forms.Label crwdbArcStepLabel;
        private System.Windows.Forms.TextBox crwdbArcStepTextBox;
        private System.Windows.Forms.TextBox crwdbBiasTextBox;
        private System.Windows.Forms.Label crwdbBiasLabel;
    }
}