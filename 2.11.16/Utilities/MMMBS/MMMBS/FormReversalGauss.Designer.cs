namespace MBSGUI
{
    partial class FormReversalGauss
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
            this.label8 = new System.Windows.Forms.Label();
            this.label7 = new System.Windows.Forms.Label();
            this.MeanCntTextBox = new System.Windows.Forms.TextBox();
            this.label6 = new System.Windows.Forms.Label();
            this.label9 = new System.Windows.Forms.Label();
            this.GaussStdTimeTextBox = new System.Windows.Forms.TextBox();
            this.StdCntTextBox = new System.Windows.Forms.TextBox();
            this.label10 = new System.Windows.Forms.Label();
            this.label11 = new System.Windows.Forms.Label();
            this.GaussProbTextBox = new System.Windows.Forms.TextBox();
            this.label12 = new System.Windows.Forms.Label();
            this.GaussMeanTimeTextBox = new System.Windows.Forms.TextBox();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // CanclButton
            // 
            this.CanclButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.CanclButton.Location = new System.Drawing.Point(72, 218);
            this.CanclButton.Name = "CanclButton";
            this.CanclButton.Size = new System.Drawing.Size(75, 26);
            this.CanclButton.TabIndex = 26;
            this.CanclButton.Text = "Cancel";
            this.CanclButton.UseVisualStyleBackColor = true;
            this.CanclButton.Click += new System.EventHandler(this.CancelButton_Click);
            // 
            // OkButton
            // 
            this.OkButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.OkButton.Location = new System.Drawing.Point(153, 218);
            this.OkButton.Name = "OkButton";
            this.OkButton.Size = new System.Drawing.Size(75, 26);
            this.OkButton.TabIndex = 24;
            this.OkButton.Text = "OK";
            this.OkButton.UseVisualStyleBackColor = true;
            this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.label8);
            this.groupBox1.Controls.Add(this.label7);
            this.groupBox1.Controls.Add(this.MeanCntTextBox);
            this.groupBox1.Controls.Add(this.label6);
            this.groupBox1.Controls.Add(this.label9);
            this.groupBox1.Controls.Add(this.GaussStdTimeTextBox);
            this.groupBox1.Controls.Add(this.StdCntTextBox);
            this.groupBox1.Controls.Add(this.label10);
            this.groupBox1.Controls.Add(this.label11);
            this.groupBox1.Controls.Add(this.GaussProbTextBox);
            this.groupBox1.Controls.Add(this.label12);
            this.groupBox1.Controls.Add(this.GaussMeanTimeTextBox);
            this.groupBox1.Location = new System.Drawing.Point(12, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(211, 196);
            this.groupBox1.TabIndex = 27;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Gaussian";
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(6, 135);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(103, 13);
            this.label8.TabIndex = 18;
            this.label8.Text = "Time In Reveral Leg";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(6, 60);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(101, 13);
            this.label7.TabIndex = 17;
            this.label7.Text = "Number of Reverals";
            // 
            // MeanCntTextBox
            // 
            this.MeanCntTextBox.Location = new System.Drawing.Point(156, 72);
            this.MeanCntTextBox.Name = "MeanCntTextBox";
            this.MeanCntTextBox.Size = new System.Drawing.Size(49, 20);
            this.MeanCntTextBox.TabIndex = 0;
            this.MeanCntTextBox.TextChanged += new System.EventHandler(this.MeanCntTextBox_TextChanged);
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(109, 173);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(41, 13);
            this.label6.TabIndex = 15;
            this.label6.Text = "std. (s):";
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(75, 76);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(75, 13);
            this.label9.TabIndex = 7;
            this.label9.Text = "mean  (count):";
            // 
            // GaussStdTimeTextBox
            // 
            this.GaussStdTimeTextBox.Location = new System.Drawing.Point(156, 170);
            this.GaussStdTimeTextBox.Name = "GaussStdTimeTextBox";
            this.GaussStdTimeTextBox.Size = new System.Drawing.Size(49, 20);
            this.GaussStdTimeTextBox.TabIndex = 4;
            this.GaussStdTimeTextBox.TextChanged += new System.EventHandler(this.GaussStdTimeTextBox_TextChanged);
            // 
            // StdCntTextBox
            // 
            this.StdCntTextBox.Location = new System.Drawing.Point(156, 98);
            this.StdCntTextBox.Name = "StdCntTextBox";
            this.StdCntTextBox.Size = new System.Drawing.Size(49, 20);
            this.StdCntTextBox.TabIndex = 1;
            this.StdCntTextBox.TextChanged += new System.EventHandler(this.StdCntTextBox_TextChanged);
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(100, 147);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(50, 13);
            this.label10.TabIndex = 13;
            this.label10.Text = "mean (s):";
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(6, 16);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(167, 13);
            this.label11.TabIndex = 11;
            this.label11.Text = "probability of reversal while diving:";
            // 
            // GaussProbTextBox
            // 
            this.GaussProbTextBox.Location = new System.Drawing.Point(131, 33);
            this.GaussProbTextBox.Name = "GaussProbTextBox";
            this.GaussProbTextBox.Size = new System.Drawing.Size(37, 20);
            this.GaussProbTextBox.TabIndex = 2;
            this.GaussProbTextBox.TextChanged += new System.EventHandler(this.GaussProbTextBox_TextChanged);
            // 
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.Location = new System.Drawing.Point(90, 102);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(60, 13);
            this.label12.TabIndex = 9;
            this.label12.Text = "std (count):";
            // 
            // GaussMeanTimeTextBox
            // 
            this.GaussMeanTimeTextBox.Location = new System.Drawing.Point(156, 144);
            this.GaussMeanTimeTextBox.Name = "GaussMeanTimeTextBox";
            this.GaussMeanTimeTextBox.Size = new System.Drawing.Size(49, 20);
            this.GaussMeanTimeTextBox.TabIndex = 3;
            this.GaussMeanTimeTextBox.TextChanged += new System.EventHandler(this.GaussMeanTimeTextBox_TextChanged);
            // 
            // FormReversalGauss
            // 
            this.AcceptButton = this.OkButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(240, 256);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.CanclButton);
            this.Controls.Add(this.OkButton);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "FormReversalGauss";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.Text = "3MB Gaussian Reversals";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button CanclButton;
        private System.Windows.Forms.Button OkButton;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.TextBox MeanCntTextBox;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.TextBox GaussStdTimeTextBox;
        private System.Windows.Forms.TextBox StdCntTextBox;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.TextBox GaussProbTextBox;
        private System.Windows.Forms.Label label12;
        private System.Windows.Forms.TextBox GaussMeanTimeTextBox;
    }
}