namespace MBSGUI
{
    partial class FormGaussParams
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
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.MeanTextBox = new System.Windows.Forms.TextBox();
            this.MeanLabel = new System.Windows.Forms.Label();
            this.StdTextBox = new System.Windows.Forms.TextBox();
            this.StdLabel = new System.Windows.Forms.Label();
            this.CoeffGTextBox = new System.Windows.Forms.TextBox();
            this.CoefficientGaussLabel = new System.Windows.Forms.Label();
            this.CanclButton = new System.Windows.Forms.Button();
            this.OkButton = new System.Windows.Forms.Button();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.MeanTextBox);
            this.groupBox1.Controls.Add(this.MeanLabel);
            this.groupBox1.Controls.Add(this.StdTextBox);
            this.groupBox1.Controls.Add(this.StdLabel);
            this.groupBox1.Controls.Add(this.CoeffGTextBox);
            this.groupBox1.Controls.Add(this.CoefficientGaussLabel);
            this.groupBox1.Location = new System.Drawing.Point(12, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(205, 105);
            this.groupBox1.TabIndex = 18;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Gaussian";
            // 
            // MeanTextBox
            // 
            this.MeanTextBox.Location = new System.Drawing.Point(151, 19);
            this.MeanTextBox.Name = "MeanTextBox";
            this.MeanTextBox.Size = new System.Drawing.Size(48, 20);
            this.MeanTextBox.TabIndex = 4;
            this.MeanTextBox.TextChanged += new System.EventHandler(this.MeanTextBox_TextChanged);
            // 
            // MeanLabel
            // 
            this.MeanLabel.Location = new System.Drawing.Point(6, 22);
            this.MeanLabel.Name = "MeanLabel";
            this.MeanLabel.Size = new System.Drawing.Size(139, 13);
            this.MeanLabel.TabIndex = 7;
            this.MeanLabel.Text = "mean:";
            this.MeanLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // StdTextBox
            // 
            this.StdTextBox.Location = new System.Drawing.Point(151, 45);
            this.StdTextBox.Name = "StdTextBox";
            this.StdTextBox.Size = new System.Drawing.Size(48, 20);
            this.StdTextBox.TabIndex = 5;
            this.StdTextBox.TextChanged += new System.EventHandler(this.StdTextBox_TextChanged);
            // 
            // StdLabel
            // 
            this.StdLabel.Location = new System.Drawing.Point(9, 48);
            this.StdLabel.Name = "StdLabel";
            this.StdLabel.Size = new System.Drawing.Size(136, 13);
            this.StdLabel.TabIndex = 9;
            this.StdLabel.Text = "std:";
            this.StdLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // CoeffGTextBox
            // 
            this.CoeffGTextBox.Location = new System.Drawing.Point(151, 71);
            this.CoeffGTextBox.Name = "CoeffGTextBox";
            this.CoeffGTextBox.Size = new System.Drawing.Size(48, 20);
            this.CoeffGTextBox.TabIndex = 6;
            this.CoeffGTextBox.TextChanged += new System.EventHandler(this.CoeffGTextBox_TextChanged);
            // 
            // CoefficientGaussLabel
            // 
            this.CoefficientGaussLabel.Location = new System.Drawing.Point(9, 74);
            this.CoefficientGaussLabel.Name = "CoefficientGaussLabel";
            this.CoefficientGaussLabel.Size = new System.Drawing.Size(136, 13);
            this.CoefficientGaussLabel.TabIndex = 11;
            this.CoefficientGaussLabel.Text = "termination coefficient:";
            this.CoefficientGaussLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // CanclButton
            // 
            this.CanclButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.CanclButton.Location = new System.Drawing.Point(55, 123);
            this.CanclButton.Name = "CanclButton";
            this.CanclButton.Size = new System.Drawing.Size(75, 26);
            this.CanclButton.TabIndex = 24;
            this.CanclButton.Text = "Cancel";
            this.CanclButton.UseVisualStyleBackColor = true;
            this.CanclButton.Click += new System.EventHandler(this.CancelButton_Click);
            // 
            // OkButton
            // 
            this.OkButton.Location = new System.Drawing.Point(136, 123);
            this.OkButton.Name = "OkButton";
            this.OkButton.Size = new System.Drawing.Size(75, 26);
            this.OkButton.TabIndex = 23;
            this.OkButton.Text = "OK";
            this.OkButton.UseVisualStyleBackColor = true;
            this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
            // 
            // FormGaussParams
            // 
            this.AcceptButton = this.OkButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(234, 160);
            this.Controls.Add(this.CanclButton);
            this.Controls.Add(this.OkButton);
            this.Controls.Add(this.groupBox1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "FormGaussParams";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.Text = "3MB Gaussian Entry";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.TextBox MeanTextBox;
        private System.Windows.Forms.Label MeanLabel;
        private System.Windows.Forms.TextBox StdTextBox;
        private System.Windows.Forms.Label StdLabel;
        private System.Windows.Forms.TextBox CoeffGTextBox;
        private System.Windows.Forms.Label CoefficientGaussLabel;
        private System.Windows.Forms.Button CanclButton;
        private System.Windows.Forms.Button OkButton;
    }
}