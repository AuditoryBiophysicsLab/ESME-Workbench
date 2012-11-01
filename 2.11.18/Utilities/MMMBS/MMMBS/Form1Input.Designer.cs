namespace MBSGUI
{
    partial class Form1Input
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
            this.CancelC = new System.Windows.Forms.Button();
            this.OkButton = new System.Windows.Forms.Button();
            this.InputTextBox = new System.Windows.Forms.TextBox();
            this.InputLabel = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // CancelC
            // 
            this.CancelC.Location = new System.Drawing.Point(74, 54);
            this.CancelC.Name = "CancelC";
            this.CancelC.Size = new System.Drawing.Size(75, 26);
            this.CancelC.TabIndex = 5;
            this.CancelC.Text = "Cancel";
            this.CancelC.UseVisualStyleBackColor = true;
            this.CancelC.Click += new System.EventHandler(this.CancelC_Click);
            // 
            // OkButton
            // 
            this.OkButton.Location = new System.Drawing.Point(155, 54);
            this.OkButton.Name = "OkButton";
            this.OkButton.Size = new System.Drawing.Size(75, 26);
            this.OkButton.TabIndex = 4;
            this.OkButton.Text = "OK";
            this.OkButton.UseVisualStyleBackColor = true;
            this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
            // 
            // InputTextBox
            // 
            this.InputTextBox.Location = new System.Drawing.Point(155, 12);
            this.InputTextBox.Name = "InputTextBox";
            this.InputTextBox.Size = new System.Drawing.Size(75, 20);
            this.InputTextBox.TabIndex = 3;
            this.InputTextBox.TextChanged += new System.EventHandler(this.InputTextBox_TextChanged);
            // 
            // InputLabel
            // 
            this.InputLabel.Location = new System.Drawing.Point(12, 10);
            this.InputLabel.Name = "InputLabel";
            this.InputLabel.Size = new System.Drawing.Size(137, 23);
            this.InputLabel.TabIndex = 6;
            this.InputLabel.Text = "Directional Bias";
            this.InputLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // Form1Input
            // 
            this.AcceptButton = this.OkButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.CancelC;
            this.ClientSize = new System.Drawing.Size(242, 92);
            this.Controls.Add(this.InputLabel);
            this.Controls.Add(this.CancelC);
            this.Controls.Add(this.OkButton);
            this.Controls.Add(this.InputTextBox);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.Name = "Form1Input";
            this.Text = "FormSingleInput";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button CancelC;
        private System.Windows.Forms.Button OkButton;
        private System.Windows.Forms.TextBox InputTextBox;
        private System.Windows.Forms.Label InputLabel;
    }
}