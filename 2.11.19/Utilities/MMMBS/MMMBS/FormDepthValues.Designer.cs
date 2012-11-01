namespace MBSGUI
{
    partial class FormDepthValues
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
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.DeepSeedingEnableButton = new System.Windows.Forms.Button();
            this.DeepSeedingDisabledLabel = new System.Windows.Forms.Label();
            this.ShoreFollowingTextBox = new System.Windows.Forms.TextBox();
            this.ShallowSeedingTextBox = new System.Windows.Forms.TextBox();
            this.DeepSeedingTextBox = new System.Windows.Forms.TextBox();
            this.WarningLabel = new System.Windows.Forms.Label();
            this.CancelC = new System.Windows.Forms.Button();
            this.DeepSeedingViolationLabel = new System.Windows.Forms.Label();
            this.ShallowSeedingViolationLabel = new System.Windows.Forms.Label();
            this.ShoreFollowingViolationLabel = new System.Windows.Forms.Label();
            this.OkButton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(148, 15);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(187, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "Bathymetry Shore Following Value (m):";
            this.label1.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(185, 47);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(150, 13);
            this.label2.TabIndex = 1;
            this.label2.Text = "Seeding Depth - Mimimum (m):";
            this.label2.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // DeepSeedingEnableButton
            // 
            this.DeepSeedingEnableButton.FlatAppearance.MouseDownBackColor = System.Drawing.Color.IndianRed;
            this.DeepSeedingEnableButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.RosyBrown;
            this.DeepSeedingEnableButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.DeepSeedingEnableButton.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.DeepSeedingEnableButton.Location = new System.Drawing.Point(164, 75);
            this.DeepSeedingEnableButton.Name = "DeepSeedingEnableButton";
            this.DeepSeedingEnableButton.Size = new System.Drawing.Size(171, 25);
            this.DeepSeedingEnableButton.TabIndex = 266;
            this.DeepSeedingEnableButton.Text = "Seeding Depth - Maximum (m):";
            this.DeepSeedingEnableButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.DeepSeedingEnableButton.UseVisualStyleBackColor = true;
            this.DeepSeedingEnableButton.Click += new System.EventHandler(this.DeepSeedingEnableButton_Click);
            // 
            // DeepSeedingDisabledLabel
            // 
            this.DeepSeedingDisabledLabel.AutoSize = true;
            this.DeepSeedingDisabledLabel.Location = new System.Drawing.Point(434, 81);
            this.DeepSeedingDisabledLabel.Name = "DeepSeedingDisabledLabel";
            this.DeepSeedingDisabledLabel.Size = new System.Drawing.Size(52, 13);
            this.DeepSeedingDisabledLabel.TabIndex = 267;
            this.DeepSeedingDisabledLabel.Text = "(disabled)";
            // 
            // ShoreFollowingTextBox
            // 
            this.ShoreFollowingTextBox.Location = new System.Drawing.Point(341, 12);
            this.ShoreFollowingTextBox.Name = "ShoreFollowingTextBox";
            this.ShoreFollowingTextBox.Size = new System.Drawing.Size(62, 20);
            this.ShoreFollowingTextBox.TabIndex = 268;
            this.ShoreFollowingTextBox.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.ShoreFollowingTextBox.TextChanged += new System.EventHandler(this.ShoreFollowingTextBox_TextChanged);
            // 
            // ShallowSeedingTextBox
            // 
            this.ShallowSeedingTextBox.Location = new System.Drawing.Point(341, 44);
            this.ShallowSeedingTextBox.Name = "ShallowSeedingTextBox";
            this.ShallowSeedingTextBox.Size = new System.Drawing.Size(62, 20);
            this.ShallowSeedingTextBox.TabIndex = 269;
            this.ShallowSeedingTextBox.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.ShallowSeedingTextBox.TextChanged += new System.EventHandler(this.ShallowSeedingTextBox_TextChanged);
            // 
            // DeepSeedingTextBox
            // 
            this.DeepSeedingTextBox.Location = new System.Drawing.Point(341, 78);
            this.DeepSeedingTextBox.Name = "DeepSeedingTextBox";
            this.DeepSeedingTextBox.Size = new System.Drawing.Size(62, 20);
            this.DeepSeedingTextBox.TabIndex = 270;
            this.DeepSeedingTextBox.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            this.DeepSeedingTextBox.TextChanged += new System.EventHandler(this.DeepSeedingTextBox_TextChanged);
            // 
            // WarningLabel
            // 
            this.WarningLabel.ForeColor = System.Drawing.Color.DarkRed;
            this.WarningLabel.Location = new System.Drawing.Point(12, 106);
            this.WarningLabel.Name = "WarningLabel";
            this.WarningLabel.Size = new System.Drawing.Size(472, 62);
            this.WarningLabel.TabIndex = 273;
            this.WarningLabel.Text = "Seeding Deep Limit (Disabled)";
            this.WarningLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // CancelC
            // 
            this.CancelC.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.CancelC.Location = new System.Drawing.Point(328, 171);
            this.CancelC.Name = "CancelC";
            this.CancelC.Size = new System.Drawing.Size(75, 26);
            this.CancelC.TabIndex = 272;
            this.CancelC.Text = "Cancel";
            this.CancelC.UseVisualStyleBackColor = true;
            this.CancelC.Click += new System.EventHandler(this.CancelC_Click);
            // 
            // DeepSeedingViolationLabel
            // 
            this.DeepSeedingViolationLabel.AutoSize = true;
            this.DeepSeedingViolationLabel.ForeColor = System.Drawing.Color.DarkRed;
            this.DeepSeedingViolationLabel.Location = new System.Drawing.Point(409, 81);
            this.DeepSeedingViolationLabel.Name = "DeepSeedingViolationLabel";
            this.DeepSeedingViolationLabel.Size = new System.Drawing.Size(19, 13);
            this.DeepSeedingViolationLabel.TabIndex = 274;
            this.DeepSeedingViolationLabel.Text = "!  !";
            // 
            // ShallowSeedingViolationLabel
            // 
            this.ShallowSeedingViolationLabel.AutoSize = true;
            this.ShallowSeedingViolationLabel.ForeColor = System.Drawing.Color.DarkRed;
            this.ShallowSeedingViolationLabel.Location = new System.Drawing.Point(409, 47);
            this.ShallowSeedingViolationLabel.Name = "ShallowSeedingViolationLabel";
            this.ShallowSeedingViolationLabel.Size = new System.Drawing.Size(19, 13);
            this.ShallowSeedingViolationLabel.TabIndex = 275;
            this.ShallowSeedingViolationLabel.Text = "!  !";
            // 
            // ShoreFollowingViolationLabel
            // 
            this.ShoreFollowingViolationLabel.AutoSize = true;
            this.ShoreFollowingViolationLabel.ForeColor = System.Drawing.Color.DarkRed;
            this.ShoreFollowingViolationLabel.Location = new System.Drawing.Point(409, 15);
            this.ShoreFollowingViolationLabel.Name = "ShoreFollowingViolationLabel";
            this.ShoreFollowingViolationLabel.Size = new System.Drawing.Size(19, 13);
            this.ShoreFollowingViolationLabel.TabIndex = 276;
            this.ShoreFollowingViolationLabel.Text = "!  !";
            // 
            // OkButton
            // 
            this.OkButton.Location = new System.Drawing.Point(409, 171);
            this.OkButton.Name = "OkButton";
            this.OkButton.Size = new System.Drawing.Size(75, 26);
            this.OkButton.TabIndex = 271;
            this.OkButton.Text = "OK";
            this.OkButton.UseVisualStyleBackColor = true;
            this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
            // 
            // FormDepthValues
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(505, 209);
            this.Controls.Add(this.ShoreFollowingViolationLabel);
            this.Controls.Add(this.ShallowSeedingViolationLabel);
            this.Controls.Add(this.DeepSeedingViolationLabel);
            this.Controls.Add(this.WarningLabel);
            this.Controls.Add(this.CancelC);
            this.Controls.Add(this.OkButton);
            this.Controls.Add(this.DeepSeedingTextBox);
            this.Controls.Add(this.ShallowSeedingTextBox);
            this.Controls.Add(this.ShoreFollowingTextBox);
            this.Controls.Add(this.DeepSeedingDisabledLabel);
            this.Controls.Add(this.DeepSeedingEnableButton);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Name = "FormDepthValues";
            this.ShowInTaskbar = false;
            this.Text = "FormDepthValues";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Button DeepSeedingEnableButton;
        private System.Windows.Forms.Label DeepSeedingDisabledLabel;
        private System.Windows.Forms.TextBox ShoreFollowingTextBox;
        private System.Windows.Forms.TextBox ShallowSeedingTextBox;
        private System.Windows.Forms.TextBox DeepSeedingTextBox;
        private System.Windows.Forms.Label WarningLabel;
        private System.Windows.Forms.Button CancelC;
        private System.Windows.Forms.Label DeepSeedingViolationLabel;
        private System.Windows.Forms.Label ShallowSeedingViolationLabel;
        private System.Windows.Forms.Label ShoreFollowingViolationLabel;
        private System.Windows.Forms.Button OkButton;
    }
}