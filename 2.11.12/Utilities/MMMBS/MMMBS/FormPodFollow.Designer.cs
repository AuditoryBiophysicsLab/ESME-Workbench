namespace MBSGUI
{
    partial class FormPodFollow
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
            this.ModelOKButton = new System.Windows.Forms.Button();
            this.StimGroupBox = new System.Windows.Forms.GroupBox();
            this.label1 = new System.Windows.Forms.Label();
            this.button2 = new System.Windows.Forms.Button();
            this.button3 = new System.Windows.Forms.Button();
            this.StimActvnTheshLabel = new System.Windows.Forms.Label();
            this.StimDeactvnTheshEnableButton = new System.Windows.Forms.Button();
            this.StimDeactvnTheshButton = new System.Windows.Forms.Button();
            this.StimActvnTheshButton = new System.Windows.Forms.Button();
            this.StimGroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // ModelOKButton
            // 
            this.ModelOKButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.ModelOKButton.Location = new System.Drawing.Point(280, 128);
            this.ModelOKButton.Name = "ModelOKButton";
            this.ModelOKButton.Size = new System.Drawing.Size(75, 34);
            this.ModelOKButton.TabIndex = 134;
            this.ModelOKButton.Text = "Done";
            this.ModelOKButton.UseVisualStyleBackColor = true;
            // 
            // StimGroupBox
            // 
            this.StimGroupBox.Controls.Add(this.label1);
            this.StimGroupBox.Controls.Add(this.button2);
            this.StimGroupBox.Controls.Add(this.button3);
            this.StimGroupBox.Controls.Add(this.StimActvnTheshLabel);
            this.StimGroupBox.Controls.Add(this.StimDeactvnTheshEnableButton);
            this.StimGroupBox.Controls.Add(this.StimDeactvnTheshButton);
            this.StimGroupBox.Controls.Add(this.StimActvnTheshButton);
            this.StimGroupBox.Location = new System.Drawing.Point(12, 12);
            this.StimGroupBox.Name = "StimGroupBox";
            this.StimGroupBox.Size = new System.Drawing.Size(338, 108);
            this.StimGroupBox.TabIndex = 135;
            this.StimGroupBox.TabStop = false;
            this.StimGroupBox.Text = "Pod Following Settings";
            // 
            // label1
            // 
            this.label1.ImageAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.label1.Location = new System.Drawing.Point(6, 24);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(88, 13);
            this.label1.TabIndex = 61;
            this.label1.Text = "Focal Distance";
            this.label1.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // button2
            // 
            this.button2.BackColor = System.Drawing.SystemColors.Control;
            this.button2.FlatAppearance.BorderColor = System.Drawing.Color.White;
            this.button2.FlatAppearance.BorderSize = 0;
            this.button2.FlatAppearance.MouseDownBackColor = System.Drawing.Color.IndianRed;
            this.button2.FlatAppearance.MouseOverBackColor = System.Drawing.Color.RosyBrown;
            this.button2.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.button2.ImageAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.button2.Location = new System.Drawing.Point(161, 19);
            this.button2.Name = "button2";
            this.button2.Size = new System.Drawing.Size(110, 23);
            this.button2.TabIndex = 60;
            this.button2.Text = "Focal Animat Type:";
            this.button2.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.button2.UseVisualStyleBackColor = false;
            // 
            // button3
            // 
            this.button3.BackColor = System.Drawing.SystemColors.Control;
            this.button3.FlatAppearance.MouseDownBackColor = System.Drawing.Color.IndianRed;
            this.button3.FlatAppearance.MouseOverBackColor = System.Drawing.Color.RosyBrown;
            this.button3.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.button3.Location = new System.Drawing.Point(213, 45);
            this.button3.Name = "button3";
            this.button3.Size = new System.Drawing.Size(109, 23);
            this.button3.TabIndex = 59;
            this.button3.Text = "Calculated Centroid";
            this.button3.UseVisualStyleBackColor = false;
            // 
            // StimActvnTheshLabel
            // 
            this.StimActvnTheshLabel.ImageAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.StimActvnTheshLabel.Location = new System.Drawing.Point(20, 44);
            this.StimActvnTheshLabel.Name = "StimActvnTheshLabel";
            this.StimActvnTheshLabel.Size = new System.Drawing.Size(77, 13);
            this.StimActvnTheshLabel.TabIndex = 47;
            this.StimActvnTheshLabel.Text = "maximum (m):";
            this.StimActvnTheshLabel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // StimDeactvnTheshEnableButton
            // 
            this.StimDeactvnTheshEnableButton.BackColor = System.Drawing.SystemColors.Control;
            this.StimDeactvnTheshEnableButton.FlatAppearance.BorderColor = System.Drawing.Color.White;
            this.StimDeactvnTheshEnableButton.FlatAppearance.BorderSize = 0;
            this.StimDeactvnTheshEnableButton.FlatAppearance.MouseDownBackColor = System.Drawing.Color.IndianRed;
            this.StimDeactvnTheshEnableButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.RosyBrown;
            this.StimDeactvnTheshEnableButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.StimDeactvnTheshEnableButton.ImageAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.StimDeactvnTheshEnableButton.Location = new System.Drawing.Point(20, 66);
            this.StimDeactvnTheshEnableButton.Name = "StimDeactvnTheshEnableButton";
            this.StimDeactvnTheshEnableButton.Size = new System.Drawing.Size(77, 23);
            this.StimDeactvnTheshEnableButton.TabIndex = 58;
            this.StimDeactvnTheshEnableButton.Text = "minimum (m):";
            this.StimDeactvnTheshEnableButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.StimDeactvnTheshEnableButton.UseVisualStyleBackColor = false;
            // 
            // StimDeactvnTheshButton
            // 
            this.StimDeactvnTheshButton.BackColor = System.Drawing.SystemColors.Control;
            this.StimDeactvnTheshButton.FlatAppearance.MouseDownBackColor = System.Drawing.Color.IndianRed;
            this.StimDeactvnTheshButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.RosyBrown;
            this.StimDeactvnTheshButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.StimDeactvnTheshButton.Location = new System.Drawing.Point(103, 66);
            this.StimDeactvnTheshButton.Name = "StimDeactvnTheshButton";
            this.StimDeactvnTheshButton.Size = new System.Drawing.Size(38, 23);
            this.StimDeactvnTheshButton.TabIndex = 49;
            this.StimDeactvnTheshButton.Text = "---";
            this.StimDeactvnTheshButton.UseVisualStyleBackColor = false;
            // 
            // StimActvnTheshButton
            // 
            this.StimActvnTheshButton.BackColor = System.Drawing.SystemColors.Control;
            this.StimActvnTheshButton.FlatAppearance.MouseDownBackColor = System.Drawing.Color.IndianRed;
            this.StimActvnTheshButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.RosyBrown;
            this.StimActvnTheshButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.StimActvnTheshButton.Location = new System.Drawing.Point(103, 39);
            this.StimActvnTheshButton.Name = "StimActvnTheshButton";
            this.StimActvnTheshButton.Size = new System.Drawing.Size(38, 23);
            this.StimActvnTheshButton.TabIndex = 48;
            this.StimActvnTheshButton.UseVisualStyleBackColor = false;
            // 
            // FormPodFollow
            // 
            this.AcceptButton = this.ModelOKButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(367, 174);
            this.Controls.Add(this.StimGroupBox);
            this.Controls.Add(this.ModelOKButton);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.Name = "FormPodFollow";
            this.Text = "3MB Pod Following Description - Biomimetica";
            this.StimGroupBox.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button ModelOKButton;
        private System.Windows.Forms.GroupBox StimGroupBox;
        private System.Windows.Forms.Button button3;
        private System.Windows.Forms.Label StimActvnTheshLabel;
        private System.Windows.Forms.Button StimDeactvnTheshEnableButton;
        private System.Windows.Forms.Button StimDeactvnTheshButton;
        private System.Windows.Forms.Button StimActvnTheshButton;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button button2;
    }
}