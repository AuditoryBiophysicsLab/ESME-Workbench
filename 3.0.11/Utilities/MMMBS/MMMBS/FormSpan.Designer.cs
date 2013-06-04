namespace MBSGUI
{
    partial class FormSpan
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
            this.I2TextBox = new System.Windows.Forms.TextBox();
            this.RangeLabel = new System.Windows.Forms.Label();
            this.InstrctnLabel = new System.Windows.Forms.Label();
            this.MyCancelButton = new System.Windows.Forms.Button();
            this.MyOKButton = new System.Windows.Forms.Button();
            this.Input2Label = new System.Windows.Forms.Label();
            this.Input1Label = new System.Windows.Forms.Label();
            this.I1TextBox = new System.Windows.Forms.TextBox();
            this.I4TextBox = new System.Windows.Forms.TextBox();
            this.I5TextBox = new System.Windows.Forms.TextBox();
            this.Colon1Label = new System.Windows.Forms.Label();
            this.Colon2Label = new System.Windows.Forms.Label();
            this.I3TextBox = new System.Windows.Forms.TextBox();
            this.I6TextBox = new System.Windows.Forms.TextBox();
            this.Colon3Label = new System.Windows.Forms.Label();
            this.Colon4Label = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // I2TextBox
            // 
            this.I2TextBox.Enabled = false;
            this.I2TextBox.Location = new System.Drawing.Point(68, 31);
            this.I2TextBox.Name = "I2TextBox";
            this.I2TextBox.Size = new System.Drawing.Size(47, 20);
            this.I2TextBox.TabIndex = 1;
            this.I2TextBox.Text = "2";
            this.I2TextBox.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            this.I2TextBox.Visible = false;
            this.I2TextBox.TextChanged += new System.EventHandler(this.TextBox_TextChanged);
            // 
            // RangeLabel
            // 
            this.RangeLabel.AutoSize = true;
            this.RangeLabel.Enabled = false;
            this.RangeLabel.Location = new System.Drawing.Point(169, 35);
            this.RangeLabel.Name = "RangeLabel";
            this.RangeLabel.Size = new System.Drawing.Size(104, 13);
            this.RangeLabel.TabIndex = 2;
            this.RangeLabel.Text = " <= Animat Depth <=";
            this.RangeLabel.Visible = false;
            // 
            // InstrctnLabel
            // 
            this.InstrctnLabel.AutoSize = true;
            this.InstrctnLabel.Enabled = false;
            this.InstrctnLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.InstrctnLabel.Location = new System.Drawing.Point(13, 9);
            this.InstrctnLabel.Name = "InstrctnLabel";
            this.InstrctnLabel.Size = new System.Drawing.Size(189, 16);
            this.InstrctnLabel.TabIndex = 3;
            this.InstrctnLabel.Text = "Set transition depth range:";
            this.InstrctnLabel.Visible = false;
            // 
            // MyCancelButton
            // 
            this.MyCancelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.MyCancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.MyCancelButton.Location = new System.Drawing.Point(283, 82);
            this.MyCancelButton.Name = "MyCancelButton";
            this.MyCancelButton.Size = new System.Drawing.Size(75, 23);
            this.MyCancelButton.TabIndex = 462;
            this.MyCancelButton.Text = "Cancel";
            this.MyCancelButton.UseVisualStyleBackColor = true;
            this.MyCancelButton.Click += new System.EventHandler(this.MyCancelButton_Click);
            // 
            // MyOKButton
            // 
            this.MyOKButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.MyOKButton.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.MyOKButton.Location = new System.Drawing.Point(364, 82);
            this.MyOKButton.Name = "MyOKButton";
            this.MyOKButton.Size = new System.Drawing.Size(75, 23);
            this.MyOKButton.TabIndex = 461;
            this.MyOKButton.Text = "OK";
            this.MyOKButton.UseVisualStyleBackColor = true;
            this.MyOKButton.Click += new System.EventHandler(this.MyOKButton_Click);
            // 
            // Input2Label
            // 
            this.Input2Label.AutoSize = true;
            this.Input2Label.Enabled = false;
            this.Input2Label.Location = new System.Drawing.Point(281, 55);
            this.Input2Label.Name = "Input2Label";
            this.Input2Label.Size = new System.Drawing.Size(54, 13);
            this.Input2Label.TabIndex = 463;
            this.Input2Label.Text = "limit: 3500";
            this.Input2Label.Visible = false;
            // 
            // Input1Label
            // 
            this.Input1Label.AutoSize = true;
            this.Input1Label.Enabled = false;
            this.Input1Label.Location = new System.Drawing.Point(13, 55);
            this.Input1Label.Name = "Input1Label";
            this.Input1Label.Size = new System.Drawing.Size(54, 13);
            this.Input1Label.TabIndex = 464;
            this.Input1Label.Text = "limit: 3500";
            this.Input1Label.Visible = false;
            // 
            // I1TextBox
            // 
            this.I1TextBox.Enabled = false;
            this.I1TextBox.Location = new System.Drawing.Point(13, 31);
            this.I1TextBox.Name = "I1TextBox";
            this.I1TextBox.Size = new System.Drawing.Size(47, 20);
            this.I1TextBox.TabIndex = 465;
            this.I1TextBox.Text = "1";
            this.I1TextBox.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            this.I1TextBox.Visible = false;
            this.I1TextBox.TextChanged += new System.EventHandler(this.TextBox_TextChanged);
            // 
            // I4TextBox
            // 
            this.I4TextBox.Enabled = false;
            this.I4TextBox.Location = new System.Drawing.Point(281, 31);
            this.I4TextBox.Name = "I4TextBox";
            this.I4TextBox.Size = new System.Drawing.Size(47, 20);
            this.I4TextBox.TabIndex = 466;
            this.I4TextBox.Text = "4";
            this.I4TextBox.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            this.I4TextBox.Visible = false;
            this.I4TextBox.TextChanged += new System.EventHandler(this.TextBox_TextChanged);
            // 
            // I5TextBox
            // 
            this.I5TextBox.Enabled = false;
            this.I5TextBox.Location = new System.Drawing.Point(334, 31);
            this.I5TextBox.Name = "I5TextBox";
            this.I5TextBox.Size = new System.Drawing.Size(47, 20);
            this.I5TextBox.TabIndex = 467;
            this.I5TextBox.Text = "5";
            this.I5TextBox.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            this.I5TextBox.Visible = false;
            this.I5TextBox.TextChanged += new System.EventHandler(this.TextBox_TextChanged);
            // 
            // Colon1Label
            // 
            this.Colon1Label.AutoSize = true;
            this.Colon1Label.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Colon1Label.Location = new System.Drawing.Point(57, 31);
            this.Colon1Label.Name = "Colon1Label";
            this.Colon1Label.Size = new System.Drawing.Size(14, 20);
            this.Colon1Label.TabIndex = 468;
            this.Colon1Label.Text = ":";
            this.Colon1Label.Visible = false;
            // 
            // Colon2Label
            // 
            this.Colon2Label.AutoSize = true;
            this.Colon2Label.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Colon2Label.Location = new System.Drawing.Point(111, 30);
            this.Colon2Label.Name = "Colon2Label";
            this.Colon2Label.Size = new System.Drawing.Size(14, 20);
            this.Colon2Label.TabIndex = 469;
            this.Colon2Label.Text = ":";
            this.Colon2Label.Visible = false;
            // 
            // I3TextBox
            // 
            this.I3TextBox.Enabled = false;
            this.I3TextBox.Location = new System.Drawing.Point(121, 31);
            this.I3TextBox.Name = "I3TextBox";
            this.I3TextBox.Size = new System.Drawing.Size(47, 20);
            this.I3TextBox.TabIndex = 470;
            this.I3TextBox.Text = "3";
            this.I3TextBox.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            this.I3TextBox.Visible = false;
            // 
            // I6TextBox
            // 
            this.I6TextBox.Enabled = false;
            this.I6TextBox.Location = new System.Drawing.Point(387, 31);
            this.I6TextBox.Name = "I6TextBox";
            this.I6TextBox.Size = new System.Drawing.Size(47, 20);
            this.I6TextBox.TabIndex = 471;
            this.I6TextBox.Text = "6";
            this.I6TextBox.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            this.I6TextBox.Visible = false;
            this.I6TextBox.TextChanged += new System.EventHandler(this.TextBox_TextChanged);
            // 
            // Colon3Label
            // 
            this.Colon3Label.AutoSize = true;
            this.Colon3Label.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Colon3Label.Location = new System.Drawing.Point(324, 30);
            this.Colon3Label.Name = "Colon3Label";
            this.Colon3Label.Size = new System.Drawing.Size(14, 20);
            this.Colon3Label.TabIndex = 472;
            this.Colon3Label.Text = ":";
            this.Colon3Label.Visible = false;
            // 
            // Colon4Label
            // 
            this.Colon4Label.AutoSize = true;
            this.Colon4Label.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Colon4Label.Location = new System.Drawing.Point(377, 30);
            this.Colon4Label.Name = "Colon4Label";
            this.Colon4Label.Size = new System.Drawing.Size(14, 20);
            this.Colon4Label.TabIndex = 473;
            this.Colon4Label.Text = ":";
            this.Colon4Label.Visible = false;
            // 
            // FormSpan
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(450, 115);
            this.Controls.Add(this.I4TextBox);
            this.Controls.Add(this.I5TextBox);
            this.Controls.Add(this.I6TextBox);
            this.Controls.Add(this.I3TextBox);
            this.Controls.Add(this.MyCancelButton);
            this.Controls.Add(this.MyOKButton);
            this.Controls.Add(this.I1TextBox);
            this.Controls.Add(this.Input1Label);
            this.Controls.Add(this.Input2Label);
            this.Controls.Add(this.InstrctnLabel);
            this.Controls.Add(this.I2TextBox);
            this.Controls.Add(this.Colon1Label);
            this.Controls.Add(this.Colon2Label);
            this.Controls.Add(this.RangeLabel);
            this.Controls.Add(this.Colon3Label);
            this.Controls.Add(this.Colon4Label);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Name = "FormSpan";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Set Input:";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        protected System.Windows.Forms.Button MyOKButton;
        protected System.Windows.Forms.Label Colon1Label;
        protected System.Windows.Forms.Label Colon2Label;
        protected System.Windows.Forms.Button MyCancelButton;
        protected System.Windows.Forms.TextBox I1TextBox;
        protected System.Windows.Forms.TextBox I4TextBox;
        protected System.Windows.Forms.TextBox I5TextBox;
        protected System.Windows.Forms.TextBox I2TextBox;
        protected System.Windows.Forms.Label RangeLabel;
        protected System.Windows.Forms.Label Input2Label;
        protected System.Windows.Forms.Label Input1Label;
        protected System.Windows.Forms.Label InstrctnLabel;
        protected System.Windows.Forms.TextBox I3TextBox;
        protected System.Windows.Forms.TextBox I6TextBox;
        protected System.Windows.Forms.Label Colon3Label;
        protected System.Windows.Forms.Label Colon4Label;
    }
}