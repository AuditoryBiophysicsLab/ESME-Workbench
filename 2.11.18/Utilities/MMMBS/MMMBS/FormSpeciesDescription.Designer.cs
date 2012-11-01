namespace MBSGUI
{
    partial class FormSpeciesDescription
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
            this.DoneButton = new System.Windows.Forms.Button();
            this.ShortDescriptionTextBox = new System.Windows.Forms.TextBox();
            this.LongCommentTextBox = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // DoneButton
            // 
            this.DoneButton.Location = new System.Drawing.Point(482, 391);
            this.DoneButton.Name = "DoneButton";
            this.DoneButton.Size = new System.Drawing.Size(75, 23);
            this.DoneButton.TabIndex = 0;
            this.DoneButton.Text = "Done";
            this.DoneButton.UseVisualStyleBackColor = true;
            this.DoneButton.Click += new System.EventHandler(this.DoneButton_Click);
            // 
            // ShortDescriptionTextBox
            // 
            this.ShortDescriptionTextBox.Location = new System.Drawing.Point(102, 39);
            this.ShortDescriptionTextBox.MaxLength = 31;
            this.ShortDescriptionTextBox.Name = "ShortDescriptionTextBox";
            this.ShortDescriptionTextBox.Size = new System.Drawing.Size(441, 20);
            this.ShortDescriptionTextBox.TabIndex = 1;
            this.ShortDescriptionTextBox.TextChanged += new System.EventHandler(this.ShortDescriptionTextBox_TextChanged);
            this.ShortDescriptionTextBox.MouseDown += new System.Windows.Forms.MouseEventHandler(this.ShortDescriptionTextBox_MouseDown);
            // 
            // LongCommentTextBox
            // 
            this.LongCommentTextBox.Location = new System.Drawing.Point(102, 99);
            this.LongCommentTextBox.MaxLength = 1023;
            this.LongCommentTextBox.Multiline = true;
            this.LongCommentTextBox.Name = "LongCommentTextBox";
            this.LongCommentTextBox.Size = new System.Drawing.Size(441, 257);
            this.LongCommentTextBox.TabIndex = 2;
            this.LongCommentTextBox.TextChanged += new System.EventHandler(this.LongCommentTextBox_TextChanged);
            this.LongCommentTextBox.MouseDown += new System.Windows.Forms.MouseEventHandler(this.LongCommentTextBox_MouseDown);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(412, 62);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(131, 13);
            this.label1.TabIndex = 3;
            this.label1.Text = "(96 characters Remaining)";
            this.label1.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(397, 359);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(146, 13);
            this.label2.TabIndex = 4;
            this.label2.Text = "(4095 characters Remaining):";
            this.label2.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(37, 102);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(59, 13);
            this.label3.TabIndex = 5;
            this.label3.Text = "Comments:";
            this.label3.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(8, 42);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(91, 13);
            this.label4.TabIndex = 6;
            this.label4.Text = "Short Description:";
            this.label4.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // FormSpeciesDescription
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(560, 426);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.LongCommentTextBox);
            this.Controls.Add(this.ShortDescriptionTextBox);
            this.Controls.Add(this.DoneButton);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Name = "FormSpeciesDescription";
            this.Text = "Form1";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button DoneButton;
        private System.Windows.Forms.TextBox ShortDescriptionTextBox;
        private System.Windows.Forms.TextBox LongCommentTextBox;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
    }
}