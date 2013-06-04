namespace MBSGUI
{
    partial class FmMsgBox
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
            this.myOKButton1 = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // myOKButton1
            // 
            this.myOKButton1.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.myOKButton1.Location = new System.Drawing.Point(300, 176);
            this.myOKButton1.Name = "myOKButton1";
            this.myOKButton1.Size = new System.Drawing.Size(75, 23);
            this.myOKButton1.TabIndex = 0;
            this.myOKButton1.Text = "OK";
            this.myOKButton1.UseVisualStyleBackColor = true;
            // 
            // label1
            // 
            this.label1.Location = new System.Drawing.Point(12, 9);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(363, 144);
            this.label1.TabIndex = 2;
            this.label1.Text = "label1";
            this.label1.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // FmMsgBox
            // 
            this.AcceptButton = this.myOKButton1;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(387, 211);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.myOKButton1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Name = "FmMsgBox";
            this.Text = "FmMsgBox";
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button myOKButton1;
        private System.Windows.Forms.Label label1;
    }
}