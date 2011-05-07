namespace EnvironmentBuilder.VisualControls
{
    partial class ShipMarkerForm
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
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.shipSbtn = new System.Windows.Forms.Button();
            this.shipCbtn = new System.Windows.Forms.Button();
            this.shiplat = new System.Windows.Forms.TextBox();
            this.shiplong = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // shipSbtn
            // 
            this.shipSbtn.Location = new System.Drawing.Point(53, 94);
            this.shipSbtn.Name = "shipSbtn";
            this.shipSbtn.Size = new System.Drawing.Size(75, 23);
            this.shipSbtn.TabIndex = 0;
            this.shipSbtn.Text = "Submit";
            this.shipSbtn.UseVisualStyleBackColor = true;
            this.shipSbtn.Click += new System.EventHandler(this.shipSbtn_Click);
            // 
            // shipCbtn
            // 
            this.shipCbtn.Location = new System.Drawing.Point(165, 94);
            this.shipCbtn.Name = "shipCbtn";
            this.shipCbtn.Size = new System.Drawing.Size(75, 23);
            this.shipCbtn.TabIndex = 1;
            this.shipCbtn.Text = "Cancel";
            this.shipCbtn.UseVisualStyleBackColor = true;
            // 
            // shiplat
            // 
            this.shiplat.Location = new System.Drawing.Point(43, 51);
            this.shiplat.Name = "shiplat";
            this.shiplat.Size = new System.Drawing.Size(95, 20);
            this.shiplat.TabIndex = 2;
            // 
            // shiplong
            // 
            this.shiplong.Location = new System.Drawing.Point(156, 51);
            this.shiplong.Name = "shiplong";
            this.shiplong.Size = new System.Drawing.Size(95, 20);
            this.shiplong.TabIndex = 3;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(40, 35);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(45, 13);
            this.label1.TabIndex = 4;
            this.label1.Text = "Latitude";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(153, 35);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(54, 13);
            this.label2.TabIndex = 5;
            this.label2.Text = "Longitude";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label3.Location = new System.Drawing.Point(3, 9);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(95, 16);
            this.label3.TabIndex = 6;
            this.label3.Text = "Ship Marker ";
            // 
            // ShipMarkerForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(268, 129);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.shiplong);
            this.Controls.Add(this.shiplat);
            this.Controls.Add(this.shipCbtn);
            this.Controls.Add(this.shipSbtn);
            this.MaximumSize = new System.Drawing.Size(284, 167);
            this.MinimumSize = new System.Drawing.Size(284, 167);
            this.Name = "ShipMarkerForm";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button shipSbtn;
        private System.Windows.Forms.Button shipCbtn;
        private System.Windows.Forms.TextBox shiplat;
        private System.Windows.Forms.TextBox shiplong;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label3;
    }
}
