namespace EnvironmentBuilder.VisualControls
{
    partial class AreaInfo
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
            this.components = new System.ComponentModel.Container();
            this.txtNorth = new System.Windows.Forms.TextBox();
            this.txtWest = new System.Windows.Forms.TextBox();
            this.txtEast = new System.Windows.Forms.TextBox();
            this.txtSouth = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.wlong = new System.Windows.Forms.Label();
            this.slat = new System.Windows.Forms.Label();
            this.elong = new System.Windows.Forms.Label();
            this.update_btn = new System.Windows.Forms.Button();
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.SuspendLayout();
            // 
            // txtNorth
            // 
            this.txtNorth.Location = new System.Drawing.Point(95, 34);
            this.txtNorth.Name = "txtNorth";
            this.txtNorth.Size = new System.Drawing.Size(74, 20);
            this.txtNorth.TabIndex = 0;
            // 
            // txtWest
            // 
            this.txtWest.Location = new System.Drawing.Point(21, 69);
            this.txtWest.Name = "txtWest";
            this.txtWest.Size = new System.Drawing.Size(74, 20);
            this.txtWest.TabIndex = 1;
            // 
            // txtEast
            // 
            this.txtEast.Location = new System.Drawing.Point(168, 69);
            this.txtEast.Name = "txtEast";
            this.txtEast.Size = new System.Drawing.Size(74, 20);
            this.txtEast.TabIndex = 2;
            // 
            // txtSouth
            // 
            this.txtSouth.Location = new System.Drawing.Point(95, 113);
            this.txtSouth.Name = "txtSouth";
            this.txtSouth.Size = new System.Drawing.Size(74, 20);
            this.txtSouth.TabIndex = 3;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(92, 18);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(74, 13);
            this.label1.TabIndex = 4;
            this.label1.Text = "North Latitude";
            // 
            // wlong
            // 
            this.wlong.AutoSize = true;
            this.wlong.Location = new System.Drawing.Point(18, 53);
            this.wlong.Name = "wlong";
            this.wlong.Size = new System.Drawing.Size(82, 13);
            this.wlong.TabIndex = 5;
            this.wlong.Text = "West Longitude";
            // 
            // slat
            // 
            this.slat.AutoSize = true;
            this.slat.Location = new System.Drawing.Point(92, 97);
            this.slat.Name = "slat";
            this.slat.Size = new System.Drawing.Size(76, 13);
            this.slat.TabIndex = 6;
            this.slat.Text = "South Latitude";
            // 
            // elong
            // 
            this.elong.AutoSize = true;
            this.elong.Location = new System.Drawing.Point(165, 53);
            this.elong.Name = "elong";
            this.elong.Size = new System.Drawing.Size(78, 13);
            this.elong.TabIndex = 7;
            this.elong.Text = "East Longitude";
            // 
            // update_btn
            // 
            this.update_btn.Location = new System.Drawing.Point(78, 141);
            this.update_btn.Name = "update_btn";
            this.update_btn.Size = new System.Drawing.Size(100, 23);
            this.update_btn.TabIndex = 8;
            this.update_btn.Text = "Update";
            this.update_btn.UseVisualStyleBackColor = true;
            this.update_btn.Click += new System.EventHandler(this.update_btn_Click);
            // 
            // toolTip1
            // 
            this.toolTip1.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info;
            // 
            // AreaInfo
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.Controls.Add(this.update_btn);
            this.Controls.Add(this.elong);
            this.Controls.Add(this.slat);
            this.Controls.Add(this.wlong);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.txtSouth);
            this.Controls.Add(this.txtEast);
            this.Controls.Add(this.txtWest);
            this.Controls.Add(this.txtNorth);
            this.Name = "AreaInfo";
            this.Size = new System.Drawing.Size(261, 163);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label wlong;
        private System.Windows.Forms.Label slat;
        private System.Windows.Forms.Label elong;
        protected System.Windows.Forms.TextBox txtNorth;
        protected System.Windows.Forms.TextBox txtWest;
        protected System.Windows.Forms.TextBox txtEast;
        protected System.Windows.Forms.TextBox txtSouth;
        protected System.Windows.Forms.Button update_btn;
        private System.Windows.Forms.ToolTip toolTip1;
    }
}
