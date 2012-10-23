namespace IESME_GEO.Controls
{
    partial class DBTables
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
            this.dbsubmit_btn = new System.Windows.Forms.Button();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.datatypelist = new System.Windows.Forms.ComboBox();
            this.datasetlist = new System.Windows.Forms.ComboBox();
            this.datasubsetlist = new System.Windows.Forms.ComboBox();
            this.label1 = new System.Windows.Forms.Label();
            this.dbcancel_btn = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // dbsubmit_btn
            // 
            this.dbsubmit_btn.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.dbsubmit_btn.Location = new System.Drawing.Point(136, 124);
            this.dbsubmit_btn.Name = "dbsubmit_btn";
            this.dbsubmit_btn.Size = new System.Drawing.Size(73, 24);
            this.dbsubmit_btn.TabIndex = 0;
            this.dbsubmit_btn.Text = "Submit";
            this.dbsubmit_btn.UseVisualStyleBackColor = true;
            this.dbsubmit_btn.Click += new System.EventHandler(this.dbsubmit_btn_Click);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(16, 43);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(57, 13);
            this.label2.TabIndex = 2;
            this.label2.Text = "Data Type";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(16, 70);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(49, 13);
            this.label3.TabIndex = 3;
            this.label3.Text = "Data Set";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(16, 97);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(68, 13);
            this.label4.TabIndex = 4;
            this.label4.Text = "Data SubSet";
            // 
            // datatypelist
            // 
            this.datatypelist.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.datatypelist.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.datatypelist.FormattingEnabled = true;
            this.datatypelist.Location = new System.Drawing.Point(96, 43);
            this.datatypelist.Name = "datatypelist";
            this.datatypelist.Size = new System.Drawing.Size(113, 21);
            this.datatypelist.TabIndex = 5;
            this.datatypelist.SelectedIndexChanged += new System.EventHandler(this.datatypelist_SelectedIndexChanged);
            // 
            // datasetlist
            // 
            this.datasetlist.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.datasetlist.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.datasetlist.FormattingEnabled = true;
            this.datasetlist.Location = new System.Drawing.Point(96, 70);
            this.datasetlist.Name = "datasetlist";
            this.datasetlist.Size = new System.Drawing.Size(113, 21);
            this.datasetlist.TabIndex = 6;
            this.datasetlist.SelectedIndexChanged += new System.EventHandler(this.datasetlist_SelectedIndexChanged);
            // 
            // datasubsetlist
            // 
            this.datasubsetlist.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.datasubsetlist.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.datasubsetlist.FormattingEnabled = true;
            this.datasubsetlist.Location = new System.Drawing.Point(96, 97);
            this.datasubsetlist.Name = "datasubsetlist";
            this.datasubsetlist.Size = new System.Drawing.Size(113, 21);
            this.datasubsetlist.TabIndex = 7;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Arial", 11.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(16, 9);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(126, 18);
            this.label1.TabIndex = 8;
            this.label1.Text = "Database Tables";
            // 
            // dbcancel_btn
            // 
            this.dbcancel_btn.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.dbcancel_btn.Location = new System.Drawing.Point(57, 124);
            this.dbcancel_btn.Name = "dbcancel_btn";
            this.dbcancel_btn.Size = new System.Drawing.Size(73, 24);
            this.dbcancel_btn.TabIndex = 9;
            this.dbcancel_btn.Text = "Cancel";
            this.dbcancel_btn.UseVisualStyleBackColor = true;
            // 
            // DBTables
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.dbcancel_btn);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.datasubsetlist);
            this.Controls.Add(this.datasetlist);
            this.Controls.Add(this.datatypelist);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.dbsubmit_btn);
            this.Name = "DBTables";
            this.Size = new System.Drawing.Size(218, 151);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        protected System.Windows.Forms.Button dbsubmit_btn;
        protected System.Windows.Forms.ComboBox datatypelist;
        protected System.Windows.Forms.ComboBox datasetlist;
        protected System.Windows.Forms.ComboBox datasubsetlist;
        private System.Windows.Forms.Label label1;
        protected System.Windows.Forms.Button dbcancel_btn;
    }
}
