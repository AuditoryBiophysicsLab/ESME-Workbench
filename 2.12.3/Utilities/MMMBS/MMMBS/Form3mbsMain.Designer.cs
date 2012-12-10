namespace MBSGUI
{
    partial class Form3mbsMain
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
            this.SpeciesModelingButton = new System.Windows.Forms.Button();
            this.ExperimentButton = new System.Windows.Forms.Button();
            this.ExitButton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // SpeciesModelingButton
            // 
            this.SpeciesModelingButton.Location = new System.Drawing.Point(588, 26);
            this.SpeciesModelingButton.Name = "SpeciesModelingButton";
            this.SpeciesModelingButton.Size = new System.Drawing.Size(113, 23);
            this.SpeciesModelingButton.TabIndex = 1;
            this.SpeciesModelingButton.Text = "Species Modeling";
            this.SpeciesModelingButton.UseVisualStyleBackColor = true;
            this.SpeciesModelingButton.Click += new System.EventHandler(this.SpeciesModelingButton_Click);
            // 
            // ExperimentButton
            // 
            this.ExperimentButton.Location = new System.Drawing.Point(588, 55);
            this.ExperimentButton.Name = "ExperimentButton";
            this.ExperimentButton.Size = new System.Drawing.Size(113, 23);
            this.ExperimentButton.TabIndex = 2;
            this.ExperimentButton.Text = "Experiment";
            this.ExperimentButton.UseVisualStyleBackColor = true;
            this.ExperimentButton.Visible = false;
            this.ExperimentButton.Click += new System.EventHandler(this.ExperimentButton_Click);
            // 
            // ExitButton
            // 
            this.ExitButton.Location = new System.Drawing.Point(626, 324);
            this.ExitButton.Name = "ExitButton";
            this.ExitButton.Size = new System.Drawing.Size(75, 43);
            this.ExitButton.TabIndex = 3;
            this.ExitButton.Text = "Exit";
            this.ExitButton.UseVisualStyleBackColor = true;
            this.ExitButton.Click += new System.EventHandler(this.ExitButton_Click);
            // 
            // Form3mbsMain
            // 
            this.AcceptButton = this.ExitButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(713, 379);
            this.Controls.Add(this.ExitButton);
            this.Controls.Add(this.ExperimentButton);
            this.Controls.Add(this.SpeciesModelingButton);
            this.Name = "Form3mbsMain";
            this.Text = "Marine Mammal Movement And Behavior - Biomimetica";
            this.Load += new System.EventHandler(this.MainForm_Load);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button SpeciesModelingButton;
        private System.Windows.Forms.Button ExperimentButton;
        private System.Windows.Forms.Button ExitButton;

    }
}

