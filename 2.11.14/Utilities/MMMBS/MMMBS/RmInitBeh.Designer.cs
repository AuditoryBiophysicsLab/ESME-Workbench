namespace MBSGUI
{
    partial class RmInitBeh
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
            this.MatrixTextBox = new System.Windows.Forms.TextBox();
            this.CanceelButton = new System.Windows.Forms.Button();
            this.BehaviorCountLabel = new System.Windows.Forms.Label();
            this.MatrixDimensionsLabel = new System.Windows.Forms.Label();
            this.BiasFormatErrorLabel = new System.Windows.Forms.Label();
            this.RefreshButton = new System.Windows.Forms.Button();
            this.DisplayGroupBox = new System.Windows.Forms.GroupBox();
            this.label1 = new System.Windows.Forms.Label();
            this.DurationGroupBoxAve = new System.Windows.Forms.GroupBox();
            this.DurationTextBoxAve = new System.Windows.Forms.Label();
            this.DurationTextBoxMax = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.BehaviorNameLabel = new System.Windows.Forms.Label();
            this.DurationGroupBoxMin = new System.Windows.Forms.GroupBox();
            this.DurationTextBoxMin = new System.Windows.Forms.Label();
            this.TransitionGroupBoxBeh2 = new System.Windows.Forms.GroupBox();
            this.TransitionLabelBeh2 = new System.Windows.Forms.Label();
            this.TransitionGroupBoxBeh1 = new System.Windows.Forms.GroupBox();
            this.TransitionLabelBeh1 = new System.Windows.Forms.Label();
            this.TransitionLabelBeh0 = new System.Windows.Forms.Label();
            this.TransitionGroupBoxBeh0 = new System.Windows.Forms.GroupBox();
            this.TransitionGroupBoxBeh3 = new System.Windows.Forms.GroupBox();
            this.TransitionLabelBeh3 = new System.Windows.Forms.Label();
            this.TransitionGroupBoxBeh4 = new System.Windows.Forms.GroupBox();
            this.TransitionLabelBeh4 = new System.Windows.Forms.Label();
            this.TransitionGroupBoxBeh5 = new System.Windows.Forms.GroupBox();
            this.TransitionLabelBeh5 = new System.Windows.Forms.Label();
            this.TransitionGroupBoxBeh7 = new System.Windows.Forms.GroupBox();
            this.TransitionLabelBeh7 = new System.Windows.Forms.Label();
            this.TransitionLabelBeh6 = new System.Windows.Forms.Label();
            this.TransitionGroupBoxBeh6 = new System.Windows.Forms.GroupBox();
            this.BehaviorNumberLabel = new System.Windows.Forms.Label();
            this.TransitionLabelBehNumber0 = new System.Windows.Forms.Label();
            this.TransitionLabelBehNumber1 = new System.Windows.Forms.Label();
            this.TransitionLabelBehNumber3 = new System.Windows.Forms.Label();
            this.TransitionLabelBehNumber2 = new System.Windows.Forms.Label();
            this.TransitionLabelBehNumber5 = new System.Windows.Forms.Label();
            this.TransitionLabelBehNumber4 = new System.Windows.Forms.Label();
            this.TransitionLabelBehNumber7 = new System.Windows.Forms.Label();
            this.TransitionLabelBehNumber6 = new System.Windows.Forms.Label();
            this.DurationGroupBoxMax = new System.Windows.Forms.GroupBox();
            this.label2 = new System.Windows.Forms.Label();
            this.NumTrialsButton = new System.Windows.Forms.Button();
            this.label17 = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // DoneButton
            // 
            this.DoneButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.DoneButton.Location = new System.Drawing.Point(560, 550);
            this.DoneButton.Name = "DoneButton";
            this.DoneButton.Size = new System.Drawing.Size(83, 37);
            this.DoneButton.TabIndex = 0;
            this.DoneButton.Text = "Done";
            this.DoneButton.UseVisualStyleBackColor = true;
            this.DoneButton.Click += new System.EventHandler(this.DoneButton_Click);
            // 
            // MatrixTextBox
            // 
            this.MatrixTextBox.AcceptsReturn = true;
            this.MatrixTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.MatrixTextBox.Font = new System.Drawing.Font("Courier New", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.MatrixTextBox.Location = new System.Drawing.Point(12, 57);
            this.MatrixTextBox.Multiline = true;
            this.MatrixTextBox.Name = "MatrixTextBox";
            this.MatrixTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.MatrixTextBox.Size = new System.Drawing.Size(631, 241);
            this.MatrixTextBox.TabIndex = 1;
            this.MatrixTextBox.Text = "0.000 24.000 0.000 0.340 0.343 0.356 0.599 0.6343 0.898 0.900 1.000 60 15\r\n1\r\n2\r\n" +
    "3\r\n4\r\n5\r\n6\r\n7";
            this.MatrixTextBox.TextChanged += new System.EventHandler(this.InitialBehaviorTextBox_TextChanged);
            // 
            // CanceelButton
            // 
            this.CanceelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.CanceelButton.Location = new System.Drawing.Point(471, 550);
            this.CanceelButton.Name = "CanceelButton";
            this.CanceelButton.Size = new System.Drawing.Size(83, 37);
            this.CanceelButton.TabIndex = 2;
            this.CanceelButton.Text = "Cancel";
            this.CanceelButton.UseVisualStyleBackColor = true;
            this.CanceelButton.Click += new System.EventHandler(this.CancelButton_Click);
            // 
            // BehaviorCountLabel
            // 
            this.BehaviorCountLabel.AutoSize = true;
            this.BehaviorCountLabel.Location = new System.Drawing.Point(13, 18);
            this.BehaviorCountLabel.Name = "BehaviorCountLabel";
            this.BehaviorCountLabel.Size = new System.Drawing.Size(164, 13);
            this.BehaviorCountLabel.TabIndex = 3;
            this.BehaviorCountLabel.Text = "Species Defined Behavior Count:";
            // 
            // MatrixDimensionsLabel
            // 
            this.MatrixDimensionsLabel.AutoSize = true;
            this.MatrixDimensionsLabel.Location = new System.Drawing.Point(13, 41);
            this.MatrixDimensionsLabel.Name = "MatrixDimensionsLabel";
            this.MatrixDimensionsLabel.Size = new System.Drawing.Size(95, 13);
            this.MatrixDimensionsLabel.TabIndex = 4;
            this.MatrixDimensionsLabel.Text = "Matrix Dimensions:";
            // 
            // BiasFormatErrorLabel
            // 
            this.BiasFormatErrorLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.BiasFormatErrorLabel.Location = new System.Drawing.Point(12, 301);
            this.BiasFormatErrorLabel.Name = "BiasFormatErrorLabel";
            this.BiasFormatErrorLabel.Size = new System.Drawing.Size(63, 17);
            this.BiasFormatErrorLabel.TabIndex = 12;
            this.BiasFormatErrorLabel.Text = "Format Error";
            this.BiasFormatErrorLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // RefreshButton
            // 
            this.RefreshButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.RefreshButton.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.RefreshButton.Location = new System.Drawing.Point(577, 304);
            this.RefreshButton.Name = "RefreshButton";
            this.RefreshButton.Size = new System.Drawing.Size(66, 26);
            this.RefreshButton.TabIndex = 1;
            this.RefreshButton.Text = "Refresh";
            this.RefreshButton.UseVisualStyleBackColor = true;
            this.RefreshButton.Click += new System.EventHandler(this.RefreshButton_Click);
            // 
            // DisplayGroupBox
            // 
            this.DisplayGroupBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.DisplayGroupBox.Location = new System.Drawing.Point(12, 334);
            this.DisplayGroupBox.Name = "DisplayGroupBox";
            this.DisplayGroupBox.Size = new System.Drawing.Size(621, 101);
            this.DisplayGroupBox.TabIndex = 13;
            this.DisplayGroupBox.TabStop = false;
            this.DisplayGroupBox.Text = "groupBox1";
            this.DisplayGroupBox.Visible = false;
            // 
            // label1
            // 
            this.label1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(201, 468);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(221, 12);
            this.label1.TabIndex = 20;
            this.label1.Text = "behavior transitioned into (total counts over all trials):";
            // 
            // DurationGroupBoxAve
            // 
            this.DurationGroupBoxAve.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.DurationGroupBoxAve.Location = new System.Drawing.Point(50, 496);
            this.DurationGroupBoxAve.Name = "DurationGroupBoxAve";
            this.DurationGroupBoxAve.Size = new System.Drawing.Size(12, 8);
            this.DurationGroupBoxAve.TabIndex = 24;
            this.DurationGroupBoxAve.TabStop = false;
            this.DurationGroupBoxAve.Text = "groupBox3";
            this.DurationGroupBoxAve.Visible = false;
            // 
            // DurationTextBoxAve
            // 
            this.DurationTextBoxAve.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.DurationTextBoxAve.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.DurationTextBoxAve.Location = new System.Drawing.Point(68, 494);
            this.DurationTextBoxAve.Name = "DurationTextBoxAve";
            this.DurationTextBoxAve.Size = new System.Drawing.Size(41, 12);
            this.DurationTextBoxAve.TabIndex = 27;
            this.DurationTextBoxAve.Text = "average";
            // 
            // DurationTextBoxMax
            // 
            this.DurationTextBoxMax.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.DurationTextBoxMax.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.DurationTextBoxMax.Location = new System.Drawing.Point(68, 483);
            this.DurationTextBoxMax.Name = "DurationTextBoxMax";
            this.DurationTextBoxMax.Size = new System.Drawing.Size(51, 12);
            this.DurationTextBoxMax.TabIndex = 26;
            this.DurationTextBoxMax.Text = "maximum";
            // 
            // label6
            // 
            this.label6.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.label6.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label6.Location = new System.Drawing.Point(42, 468);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(135, 12);
            this.label6.TabIndex = 25;
            this.label6.Text = "time in until transition (minutes):";
            // 
            // BehaviorNameLabel
            // 
            this.BehaviorNameLabel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.BehaviorNameLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.BehaviorNameLabel.Location = new System.Drawing.Point(46, 451);
            this.BehaviorNameLabel.Name = "BehaviorNameLabel";
            this.BehaviorNameLabel.Size = new System.Drawing.Size(209, 12);
            this.BehaviorNameLabel.TabIndex = 28;
            this.BehaviorNameLabel.Text = "Behavior Name";
            // 
            // DurationGroupBoxMin
            // 
            this.DurationGroupBoxMin.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.DurationGroupBoxMin.Location = new System.Drawing.Point(50, 507);
            this.DurationGroupBoxMin.Name = "DurationGroupBoxMin";
            this.DurationGroupBoxMin.Size = new System.Drawing.Size(12, 8);
            this.DurationGroupBoxMin.TabIndex = 28;
            this.DurationGroupBoxMin.TabStop = false;
            this.DurationGroupBoxMin.Text = "groupBox5";
            this.DurationGroupBoxMin.Visible = false;
            // 
            // DurationTextBoxMin
            // 
            this.DurationTextBoxMin.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.DurationTextBoxMin.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.DurationTextBoxMin.Location = new System.Drawing.Point(68, 505);
            this.DurationTextBoxMin.Name = "DurationTextBoxMin";
            this.DurationTextBoxMin.Size = new System.Drawing.Size(51, 12);
            this.DurationTextBoxMin.TabIndex = 29;
            this.DurationTextBoxMin.Text = "minimum";
            // 
            // TransitionGroupBoxBeh2
            // 
            this.TransitionGroupBoxBeh2.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionGroupBoxBeh2.Location = new System.Drawing.Point(215, 507);
            this.TransitionGroupBoxBeh2.Name = "TransitionGroupBoxBeh2";
            this.TransitionGroupBoxBeh2.Size = new System.Drawing.Size(12, 8);
            this.TransitionGroupBoxBeh2.TabIndex = 34;
            this.TransitionGroupBoxBeh2.TabStop = false;
            this.TransitionGroupBoxBeh2.Text = "groupBox1";
            this.TransitionGroupBoxBeh2.Visible = false;
            // 
            // TransitionLabelBeh2
            // 
            this.TransitionLabelBeh2.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBeh2.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBeh2.Location = new System.Drawing.Point(257, 505);
            this.TransitionLabelBeh2.Name = "TransitionLabelBeh2";
            this.TransitionLabelBeh2.Size = new System.Drawing.Size(157, 12);
            this.TransitionLabelBeh2.TabIndex = 35;
            this.TransitionLabelBeh2.Text = "behavior 3";
            // 
            // TransitionGroupBoxBeh1
            // 
            this.TransitionGroupBoxBeh1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionGroupBoxBeh1.Location = new System.Drawing.Point(215, 496);
            this.TransitionGroupBoxBeh1.Name = "TransitionGroupBoxBeh1";
            this.TransitionGroupBoxBeh1.Size = new System.Drawing.Size(12, 8);
            this.TransitionGroupBoxBeh1.TabIndex = 31;
            this.TransitionGroupBoxBeh1.TabStop = false;
            this.TransitionGroupBoxBeh1.Text = "groupBox2";
            this.TransitionGroupBoxBeh1.Visible = false;
            // 
            // TransitionLabelBeh1
            // 
            this.TransitionLabelBeh1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBeh1.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBeh1.Location = new System.Drawing.Point(257, 494);
            this.TransitionLabelBeh1.Name = "TransitionLabelBeh1";
            this.TransitionLabelBeh1.Size = new System.Drawing.Size(157, 12);
            this.TransitionLabelBeh1.TabIndex = 33;
            this.TransitionLabelBeh1.Text = "behavior 2";
            // 
            // TransitionLabelBeh0
            // 
            this.TransitionLabelBeh0.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBeh0.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBeh0.Location = new System.Drawing.Point(257, 483);
            this.TransitionLabelBeh0.Name = "TransitionLabelBeh0";
            this.TransitionLabelBeh0.Size = new System.Drawing.Size(157, 12);
            this.TransitionLabelBeh0.TabIndex = 32;
            this.TransitionLabelBeh0.Text = "behavior 1";
            // 
            // TransitionGroupBoxBeh0
            // 
            this.TransitionGroupBoxBeh0.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionGroupBoxBeh0.Location = new System.Drawing.Point(215, 485);
            this.TransitionGroupBoxBeh0.Name = "TransitionGroupBoxBeh0";
            this.TransitionGroupBoxBeh0.Size = new System.Drawing.Size(12, 8);
            this.TransitionGroupBoxBeh0.TabIndex = 30;
            this.TransitionGroupBoxBeh0.TabStop = false;
            this.TransitionGroupBoxBeh0.Text = "groupBox6";
            this.TransitionGroupBoxBeh0.Visible = false;
            // 
            // TransitionGroupBoxBeh3
            // 
            this.TransitionGroupBoxBeh3.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionGroupBoxBeh3.Location = new System.Drawing.Point(215, 519);
            this.TransitionGroupBoxBeh3.Name = "TransitionGroupBoxBeh3";
            this.TransitionGroupBoxBeh3.Size = new System.Drawing.Size(12, 8);
            this.TransitionGroupBoxBeh3.TabIndex = 36;
            this.TransitionGroupBoxBeh3.TabStop = false;
            this.TransitionGroupBoxBeh3.Text = "groupBox7";
            this.TransitionGroupBoxBeh3.Visible = false;
            // 
            // TransitionLabelBeh3
            // 
            this.TransitionLabelBeh3.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBeh3.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBeh3.Location = new System.Drawing.Point(257, 517);
            this.TransitionLabelBeh3.Name = "TransitionLabelBeh3";
            this.TransitionLabelBeh3.Size = new System.Drawing.Size(157, 12);
            this.TransitionLabelBeh3.TabIndex = 37;
            this.TransitionLabelBeh3.Text = "behavior 4";
            // 
            // TransitionGroupBoxBeh4
            // 
            this.TransitionGroupBoxBeh4.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionGroupBoxBeh4.Location = new System.Drawing.Point(426, 485);
            this.TransitionGroupBoxBeh4.Name = "TransitionGroupBoxBeh4";
            this.TransitionGroupBoxBeh4.Size = new System.Drawing.Size(12, 8);
            this.TransitionGroupBoxBeh4.TabIndex = 36;
            this.TransitionGroupBoxBeh4.TabStop = false;
            this.TransitionGroupBoxBeh4.Text = "groupBox8";
            this.TransitionGroupBoxBeh4.Visible = false;
            // 
            // TransitionLabelBeh4
            // 
            this.TransitionLabelBeh4.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBeh4.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBeh4.Location = new System.Drawing.Point(468, 483);
            this.TransitionLabelBeh4.Name = "TransitionLabelBeh4";
            this.TransitionLabelBeh4.Size = new System.Drawing.Size(157, 12);
            this.TransitionLabelBeh4.TabIndex = 37;
            this.TransitionLabelBeh4.Text = "behavior 5";
            // 
            // TransitionGroupBoxBeh5
            // 
            this.TransitionGroupBoxBeh5.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionGroupBoxBeh5.Location = new System.Drawing.Point(426, 496);
            this.TransitionGroupBoxBeh5.Name = "TransitionGroupBoxBeh5";
            this.TransitionGroupBoxBeh5.Size = new System.Drawing.Size(12, 8);
            this.TransitionGroupBoxBeh5.TabIndex = 38;
            this.TransitionGroupBoxBeh5.TabStop = false;
            this.TransitionGroupBoxBeh5.Text = "groupBox9";
            this.TransitionGroupBoxBeh5.Visible = false;
            // 
            // TransitionLabelBeh5
            // 
            this.TransitionLabelBeh5.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBeh5.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBeh5.Location = new System.Drawing.Point(468, 494);
            this.TransitionLabelBeh5.Name = "TransitionLabelBeh5";
            this.TransitionLabelBeh5.Size = new System.Drawing.Size(157, 12);
            this.TransitionLabelBeh5.TabIndex = 39;
            this.TransitionLabelBeh5.Text = "behavior 6";
            // 
            // TransitionGroupBoxBeh7
            // 
            this.TransitionGroupBoxBeh7.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionGroupBoxBeh7.Location = new System.Drawing.Point(426, 519);
            this.TransitionGroupBoxBeh7.Name = "TransitionGroupBoxBeh7";
            this.TransitionGroupBoxBeh7.Size = new System.Drawing.Size(12, 8);
            this.TransitionGroupBoxBeh7.TabIndex = 35;
            this.TransitionGroupBoxBeh7.TabStop = false;
            this.TransitionGroupBoxBeh7.Text = "groupBox10";
            this.TransitionGroupBoxBeh7.Visible = false;
            // 
            // TransitionLabelBeh7
            // 
            this.TransitionLabelBeh7.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBeh7.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBeh7.Location = new System.Drawing.Point(468, 517);
            this.TransitionLabelBeh7.Name = "TransitionLabelBeh7";
            this.TransitionLabelBeh7.Size = new System.Drawing.Size(157, 12);
            this.TransitionLabelBeh7.TabIndex = 37;
            this.TransitionLabelBeh7.Text = "behavior 8";
            // 
            // TransitionLabelBeh6
            // 
            this.TransitionLabelBeh6.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBeh6.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBeh6.Location = new System.Drawing.Point(468, 505);
            this.TransitionLabelBeh6.Name = "TransitionLabelBeh6";
            this.TransitionLabelBeh6.Size = new System.Drawing.Size(157, 12);
            this.TransitionLabelBeh6.TabIndex = 36;
            this.TransitionLabelBeh6.Text = "behavior 7";
            // 
            // TransitionGroupBoxBeh6
            // 
            this.TransitionGroupBoxBeh6.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionGroupBoxBeh6.Location = new System.Drawing.Point(426, 507);
            this.TransitionGroupBoxBeh6.Name = "TransitionGroupBoxBeh6";
            this.TransitionGroupBoxBeh6.Size = new System.Drawing.Size(12, 8);
            this.TransitionGroupBoxBeh6.TabIndex = 34;
            this.TransitionGroupBoxBeh6.TabStop = false;
            this.TransitionGroupBoxBeh6.Text = "groupBox11";
            this.TransitionGroupBoxBeh6.Visible = false;
            // 
            // BehaviorNumberLabel
            // 
            this.BehaviorNumberLabel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.BehaviorNumberLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.BehaviorNumberLabel.Location = new System.Drawing.Point(19, 451);
            this.BehaviorNumberLabel.Name = "BehaviorNumberLabel";
            this.BehaviorNumberLabel.Size = new System.Drawing.Size(27, 12);
            this.BehaviorNumberLabel.TabIndex = 40;
            this.BehaviorNumberLabel.Text = "(10)";
            this.BehaviorNumberLabel.TextAlign = System.Drawing.ContentAlignment.TopRight;
            // 
            // TransitionLabelBehNumber0
            // 
            this.TransitionLabelBehNumber0.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBehNumber0.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBehNumber0.Location = new System.Drawing.Point(229, 483);
            this.TransitionLabelBehNumber0.Name = "TransitionLabelBehNumber0";
            this.TransitionLabelBehNumber0.Size = new System.Drawing.Size(27, 12);
            this.TransitionLabelBehNumber0.TabIndex = 41;
            this.TransitionLabelBehNumber0.Text = "(1)";
            this.TransitionLabelBehNumber0.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // TransitionLabelBehNumber1
            // 
            this.TransitionLabelBehNumber1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBehNumber1.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBehNumber1.Location = new System.Drawing.Point(229, 495);
            this.TransitionLabelBehNumber1.Name = "TransitionLabelBehNumber1";
            this.TransitionLabelBehNumber1.Size = new System.Drawing.Size(27, 12);
            this.TransitionLabelBehNumber1.TabIndex = 42;
            this.TransitionLabelBehNumber1.Text = "(2)";
            this.TransitionLabelBehNumber1.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // TransitionLabelBehNumber3
            // 
            this.TransitionLabelBehNumber3.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBehNumber3.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBehNumber3.Location = new System.Drawing.Point(229, 517);
            this.TransitionLabelBehNumber3.Name = "TransitionLabelBehNumber3";
            this.TransitionLabelBehNumber3.Size = new System.Drawing.Size(27, 12);
            this.TransitionLabelBehNumber3.TabIndex = 44;
            this.TransitionLabelBehNumber3.Text = "(4)";
            this.TransitionLabelBehNumber3.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // TransitionLabelBehNumber2
            // 
            this.TransitionLabelBehNumber2.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBehNumber2.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBehNumber2.Location = new System.Drawing.Point(229, 505);
            this.TransitionLabelBehNumber2.Name = "TransitionLabelBehNumber2";
            this.TransitionLabelBehNumber2.Size = new System.Drawing.Size(27, 12);
            this.TransitionLabelBehNumber2.TabIndex = 43;
            this.TransitionLabelBehNumber2.Text = "(3)";
            this.TransitionLabelBehNumber2.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // TransitionLabelBehNumber5
            // 
            this.TransitionLabelBehNumber5.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBehNumber5.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBehNumber5.Location = new System.Drawing.Point(440, 494);
            this.TransitionLabelBehNumber5.Name = "TransitionLabelBehNumber5";
            this.TransitionLabelBehNumber5.Size = new System.Drawing.Size(27, 12);
            this.TransitionLabelBehNumber5.TabIndex = 46;
            this.TransitionLabelBehNumber5.Text = "(6)";
            this.TransitionLabelBehNumber5.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // TransitionLabelBehNumber4
            // 
            this.TransitionLabelBehNumber4.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBehNumber4.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBehNumber4.Location = new System.Drawing.Point(440, 483);
            this.TransitionLabelBehNumber4.Name = "TransitionLabelBehNumber4";
            this.TransitionLabelBehNumber4.Size = new System.Drawing.Size(27, 12);
            this.TransitionLabelBehNumber4.TabIndex = 45;
            this.TransitionLabelBehNumber4.Text = "(5)";
            this.TransitionLabelBehNumber4.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // TransitionLabelBehNumber7
            // 
            this.TransitionLabelBehNumber7.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBehNumber7.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBehNumber7.Location = new System.Drawing.Point(440, 517);
            this.TransitionLabelBehNumber7.Name = "TransitionLabelBehNumber7";
            this.TransitionLabelBehNumber7.Size = new System.Drawing.Size(27, 12);
            this.TransitionLabelBehNumber7.TabIndex = 48;
            this.TransitionLabelBehNumber7.Text = "(8)";
            this.TransitionLabelBehNumber7.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // TransitionLabelBehNumber6
            // 
            this.TransitionLabelBehNumber6.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.TransitionLabelBehNumber6.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TransitionLabelBehNumber6.Location = new System.Drawing.Point(440, 505);
            this.TransitionLabelBehNumber6.Name = "TransitionLabelBehNumber6";
            this.TransitionLabelBehNumber6.Size = new System.Drawing.Size(27, 12);
            this.TransitionLabelBehNumber6.TabIndex = 47;
            this.TransitionLabelBehNumber6.Text = "(7)";
            this.TransitionLabelBehNumber6.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // DurationGroupBoxMax
            // 
            this.DurationGroupBoxMax.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.DurationGroupBoxMax.Location = new System.Drawing.Point(50, 485);
            this.DurationGroupBoxMax.Name = "DurationGroupBoxMax";
            this.DurationGroupBoxMax.Size = new System.Drawing.Size(12, 8);
            this.DurationGroupBoxMax.TabIndex = 23;
            this.DurationGroupBoxMax.TabStop = false;
            this.DurationGroupBoxMax.Text = "groupBox4";
            this.DurationGroupBoxMax.Visible = false;
            // 
            // label2
            // 
            this.label2.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(372, 445);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(198, 13);
            this.label2.TabIndex = 51;
            this.label2.Text = "Number Of Trials Each Transition Period:";
            this.label2.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // NumTrialsButton
            // 
            this.NumTrialsButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.NumTrialsButton.FlatAppearance.MouseDownBackColor = System.Drawing.Color.IndianRed;
            this.NumTrialsButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.RosyBrown;
            this.NumTrialsButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.NumTrialsButton.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.NumTrialsButton.Location = new System.Drawing.Point(576, 441);
            this.NumTrialsButton.Name = "NumTrialsButton";
            this.NumTrialsButton.Size = new System.Drawing.Size(57, 21);
            this.NumTrialsButton.TabIndex = 97;
            this.NumTrialsButton.TabStop = false;
            this.NumTrialsButton.Text = "100";
            this.NumTrialsButton.UseVisualStyleBackColor = true;
            this.NumTrialsButton.Click += new System.EventHandler(this.NumTrialsButton_Click);
            // 
            // label17
            // 
            this.label17.AutoSize = true;
            this.label17.Location = new System.Drawing.Point(266, 18);
            this.label17.Name = "label17";
            this.label17.Size = new System.Drawing.Size(100, 13);
            this.label17.TabIndex = 257;
            this.label17.Text = "THIS GOES AWAY";
            // 
            // FrmInitBeh
            // 
            this.AcceptButton = this.DoneButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(655, 599);
            this.Controls.Add(this.label17);
            this.Controls.Add(this.TransitionGroupBoxBeh7);
            this.Controls.Add(this.TransitionGroupBoxBeh5);
            this.Controls.Add(this.TransitionGroupBoxBeh4);
            this.Controls.Add(this.TransitionGroupBoxBeh6);
            this.Controls.Add(this.TransitionGroupBoxBeh3);
            this.Controls.Add(this.TransitionGroupBoxBeh2);
            this.Controls.Add(this.DurationGroupBoxMin);
            this.Controls.Add(this.TransitionGroupBoxBeh1);
            this.Controls.Add(this.DurationGroupBoxAve);
            this.Controls.Add(this.TransitionGroupBoxBeh0);
            this.Controls.Add(this.DurationGroupBoxMax);
            this.Controls.Add(this.NumTrialsButton);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.TransitionLabelBehNumber7);
            this.Controls.Add(this.TransitionLabelBehNumber6);
            this.Controls.Add(this.TransitionLabelBehNumber5);
            this.Controls.Add(this.TransitionLabelBehNumber4);
            this.Controls.Add(this.TransitionLabelBehNumber3);
            this.Controls.Add(this.TransitionLabelBehNumber2);
            this.Controls.Add(this.TransitionLabelBehNumber1);
            this.Controls.Add(this.TransitionLabelBehNumber0);
            this.Controls.Add(this.BehaviorNumberLabel);
            this.Controls.Add(this.TransitionLabelBeh7);
            this.Controls.Add(this.TransitionLabelBeh5);
            this.Controls.Add(this.TransitionLabelBeh6);
            this.Controls.Add(this.TransitionLabelBeh4);
            this.Controls.Add(this.TransitionLabelBeh3);
            this.Controls.Add(this.TransitionLabelBeh2);
            this.Controls.Add(this.DurationTextBoxMin);
            this.Controls.Add(this.BehaviorNameLabel);
            this.Controls.Add(this.TransitionLabelBeh1);
            this.Controls.Add(this.TransitionLabelBeh0);
            this.Controls.Add(this.DurationTextBoxAve);
            this.Controls.Add(this.DurationTextBoxMax);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.DisplayGroupBox);
            this.Controls.Add(this.RefreshButton);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.BiasFormatErrorLabel);
            this.Controls.Add(this.MatrixDimensionsLabel);
            this.Controls.Add(this.BehaviorCountLabel);
            this.Controls.Add(this.CanceelButton);
            this.Controls.Add(this.MatrixTextBox);
            this.Controls.Add(this.DoneButton);
            this.MaximizeBox = false;
            this.MaximumSize = new System.Drawing.Size(6134, 2742);
            this.MinimizeBox = false;
            this.MinimumSize = new System.Drawing.Size(663, 626);
            this.Name = "FrmInitBeh";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "3MB Initial Behavior Vector Model - Biomimetica";
            this.Resize += new System.EventHandler(this.FormInitialBehavior_Resize);
            this.ResizeEnd += new System.EventHandler(this.FormInitialBehavior_ResizeEnd);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button DoneButton;
        private System.Windows.Forms.TextBox MatrixTextBox;
        private System.Windows.Forms.Button CanceelButton;
        private System.Windows.Forms.Label BehaviorCountLabel;
        private System.Windows.Forms.Label MatrixDimensionsLabel;
        private System.Windows.Forms.Label BiasFormatErrorLabel;
        private System.Windows.Forms.Button RefreshButton;
        private System.Windows.Forms.GroupBox DisplayGroupBox;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.GroupBox DurationGroupBoxAve;
        private System.Windows.Forms.Label DurationTextBoxAve;
        private System.Windows.Forms.Label DurationTextBoxMax;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Label BehaviorNameLabel;
        private System.Windows.Forms.GroupBox DurationGroupBoxMin;
        private System.Windows.Forms.Label DurationTextBoxMin;
        private System.Windows.Forms.GroupBox TransitionGroupBoxBeh2;
        private System.Windows.Forms.Label TransitionLabelBeh2;
        private System.Windows.Forms.GroupBox TransitionGroupBoxBeh1;
        private System.Windows.Forms.Label TransitionLabelBeh1;
        private System.Windows.Forms.Label TransitionLabelBeh0;
        private System.Windows.Forms.GroupBox TransitionGroupBoxBeh0;
        private System.Windows.Forms.GroupBox TransitionGroupBoxBeh3;
        private System.Windows.Forms.Label TransitionLabelBeh3;
        private System.Windows.Forms.GroupBox TransitionGroupBoxBeh4;
        private System.Windows.Forms.Label TransitionLabelBeh4;
        private System.Windows.Forms.GroupBox TransitionGroupBoxBeh5;
        private System.Windows.Forms.Label TransitionLabelBeh5;
        private System.Windows.Forms.GroupBox TransitionGroupBoxBeh7;
        private System.Windows.Forms.Label TransitionLabelBeh7;
        private System.Windows.Forms.Label TransitionLabelBeh6;
        private System.Windows.Forms.GroupBox TransitionGroupBoxBeh6;
        private System.Windows.Forms.Label BehaviorNumberLabel;
        private System.Windows.Forms.Label TransitionLabelBehNumber0;
        private System.Windows.Forms.Label TransitionLabelBehNumber1;
        private System.Windows.Forms.Label TransitionLabelBehNumber3;
        private System.Windows.Forms.Label TransitionLabelBehNumber2;
        private System.Windows.Forms.Label TransitionLabelBehNumber5;
        private System.Windows.Forms.Label TransitionLabelBehNumber4;
        private System.Windows.Forms.Label TransitionLabelBehNumber7;
        private System.Windows.Forms.Label TransitionLabelBehNumber6;
        private System.Windows.Forms.GroupBox DurationGroupBoxMax;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Button NumTrialsButton;
        private System.Windows.Forms.Label label17;
    }
}