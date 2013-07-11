namespace MBSGUI
{
    partial class FormReversal
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
            this.CancllButton = new System.Windows.Forms.Button();
            this.OkButton = new System.Windows.Forms.Button();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.label9 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.MaxCntTextBox = new System.Windows.Forms.TextBox();
            this.crwdbCoeffLabel = new System.Windows.Forms.Label();
            this.crwdbPertLabel = new System.Windows.Forms.Label();
            this.RndStdTimeTextBox = new System.Windows.Forms.TextBox();
            this.MinCntTextBox = new System.Windows.Forms.TextBox();
            this.crwdbArcStepLabel = new System.Windows.Forms.Label();
            this.RandProbTextBox = new System.Windows.Forms.TextBox();
            this.crwdbDirOfBiasLabel = new System.Windows.Forms.Label();
            this.crwdbBiasLabel = new System.Windows.Forms.Label();
            this.RndMeanTimeTextBox = new System.Windows.Forms.TextBox();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.label8 = new System.Windows.Forms.Label();
            this.label7 = new System.Windows.Forms.Label();
            this.MeanCntTextBox = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.GaussStdTimeTextBox = new System.Windows.Forms.TextBox();
            this.StdCntTextBox = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.GaussProbTextBox = new System.Windows.Forms.TextBox();
            this.label4 = new System.Windows.Forms.Label();
            this.GaussMeanTimeTextBox = new System.Windows.Forms.TextBox();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.AscentLabel = new System.Windows.Forms.Label();
            this.MeanAscentRateTextBox = new System.Windows.Forms.TextBox();
            this.MeanAscentLabel = new System.Windows.Forms.Label();
            this.StdAscentRateTextBox = new System.Windows.Forms.TextBox();
            this.StdAscentLabel = new System.Windows.Forms.Label();
            this.CoeffAscentRateTextBox = new System.Windows.Forms.TextBox();
            this.CoefficientAscentGaussLabel = new System.Windows.Forms.Label();
            this.DescentAscentLabel = new System.Windows.Forms.Label();
            this.MeanDescentAscentRateTextBox = new System.Windows.Forms.TextBox();
            this.MeanDescentAscentLabel = new System.Windows.Forms.Label();
            this.StdDescentAscentRateTextBox = new System.Windows.Forms.TextBox();
            this.StdDescentAscentLabel = new System.Windows.Forms.Label();
            this.CoeffDescentAscentRateTextBox = new System.Windows.Forms.TextBox();
            this.CoefficientDescentAscentGaussLabel = new System.Windows.Forms.Label();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.MaxDurLabel = new System.Windows.Forms.Label();
            this.MaxVctrDurTextBox = new System.Windows.Forms.TextBox();
            this.DurTextBox = new System.Windows.Forms.TextBox();
            this.StepDurLabel = new System.Windows.Forms.Label();
            this.StepDurTextBox = new System.Windows.Forms.TextBox();
            this.FrmErrDurLabel = new System.Windows.Forms.Label();
            this.VectorDurLabel = new System.Windows.Forms.Label();
            this.MaxCntLabel = new System.Windows.Forms.Label();
            this.MaxVctrCntTextBox = new System.Windows.Forms.TextBox();
            this.CountVectorTextBox = new System.Windows.Forms.TextBox();
            this.StepCntLabel = new System.Windows.Forms.Label();
            this.StepCntTextBox = new System.Windows.Forms.TextBox();
            this.ProbLabel = new System.Windows.Forms.Label();
            this.ProbTextBox = new System.Windows.Forms.TextBox();
            this.FrmErrCntLabel = new System.Windows.Forms.Label();
            this.VectorCountLabel = new System.Windows.Forms.Label();
            this.RefreshButton = new System.Windows.Forms.Button();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.groupBox4.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.SuspendLayout();
            // 
            // CancllButton
            // 
            this.CancllButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.CancllButton.Location = new System.Drawing.Point(202, 594);
            this.CancllButton.Name = "CancllButton";
            this.CancllButton.Size = new System.Drawing.Size(75, 26);
            this.CancllButton.TabIndex = 23;
            this.CancllButton.Text = "Cancel";
            this.CancllButton.UseVisualStyleBackColor = true;
            this.CancllButton.Click += new System.EventHandler(this.CancelButton_Click);
            // 
            // OkButton
            // 
            this.OkButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.OkButton.Location = new System.Drawing.Point(364, 594);
            this.OkButton.Name = "OkButton";
            this.OkButton.Size = new System.Drawing.Size(75, 26);
            this.OkButton.TabIndex = 4;
            this.OkButton.Text = "OK";
            this.OkButton.UseVisualStyleBackColor = true;
            this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.label9);
            this.groupBox1.Controls.Add(this.label6);
            this.groupBox1.Controls.Add(this.MaxCntTextBox);
            this.groupBox1.Controls.Add(this.crwdbCoeffLabel);
            this.groupBox1.Controls.Add(this.crwdbPertLabel);
            this.groupBox1.Controls.Add(this.RndStdTimeTextBox);
            this.groupBox1.Controls.Add(this.MinCntTextBox);
            this.groupBox1.Controls.Add(this.crwdbArcStepLabel);
            this.groupBox1.Controls.Add(this.RandProbTextBox);
            this.groupBox1.Controls.Add(this.crwdbDirOfBiasLabel);
            this.groupBox1.Controls.Add(this.crwdbBiasLabel);
            this.groupBox1.Controls.Add(this.RndMeanTimeTextBox);
            this.groupBox1.Location = new System.Drawing.Point(229, 135);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(210, 196);
            this.groupBox1.TabIndex = 1;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Random";
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(6, 135);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(103, 13);
            this.label9.TabIndex = 19;
            this.label9.Text = "Time In Reveral Leg";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(6, 60);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(101, 13);
            this.label6.TabIndex = 16;
            this.label6.Text = "Number of Reverals";
            // 
            // MaxCntTextBox
            // 
            this.MaxCntTextBox.Location = new System.Drawing.Point(155, 98);
            this.MaxCntTextBox.Name = "MaxCntTextBox";
            this.MaxCntTextBox.Size = new System.Drawing.Size(49, 20);
            this.MaxCntTextBox.TabIndex = 0;
            this.MaxCntTextBox.TextChanged += new System.EventHandler(this.MaxCntTextBox_TextChanged);
            // 
            // crwdbCoeffLabel
            // 
            this.crwdbCoeffLabel.AutoSize = true;
            this.crwdbCoeffLabel.Location = new System.Drawing.Point(108, 173);
            this.crwdbCoeffLabel.Name = "crwdbCoeffLabel";
            this.crwdbCoeffLabel.Size = new System.Drawing.Size(41, 13);
            this.crwdbCoeffLabel.TabIndex = 15;
            this.crwdbCoeffLabel.Text = "std. (s):";
            // 
            // crwdbPertLabel
            // 
            this.crwdbPertLabel.AutoSize = true;
            this.crwdbPertLabel.Location = new System.Drawing.Point(63, 102);
            this.crwdbPertLabel.Name = "crwdbPertLabel";
            this.crwdbPertLabel.Size = new System.Drawing.Size(86, 13);
            this.crwdbPertLabel.TabIndex = 7;
            this.crwdbPertLabel.Text = "maximum (count)";
            // 
            // RndStdTimeTextBox
            // 
            this.RndStdTimeTextBox.Location = new System.Drawing.Point(155, 170);
            this.RndStdTimeTextBox.Name = "RndStdTimeTextBox";
            this.RndStdTimeTextBox.Size = new System.Drawing.Size(49, 20);
            this.RndStdTimeTextBox.TabIndex = 4;
            this.RndStdTimeTextBox.TextChanged += new System.EventHandler(this.RndStdTimeTextBox_TextChanged);
            // 
            // MinCntTextBox
            // 
            this.MinCntTextBox.Location = new System.Drawing.Point(155, 72);
            this.MinCntTextBox.Name = "MinCntTextBox";
            this.MinCntTextBox.Size = new System.Drawing.Size(49, 20);
            this.MinCntTextBox.TabIndex = 1;
            this.MinCntTextBox.TextChanged += new System.EventHandler(this.MinCntTextBox_TextChanged);
            // 
            // crwdbArcStepLabel
            // 
            this.crwdbArcStepLabel.AutoSize = true;
            this.crwdbArcStepLabel.Location = new System.Drawing.Point(99, 147);
            this.crwdbArcStepLabel.Name = "crwdbArcStepLabel";
            this.crwdbArcStepLabel.Size = new System.Drawing.Size(50, 13);
            this.crwdbArcStepLabel.TabIndex = 13;
            this.crwdbArcStepLabel.Text = "mean (s):";
            // 
            // RandProbTextBox
            // 
            this.RandProbTextBox.Location = new System.Drawing.Point(128, 33);
            this.RandProbTextBox.Name = "RandProbTextBox";
            this.RandProbTextBox.Size = new System.Drawing.Size(37, 20);
            this.RandProbTextBox.TabIndex = 2;
            this.RandProbTextBox.TextChanged += new System.EventHandler(this.RandProbTextBox_TextChanged);
            // 
            // crwdbDirOfBiasLabel
            // 
            this.crwdbDirOfBiasLabel.AutoSize = true;
            this.crwdbDirOfBiasLabel.Location = new System.Drawing.Point(63, 76);
            this.crwdbDirOfBiasLabel.Name = "crwdbDirOfBiasLabel";
            this.crwdbDirOfBiasLabel.Size = new System.Drawing.Size(86, 13);
            this.crwdbDirOfBiasLabel.TabIndex = 9;
            this.crwdbDirOfBiasLabel.Text = "minimum (count):";
            // 
            // crwdbBiasLabel
            // 
            this.crwdbBiasLabel.AutoSize = true;
            this.crwdbBiasLabel.Location = new System.Drawing.Point(6, 16);
            this.crwdbBiasLabel.Name = "crwdbBiasLabel";
            this.crwdbBiasLabel.Size = new System.Drawing.Size(167, 13);
            this.crwdbBiasLabel.TabIndex = 11;
            this.crwdbBiasLabel.Text = "probability of reversal while diving:";
            // 
            // RndMeanTimeTextBox
            // 
            this.RndMeanTimeTextBox.Location = new System.Drawing.Point(155, 144);
            this.RndMeanTimeTextBox.Name = "RndMeanTimeTextBox";
            this.RndMeanTimeTextBox.Size = new System.Drawing.Size(49, 20);
            this.RndMeanTimeTextBox.TabIndex = 3;
            this.RndMeanTimeTextBox.TextChanged += new System.EventHandler(this.RndMeanTimeTextBox_TextChanged);
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.label8);
            this.groupBox2.Controls.Add(this.label7);
            this.groupBox2.Controls.Add(this.MeanCntTextBox);
            this.groupBox2.Controls.Add(this.label1);
            this.groupBox2.Controls.Add(this.label2);
            this.groupBox2.Controls.Add(this.GaussStdTimeTextBox);
            this.groupBox2.Controls.Add(this.StdCntTextBox);
            this.groupBox2.Controls.Add(this.label3);
            this.groupBox2.Controls.Add(this.label5);
            this.groupBox2.Controls.Add(this.GaussProbTextBox);
            this.groupBox2.Controls.Add(this.label4);
            this.groupBox2.Controls.Add(this.GaussMeanTimeTextBox);
            this.groupBox2.Location = new System.Drawing.Point(12, 135);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(211, 196);
            this.groupBox2.TabIndex = 0;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Gaussian";
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(6, 135);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(103, 13);
            this.label8.TabIndex = 18;
            this.label8.Text = "Time In Reveral Leg";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(6, 60);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(101, 13);
            this.label7.TabIndex = 17;
            this.label7.Text = "Number of Reverals";
            // 
            // MeanCntTextBox
            // 
            this.MeanCntTextBox.Location = new System.Drawing.Point(156, 72);
            this.MeanCntTextBox.Name = "MeanCntTextBox";
            this.MeanCntTextBox.Size = new System.Drawing.Size(49, 20);
            this.MeanCntTextBox.TabIndex = 0;
            this.MeanCntTextBox.TextChanged += new System.EventHandler(this.MeanCntTextBox_TextChanged);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(109, 173);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(41, 13);
            this.label1.TabIndex = 15;
            this.label1.Text = "std. (s):";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(75, 76);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(75, 13);
            this.label2.TabIndex = 7;
            this.label2.Text = "mean  (count):";
            // 
            // GaussStdTimeTextBox
            // 
            this.GaussStdTimeTextBox.Location = new System.Drawing.Point(156, 170);
            this.GaussStdTimeTextBox.Name = "GaussStdTimeTextBox";
            this.GaussStdTimeTextBox.Size = new System.Drawing.Size(49, 20);
            this.GaussStdTimeTextBox.TabIndex = 4;
            this.GaussStdTimeTextBox.TextChanged += new System.EventHandler(this.GaussStdTimeTextBox_TextChanged);
            // 
            // StdCntTextBox
            // 
            this.StdCntTextBox.Location = new System.Drawing.Point(156, 98);
            this.StdCntTextBox.Name = "StdCntTextBox";
            this.StdCntTextBox.Size = new System.Drawing.Size(49, 20);
            this.StdCntTextBox.TabIndex = 1;
            this.StdCntTextBox.TextChanged += new System.EventHandler(this.StdCntTextBox_TextChanged);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(100, 147);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(50, 13);
            this.label3.TabIndex = 13;
            this.label3.Text = "mean (s):";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(6, 16);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(167, 13);
            this.label5.TabIndex = 11;
            this.label5.Text = "probability of reversal while diving:";
            // 
            // GaussProbTextBox
            // 
            this.GaussProbTextBox.Location = new System.Drawing.Point(131, 33);
            this.GaussProbTextBox.Name = "GaussProbTextBox";
            this.GaussProbTextBox.Size = new System.Drawing.Size(37, 20);
            this.GaussProbTextBox.TabIndex = 2;
            this.GaussProbTextBox.TextChanged += new System.EventHandler(this.GaussProbTextBox_TextChanged);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(90, 102);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(60, 13);
            this.label4.TabIndex = 9;
            this.label4.Text = "std (count):";
            // 
            // GaussMeanTimeTextBox
            // 
            this.GaussMeanTimeTextBox.Location = new System.Drawing.Point(156, 144);
            this.GaussMeanTimeTextBox.Name = "GaussMeanTimeTextBox";
            this.GaussMeanTimeTextBox.Size = new System.Drawing.Size(49, 20);
            this.GaussMeanTimeTextBox.TabIndex = 3;
            this.GaussMeanTimeTextBox.TextChanged += new System.EventHandler(this.GaussMeanTimeTextBox_TextChanged);
            // 
            // groupBox4
            // 
            this.groupBox4.Controls.Add(this.AscentLabel);
            this.groupBox4.Controls.Add(this.MeanAscentRateTextBox);
            this.groupBox4.Controls.Add(this.MeanAscentLabel);
            this.groupBox4.Controls.Add(this.StdAscentRateTextBox);
            this.groupBox4.Controls.Add(this.StdAscentLabel);
            this.groupBox4.Controls.Add(this.CoeffAscentRateTextBox);
            this.groupBox4.Controls.Add(this.CoefficientAscentGaussLabel);
            this.groupBox4.Controls.Add(this.DescentAscentLabel);
            this.groupBox4.Controls.Add(this.MeanDescentAscentRateTextBox);
            this.groupBox4.Controls.Add(this.MeanDescentAscentLabel);
            this.groupBox4.Controls.Add(this.StdDescentAscentRateTextBox);
            this.groupBox4.Controls.Add(this.StdDescentAscentLabel);
            this.groupBox4.Controls.Add(this.CoeffDescentAscentRateTextBox);
            this.groupBox4.Controls.Add(this.CoefficientDescentAscentGaussLabel);
            this.groupBox4.Location = new System.Drawing.Point(12, 17);
            this.groupBox4.Name = "groupBox4";
            this.groupBox4.Size = new System.Drawing.Size(427, 106);
            this.groupBox4.TabIndex = 3;
            this.groupBox4.TabStop = false;
            this.groupBox4.Text = "Reversal Dive Rate";
            // 
            // AscentLabel
            // 
            this.AscentLabel.AutoSize = true;
            this.AscentLabel.Location = new System.Drawing.Point(12, 61);
            this.AscentLabel.Name = "AscentLabel";
            this.AscentLabel.Size = new System.Drawing.Size(40, 13);
            this.AscentLabel.TabIndex = 26;
            this.AscentLabel.Text = "Ascent";
            // 
            // MeanAscentRateTextBox
            // 
            this.MeanAscentRateTextBox.Location = new System.Drawing.Point(97, 75);
            this.MeanAscentRateTextBox.Name = "MeanAscentRateTextBox";
            this.MeanAscentRateTextBox.Size = new System.Drawing.Size(43, 20);
            this.MeanAscentRateTextBox.TabIndex = 20;
            this.MeanAscentRateTextBox.TextChanged += new System.EventHandler(this.MeanAscentRateTextBox_TextChanged);
            // 
            // MeanAscentLabel
            // 
            this.MeanAscentLabel.AutoSize = true;
            this.MeanAscentLabel.Location = new System.Drawing.Point(25, 78);
            this.MeanAscentLabel.Name = "MeanAscentLabel";
            this.MeanAscentLabel.Size = new System.Drawing.Size(66, 13);
            this.MeanAscentLabel.TabIndex = 23;
            this.MeanAscentLabel.Text = "mean  (m/s):";
            // 
            // StdAscentRateTextBox
            // 
            this.StdAscentRateTextBox.Location = new System.Drawing.Point(203, 75);
            this.StdAscentRateTextBox.Name = "StdAscentRateTextBox";
            this.StdAscentRateTextBox.Size = new System.Drawing.Size(43, 20);
            this.StdAscentRateTextBox.TabIndex = 21;
            this.StdAscentRateTextBox.TextChanged += new System.EventHandler(this.StdAscentRateTextBox_TextChanged);
            // 
            // StdAscentLabel
            // 
            this.StdAscentLabel.AutoSize = true;
            this.StdAscentLabel.Location = new System.Drawing.Point(147, 79);
            this.StdAscentLabel.Name = "StdAscentLabel";
            this.StdAscentLabel.Size = new System.Drawing.Size(54, 13);
            this.StdAscentLabel.TabIndex = 24;
            this.StdAscentLabel.Text = "std. (m/s):";
            // 
            // CoeffAscentRateTextBox
            // 
            this.CoeffAscentRateTextBox.Location = new System.Drawing.Point(378, 75);
            this.CoeffAscentRateTextBox.Name = "CoeffAscentRateTextBox";
            this.CoeffAscentRateTextBox.Size = new System.Drawing.Size(43, 20);
            this.CoeffAscentRateTextBox.TabIndex = 22;
            this.CoeffAscentRateTextBox.TextChanged += new System.EventHandler(this.CoeffAscentRateTextBox_TextChanged);
            // 
            // CoefficientAscentGaussLabel
            // 
            this.CoefficientAscentGaussLabel.AutoSize = true;
            this.CoefficientAscentGaussLabel.Location = new System.Drawing.Point(260, 79);
            this.CoefficientAscentGaussLabel.Name = "CoefficientAscentGaussLabel";
            this.CoefficientAscentGaussLabel.Size = new System.Drawing.Size(113, 13);
            this.CoefficientAscentGaussLabel.TabIndex = 25;
            this.CoefficientAscentGaussLabel.Text = "termination coefficient:";
            // 
            // DescentAscentLabel
            // 
            this.DescentAscentLabel.AutoSize = true;
            this.DescentAscentLabel.Location = new System.Drawing.Point(11, 21);
            this.DescentAscentLabel.Name = "DescentAscentLabel";
            this.DescentAscentLabel.Size = new System.Drawing.Size(85, 13);
            this.DescentAscentLabel.TabIndex = 19;
            this.DescentAscentLabel.Text = "Descent/Ascent";
            // 
            // MeanDescentAscentRateTextBox
            // 
            this.MeanDescentAscentRateTextBox.Location = new System.Drawing.Point(96, 35);
            this.MeanDescentAscentRateTextBox.Name = "MeanDescentAscentRateTextBox";
            this.MeanDescentAscentRateTextBox.Size = new System.Drawing.Size(43, 20);
            this.MeanDescentAscentRateTextBox.TabIndex = 1;
            this.MeanDescentAscentRateTextBox.TextChanged += new System.EventHandler(this.MeanDiveRateTextBox_TextChanged);
            // 
            // MeanDescentAscentLabel
            // 
            this.MeanDescentAscentLabel.AutoSize = true;
            this.MeanDescentAscentLabel.Location = new System.Drawing.Point(24, 38);
            this.MeanDescentAscentLabel.Name = "MeanDescentAscentLabel";
            this.MeanDescentAscentLabel.Size = new System.Drawing.Size(66, 13);
            this.MeanDescentAscentLabel.TabIndex = 7;
            this.MeanDescentAscentLabel.Text = "mean  (m/s):";
            // 
            // StdDescentAscentRateTextBox
            // 
            this.StdDescentAscentRateTextBox.Location = new System.Drawing.Point(202, 35);
            this.StdDescentAscentRateTextBox.Name = "StdDescentAscentRateTextBox";
            this.StdDescentAscentRateTextBox.Size = new System.Drawing.Size(43, 20);
            this.StdDescentAscentRateTextBox.TabIndex = 2;
            this.StdDescentAscentRateTextBox.TextChanged += new System.EventHandler(this.StdDiveRateTextBox_TextChanged);
            // 
            // StdDescentAscentLabel
            // 
            this.StdDescentAscentLabel.AutoSize = true;
            this.StdDescentAscentLabel.Location = new System.Drawing.Point(146, 39);
            this.StdDescentAscentLabel.Name = "StdDescentAscentLabel";
            this.StdDescentAscentLabel.Size = new System.Drawing.Size(54, 13);
            this.StdDescentAscentLabel.TabIndex = 9;
            this.StdDescentAscentLabel.Text = "std. (m/s):";
            // 
            // CoeffDescentAscentRateTextBox
            // 
            this.CoeffDescentAscentRateTextBox.Location = new System.Drawing.Point(377, 35);
            this.CoeffDescentAscentRateTextBox.Name = "CoeffDescentAscentRateTextBox";
            this.CoeffDescentAscentRateTextBox.Size = new System.Drawing.Size(43, 20);
            this.CoeffDescentAscentRateTextBox.TabIndex = 3;
            this.CoeffDescentAscentRateTextBox.TextChanged += new System.EventHandler(this.CoeffDiveRateTextBox_TextChanged);
            // 
            // CoefficientDescentAscentGaussLabel
            // 
            this.CoefficientDescentAscentGaussLabel.AutoSize = true;
            this.CoefficientDescentAscentGaussLabel.Location = new System.Drawing.Point(259, 39);
            this.CoefficientDescentAscentGaussLabel.Name = "CoefficientDescentAscentGaussLabel";
            this.CoefficientDescentAscentGaussLabel.Size = new System.Drawing.Size(113, 13);
            this.CoefficientDescentAscentGaussLabel.TabIndex = 11;
            this.CoefficientDescentAscentGaussLabel.Text = "termination coefficient:";
            // 
            // groupBox3
            // 
            this.groupBox3.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.groupBox3.Controls.Add(this.MaxDurLabel);
            this.groupBox3.Controls.Add(this.MaxVctrDurTextBox);
            this.groupBox3.Controls.Add(this.DurTextBox);
            this.groupBox3.Controls.Add(this.StepDurLabel);
            this.groupBox3.Controls.Add(this.StepDurTextBox);
            this.groupBox3.Controls.Add(this.FrmErrDurLabel);
            this.groupBox3.Controls.Add(this.VectorDurLabel);
            this.groupBox3.Controls.Add(this.MaxCntLabel);
            this.groupBox3.Controls.Add(this.MaxVctrCntTextBox);
            this.groupBox3.Controls.Add(this.CountVectorTextBox);
            this.groupBox3.Controls.Add(this.StepCntLabel);
            this.groupBox3.Controls.Add(this.StepCntTextBox);
            this.groupBox3.Controls.Add(this.ProbLabel);
            this.groupBox3.Controls.Add(this.ProbTextBox);
            this.groupBox3.Controls.Add(this.FrmErrCntLabel);
            this.groupBox3.Controls.Add(this.VectorCountLabel);
            this.groupBox3.Location = new System.Drawing.Point(12, 337);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(427, 250);
            this.groupBox3.TabIndex = 25;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Vector Modeling";
            // 
            // MaxDurLabel
            // 
            this.MaxDurLabel.AutoSize = true;
            this.MaxDurLabel.Location = new System.Drawing.Point(132, 207);
            this.MaxDurLabel.Name = "MaxDurLabel";
            this.MaxDurLabel.Size = new System.Drawing.Size(111, 13);
            this.MaxDurLabel.TabIndex = 30;
            this.MaxDurLabel.Text = "maximum leg duration:";
            // 
            // MaxVctrDurTextBox
            // 
            this.MaxVctrDurTextBox.Location = new System.Drawing.Point(249, 204);
            this.MaxVctrDurTextBox.Name = "MaxVctrDurTextBox";
            this.MaxVctrDurTextBox.ReadOnly = true;
            this.MaxVctrDurTextBox.Size = new System.Drawing.Size(37, 20);
            this.MaxVctrDurTextBox.TabIndex = 29;
            // 
            // DurTextBox
            // 
            this.DurTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.DurTextBox.Location = new System.Drawing.Point(14, 180);
            this.DurTextBox.Name = "DurTextBox";
            this.DurTextBox.Size = new System.Drawing.Size(406, 20);
            this.DurTextBox.TabIndex = 23;
            this.DurTextBox.TextChanged += new System.EventHandler(this.DurTextBox_TextChanged);
            // 
            // StepDurLabel
            // 
            this.StepDurLabel.AutoSize = true;
            this.StepDurLabel.Location = new System.Drawing.Point(11, 207);
            this.StepDurLabel.Name = "StepDurLabel";
            this.StepDurLabel.Size = new System.Drawing.Size(65, 13);
            this.StepDurLabel.TabIndex = 28;
            this.StepDurLabel.Text = "step size (s):";
            // 
            // StepDurTextBox
            // 
            this.StepDurTextBox.Location = new System.Drawing.Point(82, 204);
            this.StepDurTextBox.Name = "StepDurTextBox";
            this.StepDurTextBox.Size = new System.Drawing.Size(37, 20);
            this.StepDurTextBox.TabIndex = 27;
            this.StepDurTextBox.TextChanged += new System.EventHandler(this.StepDurTextBox_TextChanged);
            // 
            // FrmErrDurLabel
            // 
            this.FrmErrDurLabel.BackColor = System.Drawing.SystemColors.Control;
            this.FrmErrDurLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FrmErrDurLabel.Location = new System.Drawing.Point(168, 164);
            this.FrmErrDurLabel.Name = "FrmErrDurLabel";
            this.FrmErrDurLabel.Size = new System.Drawing.Size(63, 17);
            this.FrmErrDurLabel.TabIndex = 25;
            this.FrmErrDurLabel.Text = "Format Error";
            this.FrmErrDurLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // VectorDurLabel
            // 
            this.VectorDurLabel.AutoSize = true;
            this.VectorDurLabel.Location = new System.Drawing.Point(14, 164);
            this.VectorDurLabel.Name = "VectorDurLabel";
            this.VectorDurLabel.Size = new System.Drawing.Size(151, 13);
            this.VectorDurLabel.TabIndex = 24;
            this.VectorDurLabel.Text = "Reversal Leg Duration [1x200]";
            // 
            // MaxCntLabel
            // 
            this.MaxCntLabel.AutoSize = true;
            this.MaxCntLabel.Location = new System.Drawing.Point(143, 108);
            this.MaxCntLabel.Name = "MaxCntLabel";
            this.MaxCntLabel.Size = new System.Drawing.Size(164, 13);
            this.MaxCntLabel.TabIndex = 22;
            this.MaxCntLabel.Text = "maximum reversal count per dive:";
            // 
            // MaxVctrCntTextBox
            // 
            this.MaxVctrCntTextBox.Location = new System.Drawing.Point(309, 106);
            this.MaxVctrCntTextBox.Name = "MaxVctrCntTextBox";
            this.MaxVctrCntTextBox.ReadOnly = true;
            this.MaxVctrCntTextBox.Size = new System.Drawing.Size(37, 20);
            this.MaxVctrCntTextBox.TabIndex = 21;
            this.MaxVctrCntTextBox.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // CountVectorTextBox
            // 
            this.CountVectorTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.CountVectorTextBox.Location = new System.Drawing.Point(9, 77);
            this.CountVectorTextBox.Name = "CountVectorTextBox";
            this.CountVectorTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Horizontal;
            this.CountVectorTextBox.Size = new System.Drawing.Size(409, 20);
            this.CountVectorTextBox.TabIndex = 0;
            this.CountVectorTextBox.TextChanged += new System.EventHandler(this.CountVectorTextBox_TextChanged);
            // 
            // StepCntLabel
            // 
            this.StepCntLabel.AutoSize = true;
            this.StepCntLabel.Location = new System.Drawing.Point(6, 105);
            this.StepCntLabel.Name = "StepCntLabel";
            this.StepCntLabel.Size = new System.Drawing.Size(87, 13);
            this.StepCntLabel.TabIndex = 20;
            this.StepCntLabel.Text = "step size (count):";
            // 
            // StepCntTextBox
            // 
            this.StepCntTextBox.Location = new System.Drawing.Point(93, 103);
            this.StepCntTextBox.Name = "StepCntTextBox";
            this.StepCntTextBox.ReadOnly = true;
            this.StepCntTextBox.Size = new System.Drawing.Size(37, 20);
            this.StepCntTextBox.TabIndex = 19;
            this.StepCntTextBox.Text = "1";
            this.StepCntTextBox.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // ProbLabel
            // 
            this.ProbLabel.AutoSize = true;
            this.ProbLabel.Location = new System.Drawing.Point(6, 19);
            this.ProbLabel.Name = "ProbLabel";
            this.ProbLabel.Size = new System.Drawing.Size(167, 13);
            this.ProbLabel.TabIndex = 17;
            this.ProbLabel.Text = "probability of reversal while diving:";
            // 
            // ProbTextBox
            // 
            this.ProbTextBox.Location = new System.Drawing.Point(174, 16);
            this.ProbTextBox.Name = "ProbTextBox";
            this.ProbTextBox.Size = new System.Drawing.Size(37, 20);
            this.ProbTextBox.TabIndex = 14;
            this.ProbTextBox.TextChanged += new System.EventHandler(this.ProbTextBox_TextChanged);
            // 
            // FrmErrCntLabel
            // 
            this.FrmErrCntLabel.BackColor = System.Drawing.SystemColors.Control;
            this.FrmErrCntLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FrmErrCntLabel.Location = new System.Drawing.Point(168, 59);
            this.FrmErrCntLabel.Name = "FrmErrCntLabel";
            this.FrmErrCntLabel.Size = new System.Drawing.Size(63, 17);
            this.FrmErrCntLabel.TabIndex = 16;
            this.FrmErrCntLabel.Text = "Format Error";
            this.FrmErrCntLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // VectorCountLabel
            // 
            this.VectorCountLabel.AutoSize = true;
            this.VectorCountLabel.Location = new System.Drawing.Point(14, 61);
            this.VectorCountLabel.Name = "VectorCountLabel";
            this.VectorCountLabel.Size = new System.Drawing.Size(118, 13);
            this.VectorCountLabel.TabIndex = 15;
            this.VectorCountLabel.Text = "Reversal Count [1x200]";
            // 
            // RefreshButton
            // 
            this.RefreshButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.RefreshButton.Location = new System.Drawing.Point(283, 594);
            this.RefreshButton.Name = "RefreshButton";
            this.RefreshButton.Size = new System.Drawing.Size(75, 26);
            this.RefreshButton.TabIndex = 26;
            this.RefreshButton.Text = "Refresh";
            this.RefreshButton.UseVisualStyleBackColor = true;
            this.RefreshButton.Click += new System.EventHandler(this.DurRefreshButton_Click);
            // 
            // FormReversal
            // 
            this.AcceptButton = this.OkButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(451, 632);
            this.Controls.Add(this.groupBox3);
            this.Controls.Add(this.groupBox4);
            this.Controls.Add(this.RefreshButton);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.CancllButton);
            this.Controls.Add(this.OkButton);
            this.Controls.Add(this.groupBox1);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.MinimumSize = new System.Drawing.Size(459, 611);
            this.Name = "FormReversal";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "3MB Reversals Description - Biomimetica";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.groupBox4.ResumeLayout(false);
            this.groupBox4.PerformLayout();
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button CancllButton;
        private System.Windows.Forms.Button OkButton;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.TextBox MaxCntTextBox;
        private System.Windows.Forms.Label crwdbCoeffLabel;
        private System.Windows.Forms.Label crwdbPertLabel;
        private System.Windows.Forms.TextBox RndStdTimeTextBox;
        private System.Windows.Forms.TextBox MinCntTextBox;
        private System.Windows.Forms.Label crwdbArcStepLabel;
        private System.Windows.Forms.Label crwdbDirOfBiasLabel;
        private System.Windows.Forms.TextBox RndMeanTimeTextBox;
        private System.Windows.Forms.TextBox RandProbTextBox;
        private System.Windows.Forms.Label crwdbBiasLabel;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.TextBox MeanCntTextBox;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox GaussStdTimeTextBox;
        private System.Windows.Forms.TextBox StdCntTextBox;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox GaussMeanTimeTextBox;
        private System.Windows.Forms.TextBox GaussProbTextBox;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.GroupBox groupBox4;
        private System.Windows.Forms.TextBox MeanDescentAscentRateTextBox;
        private System.Windows.Forms.Label MeanDescentAscentLabel;
        private System.Windows.Forms.TextBox StdDescentAscentRateTextBox;
        private System.Windows.Forms.Label StdDescentAscentLabel;
        private System.Windows.Forms.TextBox CoeffDescentAscentRateTextBox;
        private System.Windows.Forms.Label CoefficientDescentAscentGaussLabel;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.Label MaxCntLabel;
        private System.Windows.Forms.TextBox MaxVctrCntTextBox;
        private System.Windows.Forms.TextBox CountVectorTextBox;
        private System.Windows.Forms.Label StepCntLabel;
        private System.Windows.Forms.TextBox StepCntTextBox;
        private System.Windows.Forms.Label ProbLabel;
        private System.Windows.Forms.TextBox ProbTextBox;
        private System.Windows.Forms.Label FrmErrCntLabel;
        private System.Windows.Forms.Label VectorCountLabel;
        private System.Windows.Forms.Label MaxDurLabel;
        private System.Windows.Forms.TextBox MaxVctrDurTextBox;
        private System.Windows.Forms.Button RefreshButton;
        private System.Windows.Forms.TextBox DurTextBox;
        private System.Windows.Forms.Label StepDurLabel;
        private System.Windows.Forms.TextBox StepDurTextBox;
        private System.Windows.Forms.Label FrmErrDurLabel;
        private System.Windows.Forms.Label VectorDurLabel;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Label DescentAscentLabel;
        private System.Windows.Forms.Label AscentLabel;
        private System.Windows.Forms.TextBox MeanAscentRateTextBox;
        private System.Windows.Forms.Label MeanAscentLabel;
        private System.Windows.Forms.TextBox StdAscentRateTextBox;
        private System.Windows.Forms.Label StdAscentLabel;
        private System.Windows.Forms.TextBox CoeffAscentRateTextBox;
        private System.Windows.Forms.Label CoefficientAscentGaussLabel;
    }
}