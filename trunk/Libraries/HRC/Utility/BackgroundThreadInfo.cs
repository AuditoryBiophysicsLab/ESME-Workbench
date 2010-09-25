using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace HRC.Utility
{
    public enum ThreadInfoTextField
    {
        ElapsedRemainingTime,
        Description
    }
    public partial class BackgroundThreadInfo : UserControl
    {
        private ImprovedBackgroundWorker backgroundWorker;
        private DateTime startTime;
        private int ticks = 0;
        private int tickMod;
        private int progressPercent;
        private delegate void SetTextCallback(ThreadInfoTextField whichField, string Text);
        private delegate void SetProgressCallback(int ProgressPercent);

        public BackgroundThreadInfo(ImprovedBackgroundWorker BackgroundWorker)
        {
            InitializeComponent();
            this.backgroundWorker = BackgroundWorker;
            Description = backgroundWorker.TaskName;
            backgroundWorker.ProgressChanged += new ProgressChangedEventHandler(backgroundWorker_ProgressChanged);
            startTime = DateTime.Now;
            tickMod = 1000 / timer1.Interval;
            this.progress.Minimum = 0;
            this.progress.Maximum = 100;
        }

        private void SetText(ThreadInfoTextField whichField, string Text)
        {
            if (this.InvokeRequired)
            {
                SetTextCallback d = new SetTextCallback(SetText);
                this.Invoke(d, new object[] {whichField, Text});
            }
            else
            {
                switch (whichField)
                {
                    case ThreadInfoTextField.ElapsedRemainingTime:
                        lblElapsedRemaining.Text = Text;
                        break;
                    case ThreadInfoTextField.Description:
                        lblDescription.Text = Text;
                        break;
                }
            }
        }

        private void SetProgressPercent(int ProgressPercent)
        {
            if (this.InvokeRequired)
            {
                SetProgressCallback d = new SetProgressCallback(SetProgressPercent);
                this.Invoke(d, new object[] { ProgressPercent });
            }
            else
                this.progress.Value = ProgressPercent;
        }

        private void backgroundWorker_ProgressChanged(object sender, ProgressChangedEventArgs e)
        {
            progressPercent = e.ProgressPercentage;
            SetProgressPercent(progressPercent);
            ElapsedRemainingTime();
        }

        public string Description { get { return lblDescription.Text; } set { SetText(ThreadInfoTextField.Description, value); } }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            if (backgroundWorker.WorkerSupportsCancellation)
            {
                backgroundWorker.CancelAsync();
            }
        }

        private void ElapsedRemainingTime()
        {
            string Text;
            TimeSpan elapsedTime = DateTime.Now - startTime;
            TimeSpan remainingTime;
            float SecondsElapsed, SecondsRemaining;
            Text = "Elapsed: " +
                elapsedTime.Hours.ToString("00") + ":" + elapsedTime.Minutes.ToString("00") + ":" + elapsedTime.Seconds.ToString("00");
            if (backgroundWorker.WorkerReportsProgress)
            {
                SecondsElapsed = (float)elapsedTime.TotalSeconds;
                SecondsRemaining = (((float)(100 - progressPercent)) / ((float)progressPercent)) * SecondsElapsed;
                remainingTime = new TimeSpan((long)(SecondsRemaining * 10000000));
                Text += "  Remaining (estimated): " +
                    remainingTime.Hours.ToString("00") + ":" + remainingTime.Minutes.ToString("00") + ":" + remainingTime.Seconds.ToString("00");
            }
            SetText(ThreadInfoTextField.ElapsedRemainingTime, Text);
        }

        private void timer1_Tick(object sender, EventArgs e)
        {
            if (backgroundWorker.TaskName != null)
                Description = backgroundWorker.TaskName;
            else
                Description = "Background task running...";

            if (backgroundWorker.WorkerSupportsCancellation)
            {
                btnCancel.Visible = true;
                if (backgroundWorker.CancellationPending)
                {
                    btnCancel.Text = "Canceling...";
                    btnCancel.Enabled = false;
                }
                else
                {
                    btnCancel.Text = "Cancel";
                    btnCancel.Enabled = true;
                }
            }
            else
            {
                btnCancel.Visible = false;
                btnCancel.Enabled = false;
            }

            if (backgroundWorker.WorkerReportsProgress)
            {
                if (btnCancel.Visible)
                    progress.Width = btnCancel.Left - progress.Left - 10;
                else
                    progress.Width = btnCancel.Right - progress.Left;
            }

            if (tickMod == 0)
            {
                ElapsedRemainingTime();
                if (backgroundWorker.WorkerReportsProgress)
                    ElapsedRemainingTime();
                else
                {
                    lblElapsedRemaining.Text = "Progress reporting not supported by this task, time estimate not available";
                }
            }
            ticks = (ticks + 1) % tickMod;
        }
    }
}
