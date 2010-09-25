using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Collections;

namespace HRC.Utility
{
    public partial class MultiProgressDialog : Form
    {
        private ArrayList backgroundThreadInfoArray = new ArrayList();
        private delegate void AutoLayoutDelegate();
        private delegate void ClearControlsDelegate();
        private delegate void RemoveControlDelegate(Control whichControl);
        private delegate void SetTitleDelegate(string newTitle);
        private delegate void RegisterBackgroundWorkerDelegate(ImprovedBackgroundWorker backgroundWorker);

        public MultiProgressDialog()
        {
            InitializeComponent();
        }

        public void RegisterBackgroundWorker(ImprovedBackgroundWorker backgroundWorker)
        {
            if (this.InvokeRequired)
            {
                RegisterBackgroundWorkerDelegate d = new RegisterBackgroundWorkerDelegate(RegisterBackgroundWorker);
                this.Invoke(d, new object[] { backgroundWorker });
            }
            else
            {
                BackgroundThreadInfo info = new BackgroundThreadInfo(backgroundWorker);
                int newWidth, newHeight;

                newWidth = this.ClientSize.Width;
                newHeight = this.ClientSize.Height;
                if (this.ClientSize.Width < info.MinimumSize.Width)
                    newWidth = info.MinimumSize.Width;
                if (this.ClientSize.Height < info.MinimumSize.Height)
                    newHeight = info.MinimumSize.Height;
                this.ClientSize = new Size(newWidth, newHeight);
                info.Parent = this;

                backgroundWorker.RunWorkerCompleted += new EventHandler(backgroundWorker_RunWorkerCompleted);
                backgroundThreadInfoArray.Add(new ThreadInformation(backgroundWorker, info));
                AutoLayout();
            }
        }

        private void backgroundWorker_RunWorkerCompleted(object sender, EventArgs e)
        {
            ThreadInformation info;
            for (int i = 0; i < backgroundThreadInfoArray.Count; i++)
            {
                info = (ThreadInformation)backgroundThreadInfoArray[i];
                if (info.ImprovedBackgroundWorker.Equals(sender))
                {
                    backgroundThreadInfoArray.Remove(info);
                    RemoveControl(info.BackgroundThreadInfo);
                    break;
                }
            }
            if (backgroundThreadInfoArray.Count == 0)
                ClearControls();
            AutoLayout();
        }

        private void SetTitle(string newTitle)
        {
            if (this.InvokeRequired)
            {
                SetTitleDelegate d = new SetTitleDelegate(SetTitle);
                this.Invoke(d, new object[] { newTitle });
            }
            else
            {
                this.Text = newTitle;
            }
        }

        private void ClearControls()
        {
            if (this.InvokeRequired)
            {
                ClearControlsDelegate d = new ClearControlsDelegate(ClearControls);
                this.Invoke(d, null);
            }
            else
            {
                this.Controls.Clear();
            }
        }

        private void RemoveControl(Control whichControl)
        {
            if (this.InvokeRequired)
            {
                RemoveControlDelegate d = new RemoveControlDelegate(RemoveControl);
                this.Invoke(d, new object[] { whichControl });
            }
            else
            {
                this.Controls.Remove(whichControl);
            }
        }

        private void AutoLayout()
        {
            ThreadInformation cur;
            int CurTop = 0;
            int clientWidth;

            if (this.InvokeRequired)
            {
                AutoLayoutDelegate d = new AutoLayoutDelegate(AutoLayout);
                this.Invoke(d, null);
            }
            else
            {
                this.TopMost = true;
                if (backgroundThreadInfoArray.Count == 1)
                    SetTitle("Progress of Background Task");
                if (backgroundThreadInfoArray.Count > 1)
                    SetTitle("Progress of Background Tasks");
                clientWidth = this.ClientRectangle.Width;
                if (this.HasChildren)
                {
                    if (!this.Visible)
                        this.Show();
                    this.MinimumSize = this.Controls[0].MinimumSize;
                }
                else
                {
                    if (this.Visible)
                        this.Hide();
                }
                if (this.VerticalScroll.Visible)
                    this.MinimumSize = new Size(
                        (this.Size.Width - this.ClientSize.Width) + this.Controls[0].MinimumSize.Width,
                        (this.Size.Height - this.ClientSize.Height) + this.Controls[0].MinimumSize.Height); 
                for (int task = 0; task < backgroundThreadInfoArray.Count; task++)
                {
                    cur = (ThreadInformation)backgroundThreadInfoArray[task];
                    cur.BackgroundThreadInfo.Top = CurTop;
                    cur.BackgroundThreadInfo.Width = clientWidth;
                    CurTop += cur.BackgroundThreadInfo.Height;
                }
                if (CurTop < this.ClientSize.Height)
                {
                    int ShrinkAmount = this.ClientSize.Height - CurTop;
                    this.Size = new Size(this.Size.Width, this.Size.Height - ShrinkAmount);
                }
                else
                {
                    if (CurTop < Screen.PrimaryScreen.Bounds.Height)
                    {
                        int GrowAmount = CurTop - this.ClientSize.Height;
                        this.Size = new Size(this.Size.Width, this.Size.Height + GrowAmount);
                    }
                }
            }
        }

        private void MultiProgressDialog_Resize(object sender, EventArgs e)
        {
            AutoLayout();
        }
    }

    internal struct ThreadInformation
    {
        public ImprovedBackgroundWorker ImprovedBackgroundWorker;
        public BackgroundThreadInfo BackgroundThreadInfo;

        public ThreadInformation(ImprovedBackgroundWorker ImprovedBackgroundWorker, BackgroundThreadInfo BackgroundThreadInfo)
        {
            this.ImprovedBackgroundWorker = ImprovedBackgroundWorker;
            this.BackgroundThreadInfo = BackgroundThreadInfo;
        }
    }

}