using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace MBSGUI
{
    public partial class FormConfirmDecision : Form
    {
        int m_buttonPressed = -1;
        public FormConfirmDecision()
        {
            InitializeComponent();
        }

        public void SetLocation(Control C)
        {
            //B.Parent.Location.X
            this.Location = new Point(
                C.Parent.Location.X + C.Location.X + C.Width - this.Width,
                C.Parent.Location.Y + C.Location.Y-this.Height - 5);
            this.Size = new Size(this.Size.Width, this.Size.Height + 23);
        }
        public string messageString { get { return label1.Text; } set { label1.Text = value; } }
        public string button1String { get { return button1.Text; } set { button1.Text = value; } }
        public string button2String { get { return button2.Text; } set { button2.Text = value; } }
        public string button3String { get { return button3.Text; } set { button3.Text = value; } }
        public Boolean button3Visible { get { return button3.Visible; } set { button3.Visible = value; } }
        public int buttonSelected { get { return m_buttonPressed; } }

        private void button1_Click(object sender, EventArgs e)
        {
            m_buttonPressed = 1;
            Dispose();
        }

        private void button2_Click(object sender, EventArgs e)
        {
            m_buttonPressed = 2;
            Dispose();
        }

        private void button3_Click(object sender, EventArgs e)
        {
            m_buttonPressed = 3;
            Dispose();
        }
    }
}