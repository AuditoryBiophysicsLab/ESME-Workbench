using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using MMMBSLib; // mmmb C# code code and data types.

namespace MBSGUI
{
    public partial class FormSpeciesDescription: Form
    {
        C3mbSpeciesModel m_speMdl;
        Boolean m_shortDescModified = false;
        Boolean m_longCommentModified = false;
        Boolean m_initializing = true;

        public Boolean modified { get { return m_shortDescModified || m_longCommentModified; } }

        public FormSpeciesDescription(C3mbSpeciesModel SpeMdl)
        {
            m_speMdl = SpeMdl;
            InitializeComponent();

            ShortDescriptionTextBox.Text = m_speMdl.szShortDescription;
            LongCommentTextBox.Text = m_speMdl.szLongComment;
            LongCommentTextBox.MaxLength = m_speMdl.longCommentMaxAllowedLenth-1;
            ShortDescriptionTextBox.MaxLength = m_speMdl.shortDescriptionMaxAllowedLenth-1;


            if(ShortDescriptionTextBox.Text.Length > 0)
                m_shortDescModified = true;
            else
                ShortDescriptionTextBox.Text = "Enter a short description here";
            UpdateShortDescriptionText();

            if(LongCommentTextBox.Text.Length > 0)
                m_longCommentModified = true;
            else
                LongCommentTextBox.Text =
                    "Enter general information here (description, comments, authorship, etc.)";
            UpdateLongCommentText();

            m_initializing = false;
        }

        private void DoneButton_Click(object sender, EventArgs e)
        {
            if(ShortDescriptionTextBox.Text.Length > 0 && m_shortDescModified == true)
                m_speMdl.szShortDescription = ShortDescriptionTextBox.Text;
            else
                m_speMdl.szShortDescription = "";


            if(LongCommentTextBox.Text.Length > 0 && m_longCommentModified == true)
            {
                m_speMdl.szLongComment = LongCommentTextBox.Text;
                if(m_speMdl.szLongComment.Length > m_speMdl.longCommentMaxAllowedLenth)
                {
                    m_speMdl.szLongComment =
                        m_speMdl.szLongComment.Substring(0, m_speMdl.longCommentMaxAllowedLenth-1);
                }
            }
            else
                m_speMdl.szLongComment = "";

            Dispose();
        }

        private void LongCommentTextBox_MouseDown(object sender, MouseEventArgs e)
        {
            if(m_longCommentModified == false)
            {
                m_initializing = true;
                LongCommentTextBox.Text = "";
                m_initializing = false;
            }

            //m_longCommentModified = true;
        }

        private void ShortDescriptionTextBox_MouseDown(object sender, MouseEventArgs e)
        {
            if(m_shortDescModified == false)
            {
                m_initializing = true;
                ShortDescriptionTextBox.Text = "";
                m_initializing = false;
            }
        }

        private void LongCommentTextBox_TextChanged(object sender, EventArgs e)
        {
            if(m_initializing == true)
                return;

            m_longCommentModified = true;
            UpdateLongCommentText();
        }
        private void ShortDescriptionTextBox_TextChanged(object sender, EventArgs e)
        {
            if(m_initializing == true)
                return;

            m_shortDescModified = true;
            UpdateShortDescriptionText();
        }

        private void UpdateShortDescriptionText()
        {
            int remChar = ShortDescriptionTextBox.MaxLength - ShortDescriptionTextBox.Text.Length;
            if(m_shortDescModified == true)
                label1.Text = "(" + remChar +" characters remaining)";
            else
                label1.Text = "(" + ShortDescriptionTextBox.MaxLength +" characters remaining)";
        }

        private void UpdateLongCommentText()
        {
            int remChar = LongCommentTextBox.MaxLength - LongCommentTextBox.Text.Length;
            if(m_longCommentModified == true)
                label2.Text = "(" + remChar +" characters remaining)";
            else
                label2.Text = "(" + LongCommentTextBox.MaxLength +" characters remaining)";
        }
    }
}