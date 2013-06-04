using System;
using System.IO;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using EnvironmentBuilder.ClassControls;
using ESME.Overlay;
using ESME;
namespace EnvironmentBuilder.VisualControls
{
    public partial class FileList_Control : ListBox
    {
        private List<ISelfDraw> mDrawableItems;

        public FileList_Control()
        {
            InitializeComponent();
            mDrawableItems = new List<ISelfDraw>();
        }

        public List<ISelfDraw> List { get { return mDrawableItems; } }

        public void Add(ISelfDraw NewItem)
        {
            mDrawableItems.Add(NewItem);
            this.Items.Add(NewItem);
        }

        public void RemoveAt(int Index)
        {
            mDrawableItems.Remove((ISelfDraw)this.Items[Index]);
            this.Items.RemoveAt(Index);
        }
    }
}
