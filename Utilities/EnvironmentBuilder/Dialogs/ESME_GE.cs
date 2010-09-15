using System;
using System.IO;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using ESME.Environment;
using ESME.NEMO;
using EnvironmentBuilder.ClassControls;
using EnvironmentBuilder.VisualControls;
using System.Xml;
using HRC.Navigation;
using ESME.Overlay;
using ESME;

namespace EnvironmentBuilder.Dialogs
{
    public partial class ESME_GE : Form
    {
        //Flags
        protected bool getData = false;
        protected bool zoom_in_flag = false;
        protected bool select_flag = true;
        protected bool isResize = false;
        protected bool initRect = true;
        protected string filename;

        private PointF rectCorner = new PointF();
        private RectangleF Form_Rect = new RectangleF();

        public ESME_GE()
        {
            InitializeComponent();
            map_Panel1.DrawableItems = fileList_Control1.List;
        }

        private void ESME_GE_Load(object sender, EventArgs e)
        {
            environmentFiles1.SearchDirectory = Properties.Settings.Default.EnvironmentDataDirectory;
        }

        private void ESME_GE_FormClosing(object sender, FormClosingEventArgs e)
        {
            Application.Exit();
        }


        private void map_Panel1_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left && (select_flag || zoom_in_flag))
            {


                if (map_Panel1.Width - map_Panel1.vscrollWidth - 2 <= e.X | map_Panel1.Height - map_Panel1.hscrollHeight - 2 <= e.Y)
                {
                    MessageBox.Show("Out of bound Error. Select Area within the Map");
                }

                else
                {
                    PointF XY_World = new PointF();
                    PointF XY_Form = new PointF(e.X, e.Y);
                    XY_World = Form_to_World_Coordinates(new PointF((float)e.X, (float)e.Y));

                    //Initial Points for the rectangle
                    Form_Rect.X = e.X;
                    Form_Rect.Y = e.Y;
                    rectCorner.X = XY_World.X;
                    rectCorner.Y = XY_World.Y;
                }
            }
        }

        private void map_Panel1_MouseMove(object sender, MouseEventArgs e)
        {

            PointF RectTLC = new PointF();
            PointF RectBRC = new PointF();
            RectangleF DirectionAwareFormRectangle = new RectangleF();
            RectangleDirection RectDir;

            if (e.Button == MouseButtons.Left)
            {
                if (isResize)
                {
                    isResize = false;
                    return;
                }

                if (map_Panel1.Width - map_Panel1.vscrollWidth <= e.X | map_Panel1.Height - map_Panel1.hscrollHeight <= e.Y)
                {
                    MessageBox.Show("Out of bound Error. Select Area within the Map");
                }

                else
                {


                    PointF XY_World = new PointF();
                    XY_World = Form_to_World_Coordinates(new PointF((float)e.X, (float)e.Y));
                    RectDir = checkDirection(rectCorner, XY_World);

                    switch (RectDir)
                    {
                        case RectangleDirection.Same:
                            break;
                        case RectangleDirection.BottomRight:
                            DirectionAwareFormRectangle.X = Form_Rect.X;
                            DirectionAwareFormRectangle.Y = Form_Rect.Y;
                            DirectionAwareFormRectangle.Width = e.X - Form_Rect.X;
                            DirectionAwareFormRectangle.Height = e.Y - Form_Rect.Y;
                            RectTLC.X = rectCorner.X;
                            RectTLC.Y = rectCorner.Y;
                            RectBRC.X = XY_World.X;
                            RectBRC.Y = XY_World.Y;
                            break;
                        case RectangleDirection.BottomLeft:
                            DirectionAwareFormRectangle.Width = Form_Rect.X - e.X;
                            DirectionAwareFormRectangle.Height = e.Y - Form_Rect.Y;
                            DirectionAwareFormRectangle.X = e.X;
                            DirectionAwareFormRectangle.Y = Form_Rect.Y;
                            RectTLC.X = XY_World.X;
                            RectTLC.Y = rectCorner.Y;
                            RectBRC.X = rectCorner.X;
                            RectBRC.Y = XY_World.Y;
                            break;
                        case RectangleDirection.TopRight:
                            DirectionAwareFormRectangle.Width = e.X - Form_Rect.X;
                            DirectionAwareFormRectangle.Height = Form_Rect.Y - e.Y;
                            DirectionAwareFormRectangle.X = Form_Rect.X;
                            DirectionAwareFormRectangle.Y = e.Y;
                            RectTLC.X = rectCorner.X;
                            RectTLC.Y = XY_World.Y;
                            RectBRC.X = XY_World.X;
                            RectBRC.Y = rectCorner.Y;
                            break;
                        case RectangleDirection.TopLeft:
                            DirectionAwareFormRectangle.Width = Form_Rect.X - e.X;
                            DirectionAwareFormRectangle.Height = Form_Rect.Y - e.Y;
                            DirectionAwareFormRectangle.X = e.X;
                            DirectionAwareFormRectangle.Y = e.Y;
                            RectTLC.X = XY_World.X;
                            RectTLC.Y = XY_World.Y;
                            RectBRC.X = rectCorner.X;
                            RectBRC.Y = rectCorner.Y;
                            break;
                        default:
                            throw new ApplicationException("ESME_GE.map_Panel1_MouseMove: Exception found in drawing the rectangle");
                    }


                    map_Panel1.OriginalRect = DirectionAwareFormRectangle;
                    map_Panel1.Draw_Rect(RectTLC, (float)RectBRC.X, (float)RectBRC.Y);
                }
            }

            else
            {
                PointF XY_World = new PointF();
                XY_World = Form_to_World_Coordinates(new PointF((float)e.X, (float)e.Y));
                map_Panel1.updateCoorStatus(XY_World.X, XY_World.Y);

            }
        }


        private void areaInfo1_CoordinateChanged(object sender, EventArgs e)
        {
            map_Panel1.North = areaInfo1.North;
            map_Panel1.South = areaInfo1.South;
            map_Panel1.East = areaInfo1.East;
            map_Panel1.West = areaInfo1.West;

            map_Panel1.Draw_Rect(new PointF(map_Panel1.West, map_Panel1.North), map_Panel1.East, map_Panel1.South);
        }

        private void map_Panel1_CoordinateChanged(object sender, EventArgs e)
        {
            areaInfo1.North = map_Panel1.North;
            areaInfo1.South = map_Panel1.South;
            areaInfo1.East = map_Panel1.East;
            areaInfo1.West = map_Panel1.West;
            areaInfo1.PopulateTextboxes();
        }

        private PointF Form_to_World_Coordinates(PointF Form_Coordinates)
        {
            float XTLC = map_Panel1.TopLeftPoint.X;
            float YTLC = map_Panel1.TopLeftPoint.Y;

            float XPS = map_Panel1.ContainerCoordinateScale.X;
            float YPS = map_Panel1.ContainerCoordinateScale.Y;

            PointF World_Coordinates = new PointF();

            if (XTLC >= 0) World_Coordinates.X = (Form_Coordinates.X / XPS + XTLC);
            else World_Coordinates.X = (Form_Coordinates.X / XPS - Math.Abs(XTLC));
            World_Coordinates.Y = -1 * (Form_Coordinates.Y / YPS - YTLC);

            if (World_Coordinates.Y > 90)
                World_Coordinates.Y = 90;
            else if (World_Coordinates.Y < -90)
                World_Coordinates.Y = -90;

            if (World_Coordinates.X > 180)
                World_Coordinates.X = 180;
            else if (World_Coordinates.X < -180)
                World_Coordinates.X = -180;

            return World_Coordinates;
        }

        private void map_Panel1_MouseClick(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Right)
            {
                if (Math.Abs(areaInfo1.West - areaInfo1.East) < 0.00001f || Math.Abs(areaInfo1.North - areaInfo1.South) < 0.00001f)
                {
                    toolStripMenuItem3.Enabled = false;
                    clearAreaToolStripMenuItem2.Enabled = false;
                }
                else
                {
                    if (environmentFiles1.CanExtract(environmentFiles1))
                        toolStripMenuItem3.Enabled = true;
                    else toolStripMenuItem3.Enabled = false;
                    clearAreaToolStripMenuItem2.Enabled = true;
                }

                if (environmentFiles1.CanExtract(environmentFiles1))
                    extractCurrentViewToolStripMenuItem.Enabled = true;
                else extractCurrentViewToolStripMenuItem.Enabled = false;
                if (map_Panel1.CanZoomOut)
                {
                    zoomOutToolStripMenuItem1.Enabled = true;
                    zoomToActualSizeToolStripMenuItem.Enabled = true;
                }
                contextMenuStrip1.Show(map_Panel1, new Point(e.X, e.Y));
            }
        }

        private void zoomInToolStripMenuItem2_Click(object sender, EventArgs e)
        {
            map_Panel1.Zoom_In();
        }

        private void selectAreaToolStripMenuItem1_Click(object sender, EventArgs e)
        {
            string Locations = Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "ESME WorkBench\\Locations");
            if (!Directory.Exists(Locations))
                Directory.CreateDirectory(Locations);
            string value = "New Location 1";
            while (true)
            {
                if (ESME_GE.InputBox("New Location", "New Location name:", ref value) != DialogResult.OK)
                    return;
                string LocationFile = Path.Combine(Locations, value + ".eeb");
                if (!File.Exists(LocationFile))
                {
                    getEnvironmentalData(LocationFile, areaInfo1.North, areaInfo1.West, areaInfo1.South, areaInfo1.East);
                    return;
                }
                else
                {
                    var result = MessageBox.Show("Specified location file already exists.  Overwrite?", "Location file already exists", MessageBoxButtons.YesNo, MessageBoxIcon.Warning);
                    if (result == DialogResult.Yes)
                    {
                        getEnvironmentalData(LocationFile, areaInfo1.North, areaInfo1.West, areaInfo1.South, areaInfo1.East);
                        return;
                    }
                }
            }
        }

        private void clearAreaToolStripMenuItem2_Click(object sender, EventArgs e)
        {
            areaInfo1.North = 0;
            areaInfo1.South = 0;
            areaInfo1.East = 0;
            areaInfo1.West = 0;
            areaInfo1.PopulateTextboxes();
            rectCorner.X = 0;
            rectCorner.Y = 0;
            map_Panel1.Draw_Rect(rectCorner, 0, 0);

            zoom_in_flag = false;
            select_flag = true;
        }

        private void zoomInToolStripMenuItem_Click(object sender, EventArgs e)
        {
            map_Panel1.Zoom_In();
        }

        private void zoomOutToolStripMenuItem_Click(object sender, EventArgs e)
        {
            map_Panel1.Zoom_Out();
        }

        private void zoomFullToolStripMenuItem_Click(object sender, EventArgs e)
        {
            map_Panel1.Zoom_Out_Full();
        }

        private void clearAreaToolStripMenuItem_Click(object sender, EventArgs e)
        {
            clearAreaToolStripMenuItem2_Click(null, new EventArgs());
        }

        private void zoomOutToolStripMenuItem1_Click(object sender, EventArgs e)
        {
            map_Panel1.Zoom_Out();
        }

        private void zoomToActualSizeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            map_Panel1.Zoom_Out_Full();
        }

        private void ESME_GE_Resize(object sender, EventArgs e)
        {
            
            map_Panel1.Height = this.ClientSize.Height - 124;
            isResize = true;
        }

        private RectangleDirection checkDirection(PointF firstPoint, PointF currentPoint)
        {
            if (firstPoint.X > currentPoint.X && firstPoint.Y > currentPoint.Y)
                return RectangleDirection.BottomLeft;
            else if (firstPoint.X > currentPoint.X && firstPoint.Y < currentPoint.Y)
                return RectangleDirection.TopLeft;
            else if (firstPoint.X < currentPoint.X && firstPoint.Y > currentPoint.Y)
                return RectangleDirection.BottomRight;
            else if (firstPoint.X < currentPoint.X && firstPoint.Y < currentPoint.Y)
                return RectangleDirection.TopRight;
            else
                return RectangleDirection.Same;

        }

        private void getEnvironmentalData(string OutputFileName, float North, float West, float South, float East)
        {
            DataFile outputFile = DataFile.Create(OutputFileName);
            foreach (DataLayer cur in environmentFiles1.SelectedLayers)
            {
                DataLayer newLayer = new DataLayer(cur, outputFile, North, West, South, East);
                outputFile.Layers.Add(newLayer);
            }
            outputFile.Close();
        }

        private void extractCurrentViewToolStripMenuItem_Click(object sender, EventArgs e)
        {
            string value = "New Location 1";
            if (ESME_GE.InputBox("New Location", "New Location name:", ref value) == DialogResult.OK)
            {
                String MyDocuments = Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "ESME WorkBench");
                Directory.CreateDirectory(Path.Combine(MyDocuments, value));
                //ESME_GE.WaitBox("Please Wait");
                //Application.DoEvents();
                getEnvironmentalData(Path.Combine(Path.Combine(MyDocuments, value), value + ".eeb"), map_Panel1.NorthBound, map_Panel1.WestBound, map_Panel1.SouthBound, map_Panel1.EastBound);
                //map_Panel1.CurrentView.VisibleClipBounds.Bottom, map_Panel1.CurrentView.VisibleClipBounds.Left, map_Panel1.CurrentView.VisibleClipBounds.Top, map_Panel1.CurrentView.VisibleClipBounds.Right);

            }
        }

        private void map_Panel1_MouseUp(object sender, MouseEventArgs e)
        {
            initRect = true;
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Application.Exit();
        }

        private void zoomIn_Click(object sender, EventArgs e)
        {
            map_Panel1.Zoom_In();
        }

        private void zoomOut_Click(object sender, EventArgs e)
        {
            map_Panel1.Zoom_Out();
        }

        private void resetToolStripMenuItem_Click(object sender, EventArgs e)
        {
            map_Panel1.Zoom_Out_Full();
        }

        private void chooseEnvironmentDatabaseDirectoryToolStripMenuItem_Click(object sender, EventArgs e)
        {
            folderBrowserDialog1.SelectedPath = Properties.Settings.Default.EnvironmentDataDirectory;
            if (folderBrowserDialog1.ShowDialog() == DialogResult.OK)
            {
                Properties.Settings.Default.EnvironmentDataDirectory = folderBrowserDialog1.SelectedPath;
                Properties.Settings.Default.Save();
                environmentFiles1.SearchDirectory = Properties.Settings.Default.EnvironmentDataDirectory;
            }
        }

        private void chooseNEMODataFilesDirectoryToolStripMenuItem_Click(object sender, EventArgs e)
        {
            folderBrowserDialog2.SelectedPath = Properties.Settings.Default.NemoDataDirectory;
            if (folderBrowserDialog2.ShowDialog() == DialogResult.OK)
            {
                Properties.Settings.Default.NemoDataDirectory = folderBrowserDialog2.SelectedPath;
                Properties.Settings.Default.Save();
            }
        }

        private void loadNEMOScenarioFileToolStripMenuItem_Click(object sender, EventArgs e)
        {
            openFileDialog1.Filter = "NEMO Scenario files (*.nemo)|*.nemo|All files (*.*)|*.*";
            openFileDialog1.InitialDirectory = Properties.Settings.Default.NemoDataDirectory;
            if (openFileDialog1.ShowDialog() == DialogResult.OK)
            {
                try
                {
                    NemoFile nemo = new NemoFile(openFileDialog1.FileName, Properties.Settings.Default.NemoDataDirectory);
                    nemo.Display = true;
                    fileList_Control1.Add(nemo);
                }
                catch (FormatException ex)
                {
                    MessageBox.Show("In file: " + openFileDialog1.FileName + "\n" + ex.Message, "NEMO Scenario File format error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }
        }

        private void importOverlayFileToolStripMenuItem_Click(object sender, EventArgs e)
        {
            openFileDialog1.Filter = "Overlay files (*.ovr)|*.ovr|All files (*.*)|*.*";
            if (openFileDialog1.ShowDialog() == DialogResult.OK)
            {

                fileList_Control1.Add(new OverlayFile { FileName = openFileDialog1.FileName, Display = true });
                map_Panel1.Refresh();
            }
            this.toolStripStatusLabel1.Text = "Overlay File Added.";
        }

        private void fileList_Control1_MouseDown(object sender, MouseEventArgs e)
        {
            if ((e.Button == MouseButtons.Right) || (fileList_Control1.IndexFromPoint(e.X, e.Y) == -1))
            {
                if (fileList_Control1.Items.Count == 0)
                {

                    displayToolStripMenuItem.Enabled = false;
                    deleteToolStripMenuItem.Enabled = false;
                    addOverlayFileToolStripMenuItem.Enabled = true;

                    contextMenuStrip2.Show(fileList_Control1, new Point(e.X, e.Y));
                }

                else
                {
                    if ((fileList_Control1.IndexFromPoint(e.X, e.Y) == -1))
                    {
                        displayToolStripMenuItem.Enabled = false;
                        deleteToolStripMenuItem.Enabled = false;
                        addOverlayFileToolStripMenuItem.Enabled = true;

                        contextMenuStrip2.Show(fileList_Control1, new Point(e.X, e.Y));
                    }
                    else
                    {
                        displayToolStripMenuItem.Enabled = true;
                        deleteToolStripMenuItem.Enabled = true;
                        addOverlayFileToolStripMenuItem.Enabled = true;
                        fileList_Control1.SelectedIndex = fileList_Control1.IndexFromPoint(e.X, e.Y);

                        filename = fileList_Control1.SelectedItem.ToString();

                        contextMenuStrip2.Show(fileList_Control1, new Point(e.X, e.Y));
                    }
                }
            }
        }

        private void deleteToolStripMenuItem_MouseDown(object sender, MouseEventArgs e)
        {
            fileList_Control1.RemoveAt(fileList_Control1.SelectedIndex);
            map_Panel1.Refresh();
            this.toolStripStatusLabel1.Text = "Overlay File Deleted.";
        }

        private void addOverlayFileToolStripMenuItem_Click(object sender, EventArgs e)
        {
            importOverlayFileToolStripMenuItem_Click(null, new EventArgs());
            this.toolStripStatusLabel1.Text = "Overlay File Added.";
        }

        private void displayToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ((ISelfDraw)fileList_Control1.Items[fileList_Control1.SelectedIndex]).Display = displayToolStripMenuItem.Checked;
            map_Panel1.Refresh();
            this.toolStripStatusLabel1.Text = "Overlay File Display Selection Changed.";
        }

        public static DialogResult InputBox(string title, string promptText, ref string value)
        {
            Form form = new Form();
            Label label = new Label();
            TextBox textBox = new TextBox();
            Button buttonOk = new Button();
            Button buttonCancel = new Button();

            form.Text = title;
            label.Text = promptText;
            textBox.Text = value;

            buttonOk.Text = "OK";
            buttonCancel.Text = "Cancel";
            buttonOk.DialogResult = DialogResult.OK;
            buttonCancel.DialogResult = DialogResult.Cancel;

            label.SetBounds(9, 20, 372, 13);
            textBox.SetBounds(12, 36, 372, 20);
            buttonOk.SetBounds(228, 72, 75, 23);
            buttonCancel.SetBounds(309, 72, 75, 23);

            label.AutoSize = true;
            textBox.Anchor = textBox.Anchor | AnchorStyles.Right;
            buttonOk.Anchor = AnchorStyles.Bottom | AnchorStyles.Right;
            buttonCancel.Anchor = AnchorStyles.Bottom | AnchorStyles.Right;

            form.ClientSize = new Size(396, 107);
            form.Controls.AddRange(new Control[] { label, textBox, buttonOk, buttonCancel });
            form.ClientSize = new Size(Math.Max(300, label.Right + 10), form.ClientSize.Height);
            form.FormBorderStyle = FormBorderStyle.FixedDialog;
            form.StartPosition = FormStartPosition.CenterScreen;
            form.MinimizeBox = false;
            form.MaximizeBox = false;
            form.AcceptButton = buttonOk;
            form.CancelButton = buttonCancel;

            DialogResult dialogResult = form.ShowDialog();
            value = textBox.Text;
            return dialogResult;
        }

        public static DialogResult WaitBox(string promptText)
        {
            Form form = new Form();
            Label label = new Label();

            label.Text = promptText;

            label.SetBounds(9, 20, 372, 13);

            label.AutoSize = true;

            form.ClientSize = new Size(150, 80);
            form.Controls.AddRange(new Control[] { label });
            form.ClientSize = new Size(Math.Max(300, label.Right + 10), form.ClientSize.Height);
            form.FormBorderStyle = FormBorderStyle.FixedDialog;
            form.StartPosition = FormStartPosition.CenterScreen;
            form.MinimizeBox = false;
            form.MaximizeBox = false;

            DialogResult dialogResult = form.ShowDialog();
            return dialogResult;
        }

        private void map_Panel1_DragEnter(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                string[] files = e.Data.GetData(DataFormats.FileDrop, true) as string[];
                e.Effect = DragDropEffects.All;
                foreach (string s in files)
                {
                    switch (Path.GetExtension(s).ToLower())
                    {
                        case ".ovr":
                        case ".nemo":
                            break;
                        default:
                            e.Effect = DragDropEffects.None;
                            break;
                    }
                }
            }
        }

        private void map_Panel1_DragDrop(object sender, DragEventArgs e)
        {
            string[] files = e.Data.GetData(DataFormats.FileDrop, true) as string[];
            foreach (string s in files)
            {
                switch (Path.GetExtension(s).ToLower())
                {
                    case ".ovr":
                        fileList_Control1.Add(new OverlayFile { FileName = s, Display = true });
                        break;
                    case ".nemo":
                        NemoFile nemo = new NemoFile(s, Properties.Settings.Default.NemoDataDirectory);
                        nemo.Display = true;
                        fileList_Control1.Add(nemo);
                        break;
                    default:
                        MessageBox.Show("Dropping files of this type is not supported", "File Drop error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                        break;
                }
            }
            map_Panel1.Refresh();
            
            this.toolStripStatusLabel1.Text = "File Added.";
        }

        private void fileList_Control1_DragEnter(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                string[] files = e.Data.GetData(DataFormats.FileDrop, true) as string[];
                e.Effect = DragDropEffects.All;
                foreach (string s in files)
                {
                    switch (Path.GetExtension(s).ToLower())
                    {
                        case ".ovr":
                        case ".nemo":
                            break;
                        default:
                            e.Effect = DragDropEffects.None;
                            break;
                    }
                }
            }
        }

        private void fileList_Control1_DragDrop(object sender, DragEventArgs e)
        {

            string[] files = e.Data.GetData(DataFormats.FileDrop, true) as string[];
            foreach (string s in files)
            {
                switch (Path.GetExtension(s).ToLower())
                {
                    case ".ovr":
                        fileList_Control1.Add(new OverlayFile { FileName = s, Display = true });
                        break;
                    case ".nemo":
                        NemoFile nemo = new NemoFile(s, Properties.Settings.Default.NemoDataDirectory);
                        nemo.Display = true;
                        fileList_Control1.Add(nemo);
                        break;
                    default:
                        MessageBox.Show("Dropping files of this type is not supported", "File Drop error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                        break;
                }
            }
            map_Panel1.Refresh();

            this.toolStripStatusLabel1.Text = "File Added.";
        }
    }

    public enum RectangleDirection
    {
        TopLeft,
        TopRight,
        BottomLeft,
        BottomRight,
        Same
    }

}