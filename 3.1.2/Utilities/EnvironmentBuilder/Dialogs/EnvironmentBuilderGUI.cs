using System;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using EnvironmentBuilder.Properties;
using ESME;
using ESME.Environment;
using ESME.NEMO;
using ESME.Overlay;

namespace EnvironmentBuilder.Dialogs
{
    public partial class EnvironmentBuilderGUI : Form
    {
        //Flags
        RectangleF _formRect;
        protected string Filename;
        protected bool GetData;
        protected bool InitRect = true;
        protected bool IsResize;

        PointF _rectCorner;
        protected bool SelectFlag = true;
        protected bool ZoomInFlag;

        public EnvironmentBuilderGUI(string[] args)
        {
            InitializeComponent();
            if ((args.Length == 1) && (Directory.Exists(args[0])))
            {
                Settings.Default.EnvironmentDataDirectory = args[0];
                Settings.Default.Save();
                environmentFiles1.SearchDirectory = Settings.Default.EnvironmentDataDirectory;
            }
            map_Panel1.DrawableItems = fileList_Control1.List;
        }

        void Form_Load(object sender, EventArgs e)
        {
            environmentFiles1.SearchDirectory = Settings.Default.EnvironmentDataDirectory;
        }

        void Form_Closing(object sender, FormClosingEventArgs e)
        {
            Application.Exit();
        }

        void MapPanel_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button != MouseButtons.Left || (!SelectFlag && !ZoomInFlag)) return;

            if (map_Panel1.Width - map_Panel1.vscrollWidth - 2 <= e.X | map_Panel1.Height - map_Panel1.hscrollHeight - 2 <= e.Y)
                MessageBox.Show(@"Selection out of bounds. Please Select an area that is entirely contained within the map.");

            else
            {
                var xyWorld = Form_to_World_Coordinates(new PointF(e.X, e.Y));

                //Initial Points for the rectangle
                _formRect.X = e.X;
                _formRect.Y = e.Y;
                _rectCorner.X = xyWorld.X;
                _rectCorner.Y = xyWorld.Y;
            }
        }

        void MapPanel_MouseMove(object sender, MouseEventArgs e)
        {
            var rectTopLeft = new PointF();
            var rectBottomRight = new PointF();
            var directionAwareFormRectangle = new RectangleF();

            if (e.Button == MouseButtons.Left)
            {
                if (IsResize)
                {
                    IsResize = false;
                    return;
                }

                if (map_Panel1.Width - map_Panel1.vscrollWidth <= e.X | map_Panel1.Height - map_Panel1.hscrollHeight <= e.Y)
                    MessageBox.Show(@"Selection out of bounds. Please Select an area that is entirely contained within the map.");

                else
                {
                    PointF xyWorld = Form_to_World_Coordinates(new PointF(e.X, e.Y));
                    RectangleDirection rectDir = checkDirection(_rectCorner, xyWorld);

                    switch (rectDir)
                    {
                        case RectangleDirection.Same:
                            break;
                        case RectangleDirection.BottomRight:
                            directionAwareFormRectangle.X = _formRect.X;
                            directionAwareFormRectangle.Y = _formRect.Y;
                            directionAwareFormRectangle.Width = e.X - _formRect.X;
                            directionAwareFormRectangle.Height = e.Y - _formRect.Y;
                            rectTopLeft.X = _rectCorner.X;
                            rectTopLeft.Y = _rectCorner.Y;
                            rectBottomRight.X = xyWorld.X;
                            rectBottomRight.Y = xyWorld.Y;
                            break;
                        case RectangleDirection.BottomLeft:
                            directionAwareFormRectangle.Width = _formRect.X - e.X;
                            directionAwareFormRectangle.Height = e.Y - _formRect.Y;
                            directionAwareFormRectangle.X = e.X;
                            directionAwareFormRectangle.Y = _formRect.Y;
                            rectTopLeft.X = xyWorld.X;
                            rectTopLeft.Y = _rectCorner.Y;
                            rectBottomRight.X = _rectCorner.X;
                            rectBottomRight.Y = xyWorld.Y;
                            break;
                        case RectangleDirection.TopRight:
                            directionAwareFormRectangle.Width = e.X - _formRect.X;
                            directionAwareFormRectangle.Height = _formRect.Y - e.Y;
                            directionAwareFormRectangle.X = _formRect.X;
                            directionAwareFormRectangle.Y = e.Y;
                            rectTopLeft.X = _rectCorner.X;
                            rectTopLeft.Y = xyWorld.Y;
                            rectBottomRight.X = xyWorld.X;
                            rectBottomRight.Y = _rectCorner.Y;
                            break;
                        case RectangleDirection.TopLeft:
                            directionAwareFormRectangle.Width = _formRect.X - e.X;
                            directionAwareFormRectangle.Height = _formRect.Y - e.Y;
                            directionAwareFormRectangle.X = e.X;
                            directionAwareFormRectangle.Y = e.Y;
                            rectTopLeft.X = xyWorld.X;
                            rectTopLeft.Y = xyWorld.Y;
                            rectBottomRight.X = _rectCorner.X;
                            rectBottomRight.Y = _rectCorner.Y;
                            break;
                        default:
                            throw new ApplicationException("EnvironmentBuilderGUI.MapPanel_MouseMove: Exception found in drawing the rectangle");
                    }

                    map_Panel1.OriginalRect = directionAwareFormRectangle;
                    map_Panel1.Draw_Rect(rectTopLeft, rectBottomRight.X, rectBottomRight.Y);
                }
            }

            else
            {
                var xyWorld = Form_to_World_Coordinates(new PointF(e.X, e.Y));
                map_Panel1.updateCoorStatus(xyWorld.X, xyWorld.Y);
            }
        }

        void areaInfo1_CoordinateChanged(object sender, EventArgs e)
        {
            map_Panel1.North = areaInfo1.North;
            map_Panel1.South = areaInfo1.South;
            map_Panel1.East = areaInfo1.East;
            map_Panel1.West = areaInfo1.West;

            map_Panel1.Draw_Rect(new PointF(map_Panel1.West, map_Panel1.North), map_Panel1.East, map_Panel1.South);
        }

        void map_Panel1_CoordinateChanged(object sender, EventArgs e)
        {
            areaInfo1.North = map_Panel1.North;
            areaInfo1.South = map_Panel1.South;
            areaInfo1.East = map_Panel1.East;
            areaInfo1.West = map_Panel1.West;
            areaInfo1.PopulateTextboxes();
        }

        PointF Form_to_World_Coordinates(PointF Form_Coordinates)
        {
            float XTLC = map_Panel1.TopLeftPoint.X;
            float YTLC = map_Panel1.TopLeftPoint.Y;

            float XPS = map_Panel1.ContainerCoordinateScale.X;
            float YPS = map_Panel1.ContainerCoordinateScale.Y;

            var World_Coordinates = new PointF();

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

        void map_Panel1_MouseClick(object sender, MouseEventArgs e)
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

        void zoomInToolStripMenuItem2_Click(object sender, EventArgs e)
        {
            map_Panel1.Zoom_In();
        }

        void selectAreaToolStripMenuItem1_Click(object sender, EventArgs e)
        {
            string Locations = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "ESME WorkBench\\Locations");
            if (!Directory.Exists(Locations))
                Directory.CreateDirectory(Locations);
            string value = "New Location 1";
            while (true)
            {
                if (InputBox("New Location", "New Location name:", ref value) != DialogResult.OK)
                    return;
                string LocationFile = Path.Combine(Locations, value + ".eeb");
                if (!File.Exists(LocationFile))
                {
                    getEnvironmentalData(LocationFile, areaInfo1.North, areaInfo1.West, areaInfo1.South, areaInfo1.East);
                    return;
                }
                else
                {
                    DialogResult result = MessageBox.Show("Specified location file already exists.  Overwrite?", "Location file already exists", MessageBoxButtons.YesNo, MessageBoxIcon.Warning);
                    if (result == DialogResult.Yes)
                    {
                        getEnvironmentalData(LocationFile, areaInfo1.North, areaInfo1.West, areaInfo1.South, areaInfo1.East);
                        return;
                    }
                }
            }
        }

        void clearAreaToolStripMenuItem2_Click(object sender, EventArgs e)
        {
            areaInfo1.North = 0;
            areaInfo1.South = 0;
            areaInfo1.East = 0;
            areaInfo1.West = 0;
            areaInfo1.PopulateTextboxes();
            _rectCorner.X = 0;
            _rectCorner.Y = 0;
            map_Panel1.Draw_Rect(_rectCorner, 0, 0);

            ZoomInFlag = false;
            SelectFlag = true;
        }

        void zoomOutToolStripMenuItem1_Click(object sender, EventArgs e)
        {
            map_Panel1.Zoom_Out();
        }

        void zoomToActualSizeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            map_Panel1.Zoom_Out_Full();
        }

        void ESME_GE_Resize(object sender, EventArgs e)
        {
            map_Panel1.Height = ClientSize.Height - 124;
            IsResize = true;
        }

        RectangleDirection checkDirection(PointF firstPoint, PointF currentPoint)
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

        void getEnvironmentalData(string OutputFileName, float North, float West, float South, float East)
        {
            DataFile outputFile = DataFile.Create(OutputFileName);
            foreach (DataLayer cur in environmentFiles1.SelectedLayers)
            {
                var newLayer = new DataLayer(cur, outputFile, North, West, South, East);
                outputFile.Layers.Add(newLayer);
            }
            outputFile.Close();
        }

        void extractCurrentViewToolStripMenuItem_Click(object sender, EventArgs e)
        {
            string value = "New Location 1";
            if (InputBox("New Location", "New Location name:", ref value) == DialogResult.OK)
            {
                String MyDocuments = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "ESME WorkBench");
                Directory.CreateDirectory(Path.Combine(MyDocuments, value));
                //EnvironmentBuilderGUI.WaitBox("Please Wait");
                //Application.DoEvents();
                getEnvironmentalData(Path.Combine(Path.Combine(MyDocuments, value), value + ".eeb"), map_Panel1.NorthBound, map_Panel1.WestBound, map_Panel1.SouthBound, map_Panel1.EastBound);
                //map_Panel1.CurrentView.VisibleClipBounds.Bottom, map_Panel1.CurrentView.VisibleClipBounds.Left, map_Panel1.CurrentView.VisibleClipBounds.Top, map_Panel1.CurrentView.VisibleClipBounds.Right);
            }
        }

        void map_Panel1_MouseUp(object sender, MouseEventArgs e)
        {
            InitRect = true;
        }

        void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Application.Exit();
        }

        void zoomIn_Click(object sender, EventArgs e)
        {
            map_Panel1.Zoom_In();
        }

        void zoomOut_Click(object sender, EventArgs e)
        {
            map_Panel1.Zoom_Out();
        }

        void resetToolStripMenuItem_Click(object sender, EventArgs e)
        {
            map_Panel1.Zoom_Out_Full();
        }

        void chooseEnvironmentDatabaseDirectoryToolStripMenuItem_Click(object sender, EventArgs e)
        {
            folderBrowserDialog1.SelectedPath = Settings.Default.EnvironmentDataDirectory;
            if (folderBrowserDialog1.ShowDialog() == DialogResult.OK)
            {
                Settings.Default.EnvironmentDataDirectory = folderBrowserDialog1.SelectedPath;
                Settings.Default.Save();
                environmentFiles1.SearchDirectory = Settings.Default.EnvironmentDataDirectory;
            }
        }

        void chooseNEMODataFilesDirectoryToolStripMenuItem_Click(object sender, EventArgs e)
        {
            folderBrowserDialog2.SelectedPath = Settings.Default.NemoDataDirectory;
            if (folderBrowserDialog2.ShowDialog() == DialogResult.OK)
            {
                Settings.Default.NemoDataDirectory = folderBrowserDialog2.SelectedPath;
                Settings.Default.Save();
            }
        }

        void fileList_Control1_MouseDown(object sender, MouseEventArgs e)
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

                        Filename = fileList_Control1.SelectedItem.ToString();

                        contextMenuStrip2.Show(fileList_Control1, new Point(e.X, e.Y));
                    }
                }
            }
        }

        void deleteToolStripMenuItem_MouseDown(object sender, MouseEventArgs e)
        {
            fileList_Control1.RemoveAt(fileList_Control1.SelectedIndex);
            map_Panel1.Refresh();
            toolStripStatusLabel1.Text = "Overlay File Deleted.";
        }

        void displayToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ((ISelfDraw)fileList_Control1.Items[fileList_Control1.SelectedIndex]).Display = displayToolStripMenuItem.Checked;
            map_Panel1.Refresh();
            toolStripStatusLabel1.Text = "Overlay File Display Selection Changed.";
        }

        public static DialogResult InputBox(string title, string promptText, ref string value)
        {
            var form = new Form();
            var label = new Label();
            var textBox = new TextBox();
            var buttonOk = new Button();
            var buttonCancel = new Button();

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
            var form = new Form();
            var label = new Label();

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

        void map_Panel1_DragEnter(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                var files = e.Data.GetData(DataFormats.FileDrop, true) as string[];
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

        void fileList_Control1_DragEnter(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                var files = e.Data.GetData(DataFormats.FileDrop, true) as string[];
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