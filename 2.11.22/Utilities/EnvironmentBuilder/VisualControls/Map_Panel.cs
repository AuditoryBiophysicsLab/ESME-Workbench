using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Reflection;
using EnvironmentBuilder.ClassControls;
using ESME.Overlay;
using ESME;

namespace EnvironmentBuilder.VisualControls
{
    public partial class Map_Panel : SelectionAwareControl
    {
        #region Map Panel Private Data Members
        
        private Image ESME_map;
        private ImageView mapView;
        private PointF mTopLeftCorner = new PointF(-180, 90);
        private RectangleF area_rect = new RectangleF();
        private RectangleF originalRect = new RectangleF();
        
        private float PixelsPerDegree_X = 1;
        private float PixelsPerDegree_Y = 1;
        private float mNorthBound = 0;
        private float mSouthBound = 0;
        private float mWestBound = 0;
        private float mEastBound = 0;

        private const float DefaultScrollMaximum = 500;
        private const float ScrollWheelConstantZoomFactor = 1.3f;
        #endregion

        #region Map Panel Private Properties
        private int ClientWidth
        {
            get { return this.Width - vscrollWidth - 1; }
        }

        private int ClientHeight
        {
            get { return this.Height - hscrollHeight - statusStrip1.Height - 1; }
        }

        private new Size ClientSize
        {
            get { return new Size(ClientWidth, ClientHeight); }
        }

        #endregion

        #region Map Panel Constructor
        public Map_Panel()
        {
            InitializeComponent();
            this.ResizeRedraw = true;

            Assembly myAssembly = Assembly.GetExecutingAssembly();
            Stream myStream = myAssembly.GetManifestResourceStream(
                "EnvironmentBuilder.Resources.World Map.jpg");
            
            ESME_map = new Bitmap(myStream);

            this.SetStyle(ControlStyles.AllPaintingInWmPaint | 
                ControlStyles.UserPaint | ControlStyles.OptimizedDoubleBuffer, true);
            this.SetStyle(ControlStyles.ContainerControl, true);
        }
        #endregion

        #region Map Panel Private Methods
        private void Map_Panel_Load(object sender, EventArgs e)
        {
            mapView = new ImageView((Bitmap)ESME_map, new Size((int)ClientWidth, 
                (int)ClientHeight));
            mapView.ViewChanged += new EventHandler<EventArgs>(mapView_ViewChanged);
        }

        private void mapView_ViewChanged(object sender, EventArgs e)
        {
            this.Invalidate(); 
        }

        private void vScrollBar1_ValueChanged(object sender, EventArgs e)
        {
            mapView.ViewLocation = new RelativePoint(hScrollBar1.Value 
                / DefaultScrollMaximum, vScrollBar1.Value / DefaultScrollMaximum);
        }

        private void hScrollBar1_ValueChanged(object sender, EventArgs e)
        {
            mapView.ViewLocation = new RelativePoint(hScrollBar1.Value 
                / DefaultScrollMaximum, vScrollBar1.Value / DefaultScrollMaximum);
        }

        private Graphics toWorldCoordinates(PaintEventArgs e)
        {
            Graphics newG = e.Graphics;
            // Invert the Y-axis, so that negative numbers are 
            //below positive ones, in screen coordinates
            // Basically, we are using a Cartesian plane
            Matrix mm = new Matrix(1, 0, 0, -1, 0, 0);
            //Width and Height presented from total map in degrees.
            float WidthViewed = mapView.ViewCoverage.Width * 360f;
            float HeightViewed = mapView.ViewCoverage.Height * 180f;
            //In case of blackeffects, get the horizontal and vertical blackeffect
            float HorizontalBlackEffect, VerticalBlackEffect;
            HorizontalBlackEffect = VerticalBlackEffect = 0;
            if (mapView.Blackorientation == BlackOrientation.Horizontal)
                HorizontalBlackEffect = (180 * (mapView.FirstBlack.Height +
                    mapView.SecondBlack.Height)) / mapView.IntermediateSize.Height;
            if (mapView.Blackorientation == BlackOrientation.Vertical)
                VerticalBlackEffect = (360 * (mapView.FirstBlack.Width +
                    mapView.SecondBlack.Width)) / mapView.IntermediateSize.Width;
            //dimension data in Degrees.
            float TotalWidth = WidthViewed + VerticalBlackEffect;
            float TotalHeight = HeightViewed + HorizontalBlackEffect;
            PixelsPerDegree_X = (ClientWidth) / (TotalWidth);
            PixelsPerDegree_Y = (ClientHeight) / (TotalHeight);
            //Scaling Axes
            mm.Scale((float)PixelsPerDegree_X, (float)PixelsPerDegree_Y);
            //Translation of Origin
            if (mapView.Blackorientation != null)
                mm.Translate(TotalWidth / 2, -1 * TotalHeight / 2);
            else {
                float newLocationX = -180 + (mapView.ViewLocation.X * 360);
                float newLocationY = 90 - (mapView.ViewLocation.Y * 180);
                mm.Translate((float)(-1*newLocationX), (float)(-1*newLocationY));
            }
            //New Graphics
            newG.Transform = mm;
            mTopLeftCorner = new PointF(newG.VisibleClipBounds.Left, 
                newG.VisibleClipBounds.Bottom);
            //Set the Geographical Bounds of Current View
            mNorthBound = newG.VisibleClipBounds.Bottom;
            mWestBound = newG.VisibleClipBounds.Left;
            mEastBound = newG.VisibleClipBounds.Right;
            mSouthBound = newG.VisibleClipBounds.Top;
            //Return the New Graphics.
            return newG;
        }

        private void SetScrollBars()
        {
           
            //Parameters to set the tab.
            float DegreesToTop = 180 * mapView.ViewLocation.X;
            float DegreesToBottom = 180 - (DegreesToTop + 
                (mapView.ViewCoverage.Height * 180));
            float DegreesToLeft = 360 * mapView.ViewLocation.Y;
            float DegreesToRight = 360 - (DegreesToLeft + 
                (mapView.ViewCoverage.Width * 360));
            float HorizontalOffView = DegreesToLeft + DegreesToRight;
            float VerticalOffView = DegreesToTop + DegreesToBottom;

            vScrollBar1.Minimum = hScrollBar1.Minimum = 0;
            
            //Enable Check
            vScrollBar1.Enabled = mapView.CanVScroll;
            hScrollBar1.Enabled = mapView.CanHScroll;
            
            vScrollBar1.Maximum = (int)(DefaultScrollMaximum -
                VerticalOffView);
            hScrollBar1.Maximum = (int)(DefaultScrollMaximum -
                HorizontalOffView);

            float HScrollBarCurrentValue = mapView.ViewLocation.X;
            float VScrollBarCurrentValue = mapView.ViewLocation.Y;
            vScrollBar1.Value = (int)(VScrollBarCurrentValue * DefaultScrollMaximum);
            hScrollBar1.Value = (int)(HScrollBarCurrentValue * DefaultScrollMaximum);
        }
        #endregion

        #region Map Panel Public Properties
        public RectangleF OriginalRect
        {
            set { originalRect = value; }
        }

        public PointF ContainerCoordinateScale
        {
            get { return new PointF(PixelsPerDegree_X, PixelsPerDegree_Y); }
        }
        public PointF TopLeftPoint
        {
            get { return mTopLeftCorner; }
        }

        public int vscrollWidth
        {
            get { return vScrollBar1.Width; }
        }

        public int hscrollHeight
        {
            get { return hScrollBar1.Height; }
        }

        public List<ISelfDraw> DrawableItems { get; set; }

        public float NorthBound
        {
            get { return mNorthBound; }
        }
        public float SouthBound
        {
            get { return mSouthBound; }
        }
        public float WestBound
        {
            get { return mWestBound; }
        }
        public float EastBound
        {
            get { return mEastBound; }
        }
        public bool CanZoomOut
        {
            get { return (mapView.ZoomFactor != 1); }
        }
        #endregion

        #region Map Panel Public Methods
        public void Zoom_In()
        {
            if (area_rect.Height != 0 && area_rect.Width != 0)
            {
                float WidthRatio = originalRect.Width / this.Width;
                float HeightRatio = originalRect.Height / this.Height;
                float TopRatio = originalRect.Top / this.Height;
                float LeftRatio = originalRect.Left/ this.Width;
                               
                PointF TopLeftRatio = new PointF(LeftRatio, TopRatio);
                mapView.ZoomTo(TopLeftRatio, 1 / Math.Max(WidthRatio,HeightRatio));
                North = 0;
                West = 0;
                East = 0;
                South = 0;
            }
            else
            {
                mapView.ZoomFactor *= ScrollWheelConstantZoomFactor; 
            }
            SetScrollBars();
        }
        public void Zoom_Out()
        {
            mapView.ZoomFactor *= 1 / ScrollWheelConstantZoomFactor;
            SetScrollBars();
        }
        public void Zoom_Out_Full()
        {
            mapView.ZoomFactor = 1;
            SetScrollBars();
        }
        public void updateCoorStatus(float eX, float eY)
        {
            
            if ((eX < -180) || (eX > 180) || (eY > 90) || (eY < -90))
                coorstatus.Text = "Out of Bounds";
            else
             coorstatus.Text ="Longitude " + (eX).ToString("0.000") + 
                 " Latitude " + (eY).ToString("0.000") + " " 
                 + mapView.ZoomFactor.ToString();
        }

        /// <summary>
        /// Sets the new parameters for the rectangle to be drawn on the map.
        /// </summary>
        /// <param name="rectCorner"> The value for the Top Left Corner of the Rectangle. </param> 
        /// <param name="eX"> The value of the East Longitude. </param> 
        /// <param name="eY"> The value of the South Latitude. </param> 
        /// <param name="updateFlag"> Set True if new values are from Area Info. </param>
        public void Draw_Rect(PointF rectCorner, float eX, float eY)
        {
            area_rect.X = rectCorner.X;
            area_rect.Y = rectCorner.Y;
            area_rect.Width = Math.Abs(area_rect.X - eX);
            area_rect.Height = Math.Abs(area_rect.Y - eY);

            North = area_rect.Y;
            West = area_rect.X;
            East = eX;
            South = eY;

            this.Invalidate();

        }
        #endregion

        #region Map Panel Override Methods
        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);
            RectangleF beforeResizeAreaRect = new RectangleF();
            beforeResizeAreaRect = area_rect;
            if (mapView != null)
            {
                mapView.ViewSize = this.ClientSize;
            }
            area_rect = beforeResizeAreaRect;
            if (mapView != null)
            {
                if (mapView.CanVScroll || mapView.CanHScroll) 
                    SetScrollBars();
            }
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            Bitmap ImageToView;
            RectangleF r = new RectangleF(0, 0, 0, 0);

            if (mapView == null)
                return;

            Pen p = new Pen(Color.White, (1 / PixelsPerDegree_X));
            Graphics g = toWorldCoordinates(e);

            p.DashCap = DashCap.Flat;
            p.DashStyle = DashStyle.Dash;
            p.DashPattern = new float[] { (3 / mapView.ZoomFactor), 
                (3 / mapView.ZoomFactor) };
           
            ImageToView = mapView.ViewBitmap;
            ImageToView.RotateFlip(RotateFlipType.RotateNoneFlipY);
            r = area_rect;
            r.Offset(0, -1 * (r.Height));

            g.DrawImage(ImageToView, g.VisibleClipBounds, 
                new RectangleF(0, 0, ImageToView.Width, ImageToView.Height),
                GraphicsUnit.Pixel);
            ImageToView.RotateFlip(RotateFlipType.RotateNoneFlipY);
            g.DrawRectangle(p, r.X, r.Y, Math.Abs(r.Width), Math.Abs(r.Height));
            if (DrawableItems != null)
                foreach (ISelfDraw DrawableItem in DrawableItems)
                    if (DrawableItem.Display)
                        DrawableItem.Draw(g);
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            base.OnMouseWheel(e);
            if (e.Delta > 0)
            {
                mapView.ZoomFactor *= ScrollWheelConstantZoomFactor;
                SetScrollBars();
            }
            else if (e.Delta < 0)
            {
                mapView.ZoomFactor /= ScrollWheelConstantZoomFactor;
                SetScrollBars();
            }
        }
        #endregion
    }
}
