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

namespace IESME_GEO.Controls
{
    public partial class Map_Panel : SelectionAwareControl
    {
        public float XPS = 1;
        public float YPS = 1;

        #region Map Panel Private Data Members
        private Image ESME_map;
        private ImageView mapView;
        private Bitmap ImageToView;
        
        private const float DefaultScrollMaximum = 1000;
        private const float ScrollWheelConstantZoomFactor = 1.3f;
       
        public PointF TLC = new PointF(-180,90);
        public PointF bottomCorner = new PointF(180,-90);
        private PointF viewLocation = new PointF(0, 0);

        private RectangleF area_rect = new RectangleF();
        private RectangleF r = new RectangleF(0, 0, 0, 0);
        private RectangleF originalRect = new RectangleF();

        private float CurrentHRatio = 0;

        private bool drawRect = false;
        
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

            vScrollBar1.Enabled = false;
            hScrollBar1.Enabled = false;
            
            Assembly myAssembly = Assembly.GetExecutingAssembly();
            Stream myStream = myAssembly.GetManifestResourceStream("IESME_GEO.Resources.World Map.jpg");
            ESME_map = new Bitmap(myStream);

            //ESME_map.RotateFlip(RotateFlipType.RotateNoneFlipY);
            this.SetStyle(ControlStyles.AllPaintingInWmPaint | ControlStyles.UserPaint | ControlStyles.OptimizedDoubleBuffer, true);
            this.SetStyle(ControlStyles.ContainerControl, true);
        }
        #endregion

        #region Map Panel Private Methods
        private void Map_Panel_Load(object sender, EventArgs e)
        {
            mapView = new ImageView((Bitmap)ESME_map, new Size((int)ClientWidth, (int)ClientHeight));
            mapView.ViewChanged += new EventHandler<EventArgs>(mapView_ViewChanged);
        }

        private void mapView_ViewChanged(object sender, EventArgs e)
        {
            this.Invalidate();
        }

        private void vScrollBar1_ValueChanged(object sender, EventArgs e)
        {
            mapView.ViewLocation = new RelativePoint(hScrollBar1.Value / DefaultScrollMaximum, vScrollBar1.Value / DefaultScrollMaximum);
        }

        private void hScrollBar1_ValueChanged(object sender, EventArgs e)
        {
            mapView.ViewLocation = new RelativePoint(hScrollBar1.Value / DefaultScrollMaximum, vScrollBar1.Value / DefaultScrollMaximum);
        }

        private Graphics toWorldCoordinates(PaintEventArgs e)
        {
            Graphics newG = e.Graphics;

            //The Whole World is viewed and not filling the whole container.
            if (mapView.BlackOrientation != "Null")
            {
               if(mapView.BlackOrientation == "Horizontal")
               {
                   Matrix mm = new Matrix(1,0,0,-1,0,0);
                   float BlackEffect = (180 * (mapView.IntermediateSize.Height + mapView.FirstBlack.Height + mapView.SecondBlack.Height))/mapView.IntermediateSize.Height;
                   XPS = (ClientWidth - 1) / 360f;
                   YPS = (ClientHeight - 1) / BlackEffect;
                   mm.Scale((float)XPS, (float)YPS);
                   mm.Translate(180, -1 * (BlackEffect / 2));
                   newG.Transform = mm;
                   TLC = new PointF(newG.VisibleClipBounds.Left, newG.VisibleClipBounds.Bottom);
               }
               else if (mapView.BlackOrientation == "Vertical")
               {
                   Matrix mm = new Matrix(1,0,0,-1,0,0);
                   float BlackEffect = (360 * (mapView.IntermediateSize.Width + mapView.FirstBlack.Width + mapView.SecondBlack.Width)) / mapView.IntermediateSize.Width;
                   XPS = (ClientWidth - 1) / BlackEffect;
                   YPS = (ClientHeight - 1) / 180f;
                   mm.Scale((float)XPS,(float)YPS);
                   mm.Translate(BlackEffect / 2, -90);
                   newG.Transform = mm;
                   TLC = new PointF(newG.VisibleClipBounds.Left, newG.VisibleClipBounds.Bottom);
               }
            }
            else
            {
                newG.Transform = new Matrix(1, 0, 0, 1, 0, 0);
                Matrix mm = new Matrix(1, 0, 0, -1, 0, 0);
                XPS = (ClientWidth - 1) / (mapView.ViewCoverage.Width * 360f);
                YPS = (ClientHeight - 1) / (mapView.ViewCoverage.Height * 180f);
                mm.Scale((float)XPS,(float)YPS);
                //mm.Translate(180 - (180 - ((1 - mapView.ViewLocation.X) * -180)), -90 - ((-90 - (1 - mapView.ViewLocation.Y) * 90))); 
                float newLocationX = -180 + (mapView.ViewLocation.X * 360);
                float newLocationY = 90 - (mapView.ViewLocation.Y * 180);
                float OffsetX = Math.Abs(-180 - newLocationX);
                float OffsetY = -90 + newLocationY;
                mm.Translate((float)(180 - OffsetX), (float)(-90 - OffsetY));
                newG.Transform = mm;
                TLC = new PointF(newG.VisibleClipBounds.Left, newG.VisibleClipBounds.Bottom);
            }

            return newG;
        }
        #endregion

        #region Map Panel Public Properties

        public RectangleF OriginalRect
        {
            set
            {
                originalRect = value;
            }
        }

        public PointF ContainerCoordinateScale
        {
            get
            {
                return new PointF(XPS, YPS);
            }
        }
        public PointF TopLeftPoint
        {
            get
            {
                return TLC;
            }
        }

        public int vscrollWidth
        {
            get { return vScrollBar1.Width; }
        }

        public int hscrollHeight
        {
            get { return hScrollBar1.Height; }
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
 
                Draw_Rect(new PointF(0f, 0f), 0, 0);
                
            }
            else
            {
                mapView.ZoomFactor *= ScrollWheelConstantZoomFactor; 
            }
        }

        public void Zoom_Out()
        {
            mapView.ZoomFactor *= 1 / ScrollWheelConstantZoomFactor;
        }

        public void Zoom_Full()
        {
            mapView.ZoomFactor = 1;
        }
  
        public void updateCoorStatus(float eX, float eY)
        {
            
            if ((eX < -180) || (eX > 180) || (eY > 90) || (eY < -90))
                coorstatus.Text = "Out of Bounds";
            else
             coorstatus.Text ="Longitude " + (eX).ToString("0.000") + " Latitude " + (eY).ToString("0.000");
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

            drawRect = true;

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
            drawRect = true;
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            if (mapView == null)
                return;

            Graphics g = toWorldCoordinates(e);

            ImageToView = mapView.ViewBitmap;
            ImageToView.RotateFlip(RotateFlipType.RotateNoneFlipY);
            vScrollBar1.Enabled = mapView.CanVScroll;
            if (vScrollBar1.Enabled)
            {
                vScrollBar1.Minimum = 0;
                vScrollBar1.Maximum = (int)DefaultScrollMaximum - (int)(DefaultScrollMaximum * mapView.ViewCoverage.Height);
                float vValue = (mapView.ViewLocation.Y * DefaultScrollMaximum);
                
                CurrentHRatio = mapView.ViewLocation.X;
                
                if (mapView.ViewLocation.Y + mapView.ViewCoverage.Height >= 1)
                    vScrollBar1.Value = vScrollBar1.Maximum;
                else
                    vScrollBar1.Value = (int)(vValue);
            }
           
            hScrollBar1.Enabled = mapView.CanHScroll;
            if(hScrollBar1.Enabled)
            {
                hScrollBar1.Minimum = 0;
                hScrollBar1.Maximum = (int)DefaultScrollMaximum - (int)(DefaultScrollMaximum * mapView.ViewCoverage.Width);
                float hValue = (CurrentHRatio * DefaultScrollMaximum);
                if (CurrentHRatio + mapView.ViewCoverage.Width >= 1)
                    hScrollBar1.Value = hScrollBar1.Maximum;
                else
                    hScrollBar1.Value = (int)(hValue);
            }

            g.DrawImage(ImageToView, g.VisibleClipBounds, new RectangleF(0,0,ImageToView.Width,ImageToView.Height),GraphicsUnit.Pixel);
            ImageToView.RotateFlip(RotateFlipType.RotateNoneFlipY);
            
            if (drawRect)
            {
                    Pen p = new Pen(Color.Black, 1 / XPS);
                    p.DashCap = DashCap.Round;
                    p.DashStyle = DashStyle.DashDotDot;

                    r = area_rect;
                    r.Offset(0, -1 * (r.Height));
                    
                    g.DrawRectangle(p, r.X, r.Y, Math.Abs(r.Width), Math.Abs(r.Height));

            }
            drawRect = false;
        }


        protected override void OnMouseWheel(MouseEventArgs e)
        {
            base.OnMouseWheel(e);
            if (e.Delta > 0)
            {
                mapView.ZoomFactor *= ScrollWheelConstantZoomFactor;
            }
            else if (e.Delta < 0)
            {
                mapView.ZoomFactor /= ScrollWheelConstantZoomFactor;
            }


        }
        #endregion
        
    }
}
