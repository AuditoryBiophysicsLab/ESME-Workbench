using System;
using System.Windows.Forms;
using System.Collections.Generic;
using System.Text;
using System.Drawing;
using System.Drawing.Imaging;
using System.Diagnostics;
using mbs;  
using MMMBSLib;


namespace MBSGUI
{
    //*****************************************************************************************************************
    //*****************************************************************************************************************
    //----------------------------------------------------------------------------------//
    // Species Depth Scale Bitmap for the Dive Window Bitmap
    //----------------------------------------------------------------------------------//
    class CBitmapDepthScale : CBitmapSpe
    {
        // X and Y label placements for depth or coordinate hash marks.
        private SLIDINGHASHMARKSCALESET m_sliderSet;
        private int m_pixelPosMinTopSlider; // minimum top-most pixel position the left slider can be set to
        private int m_pixelPosMaxBottomSlider; // maximum bottom-most pixel position the right slider can be set to.
        private int m_sliderRange; // slider range value (max - min);
        private float m_scaleValue; // current scale based upon minimum and maximum slider positions
        private SLIDINGHASHRESULTS m_results;

        public SLIDINGHASHRESULTS slideResults { get { return m_results; } }

        //------------------------------------------------------------------------------//
        // Time Scale Bitmap Constructor
        //------------------------------------------------------------------------------//
        public CBitmapDepthScale(Rectangle BitmapRec)
            : base(BitmapRec)
        {
            m_dataRect = m_displayRect;
            m_pixelPosMinTopSlider = m_dataRect.Y;// +(BMPCONSTS.MOVEABLE_HASH_THICKNESS - 1)/2;
            m_pixelPosMaxBottomSlider = m_dataRect.Y + m_dataRect.Height - 1; // (BMPCONSTS.MOVEABLE_HASH_THICKNESS-1)/2 - 1;
            m_sliderRange = m_pixelPosMaxBottomSlider - m_pixelPosMinTopSlider + 1;

            // time scale sliders
            m_sliderSet.min.point = new Point(m_dataRect.X, m_pixelPosMinTopSlider);
            m_sliderSet.max.point = new Point(m_dataRect.X, m_pixelPosMaxBottomSlider);
            m_sliderSet.mid.point = new Point(m_dataRect.X + m_dataRect.Width/2, m_pixelPosMinTopSlider + (m_pixelPosMaxBottomSlider - m_pixelPosMinTopSlider)/2);
            m_scaleValue = (float)(m_pixelPosMaxBottomSlider - m_pixelPosMinTopSlider) / (float)m_sliderRange;

            //m_bitmap = new Bitmap(m_dataRect.Width, m_dataRect.Height);
        }

        //------------------------------------------------------------------------------//
        // Time Scale Bitmap OnPaint()
        //------------------------------------------------------------------------------//
        public override void MyOnPaint(Graphics GraphicsObject)
        {
            base.MyOnPaint(GraphicsObject);
            GraphicsObject.FillRectangle(m_waterBrush, m_dataRect);


            Point p1 = new Point();
            Point p2 = new Point();

            Pen pen;

            //--------------------------------------//
            // top (minimum) depth scale slider line
            //--------------------------------------//
            p1 = p2 = m_sliderSet.min.point;
            p2.X += m_dataRect.Width;
            // Adjust for the height of the hash mark.
            p1.Y += (BMPCONSTS.MOVEABLE_HASH_THICKNESS-1)/2;
            p2.Y = p1.Y;
            pen = m_scaleHashPen;
            if(m_sliderSet.min.active == true || m_sliderSet.mid.active == true)
                pen = m_activeSideHashMarkPen;
            GraphicsObject.DrawLine(pen, p1, p2);

            //-----------------------------------------//
            // Bottom (maximum) depth scale slider line
            //-----------------------------------------//
            p1 = p2 = m_sliderSet.max.point;
            p1.X += m_dataRect.Width;
            // Adjust for the height of the hash mark.
            p1.Y -= (BMPCONSTS.MOVEABLE_HASH_THICKNESS-1)/2;
            p2.Y = p1.Y;
            pen = m_scaleHashPen;
            if(m_sliderSet.max.active == true || m_sliderSet.mid.active == true)
                pen = m_activeSideHashMarkPen;
            GraphicsObject.DrawLine(pen, p1, p2);

            // connecting depth scale slider line
            // previous code
            p1 = m_sliderSet.min.point;
            p2 = m_sliderSet.max.point;
            p1.X = p2.X = m_dataRect.X + m_dataRect.Width/2;
            p1.Y += BMPCONSTS.MOVEABLE_HASH_THICKNESS;
            p2.Y -= (BMPCONSTS.MOVEABLE_HASH_THICKNESS-1);
            pen = m_scaleBetwenHashPen;
            if(m_sliderSet.mid.active == true)
                pen = m_activeSideHashMarkPen;
            GraphicsObject.DrawLine(pen, p1, p2);

            //GraphicsObject.DrawImage(m_bitmap, m_dataRect.X, m_dataRect.Y);
        }

        //------------------------------------------------------------------------------//
        // Dive Scale Bitmap SetDisplayData()
        //------------------------------------------------------------------------------//
        public override void SetDisplayData(ANIMATSTATEDATABITMAP[] StateData,
            DATA_EXTREMES_SPE_BITMAP StateDataExtremes, Boolean MaintainScaling)
        {
            base.SetDisplayData(StateData, StateDataExtremes, MaintainScaling); // copies it to local variables

            if(MaintainScaling == false)
            {
                // time scale sliders
                m_sliderSet.min.point = new Point(m_dataRect.X, m_pixelPosMinTopSlider);
                m_sliderSet.max.point = new Point(m_dataRect.X, m_pixelPosMaxBottomSlider);
                m_sliderSet.mid.point = new Point(m_dataRect.X + m_dataRect.Width / 2, m_pixelPosMinTopSlider + (m_pixelPosMaxBottomSlider - m_pixelPosMinTopSlider) / 2);
                m_scaleValue = (float)(m_pixelPosMaxBottomSlider - m_pixelPosMinTopSlider) / (float)m_sliderRange;
            }
        }


        //------------------------------------------------------------------------------//
        // Time Scale Bitmap HandleMouseDown
        //------------------------------------------------------------------------------//
        public override Boolean HandleMouseDown(MouseEventArgs e)
        {
            // if a true is returned from HandleMouseDown()
            if(false == LocationWithinDataBitmap(e.Location))
                return false;

            // Check if any slider activates.  Only one slider may be active at a time.
            if(true == CheckSliderActivates(e.Location))
            {
                m_mouseLastPosition = e.Location;
                return true; // only one slider line may be active at once
            }
            return false;
        }
        //------------------------------------------------------------------------------//
        // Time Scale Bitmap HandleMouseUp
        //------------------------------------------------------------------------------//
        public override Boolean HandleMouseUp(MouseEventArgs e)
        {
            // update the mouse move location
            int deltaMeters;
            float metersPerPixle; // meters per pixel of the scaling bitmap, not the
            // dive display bitmap.

            Boolean mouseWithinBitmapBoarder = base.HandleMouseUp(e);

            if(m_sliderSet.min.active == false && m_sliderSet.max.active == false && m_sliderSet.mid.active == false)
                return false;

            m_sliderSet.min.active = m_sliderSet.max.active = m_sliderSet.mid.active = false;


            // Need to calculate starting Y and length of data to display
            metersPerPixle = (float)(m_dataExtremes.zMax-m_dataExtremes.zMin)/(float)m_dataRect.Height;
            m_scaleValue = (float)(m_sliderSet.max.point.Y - m_sliderSet.min.point.Y)/(float)m_sliderRange;

            // Calculate the starting depth.  Results are in meters.
            m_results.start = (int)Math.Floor((m_sliderSet.min.point.Y-m_pixelPosMinTopSlider) * metersPerPixle + m_dataExtremes.zMin);


            // Calculate the ending depth perform a safety check.
            deltaMeters = (int)Math.Ceiling(m_scaleValue * (m_dataExtremes.zMax - m_dataExtremes.zMin));
            m_results.end = m_results.start + deltaMeters;
            if(m_results.end > m_dataExtremes.zMax)
                m_results.end = (int)m_dataExtremes.zMax;

            return true; // update bitmap
        }
        //------------------------------------------------------------------------------//
        // Time Scale Bitmap HandleMouseMove
        //------------------------------------------------------------------------------//
        public override Boolean HandleMouseMove(MouseEventArgs e)
        {
            int mouseMoveDelta;
            int maxY;
            int minY;
            int hashLineDiff;

            // If no sliders are active ignor the mouse move.
            if(m_sliderSet.min.active == false && m_sliderSet.max.active == false && m_sliderSet.mid.active == false)
                return false;

            // If the cursor went off the window stop the mouse move and deactivate the
            // active slider (set all to not active).
            base.HandleMouseMove(e);

            // Update the mouse move position and calculate the change in position.
            m_mouseMove = e.Location;
            mouseMoveDelta = m_mouseMove.Y - m_mouseLastPosition.Y; // only interested in change in X
            hashLineDiff = m_sliderSet.max.point.Y - m_sliderSet.min.point.Y;
            m_mouseLastPosition.Y = m_mouseMove.Y;

            // Handle the mouse move message based on the active slider.
            if(m_sliderSet.min.active == true)
            {
                // Calculate the minimum and maximum Y position for the minimum slider
                // The maximum Y position of the top-most slider is the position of the
                // bottom-most slider minus the thickness of the two slider hash lines
                maxY = m_sliderSet.max.point.Y - (2*BMPCONSTS.MOVEABLE_HASH_THICKNESS - 1);

                // If the mouse cursor went too far to either side stop the mouse move.
                if(m_mouseMove.Y <= m_pixelPosMinTopSlider)
                    m_sliderSet.min.point.Y = m_pixelPosMinTopSlider;
                else if(m_mouseMove.Y >= maxY)
                    m_sliderSet.min.point.Y = maxY;
                else
                    m_sliderSet.min.point.Y = m_mouseMove.Y;
                return true;
            }

            if(m_sliderSet.max.active == true)
            {
                // Calculate the minimum and maximum X position for the minimum slider
                minY = m_sliderSet.min.point.Y + (2*BMPCONSTS.MOVEABLE_HASH_THICKNESS - 1);

                // If the mouse cursor went too far to either side stop the mouse move.
                if(m_mouseMove.Y <= minY)
                    m_sliderSet.max.point.Y = minY;
                else if(m_mouseMove.Y >= m_pixelPosMaxBottomSlider)
                    m_sliderSet.max.point.Y = m_pixelPosMaxBottomSlider;
                else
                    m_sliderSet.max.point.Y = m_mouseMove.Y;
                return true;
            }

            if(m_sliderSet.mid.active == true)
            {
                if(m_sliderSet.max.point.Y + mouseMoveDelta > m_pixelPosMaxBottomSlider)
                {
                    m_sliderSet.max.point.Y = m_pixelPosMaxBottomSlider;
                    m_sliderSet.min.point.Y = m_sliderSet.mid.point.Y = m_pixelPosMaxBottomSlider - hashLineDiff;
                }
                else if(m_sliderSet.min.point.Y + mouseMoveDelta < m_pixelPosMinTopSlider)
                {
                    m_sliderSet.min.point.Y = m_sliderSet.mid.point.Y = m_pixelPosMinTopSlider;
                    m_sliderSet.max.point.Y = m_pixelPosMinTopSlider + hashLineDiff;
                }
                else
                {
                    m_sliderSet.max.point.Y += mouseMoveDelta;
                    m_sliderSet.mid.point.Y += mouseMoveDelta;
                    m_sliderSet.min.point.Y += mouseMoveDelta;
                }
                return true;
            }
            return false;
        }

        public Boolean CheckSliderActivates(Point Location)
        {
            m_sliderSet.min.active = false;
            m_sliderSet.max.active = false;

            // Check if any slider exists at the passed in Point.  If so, activate it, return true.
            if(Location.Y >= m_sliderSet.min.point.Y &&
                Location.Y < m_sliderSet.min.point.Y + BMPCONSTS.MOVEABLE_HASH_THICKNESS)
            {
                m_sliderSet.min.active = true;
                return true;
            }
            else if(Location.Y <= m_sliderSet.max.point.Y &&
                Location.Y > m_sliderSet.max.point.Y - BMPCONSTS.MOVEABLE_HASH_THICKNESS)
            {
                m_sliderSet.max.active = true;
                return true;
            }
            else if(Location.Y >= m_sliderSet.min.point.Y &&
                Location.Y <= m_sliderSet.max.point.Y &&
                Location.X >= m_sliderSet.mid.point.X - BMPCONSTS.BETWEEN_MOVEABLE_HASH_THICKNESS &&
                Location.X <= m_sliderSet.mid.point.X + BMPCONSTS.BETWEEN_MOVEABLE_HASH_THICKNESS)
            {
                m_sliderSet.mid.active = true;
                return true;
            }
            return false;
        }
    }
}
