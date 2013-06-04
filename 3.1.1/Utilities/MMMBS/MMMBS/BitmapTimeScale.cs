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
    class CBitmapTimeScale : CBitmapSpe
    {
        // X and Y label placements for depth or coordinate hash marks.
        private SLIDINGHASHMARKSCALESET m_sliderSet;
        private int m_pixlePntMinLSlider; // minimum left-most pixel position the left slider can be set to
        private int m_pixlePntMaxRSlider; // maximum right-most pixel position the right slider can be set to.
        private int m_sliderRange; // slider range value (max - min);
        private float m_scaleValue; // current scale based upon minimum and maximum slider positions
        private float m_dataPointsPerPixle;
        private SLIDINGHASHRESULTS m_results;

        public SLIDINGHASHRESULTS slideResults { get { return m_results; } }

        // Testing and debug
        DATASCALE ds = new DATASCALE();


        //------------------------------------------------------------------------------//
        // Time Scale Bitmap Constructor
        //------------------------------------------------------------------------------//
        public CBitmapTimeScale(Rectangle BitmapRec)
            : base(BitmapRec)
        {
            // The call to the base class set up the display rect (m_displayRect) such
            // that its size and location are ready to go for displaying information
            // without having to account for the display area's border space.
            m_dataRect = m_displayRect;
            m_pixlePntMinLSlider = m_dataRect.X; // +(BITMATCONSTANTS.MOVEABLE_HASH_THICKNESS - 1)/2;
            m_pixlePntMaxRSlider = m_dataRect.X + m_dataRect.Width - 1;  // -1 for zero-indexing.  // (BITMATCONSTANTS.MOVEABLE_HASH_THICKNESS-1)/2 - 1;
            m_sliderRange = m_pixlePntMaxRSlider - m_pixlePntMinLSlider + 1; // +1 accounts for zero-indexing.
//            m_scaleValue = (float)(m_pixlePntMaxRSlider - m_pixlePntMinLSlider + 1)/(float)m_sliderRange;

            // time scale sliders
            m_sliderSet.min.point = new Point(m_pixlePntMinLSlider, m_dataRect.Y);
            m_sliderSet.max.point = new Point(m_pixlePntMaxRSlider, m_dataRect.Y);
            m_sliderSet.mid.point = new Point(m_pixlePntMinLSlider + (m_pixlePntMaxRSlider - m_pixlePntMinLSlider)/2, m_dataRect.Y + m_dataRect.Height/2);

            m_scaleValue = (float)(m_sliderSet.max.point.X - m_sliderSet.min.point.X) / (float)m_sliderRange;

            m_bitmap = new Bitmap(m_dataRect.Width, m_dataRect.Height);
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

            //---------------------------------------//
            // left (minimum) depth scale slider line
            //---------------------------------------//
            p1 = p2 = m_sliderSet.min.point;
            p2.Y += m_dataRect.Height;
            // Adjust for the width of the hash mark.
            p1.X += (BMPCONSTS.MOVEABLE_HASH_THICKNESS-1)/2;
            p2.X = p1.X;
            pen = m_scaleHashPen;
            if(m_sliderSet.min.active == true || m_sliderSet.mid.active == true)
                pen = m_activeSideHashMarkPen;
            GraphicsObject.DrawLine(pen, p1, p2);

            //---------------------------------------//
            // right (maximum) depth scale slider line
            //---------------------------------------//
            p1 = p2 = m_sliderSet.max.point;
            p1.Y += m_dataRect.Height;
            // Adjust for the width of the hash mark.
            p1.X -= (BMPCONSTS.MOVEABLE_HASH_THICKNESS-1)/2;
            p2.X = p1.X;
            pen = m_scaleHashPen;
            if(m_sliderSet.max.active == true || m_sliderSet.mid.active == true)
                pen = m_activeSideHashMarkPen;
            GraphicsObject.DrawLine(pen, p1, p2);

            // connecting depth scale slider line
            // previous code
            p1 = m_sliderSet.min.point;
            p2 = m_sliderSet.max.point;
            p1.Y = p2.Y = m_dataRect.Y + m_dataRect.Height/2;
            p1.X += BMPCONSTS.MOVEABLE_HASH_THICKNESS;
            p2.X -= (BMPCONSTS.MOVEABLE_HASH_THICKNESS-1);
            pen = m_scaleBetwenHashPen;
            if(m_sliderSet.mid.active == true)
                pen = m_activeSideHashMarkPen;
            GraphicsObject.DrawLine(pen, p1, p2);

            GraphicsObject.DrawImage(m_bitmap, m_dataRect.X, m_dataRect.Y);
        }

        //------------------------------------------------------------------------------//
        // Dive Scale Bitmap SetDisplayData()
        //------------------------------------------------------------------------------//
        // Called when new data is generated and needs to be displayed.
        public override void SetDisplayData(ANIMATSTATEDATABITMAP[] StateData,
            DATA_EXTREMES_SPE_BITMAP StateDataExtremes, Boolean MaintainScaling)
        {
            int i;
            int xPixel;
            int zPixel;
            float delta;
            base.SetDisplayData(StateData, StateDataExtremes, MaintainScaling);



            m_bitmap = new Bitmap(m_dataRect.Width, m_dataRect.Height);

            if(StateData.Length > 1 && m_dataRect.Width > 1)
                m_dataScale.x = ((float)m_dataRect.Width-1/*-1 for zero index*/)/(float)(StateData.Length-1); // pixels per datum
            else
                m_dataScale.x = 1.0F;

            delta = (float)(StateDataExtremes.zMax - StateDataExtremes.zMin);
            if(delta <= 0)
                delta = 1;

            if(m_dataRect.Height > 1)
                m_dataScale.y = (m_dataRect.Height - 1)/delta; // pixels per datum
            else
                m_dataScale.y = 1.0F;

            for(i=0; i<m_stateData.Length; i++)
            {
                xPixel = (int)Math.Floor(m_dataScale.x*i); // minus 1 for zero index
                zPixel = (int)Math.Floor(m_dataScale.y*(m_stateData[i].z - StateDataExtremes.zMin));

                ds.x = m_dataScale.x;
                ds.y = m_dataScale.y;

                m_bitmap.SetPixel(xPixel, zPixel, GetCurrentBehaviorColor(m_stateData[i].animatState.behavior));
            }

            // Calculate the number of data points each pixel represents.
            m_dataPointsPerPixle = (float)m_stateData.Length / (float)m_dataRect.Width;

            if(MaintainScaling == false)
            {
                // time scale sliders
                m_sliderSet.min.point = new Point(m_pixlePntMinLSlider, m_dataRect.Y);
                m_sliderSet.max.point = new Point(m_pixlePntMaxRSlider, m_dataRect.Y);
                m_sliderSet.mid.point = new Point(m_pixlePntMinLSlider + (m_pixlePntMaxRSlider - m_pixlePntMinLSlider) / 2, m_dataRect.Y + m_dataRect.Height / 2);

                m_scaleValue = (float)(m_sliderSet.max.point.X - m_sliderSet.min.point.X) / (float)m_sliderRange;
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
            int dataLength;

            //---------------------------------------------------------//
            // This function call not currently required.
            //Boolean mouseWithinBitmapBoarder = base.HandleMouseUp(e);
            //---------------------------------------------------------//

            // One of the three sliders needs to currently be active (pressed) for event
            // mouse up to generate any kind of response.  If a slider is active then a
            // response is generated even if the mouse up even occurs off the Time Scale
            // bitmap.
            if(m_sliderSet.min.active == false && m_sliderSet.max.active == false && m_sliderSet.mid.active == false)
                return false;

            // Set the state of the sliders to deactivated.
            m_sliderSet.min.active = m_sliderSet.max.active = m_sliderSet.mid.active = false;

            // A slider was moved so first update the scaling value
            m_scaleValue = (float)(m_sliderSet.max.point.X - m_sliderSet.min.point.X)/(float)m_sliderRange;

            // Next update the new starting x data index and perform a safety check
            m_results.start = (int)Math.Floor((m_sliderSet.min.point.X-m_pixlePntMinLSlider) * m_dataPointsPerPixle);
            Debug.Assert(m_results.start >= 0);
            if(m_results.start < 0)
                m_results.start = 0;

            // Next update displayable amount of data displayable (in the dive profile
            // bitmap) based on the scale (data points per pixel) and the amount of data
            // (time, in seconds)
            dataLength = (int)Math.Floor(m_scaleValue * m_stateData.Length);

            // Finally calculate the ending x data index and perform a safety check.
            m_results.end = m_results.start + dataLength;
            if(m_results.end >= m_stateData.Length)
                m_results.end = m_stateData.Length-1;

            return true; // update bitmap
        }
        //------------------------------------------------------------------------------//
        // Time Scale Bitmap HandleMouseMove
        //------------------------------------------------------------------------------//
        public override Boolean HandleMouseMove(MouseEventArgs e)
        {
            int mouseMoveDelta;
            int maxX;
            int minX;
            int hashLineDiff;

            // If no sliders are active ignor the mouse move.
            if(m_sliderSet.min.active == false && m_sliderSet.max.active == false && m_sliderSet.mid.active == false)
                return false;

            // If the cursor went off the window stop the mouse move and deactivate the
            // active slider (set all to not active).
            base.HandleMouseMove(e);

            // Update the mouse move position and calculate the change in position.
            m_mouseMove = e.Location;
            mouseMoveDelta = m_mouseMove.X - m_mouseLastPosition.X; // only interested in change in X
            hashLineDiff = m_sliderSet.max.point.X - m_sliderSet.min.point.X;
            m_mouseLastPosition.X = m_mouseMove.X;

            // Handle the mouse move message based on the active slider.
            if(m_sliderSet.min.active == true)
            {
                // Calculate the minimum and maximum X position for the left-most slider.
                // The maximum X position of the left-most slider is the position of the
                // right-most slider minus the thickness of the two slider hash lines.
                maxX = m_sliderSet.max.point.X - (2 * BMPCONSTS.MOVEABLE_HASH_THICKNESS - 1);

                // If the mouse cursor went too far to either side stop the mouse move.
                if(m_mouseMove.X <= m_pixlePntMinLSlider)
                    m_sliderSet.min.point.X = m_pixlePntMinLSlider;
                else if(m_mouseMove.X >= maxX)
                    m_sliderSet.min.point.X = maxX;
                else
                    m_sliderSet.min.point.X = m_mouseMove.X;
                return true;
            }

            if(m_sliderSet.max.active == true)
            {
                // Calculate the minimum and maximum X position for the minimum slider
                minX = m_sliderSet.min.point.X + (2* BMPCONSTS.MOVEABLE_HASH_THICKNESS - 1);

                // If the mouse cursor went too far to either side stop the mouse move.
                if(m_mouseMove.X <= minX)
                    m_sliderSet.max.point.X = minX;
                else if(m_mouseMove.X >= m_pixlePntMaxRSlider)
                    m_sliderSet.max.point.X = m_pixlePntMaxRSlider;
                else
                    m_sliderSet.max.point.X = m_mouseMove.X;
                return true;
            }

            if(m_sliderSet.mid.active == true)
            {
                if(m_sliderSet.max.point.X + mouseMoveDelta > m_pixlePntMaxRSlider)
                {
                    m_sliderSet.max.point.X = m_pixlePntMaxRSlider;
                    m_sliderSet.min.point.X = m_sliderSet.mid.point.X = m_pixlePntMaxRSlider - hashLineDiff;
                }
                else if(m_sliderSet.min.point.X + mouseMoveDelta < m_pixlePntMinLSlider)
                {
                    m_sliderSet.min.point.X = m_sliderSet.mid.point.X = m_pixlePntMinLSlider;
                    m_sliderSet.max.point.X = m_pixlePntMinLSlider + hashLineDiff;
                }
                else
                {
                    m_sliderSet.max.point.X += mouseMoveDelta;
                    m_sliderSet.mid.point.X += mouseMoveDelta;
                    m_sliderSet.min.point.X += mouseMoveDelta;
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
            if(Location.X >= m_sliderSet.min.point.X &&
                Location.X < m_sliderSet.min.point.X + BMPCONSTS.MOVEABLE_HASH_THICKNESS)
            {
                m_sliderSet.min.active = true;
                return true;
            }
            else if(Location.X <= m_sliderSet.max.point.X &&
                Location.X > m_sliderSet.max.point.X - BMPCONSTS.MOVEABLE_HASH_THICKNESS)
            {
                m_sliderSet.max.active = true;
                return true;
            }
            else if(Location.X >= m_sliderSet.min.point.X &&
                Location.X <= m_sliderSet.max.point.X &&
                Location.Y >= m_sliderSet.mid.point.Y - BMPCONSTS.BETWEEN_MOVEABLE_HASH_THICKNESS &&
                Location.Y <= m_sliderSet.mid.point.Y + BMPCONSTS.BETWEEN_MOVEABLE_HASH_THICKNESS)
            {
                // checks if mouse is on middle line
                m_sliderSet.mid.active = true;
                return true;
            }
            return false;
        }
    }
}