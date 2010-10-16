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
    class CBitmapSpeMovement : CBitmapSpe
    {
        public int m_startMeters;
        public int m_endMeters;

        //------------------------------------------------------------------------------//
        // Movement Bitmap Constructor
        //------------------------------------------------------------------------------//
        public CBitmapSpeMovement(Rectangle BitmapRec)
            : base(BitmapRec)
        {
            int i;
            int numHashMarkRegionsX = BMPCONSTS.NUM_MOVMENT_X_HASHMARKS - 1;
            int numHashMarkRegionsY = BMPCONSTS.NUM_MOVMENT_Y_HASHMARKS - 1;

            // Allocate the hashmarks
            m_hashMarkArrayYLeft = new HASHMARK[BMPCONSTS.NUM_MOVMENT_Y_HASHMARKS];
            m_hashMarkArrayXBottom = new HASHMARK[BMPCONSTS.NUM_MOVMENT_X_HASHMARKS];

            //--------------------------------------------------------------------------//
            // Override default rect settings set in the base constructor
            //-----------------------------------------------------------//
            m_dataRect = m_windowRect;

            m_displayRect.X = m_dataRect.X - BMPCONSTS.HASH_REGION_VERT_WIDTH_MARK;
            m_displayRect.Y = m_dataRect.Y - BMPCONSTS.HASH_REGION_HORZ_HEIGHT_NOMARK;
            m_displayRect.Width = m_dataRect.Width + BMPCONSTS.HASH_REGION_VERT_WIDTH_MARK + BMPCONSTS.HASH_REGION_VERT_WIDTH_NOMARK;
            m_displayRect.Height = m_dataRect.Height + BMPCONSTS.HASH_REGION_HORZ_HEIGHT_NOMARK + BMPCONSTS.HASH_REGION_HORZ_HEIGHT_MARK;

            m_windowRect = m_displayRect;
            m_windowRect.X -= BMPCONSTS.BOARDER_THICKNESS;
            m_windowRect.Y -= BMPCONSTS.BOARDER_THICKNESS;
            m_windowRect.Width += 2 * BMPCONSTS.BOARDER_THICKNESS;
            m_windowRect.Height += 2 * BMPCONSTS.BOARDER_THICKNESS;
            //--------------------------------------------------------------------------//

            m_bitmap = new Bitmap(m_dataRect.Width, m_dataRect.Height);


            // Determine the placement of the hashmarks on the X-axis
            m_pixelsPerHashmarkRegionY = m_dataRect.Height / numHashMarkRegionsY; // calculates pixels per hashmark.
            for(i = 0; i < m_hashMarkArrayYLeft.Length; i++)
            {
                m_hashMarkArrayYLeft[i].point.X = m_displayRect.X + BMPCONSTS.HASH_REGION_VERT_WIDTH_MARK;
                m_hashMarkArrayYLeft[i].point.Y = m_dataRect.Y + i * m_pixelsPerHashmarkRegionY;
            }

            // Determine the placement of the hashmarks on the Y-axis
            m_pixelsPerHashmarkRegionX = m_dataRect.Width / numHashMarkRegionsX; // calculates pixels per hashmark.
            for(i = 0; i < m_hashMarkArrayXBottom.Length; i++)
            {
                m_hashMarkArrayXBottom[i].point.X = m_dataRect.X + i * m_pixelsPerHashmarkRegionX;
                m_hashMarkArrayXBottom[i].point.Y = m_displayRect.Y + m_displayRect.Height - BMPCONSTS.HASH_REGION_HORZ_HEIGHT_MARK;
            }

            // Create text brush
            m_textBrush = new SolidBrush(Color.Black);
            m_textFont = new Font("Tahoma", 7.25F, FontStyle.Regular, GraphicsUnit.Point, 0);

        }

        //------------------------------------------------------------------------------//
        // Movement Bitmap OnPaint()
        //------------------------------------------------------------------------------//
        public override void MyOnPaint(Graphics GraphicsObject)
        {
            base.MyOnPaint(GraphicsObject);
            int i;
            Point p1, p2;

            GraphicsObject.FillRectangle(m_hashAreaBrush, m_displayRect);
            GraphicsObject.FillRectangle(m_waterBrush, m_dataRect);
            GraphicsObject.DrawImage(m_bitmap, m_dataRect.X, m_dataRect.Y);


            // Draw Y (latitude) hashmarks and hashmark string values.
            for(i = 0; i < m_hashMarkArrayYLeft.Length; i++)
            {
                p1 = p2 = m_hashMarkArrayYLeft[i].point;
                p1.X -= 5;
                GraphicsObject.DrawLine(m_borderPen, p1, p2);
                GraphicsObject.DrawString(m_hashMarkArrayYLeft[i].sz, m_textFont, m_textBrush, p1.X - 25, p2.Y - 7);
            }

            // Draw X (longitude) hashmarks and hashmarks string values
            for(i = 0; i < m_hashMarkArrayXBottom.Length; i++)
            {
                p1 = p2 = m_hashMarkArrayXBottom[i].point;
                p1.Y += 5;
                GraphicsObject.DrawLine(m_borderPen, p1, p2);
                GraphicsObject.DrawString(m_hashMarkArrayXBottom[i].sz, m_textFont, m_textBrush, p1.X - 10, p2.Y + 7);
            }
        }

        //------------------------------------------------------------------------------//
        // Movement Bitmap UpdateDataDisplay()
        //------------------------------------------------------------------------------//
        public override void SetDisplayData(ANIMATSTATEDATABITMAP[] StateData,
            DATA_EXTREMES_SPE_BITMAP StateDataExtremes, Boolean MaintainScaling)
        {
            int xStartMeters;
            int xEndMeters;
            int yStartMeters;
            int yEndMeters;

            base.SetDisplayData(StateData, StateDataExtremes, MaintainScaling);

            m_timeStartIndex = 0;
            m_timeEndIndex = StateData.Length - 1;

            // These are for the hashmarks
            xStartMeters = (int)Math.Floor(StateDataExtremes.xMin); // longitude start
            xEndMeters = (int)Math.Ceiling(StateDataExtremes.xMax); // longitude end
            yStartMeters = (int)Math.Floor(StateDataExtremes.yMin); // latitude start
            yEndMeters = (int)Math.Ceiling(StateDataExtremes.yMax); // latitude end

            if(xStartMeters < yStartMeters)
                m_startMeters = xStartMeters;
            else
                m_startMeters = yStartMeters;

            if(xEndMeters > yEndMeters)
                m_endMeters = xEndMeters;
            else
                m_endMeters = yEndMeters;

            RedrawBitmap();
        }

        //------------------------------------------------------------------------------//
        // Movement Bitmap RedetermineExtremes()
        //------------------------------------------------------------------------------//
        private void RedetermineExtremes()
        {
            int i;
            m_dataExtremes.xMax = m_dataExtremes.xMin = m_stateData[m_timeStartIndex].x;
            m_dataExtremes.yMax = m_dataExtremes.yMin = m_stateData[m_timeStartIndex].y;
            m_dataExtremes.zMax = m_dataExtremes.zMin = m_stateData[m_timeStartIndex].z;


            for(i = m_timeStartIndex; i < m_stateData.Length && i <= m_timeEndIndex; i++)
            {
                if(m_dataExtremes.xMax < m_stateData[i].x)
                    m_dataExtremes.xMax = m_stateData[i].x;

                if(m_dataExtremes.xMin > m_stateData[i].x)
                    m_dataExtremes.xMin = m_stateData[i].x;

                if(m_dataExtremes.yMax < m_stateData[i].y)
                    m_dataExtremes.yMax = m_stateData[i].y;

                if(m_dataExtremes.yMin > m_stateData[i].y)
                    m_dataExtremes.yMin = m_stateData[i].y;

                if(m_dataExtremes.zMax < m_stateData[i].z)
                    m_dataExtremes.zMax = m_stateData[i].z;

                if(m_dataExtremes.zMin > m_stateData[i].z)
                    m_dataExtremes.zMin = m_stateData[i].z;
            }
        }


        //------------------------------------------------------------------------------//
        // Dive Bitmap RescaleTime()
        //------------------------------------------------------------------------------//
        public void RescaleTime(int Start, int End)
        {
            int xStartMeters;
            int xEndMeters;
            int yStartMeters;
            int yEndMeters;

            m_timeStartIndex = Start;
            m_timeEndIndex = End;

            RedetermineExtremes();

            // These are for the hashmarks
            xStartMeters = (int)Math.Floor(m_dataExtremes.xMin); // longitude start
            xEndMeters = (int)Math.Ceiling(m_dataExtremes.xMax); // longitude end
            yStartMeters = (int)Math.Floor(m_dataExtremes.yMin); // latitude start
            yEndMeters = (int)Math.Ceiling(m_dataExtremes.yMax); // latitude end

            if(xStartMeters < yStartMeters)
                m_startMeters = xStartMeters;
            else
                m_startMeters = yStartMeters;

            if(xEndMeters > yEndMeters)
                m_endMeters = xEndMeters;
            else
                m_endMeters = yEndMeters;

            RedrawBitmap();
        }

        //------------------------------------------------------------------------------//
        // Dive Bitmap RedrawBitmap()
        //------------------------------------------------------------------------------//
        public void RedrawBitmap()
        {
            int i;
            int xPixel;
            int yPixel;
            int yPixelTemp;
            int c;
            //double dataRangeX = m_stateDataExtremes.xMax - m_stateDataExtremes.xMin; // longitude
            //double dataRangeY = m_stateDataExtremes.yMax - m_stateDataExtremes.yMin; // latitude
            double maxDataRange;
            float value;

            m_bitmap = new Bitmap(m_dataRect.Width, m_dataRect.Height);

            // Update scaling.
            maxDataRange = m_endMeters - m_startMeters;
            if(maxDataRange == 0)
                maxDataRange = 1; // avoid division by zero.
            m_dataScale.x = (float)(m_dataRect.Width - 1) / (float)(maxDataRange); // pixels per datum
            m_dataScale.y = (float)(m_dataRect.Height - 1) / (float)(maxDataRange); // pixels per datum


            for(i = m_timeStartIndex; i <= m_timeEndIndex && i < m_stateData.Length; i++)
            {
                xPixel = (int)Math.Floor((float)m_dataScale.x * (float)(m_stateData[i].x - m_startMeters));
                yPixelTemp = (int)Math.Floor((float)m_dataScale.y * (float)(m_stateData[i].y - m_startMeters));
                yPixel = (m_dataRect.Height - 1) - yPixelTemp;
                m_bitmap.SetPixel(xPixel, yPixel, GetCurrentBehaviorColor(m_stateData[i].animatState.behavior));
            }

            // Y (latitude) hashmark string
            for(i = 0, c = m_timeStartIndex; i < m_hashMarkArrayYLeft.Length && c <= m_timeEndIndex; i++, c++)
            {
                // previous method
                value = ((float)m_startMeters + (float)i * ((float)(m_endMeters - m_startMeters) / (float)(m_hashMarkArrayYLeft.Length - 1)));
                m_hashMarkArrayYLeft[m_hashMarkArrayYLeft.Length - 1 - i].sz = "" + (int)value;
            }

            // X (longitude) hashmark string
            // c is just assurance that indexing doesn't go out of bounds.
            for(i = 0, c = m_timeStartIndex; i < m_hashMarkArrayXBottom.Length && c <= m_timeEndIndex; i++, c++)
            {
                value = (float)(m_startMeters + (float)i * ((float)(m_endMeters - m_startMeters) / (float)(m_hashMarkArrayXBottom.Length - 1)));
                // previous method
                m_hashMarkArrayXBottom[i].sz = "" + (int)value;

            }
        }


        //------------------------------------------------------------------------------//
        // Movement Bitmap HandleMouseDown
        //------------------------------------------------------------------------------//
        public override Boolean HandleMouseDown(MouseEventArgs e)
        {
            base.HandleMouseDown(e);
            return false;
        }
        //------------------------------------------------------------------------------//
        // Movement Bitmap HandleMouseUp
        //------------------------------------------------------------------------------//
        public override Boolean HandleMouseUp(MouseEventArgs e)
        {
            base.HandleMouseUp(e);
            return false;
        }
        //------------------------------------------------------------------------------//
        // Movement Bitmap HandleMouseMove
        //------------------------------------------------------------------------------//
        public override Boolean HandleMouseMove(MouseEventArgs e)
        {
            base.HandleMouseMove(e);
            return false;
        }
    }
}