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
    //----------------------------------------------------------------------------------//
    // Species Dive Bitmap Window
    //----------------------------------------------------------------------------------//
    public struct DisplayInf
    {
        public Boolean noDisplay;

        public double fDepthStartMeters; // shallowest depth (surface)
        public double fDepthEndMeters;   // deepest displayed

        public int nDepthStartMeters; // shallowest depth (surface)
        public int nDepthEndMeters;   // deepest displayed

        public int nSetBathyDepth;
        public double fSetBathyDepth;
    }




    class CBitmapSpeDiveProfile : CBitmapSpe
    {

        //DepthDisplayInformation ddi;
        DisplayInf m_displayInf;
        //private int m_timeEndIndex;


        // Fixed bathy depth the scenario is set to run at.  Used to determine if and
        // where to display bathymetry depth on the dive profile.
        private Color[] m_prevColorArray = null;
        private int m_nPrevColoredRow = -1;

        private Boolean m_displayBathy = true;
        private Boolean m_displayTarget = true;
        public Color BathyColor{ get { return BMPCONSTS.GROUND_COLOR; } }
        public Color TargetDepthColor { get { return BMPCONSTS.TARGET_DEPTH_COLOR; } }
        public Boolean DisplayBathy
        { get { return m_displayBathy; }
          set
          {
              m_displayBathy = value;
              ConstructBitmap();
          } 
        }
        public Boolean DisplayTargetDepth
        {
            get { return m_displayTarget; }
            set
            {
                m_displayTarget = value;
                ConstructBitmap();
            }
        }


        //------------------------------------------------------------------------------//
        // Dive Bitmap Constuctor
        //------------------------------------------------------------------------------//
        public CBitmapSpeDiveProfile(Rectangle BitmapRec)
            : base(BitmapRec) // CBitmapSpeciesParent() constructor
        {
            int i;

            //            m_bBathyUserSet = false;
            //          m_bLeftMouseDown = false;
            // m_displayRect is the remaining region after boarders have been taken away

            m_dataRect = new Rectangle();
            m_dataRect.X = m_displayRect.X + BMPCONSTS.HASH_REGION_VERT_WIDTH_MARK;
            m_dataRect.Y = m_displayRect.Y + BMPCONSTS.HASH_REGION_HORZ_HEIGHT_NOMARK;
            m_dataRect.Width = m_displayRect.Width - BMPCONSTS.HASH_REGION_VERT_WIDTH_MARK - BMPCONSTS.HASH_REGION_VERT_WIDTH_NOMARK;
            m_dataRect.Height = m_displayRect.Height - BMPCONSTS.HASH_REGION_HORZ_HEIGHT_NOMARK - BMPCONSTS.HASH_REGION_HORZ_HEIGHT_MARK;

            m_bitmap = new Bitmap(m_dataRect.Width, m_dataRect.Height);

            // Hashmarks
            m_hashMarkArrayYLeft = new HASHMARK[BMPCONSTS.NUM_DEPTH_HASHMARKS];
            m_hashMarkArrayXBottom = new HASHMARK[BMPCONSTS.NUM_TIME_HASHMARKS];

            m_pixelsPerHashmarkRegionY = m_dataRect.Height / (BMPCONSTS.NUM_DEPTH_HASHMARKS - 1); // calculates pixels per hashmark.
            for(i = 0; i < m_hashMarkArrayYLeft.Length; i++)
            {
                m_hashMarkArrayYLeft[i].point.X = m_displayRect.X + BMPCONSTS.HASH_REGION_VERT_WIDTH_MARK;
                m_hashMarkArrayYLeft[i].point.Y = m_dataRect.Y + i * m_pixelsPerHashmarkRegionY;
            }

            // calculates pixels per hashmark region.
            m_pixelsPerHashmarkRegionX = m_dataRect.Width / (BMPCONSTS.NUM_TIME_HASHMARKS - 1);


            for(i = 0; i < m_hashMarkArrayXBottom.Length; i++)
            {
                m_hashMarkArrayXBottom[i].point.X = m_dataRect.X + i * m_pixelsPerHashmarkRegionX;
                m_hashMarkArrayXBottom[i].point.Y = m_displayRect.Y + m_displayRect.Height - BMPCONSTS.HASH_REGION_HORZ_HEIGHT_MARK;
            }

            // Create text brush
            m_textBrush = new SolidBrush(Color.Black);
            m_textFont = new Font("Tahoma", 7.25F, FontStyle.Regular, GraphicsUnit.Point, 0);

        }



        private int CalcCircBuffIndex(int CurrentIndex, int Offset, int Length)
        {
            int v = CurrentIndex + Offset;

            if(v < 0)
                v = Length + v;

            v = v % Length;
            Debug.Assert(v >= 0);
            Debug.Assert(v < Length);
            if(v < 0 || v >= Length)
                v = 0;

            return v;
        }

        private Point[] UpdateTargetDepthDisplayData()
        {
            int i, c;
            int dataLength = m_timeEndIndex - m_timeStartIndex + 1;
            int deltaMeters = m_displayInf.nDepthEndMeters - m_displayInf.nDepthStartMeters;
            int activeIndex;
            int lastIndex;
            double lastDepth;
            int index;
            Point pt = new Point();
            Point[][] arr = new Point[2][];

            arr[0] = new Point[1];
            arr[1] = new Point[1];

            if(deltaMeters < 1)
                deltaMeters = 1;

            //------------------------------------------//
            // Get the target depth lines end points
            //------------------------------------------//
            activeIndex = 0;
            lastDepth = 0;
            for(i = m_timeStartIndex, c = 0; i <= m_timeEndIndex && i < m_stateData.Length; i++, c++)
            {
                // Animat Depth
                if(m_stateData[i].targetDepth < (float)m_displayInf.nDepthStartMeters ||
                    m_stateData[i].targetDepth > (float)m_displayInf.nDepthEndMeters)
                    continue;

                lastDepth = m_stateData[i].targetDepth;
                index = arr[activeIndex].Length - 1;
                arr[activeIndex][index].X = (int)Math.Floor(m_dataScale.x * c);
                arr[activeIndex][index].Y = (int)Math.Floor(m_dataScale.y * (m_stateData[i].targetDepth - m_displayInf.nDepthStartMeters));
                break; // found the first valid value so break;
            }

            if(i == m_timeEndIndex + 1 || i == m_stateData.Length || lastDepth == 0)
                return null;

            for(i++, c++; i <= m_timeEndIndex && i < m_stateData.Length; i++, c++)
            {
                // Animat Depth
                if(m_stateData[i].targetDepth < (float)m_displayInf.nDepthStartMeters ||
                    m_stateData[i].targetDepth > (float)m_displayInf.nDepthEndMeters)
                    continue;

                // Only iterested in change of depths or indeces that are not the last index.
                if((lastDepth == m_stateData[i].targetDepth) && (i <= m_timeEndIndex - 1 && i < m_stateData.Length - 1))
                    continue;

                lastIndex = activeIndex;
                activeIndex = CalcCircBuffIndex(activeIndex, 1, arr.Length);
                lastDepth = m_stateData[i].targetDepth;
                arr[activeIndex] = new Point[arr[lastIndex].Length + 2];
                arr[lastIndex].CopyTo(arr[activeIndex], 0);

                pt.X = (int)Math.Floor(m_dataScale.x * c);
                pt.Y = (int)Math.Floor(m_dataScale.y * (m_stateData[i].targetDepth - m_displayInf.nDepthStartMeters));

                index = arr[activeIndex].Length - 2;
                arr[activeIndex][index].X = pt.X;
                arr[activeIndex][index].Y = arr[lastIndex][arr[lastIndex].Length - 1].Y;

                index = arr[activeIndex].Length - 1;
                arr[activeIndex][index].X = pt.X;
                arr[activeIndex][index].Y = pt.Y;
                //m_bitmap.SetPixel(xPixel, yPixel, GetCurrentBehaviorColor(m_stateData[i].animatState.behavior));
            }

            return arr[activeIndex];
        }

        //------------------------------------------------------------------------------//
        // Dive Bitmap OnPaint
        //------------------------------------------------------------------------------//
        // Called when an area of the bitmap needs to be redrawn either because the bitmap
        // has been updated or because window has been uncovered.
        //
        // MyOnPaint() is called from from the parent form's (main modeling page here)
        // OnPaint() function call.  Therefore the passed in GraphicsObject is associated
        // with the entire form rather than any specific rect and must be handled
        // accordingly.
        public override void MyOnPaint(Graphics GraphicsObject)
        {
            base.MyOnPaint(GraphicsObject);
            int i;
            Point p1, p2;
            double bathyDepth;

            p1 = new Point();
            p2 = new Point();

            GraphicsObject.FillRectangle(m_hashAreaBrush, m_displayRect);
            GraphicsObject.FillRectangle(m_waterBrush, m_dataRect);
            //GraphicsObject.DrawImage(m_bitmap, m_dataRect.X, m_dataRect.Y);

            //--------------------------------------------------------------------------//
            // Draw depth hashmarks and hashmark string values.
            //-------------------------------------------------//
            for(i = 0; i < m_hashMarkArrayYLeft.Length; i++)
            {
                p1 = p2 = m_hashMarkArrayYLeft[i].point;
                p1.X -= 5;
                GraphicsObject.DrawLine(m_borderPen, p1, p2);
                GraphicsObject.DrawString(m_hashMarkArrayYLeft[i].sz, m_textFont, m_textBrush, p1.X - 25, p2.Y - 7);
            }

            //--------------------------------------------------------------------------//
            // Draw time hashmarks and hashmarks string values
            //-------------------------------------------------//
            for(i = 0; i < m_hashMarkArrayXBottom.Length; i++)
            {
                p1 = p2 = m_hashMarkArrayXBottom[i].point;
                p1.Y += 5;
                GraphicsObject.DrawLine(m_borderPen, p1, p2);

                //m_timeHashMarksArr[i].sz = "" + 5000;
                GraphicsObject.DrawString(m_hashMarkArrayXBottom[i].sz, m_textFont, m_textBrush, p1.X - 10, p2.Y + 7);
            }

            //--------------------------------------------------------------------------//
            // Draw Animat Target Depth
            //-------------------------//
#if false
            depthTargetPointArr = UpdateTargetDepthDisplayData();
            if(depthTargetPointArr == null || depthTargetPointArr.Length < 2)
                depthTargetPointArr = null;

            // Update the values stored in the array of points so that lines representing
            //the animat's target depth are drawn in the proper location
            for(i = 0; depthTargetPointArr != null && i < depthTargetPointArr.Length; i++)
            {
                depthTargetPointArr[i].X += (m_dataRect.X);
                depthTargetPointArr[i].Y += (m_dataRect.Y);
            }

            if(depthTargetPointArr != null)
            {
                try
                {
                    GraphicsObject.DrawLines(m_targetDepthPen, depthTargetPointArr);
                }
                catch
                {
                    depthTargetPointArr[i].X = depthTargetPointArr[i].X;
                }
            }
#endif

            if(m_displayBathy == true)
            {

                //--------------------------------------------------------------------------//
                // Draw Bathymetry Depth
                //-------------------------//
                // Can use the first bathy depth at index zero since bathy depth is constant
                // for the duration of the scenario.
                bathyDepth = -5000;
                // The first animat state doesn't contain a proper bathymetry depth.
                if(m_stateData.Length > 1)
                    bathyDepth = -m_stateData[1].animatState.bathyDepth;

                if(bathyDepth >= (float)m_displayInf.nDepthStartMeters && bathyDepth <= (float)m_displayInf.nDepthEndMeters)
                {
                    p1.X = m_dataRect.X;
                    p2.X = m_dataRect.X + m_dataRect.Width - 1;
                    p1.Y = p2.Y = m_dataRect.Y +
                    (int)Math.Floor(m_dataScale.y * (bathyDepth - m_displayInf.nDepthStartMeters));
                    if(p1.Y >= 0)// && p2.Y < m_dataRect.Y
                        GraphicsObject.DrawLine(m_bathyPen, p1, p2);
                }
            }
            GraphicsObject.DrawImage(m_bitmap, m_dataRect.X, m_dataRect.Y);

        }


        //------------------------------------------------------------------------------//
        // Dive Bitmap ConstructBitmap()
        //------------------------------------------------------------------------------//
        // Called when the bitmap needs to be updated because data has changed or desired
        // characteristics of how the data is displayed has changed.  Does not actually
        // draw the bitmap.  The bitmap is drawn when the OnMyPaint() function is called.
        public void ConstructBitmap()
        {
            int i, j, k;
            int nVal;
            float z;
            int xPixel;
            int yPixel;
            int previousYPixel = -1;
            float prevTargetDepth = -1;
            int c;
            int dataLength = m_timeEndIndex - m_timeStartIndex + 1;
            int deltaMeters = m_displayInf.nDepthEndMeters - m_displayInf.nDepthStartMeters;
            if(deltaMeters < 1)
                deltaMeters = 1;
            Boolean nextBehPreviouslySel = false;

            m_bitmap.Dispose();
            m_bitmap = new Bitmap(m_dataRect.Width, m_dataRect.Height);


            //--------------//
            // Target Depth
            //--------------//
            if(m_displayTarget == true)
            {
                for(i = m_timeStartIndex, c = 0; i <= m_timeEndIndex && i < m_stateData.Length; i++, c++)
                {
                    z = m_stateData[i].targetDepth;

                    if(z <= (float)m_displayInf.nDepthStartMeters || z >= (float)m_displayInf.nDepthEndMeters)
                        continue;
                    xPixel = (int)Math.Floor(m_dataScale.x * c);
                    yPixel = (int)Math.Floor(m_dataScale.y * (z - m_displayInf.nDepthStartMeters));
                    m_bitmap.SetPixel(xPixel, yPixel, BMPCONSTS.TARGET_DEPTH_COLOR);

                    // If the previous target depth doesn't equal the current target depth
                    // then draw a line vertical line between the two.
                    if(prevTargetDepth != z)
                    {
                        if(prevTargetDepth != -1 && previousYPixel != -1)
                        {
                            while(previousYPixel != yPixel)
                            {
                                m_bitmap.SetPixel(xPixel, previousYPixel, BMPCONSTS.TARGET_DEPTH_COLOR);
                                if(previousYPixel < yPixel)
                                    previousYPixel++;
                                else
                                    previousYPixel--;
                            }
                        }
                        prevTargetDepth = z;
                    }
                    previousYPixel = yPixel;
                }
            }


            //------------------------------------------//
            // Draw the animat dive profile data
            //------------------------------------------//
            for(i = m_timeStartIndex, c = 0; i <= m_timeEndIndex && i < m_stateData.Length; i++, c++)
            {
                if(m_stateData[i].animatState.nextBehaviorSelected == 0 && nextBehPreviouslySel == true)
                    nextBehPreviouslySel = false;

                if(m_stateData[i].animatState.nextBehaviorSelected == 1 && nextBehPreviouslySel == false)
                {
                    //--------------//
                    // Animat Depth
                    //--------------//
                    z = m_stateData[i].z;
                    if(z < (float)m_displayInf.nDepthStartMeters || z > (float)m_displayInf.nDepthEndMeters)
                        continue;

                    xPixel = (int)Math.Floor(m_dataScale.x * c);
                    yPixel = (int)Math.Floor(m_dataScale.y * (z - m_displayInf.nDepthStartMeters));


                    nextBehPreviouslySel = true;

                    for(j = -3; j <= 3; j++)
                    {
                        for(k = -3; k <= 3; k++)
                        {
                            if(xPixel+j < 0 || xPixel+j >= m_bitmap.Width)
                                continue;
                            if(yPixel+k < 0 || yPixel+k >= m_bitmap.Height)
                                continue;

                            // Make it no look so square.
                            if((j == k && j == 3) || (j==k && j == -3))
                                continue;
                            m_bitmap.SetPixel(xPixel + j, yPixel + k, GetCurrentBehaviorColor((uint)m_stateData[i].animatState.nextBehavior));
                        }
                    }
                }
            }


            //---------------------//
            // Animat Dive Profile
            //---------------------//
            for(i = m_timeStartIndex, c = 0; i <= m_timeEndIndex && i < m_stateData.Length; i++, c++)
            {
                z = m_stateData[i].z;

                if(z <= (float)m_displayInf.nDepthStartMeters || z >= (float)m_displayInf.nDepthEndMeters)
                    continue;
                xPixel = (int)Math.Floor(m_dataScale.x * c);
                yPixel = (int)Math.Floor(m_dataScale.y * (z - m_displayInf.nDepthStartMeters));
                m_bitmap.SetPixel(xPixel, yPixel, GetCurrentBehaviorColor(m_stateData[i].animatState.behavior));
            }



            //------------------------//
            // Time hashmark strings
            //------------------------//
            // c is just assurance that indexing doesn't go out of bounds.
            for(i = 0, c = m_timeStartIndex; i < m_hashMarkArrayXBottom.Length && c <= m_timeEndIndex; i++, c++)
            {
                nVal = (m_timeStartIndex + i * (int)(m_pixelsPerHashmarkRegionX / m_dataScale.x));
                m_hashMarkArrayXBottom[i].sz = "" + (nVal);
            }


            //------------------------//
            // Depth hashmark strings
            //------------------------//
            for(i = 0, c = m_timeStartIndex; i < m_hashMarkArrayYLeft.Length && c <= m_timeEndIndex; i++, c++)
            {
                nVal = (m_displayInf.nDepthStartMeters + i *
                    ((m_displayInf.nDepthEndMeters - m_displayInf.nDepthStartMeters) /
                    (m_hashMarkArrayYLeft.Length - 1)));
                m_hashMarkArrayYLeft[i].sz = "" + (-nVal);
            }
        }


        public double DepthValueAtPoint(Point P)
        {
            double depth;
            double percentDeep;

            Debug.Assert(true == LocationWithinDataBitmap(P));
            P.Y -= m_dataRect.Y;
            percentDeep = (double)P.Y / (double)(m_dataRect.Height);
            depth = (m_displayInf.nDepthStartMeters + percentDeep * (m_displayInf.nDepthEndMeters - m_displayInf.nDepthStartMeters));
            return depth;
        }

        //------------------------------------------------------------------------------//
        // Dive Bitmap SetDisplayData()
        //------------------------------------------------------------------------------//
        public override void SetDisplayData(ANIMATSTATEDATABITMAP[] StateData, DATA_EXTREMES_SPE_BITMAP DataExtremes,
            Boolean MaintainScaling)
        {
            Debug.Assert(StateData != null);

            m_stateData = StateData;
            m_dataExtremes = DataExtremes;

            m_displayInf.noDisplay = false;
            if(StateData.Length <= 1)
            {
                m_displayInf.noDisplay = true;
                return;
            }

            if(MaintainScaling == false)
            {

                // Initially (until user scales the time) the number of seconds to
                // display is equal to the length of the data array passed in.
                m_timeStartIndex = 0;
                m_timeEndIndex = m_stateData.Length - 1;

                m_displayInf.fDepthStartMeters = DataExtremes.zMin;
                m_displayInf.fDepthEndMeters = DataExtremes.zMax;

                m_displayInf.nDepthStartMeters = (int)Math.Floor(DataExtremes.zMin);
                m_displayInf.nDepthEndMeters = (int)Math.Ceiling(DataExtremes.zMax);

                // Scale the display based on the passed in display data.
                UpdateXYScale();
            }

            // Redraw the bitmap.
            ConstructBitmap();
        }


        private void UpdateXYScale()
        {
            int dataLength;
            int deltaMeters;

            /*
             *       0  1  2  3  4  5  6  7  8  9 10 11
             *             S                    E         length = 8: 9-2+1 = 8
             *            S/E                             length = 1: 2-2+1 = 1
             */
            dataLength = m_timeEndIndex - m_timeStartIndex + 1;
            deltaMeters = m_displayInf.nDepthEndMeters - m_displayInf.nDepthStartMeters;

            //---------------------/
            // Testing and Debug
            //deltaMeters = 1;
            //---------------------/

            Debug.Assert(dataLength != 0 && deltaMeters != 0);

            //--------------------------------------------------------------------------//
            // Get the scaling in the X (time) dimension and Y (depth) dimensions
            //-------------------------------------------------------------------//
            // Each index represents one second of time.
            //m_dataScale.x = (float)Math.Ceiling(((float)m_dataRect.Width) / (float)(dataLength)); // pixels per data point
            //m_dataScale.y =  (float)Math.Ceiling((float)m_dataRect.Height / (float)(deltaMeters)); // pixels per datum
            m_dataScale.x = ((float)m_dataRect.Width) / (float)(dataLength); // pixels per data point
            m_dataScale.y = (float)m_dataRect.Height / (float)(deltaMeters); // pixels per datum
        }


        //------------------------------------------------------------------------------//
        // Dive Bitmap RescaleTime()
        //------------------------------------------------------------------------------//
        public void SetTimeIndices(int StartIndex, int EndIndex)
        {
            if(StartIndex == EndIndex)
                return;// do nothing.  Bad value entered.

            // Set appropriate member variables
            m_timeStartIndex = StartIndex;
            m_timeEndIndex = EndIndex;

            // Scale the display based on the scaled time then redraw the bitmap.
            UpdateXYScale();
            ConstructBitmap();
        }

        //------------------------------------------------------------------------------//
        // Dive Bitmap RescaleDepth()
        //------------------------------------------------------------------------------//
        public void ScaleDepthRange(int Shallow, int Deep)
        {
            if(Shallow == Deep || Shallow > Deep)
                return; // do nothing.  Bad entry.
            if(/*Shallow < 0 || */Deep < 0)
                return; // do nothing, bad entry.

            // Set appropriate member variables.
            m_displayInf.nDepthStartMeters = Shallow;
            m_displayInf.nDepthEndMeters = Deep;

            // Scale the display based on the scaled depth then redraw the bitmap.
            UpdateXYScale();
            ConstructBitmap();
        }

        private void DrawBathyLine(int Row)
        {
            int i;
            Color[] colorArray;

            // If the clicked on row is the same as the previous then do nothing.
            if(Row == m_nPrevColoredRow)
                return;

            // Calculate depth associated with this row.  If it is too shallow then
            // ignore.


            // Copy the current color values of the pixels to be written into into a
            // temporary array and set pixels to ground color.
            colorArray = new Color[m_dataRect.Width];
            for(i = 0; i < m_dataRect.Width; i++)
            {
                colorArray[i] = m_bitmap.GetPixel(i, Row);
                m_bitmap.SetPixel(i, Row, Color.BlanchedAlmond);
            }

            if(m_prevColorArray != null)
            {
                for(i = 0; i < m_dataRect.Width; i++)
                    m_bitmap.SetPixel(i, m_nPrevColoredRow, m_prevColorArray[i]);
            }

            m_prevColorArray = colorArray;
            m_nPrevColoredRow = Row;
        }

        //------------------------------------------------------------------------------//
        // Dive Bitmap HandleMouseDown
        //------------------------------------------------------------------------------//
        public override Boolean HandleMouseDown(MouseEventArgs e)
        {
            base.HandleMouseDown(e);

            if(LocationWithinDataBitmap(e.Location) == true)
            {
                if(e.Button == MouseButtons.Left)
                {
                    //m_bLeftMouseDown = true;
                    DrawBathyLine(e.Y - m_dataRect.Y);
                }
                else if(e.Button == MouseButtons.Right)
                {
//                    m_bLeftMouseDown = false;
                }
                return true;// returns true to tell calling routing to invalidate rect.
            }
            return false;
        }
        //------------------------------------------------------------------------------//
        // Dive Bitmap HandleMouseUp
        //------------------------------------------------------------------------------//
        public override Boolean HandleMouseUp(MouseEventArgs e)
        {
            base.HandleMouseUp(e);
            return false;
        }
        //------------------------------------------------------------------------------//
        // Dive Bitmap HandleMouseMove
        //------------------------------------------------------------------------------//
        public override Boolean HandleMouseMove(MouseEventArgs e)
        {
            base.HandleMouseMove(e);
            return false;
        }
    }
}