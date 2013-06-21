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
    // Manager class exists so that it can coordinate responses of mulitple bitmap windows
    // to the events of any single bitmap window.

    // Currently the behavior transition as a function of time form has only a single
    // bitmap window associated with it so there is no need for a bitmap manager but one
    // is being used for consistency and in case additional bitmap windows are added to
    // the Behavior Transition as a Function of Time Form.
    public struct MULTIBEHTRANSSTRUCT
    {
        public mbsSNGLBEHTRANSTRUCT behTrans;
    }

    public struct ROWCOL
    {
        public int row;
        public int col;
    }

    //public enum BITMAPDISPLAYTYPE { INITIAL_BEHAVIOR, BEHAVIOR_TRANSITION}



    public struct TRANSPERIOD
    {
        public int maxDuration; // Of all trials conducted for this transition period the maximum time before the behavior transitioned
        public int minDuration; // Of all trials conducted for this transition period the manimum time before the behavior transitioned
        public double aveDuration; // The average time of all trials before the behavior transitioned to a new behaivor.
        public int[] toBehaviorCount; // The counts of the behavior index that the behavior tranistioned into.
    }

    public struct SINGLEBEHTRANSSTATS
    {
        // The number of translational periods defined in the behavior transition model.
        // Equivelent to the number of rows in the behavior transition matrix.
        public int periodCnt;

        // The number of trials conducted per translational period.  This value is defined
        // by the application calling the behavior translation test.
        public int trialsPerPeriodCnt;

        // Maximums of all time periods and all trials.  Used for scaling the bitmap
        // display and determining hashmark values.
        public int maxPeriodDuration;
        public int minPeriodDuration; // not useful at this time but may be later.
        public int maxBehaviorCount;
        // a minBehaviorCount would be zero.

        // The trial results in a format usable by the process that displays those results
        // in a bitmap.
        public TRANSPERIOD[] periodArray;
    }

    class CBitmapSingleBehaviorTimeTransitionManager
    {
        //-----------------//
        // Member Variables
        //-----------------//
        private CBitmapSingleBehaviorTimeTransition m_transBitMap; // The bitmap window class for displaying data
        private Rectangle m_transDurationMinRect;
        private Rectangle m_transDurationAveRect;
        private Rectangle m_transDurationMaxRect;
        private Rectangle[] m_behaviorColorRectArray;
        MBSDEFAULTS.BITMAPDISPLAYTYPE m_displayType;
        private int m_numBehaviors;

        private Form m_parentForm; // The C# form (panel, as Dorians calls them) the bitmap is displayed on.

        //------------//
        // Constructor
        //------------//
        public CBitmapSingleBehaviorTimeTransitionManager(Form Parent,
            MBSDEFAULTS.BITMAPDISPLAYTYPE DisplayType, // display type, either initial behavor or behavior transition
            Rectangle TransitionRect, // bitmap setup of the main (results) display
            Rectangle TransDurationMaxRect,// bitmap setup of the maximum time rectangle key
            Rectangle TransDurationAveRect,// bitmap setup of the average time rectangles key
            Rectangle TransDurationMinRect, // bitmap setup of the minimum time rectangle key
            Rectangle[] BehaviorColorRectArray) // bitmaps setup of the behavior rectangles key
        {
            m_parentForm = Parent;
            m_displayType = DisplayType;
            m_transBitMap = new CBitmapSingleBehaviorTimeTransition(DisplayType, TransitionRect);
            m_transDurationMaxRect = TransDurationMaxRect;
            m_transDurationAveRect = TransDurationAveRect;
            m_transDurationMinRect = TransDurationMinRect;
            m_behaviorColorRectArray = BehaviorColorRectArray;
        }

        // Called when the form is resized
        public void SetWindowRectanglesForm(
            Rectangle TransitionRect,
            Rectangle TransDurationMaxRect,
            Rectangle TransDurationAveRect,
            Rectangle TransDurationMinRect,
            Rectangle[] BehaviorColorRectArray)
        {
            m_transBitMap.SetWindowRect(TransitionRect);

            // These windows are used for the key showing color associations
            m_transDurationMaxRect = TransDurationMaxRect;
            m_transDurationAveRect = TransDurationAveRect;
            m_transDurationMinRect = TransDurationMinRect;
            m_behaviorColorRectArray = BehaviorColorRectArray;

            // Should there be other bitmap windows (that resize) add their SetWindowRects here

            // Invalidate the rect of the parent form that the data is displayed in so
            // that the OS displays the updated display.
            m_parentForm.Invalidate();
        }


        //------------------------------------------------------------------------------//
        // Behavior Transition Over Time Manager UpdateDataDisplay()
        //------------------------------------------------------------------------------//
        // Called by the parent form when data has changed or a characteristic about how
        // the data is to be displayed has been modified.
        public void SetDisplayData(int NumBehaviors, mbsSNGLBEHTRANSTRUCT Data)
        {
            m_numBehaviors = NumBehaviors;


            // Have the bitmap class update its data display.
            m_transBitMap.SetDisplayData(NumBehaviors, Data);

            // Invalidate the rect of the parent form that the data is displayed in so
            // that the OS displays the updated display.
            m_parentForm.Invalidate();
        }
        //------------------------------------------------------------------------------//
        // Behavior Transition Over Time Manager HandleMouseDown()
        //------------------------------------------------------------------------------//
        public Boolean HandleMouseDown(MouseEventArgs e)
        {
            // Have the bitmap class(es) update its/their data display by taking action
            // based upon a mouse down even.  A return of 'true' means the data display
            // was updated and the parent form needs to invalidate the rect so the display
            // area is updated by the OS.
            if(m_transBitMap.HandleMouseDown(e) == true)
                m_parentForm.Invalidate(m_transBitMap.m_displayRect);
            return true;
        }
        //------------------------------------------------------------------------------//
        // Behavior Transition Over Time Manager HandleMouseUp()
        //------------------------------------------------------------------------------//
        public Boolean HandleMouseUp(MouseEventArgs e)
        {
            // Have the bitmap class(es) update its/their data display by taking action
            // based upon a mouse up even.  A return of 'true' means the data display
            // was updated and the parent form needs to invalidate the rect so the display
            // area is updated by the OS.
            if(m_transBitMap.HandleMouseUp(e) == true)
                m_parentForm.Invalidate(m_transBitMap.m_displayRect);
            return true;
        }
        //------------------------------------------------------------------------------//
        // Behavior Transition Over Time Manager HandleMouseMove()
        //------------------------------------------------------------------------------//
        public Boolean HandleMouseMove(MouseEventArgs e)
        {
            // Have the bitmap class(es) update its/their data display by taking action
            // based upon a mouse move even.  A return of 'true' means the data display
            // was updated and the parent form needs to invalidate the rect so the display
            // area is updated by the OS.
            if(m_transBitMap.HandleMouseMove(e) == true)
                m_parentForm.Invalidate(m_transBitMap.m_displayRect);
            return true;
        }

        //------------------------------------------------------------------------------//
        // BitmapManager OnPaint()
        //------------------------------------------------------------------------------//
        public void MyOnPaint(Graphics GraphicsObject)
        {
            int i;
            SolidBrush behBrush;

            if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION)
            {
                behBrush = new SolidBrush(BMPCONSTS.TRANS_PER_MAX_COLOR);
                GraphicsObject.FillRectangle(behBrush, m_transDurationMaxRect);

                behBrush = new SolidBrush(BMPCONSTS.TRANS_PER_AVE_COLOR);
                GraphicsObject.FillRectangle(behBrush, m_transDurationAveRect);

                behBrush = new SolidBrush(BMPCONSTS.TRANS_PER_MIN_COLOR);
                GraphicsObject.FillRectangle(behBrush, m_transDurationMinRect);
            }

            // Fill in the small rectangles on the panel associated with each behavior
            // next to the behavior names.
            for(i=0; i< m_numBehaviors; i++)
            {
                // Function GetDefaultColor() returns the normal default display color of
                // the behaviors rather than the current color being used at the moment on
                // the main panel which can change if user selects to de-highlight
                // specific behaviors.
                behBrush = new SolidBrush(BMPCONSTS.GetDefaultColor((uint)i));
                GraphicsObject.FillRectangle(behBrush, m_behaviorColorRectArray[i]);
            }
            m_transBitMap.MyOnPaint(GraphicsObject);
        }
    }

    //***************************************************************************************************************//
    //----------------------------------------------------------------------------------//
    // Behavior Transition Over Time Bitmap Window Parent Class
    //----------------------------------------------------------------------------------//
    class CBitmapSingleBehaviorTimeTransitionParent : CBitmapSpe
    {
        public mbsSNGLBEHTRANSTRUCT m_data;
        public int m_numBehaviors;
        public MBSDEFAULTS.BITMAPDISPLAYTYPE m_displayType;

        //------------------------------------------------------------------------------//
        // Behavior Transition Over Time Bitmap Window Constructor
        //------------------------------------------------------------------------------//
        public CBitmapSingleBehaviorTimeTransitionParent(MBSDEFAULTS.BITMAPDISPLAYTYPE DisplayType, Rectangle BitmapRec)
            : base(BitmapRec) //CBitmapParent() constructor
        {
            m_displayType = DisplayType;
        }
    }
    //***************************************************************************************************************//


    //***************************************************************************************************************//
    //----------------------------------------------------------------------------------//
    // Behavior Transition Over Time Bitmap Window Class
    //----------------------------------------------------------------------------------//
    class CBitmapSingleBehaviorTimeTransition : CBitmapSingleBehaviorTimeTransitionParent
    {
        public CBitmapSingleBehaviorTimeTransition(MBSDEFAULTS.BITMAPDISPLAYTYPE DisplayType, Rectangle BitmapRec)
            : base(DisplayType, BitmapRec)
        {
            //--------------------------------------------------------------------------//
            // Initialize window Rectangles to default values.
            //-----------------------------------------------//
            // m_displayRect is the remaining region after boarders have been taken away
            // and is set in the parent constructor.

            // Shell Rectangle
            m_shellRect.X = m_displayRect.X + BMPCONSTS.TRANS_HASH_REGION_VERT_WIDTH_MARK;
            m_shellRect.Y = m_displayRect.Y + BMPCONSTS.HASH_REGION_HORZ_HEIGHT_NOMARK;
            m_shellRect.Width = m_displayRect.Width - 2* BMPCONSTS.TRANS_HASH_REGION_VERT_WIDTH_MARK;
            m_shellRect.Height = m_displayRect.Height - BMPCONSTS.HASH_REGION_HORZ_HEIGHT_MARK -
                                                          BMPCONSTS.TRANS_HASH_REGION_HORZ_HEIGHT_MARK - 10;
            // Data Rectangle
            m_dataRect.X = m_shellRect.X + BMPCONSTS.SHELL_REGION_VERT_WIDTH;
            m_dataRect.Y = m_shellRect.Y + BMPCONSTS.SHELL_REGION_HORZ_HEIGHT;
            m_dataRect.Width = m_shellRect.Width - 2* BMPCONSTS.SHELL_REGION_VERT_WIDTH;
            m_dataRect.Height = m_shellRect.Height - BMPCONSTS.SHELL_REGION_HORZ_HEIGHT;
            
            //--------------------------------------------------------------------------//
            // Create text brush
            m_textBrush = new SolidBrush(Color.Black);
            m_textFont = new Font("Tahoma", 7.25F, FontStyle.Regular, GraphicsUnit.Point, 0);
        }

        public void SetWindowRect(Rectangle Rect)
        {
            //--------------------------------------------------------------------------//
            // Initialize window Rectangles to default values.
            //-----------------------------------------------//
            m_windowRect = Rect;

            // Shrink the display area to account for the boarder.
            m_displayRect.X = m_windowRect.X + BMPCONSTS.BOARDER_THICKNESS; // move right
            m_displayRect.Y = m_windowRect.Y + BMPCONSTS.BOARDER_THICKNESS; // move down
            m_displayRect.Width = m_windowRect.Width - 2 * BMPCONSTS.BOARDER_THICKNESS; // subtract from width
            m_displayRect.Height = m_windowRect.Height - 2 * BMPCONSTS.BOARDER_THICKNESS; // subtract from bottom

            // These Rectangles get overriden in derived classes.
            m_dataRect = m_shellRect = m_displayRect;

            //--------------------------------------------------------------------------//

            //--------------------------------------------------------------------------//
            // Initialize window Rectangles to default values.
            //-----------------------------------------------//
            // m_displayRect is the remaining region after boarders have been taken away
            // and is set in the parent constructor.

            // Shell Rectangle
            m_shellRect.X = m_displayRect.X + BMPCONSTS.TRANS_HASH_REGION_VERT_WIDTH_MARK;
            m_shellRect.Y = m_displayRect.Y + BMPCONSTS.HASH_REGION_HORZ_HEIGHT_NOMARK;
            m_shellRect.Width = m_displayRect.Width - 2* BMPCONSTS.TRANS_HASH_REGION_VERT_WIDTH_MARK;
            m_shellRect.Height = m_displayRect.Height - BMPCONSTS.HASH_REGION_HORZ_HEIGHT_MARK -
                                                          BMPCONSTS.TRANS_HASH_REGION_HORZ_HEIGHT_MARK -10;
            // Data Rectangle
            m_dataRect.X = m_shellRect.X + BMPCONSTS.SHELL_REGION_VERT_WIDTH;
            m_dataRect.Y = m_shellRect.Y + BMPCONSTS.SHELL_REGION_HORZ_HEIGHT;
            m_dataRect.Width = m_shellRect.Width - 2* BMPCONSTS.SHELL_REGION_VERT_WIDTH;
            m_dataRect.Height = m_shellRect.Height - BMPCONSTS.SHELL_REGION_HORZ_HEIGHT;

            RedrawBitmap();
        }
        //------------------------------------------------------------------------------//
        // Behavior Transition Over Time Bitmap OnPaint()
        //------------------------------------------------------------------------------//
        public override void MyOnPaint(Graphics GraphicsObject)
        {
            int i, j;
            Point p1, p2;
            SolidBrush behBrush;

            base.MyOnPaint(GraphicsObject); // Draws the borders around the window.
            GraphicsObject.FillRectangle(m_hashAreaBrush, m_displayRect);
            GraphicsObject.FillRectangle(m_shellBrush, m_shellRect);
            GraphicsObject.FillRectangle(m_shellBrush, m_dataRect);

            // Draw the bitmap that was set up in the RedrawBitmap() function.
            GraphicsObject.DrawImage(m_bitmap, m_dataRect.X, m_dataRect.Y);

            // Draw Y hashmarks and hashmark string values set up in the
            // RedrawBitmap() function.
            for(i = 0; m_hashMarkArrayYLeft != null && i < m_hashMarkArrayYLeft.Length; i++)
            {
                p1 = p2 = m_hashMarkArrayYLeft[i].point;
                p1.X -= 5;
                GraphicsObject.DrawLine(m_borderPen, p1, p2);
                GraphicsObject.DrawString(m_hashMarkArrayYLeft[i].sz, m_textFont, m_textBrush, p1.X - 22, p2.Y - 7);
            }


            for(i = 0; m_hashMarkArrayYRight != null && i < m_hashMarkArrayYRight.Length; i++)
            {
                p1 = p2 = m_hashMarkArrayYRight[i].point;
                p1.X += 5;
                GraphicsObject.DrawLine(m_borderPen, p1, p2);
                GraphicsObject.DrawString(m_hashMarkArrayYRight[i].sz, m_textFont, m_textBrush, p2.X + 10, p2.Y - 7);
            }

            GraphicsObject.DrawString(m_hashMarkTimeText.sz, m_textFont, m_textBrush, m_hashMarkTimeText.point.X, m_hashMarkTimeText.point.Y);
            GraphicsObject.DrawString(m_hashMarkCountText.sz, m_textFont, m_textBrush, m_hashMarkCountText.point.X, m_hashMarkCountText.point.Y);
            GraphicsObject.DrawString(m_hashMarkTransitionLabel.sz, m_textFont, m_textBrush, m_hashMarkTransitionLabel.point.X, m_hashMarkTransitionLabel.point.Y);

            // Draw X hashmarks and hashmarks string values
            for(i = 0; i < m_hashMarkArrayXBottom.Length; i++)
            {
                p1 = p2 = m_hashMarkArrayXBottom[i].point;
                // This needs to be better coded
                //if(i%2 == 0)
                //{
                    p1.Y += 5;
                    GraphicsObject.DrawString(m_hashMarkArrayXBottom[i].sz, m_textFont, m_textBrush, p1.X - 8, p2.Y + 7);
                //}
                //else
                //{
                  //  p1.X = p2.X = p1.X - 1;
                   // p1.Y += 15;
                   // GraphicsObject.DrawString(m_hashMarkArrayXBottom[i].sz, m_textFont, m_textBrush, p1.X - 12, p2.Y + 7 + 10);
                //}
                GraphicsObject.DrawLine(m_borderPen, p1, p2);
            }

            // Draw behavior transition count rectangles within the transition period rectangles.
            for(i = 0; i < m_dynamicDataRectArray.Length; i++)
            {
                GraphicsObject.FillRectangle(m_transitionPeriodBrushMax, m_dynamicDataRectArray[i].rectMax);
                GraphicsObject.FillRectangle(m_transitionPeriodBrushAve, m_dynamicDataRectArray[i].rectAve);
                GraphicsObject.FillRectangle(m_transitionPeriodBrushMin, m_dynamicDataRectArray[i].rectMin);

                for(j=0; j<m_dynamicDataRectArray[i].behavorRectArray.Length; j++)
                {
                    behBrush = new SolidBrush(GetCurrentBehaviorColor((uint)j));
                    GraphicsObject.FillRectangle(behBrush, m_dynamicDataRectArray[i].behavorRectArray[j]);
                }
            }

        }

        //------------------------------------------------------------------------------//
        // Behavior Transition Over Time Bitmap UpdateDataDisplay()
        //------------------------------------------------------------------------------//
        public void SetDisplayData(int NumBehaviors, mbsSNGLBEHTRANSTRUCT Data)
        {
            int i;

            m_data = Data;
            m_numBehaviors = NumBehaviors;

            // Run debug assertions on the data passed in
            Debug.Assert(m_data.timePeriodArray.Length > 0);
            for(i=0; i<m_data.timePeriodArray.Length; i++)
                Debug.Assert(m_data.timePeriodArray[i].trialArray.Length > 0);
            RedrawBitmap();
        }

        private SINGLEBEHTRANSSTATS GetSingleBehaviorTransitionStats(mbsSNGLBEHTRANSTRUCT Data)
        {
            int i, j, beh; // indexing vars.

            SINGLEBEHTRANSSTATS stats = new SINGLEBEHTRANSSTATS();
            stats.periodArray = new TRANSPERIOD[Data.timePeriodArray.Length];

            // The number of translational periods defined in the behavior transition
            // model. Equivelent to the number of rows in the behavior transition matrix.
            stats.periodCnt = m_data.timePeriodArray.Length;

            // The number of trials conducted per translational period.  This value is
            // defined by the application calling the behavior translation test.
            stats.trialsPerPeriodCnt = m_data.timePeriodArray[0].trialArray.Length;

            
            if(stats.trialsPerPeriodCnt == 0)
                return stats; // handle this better!!!!

            stats.maxPeriodDuration = 0;
            stats.maxBehaviorCount = 0;

            // Allocate memory for stats for each transitional period in the model and
            // memory for recording the total counts of behavior transitions achieved
            // during those trials.
            stats.periodArray = new TRANSPERIOD[stats.periodCnt];
            for(i=0; i<stats.periodCnt; i++)
                stats.periodArray[i].toBehaviorCount = new int[m_numBehaviors];

            //--------------------------------------------------------------------------//
            // Generate the stats
            //-------------------//
            // i is the transitional period
            // j is the j number for that transitional period
            // beh is the behavior index
            stats.maxPeriodDuration = m_data.timePeriodArray[0].trialArray[0].sec;
            stats.minPeriodDuration = m_data.timePeriodArray[0].trialArray[0].sec;
            stats.maxBehaviorCount = 0;
            for(i=0; i<stats.periodCnt; i++)
            {
                stats.periodArray[i].aveDuration = 0;
                stats.periodArray[i].minDuration = m_data.timePeriodArray[i].trialArray[0].sec;
                stats.periodArray[i].maxDuration = m_data.timePeriodArray[i].trialArray[0].sec;

                // set max duraiton to initial value
                // set min duration to initial value
                // initial ave duration
                for(j=0; j<stats.trialsPerPeriodCnt; j++)
                {
                    // m_data.timePeriodArray[i].trialArray[j].sec holds time the behavior persisted until
                    // it transitioned into a new behavior.
                    stats.periodArray[i].aveDuration += m_data.timePeriodArray[i].trialArray[j].sec;

                    // Update the minimum for this transition period
                    if(stats.periodArray[i].minDuration > m_data.timePeriodArray[i].trialArray[j].sec)
                        stats.periodArray[i].minDuration = m_data.timePeriodArray[i].trialArray[j].sec;

                    // Update the maximum for this transition period.
                    if(stats.periodArray[i].maxDuration < m_data.timePeriodArray[i].trialArray[j].sec)
                        stats.periodArray[i].maxDuration = m_data.timePeriodArray[i].trialArray[j].sec;

                    // m_data.timePeriodArray[i].trialArray[j].trans holds the behavior transitioned into
                    // when the behavior transition being tested transitioned into
                    // a new behavior.
                    beh = m_data.timePeriodArray[i].trialArray[j].trans;
                    stats.periodArray[i].toBehaviorCount[beh]++;
                }

                // Average time before transitioning to a new behavior from being being
                // tested.
                stats.periodArray[i].aveDuration /= (double)stats.trialsPerPeriodCnt;

                // Update the maximum count of all behavior transitions for all time periods and all trials conducted
                for(beh=0; beh<m_numBehaviors; beh++)
                {
                    // Maximum of all transitional periods
                    if(stats.maxBehaviorCount < stats.periodArray[i].toBehaviorCount[beh])
                        stats.maxBehaviorCount = stats.periodArray[i].toBehaviorCount[beh];
                }

                // Update the maximum of all period durations for all transiton periods and trials
                if(stats.maxPeriodDuration < stats.periodArray[i].maxDuration)
                    stats.maxPeriodDuration = stats.periodArray[i].maxDuration;

                // Update the minimum of all period durations for all transiton periods and trials
                if(stats.minPeriodDuration > stats.periodArray[i].minDuration)
                    stats.minPeriodDuration = stats.periodArray[i].minDuration;
            }
            return stats;
        }


        //------------------------------------------------------------------------------//
        // Behavior Transition Over Time Bitmap RedrawBitmap()
        //------------------------------------------------------------------------------//
        // Called when the bitmap needs to be updated because data has changed or desired
        // characteristics of how the data is displayed has changed.
        private void RedrawBitmap()
        {
            int i,j;
            int numHasmarkRegionsY;
            int numHasmarkRegionsX;

            double minutesPerHashmarkRegionY = 60; // just a safe value
            double transitionsPerHashmarkRegionY = 60; // just a safe value

            double pixelsPerMinuteY;
            double pixelsPerBehCntY;

            SINGLEBEHTRANSSTATS stats = GetSingleBehaviorTransitionStats(m_data);
            TRANSPERIOD period;

            // Redundant variables placed to make code easier to understand.

            m_bitmap = new Bitmap(m_dataRect.Width, m_dataRect.Height);

            m_hashMarkTimeText.point.X = m_dataRect.X - 70;
            m_hashMarkTimeText.point.Y = m_dataRect.Y + m_dataRect.Height/2;
            m_hashMarkTimeText.sz = "Time (min)";

            m_hashMarkCountText.point.X = m_dataRect.X + m_dataRect.Width + 20;
            m_hashMarkCountText.point.Y = m_dataRect.Y + m_dataRect.Height/2;
            m_hashMarkCountText.sz = "Counts";

            m_hashMarkTransitionLabel.point.X = m_dataRect.X + m_dataRect.Width/2;// -50;
            m_hashMarkTransitionLabel.point.Y = m_dataRect.Y +m_dataRect.Height;// -50;
            m_hashMarkTransitionLabel.sz = "Transition Periods";

            //--------------------------------------------------------------------------//
            // Hashmarks
            //----------//

            numHasmarkRegionsY = BMPCONSTS.NUM_BEHAVIORTRANS_Y_HASHMARKS - 1;
            numHasmarkRegionsX = m_data.timePeriodArray.Length;
            transitionsPerHashmarkRegionY = (float)stats.maxBehaviorCount/(float)numHasmarkRegionsY;

            // Allocate hashmarks in the for the x and y dimensions.
            if(m_displayType == MBSDEFAULTS.BITMAPDISPLAYTYPE.BEHAVIOR_TRANSITION)
            {
                m_hashMarkArrayYLeft = new HASHMARK[numHasmarkRegionsY + 1];
                minutesPerHashmarkRegionY = (int)(((double)stats.maxPeriodDuration/60.0)/(double)numHasmarkRegionsY);
            }

            m_hashMarkArrayYRight = new HASHMARK[numHasmarkRegionsY + 1];

            // Two hashmarks for each transition region... one on each side.
            m_hashMarkArrayXBottom = new HASHMARK[numHasmarkRegionsX*2];

            // Calculate pixels per hashmark in for the x and y dimensions.
            m_pixelsPerHashmarkRegionY = m_dataRect.Height / numHasmarkRegionsY;
            m_pixelsPerHashmarkRegionX = m_dataRect.Width / numHasmarkRegionsX;

            // Set hashmark placement for the y dimension and set hashmark strings.
            for(i=0; m_hashMarkArrayYLeft != null && i < m_hashMarkArrayYLeft.Length; i++)
            {
                m_hashMarkArrayYLeft[i].point.X = m_displayRect.X + BMPCONSTS.TRANS_HASH_REGION_VERT_WIDTH_MARK;
                m_hashMarkArrayYLeft[i].point.Y = m_dataRect.Y + m_dataRect.Height - i * m_pixelsPerHashmarkRegionY;
                m_hashMarkArrayYLeft[i].sz = String.Format("{0,2}", minutesPerHashmarkRegionY*i);
            }

            for(i=0; m_hashMarkArrayYRight != null && i < m_hashMarkArrayYRight.Length; i++)
            {
                m_hashMarkArrayYRight[i].point.X = m_dataRect.X + m_dataRect.Width + BMPCONSTS.SHELL_REGION_VERT_WIDTH;
                m_hashMarkArrayYRight[i].point.Y = m_dataRect.Y + m_dataRect.Height - i * m_pixelsPerHashmarkRegionY;
                m_hashMarkArrayYRight[i].sz = String.Format("{0:0.0}", transitionsPerHashmarkRegionY*i);
            }
            //--------------------------------------------------------------------------//


            //--------------------------------------------------------------------------//
            // Determine Dynamic Rect Placements
            //------------------------------------//
            
            // The total number of pixels available for all the transition periods to be
            // displayed is equal to the total number of pixels minus the percentage to be
            // reserved for space in-between each transition period.
            int pixPerTransPeriod = (m_dataRect.Width - 
                (int)(m_dataRect.Width*(BMPCONSTS.TRANS_SPACE_INBETWEEN_PRCNT_MIN/100.0)))/stats.periodCnt;
            int pixIncPerTransPeriod = (m_dataRect.Width)/(stats.periodCnt);  // Number of pixels to increment the placement of succussive transition periods.
            int pixCenterTransPeriod;
            int pixPerBehavior;
            int pixIncPerBehavior;
            int pixCenterBehavior;


            if(pixPerTransPeriod > BMPCONSTS.TRANS_TRIAL_WIDTH_MAX_PIXEL)
                pixPerTransPeriod = BMPCONSTS.TRANS_TRIAL_WIDTH_MAX_PIXEL;

            
            pixPerBehavior = (pixPerTransPeriod - 
                (int)(pixPerTransPeriod*(BMPCONSTS.BEH_SPACE_INBETWEEN_PRCNT_MIN/100.0)))/m_numBehaviors;


            pixelsPerMinuteY = 60; // a safe number
            if(stats.maxPeriodDuration > 0)
                pixelsPerMinuteY = (double)m_dataRect.Height/((double)stats.maxPeriodDuration/60.0);

            pixelsPerBehCntY = 60; // a safe number
            if(stats.maxBehaviorCount > 0)
                pixelsPerBehCntY = (double)m_dataRect.Height/(double)stats.maxBehaviorCount;

            m_dynamicDataRectArray = new DYNAMICDATARECTCONTAINER[stats.periodCnt];

            
            pixCenterTransPeriod = m_dataRect.X + pixIncPerTransPeriod/2;

            // i represents transition period index.
            // j represents behavior index of transition period i.
            for(i = 0; i < stats.periodCnt; i++)
            {
                period = stats.periodArray[i];

                //----------------------//
                // Transition Rectangles
                //----------------------//
                // Min value
                m_dynamicDataRectArray[i].rectMin.X =  pixCenterTransPeriod - pixPerTransPeriod/2;
                m_dynamicDataRectArray[i].rectMin.Width = pixPerTransPeriod;
                m_dynamicDataRectArray[i].rectMin.Y = m_dataRect.Y + m_dataRect.Height - (int)(pixelsPerMinuteY * (period.minDuration/60.0));
                m_dynamicDataRectArray[i].rectMin.Height = (int)(pixelsPerMinuteY *period.minDuration/60.0);
                // Max value
                m_dynamicDataRectArray[i].rectMax.X =  pixCenterTransPeriod - pixPerTransPeriod/2;
                m_dynamicDataRectArray[i].rectMax.Width = pixPerTransPeriod;
                m_dynamicDataRectArray[i].rectMax.Y = m_dataRect.Y + m_dataRect.Height - (int)(pixelsPerMinuteY * (period.maxDuration/60.0));
                m_dynamicDataRectArray[i].rectMax.Height = (int)(pixelsPerMinuteY *period.maxDuration/60.0);
                // Average value
                m_dynamicDataRectArray[i].rectAve.X =  pixCenterTransPeriod - pixPerTransPeriod/2;
                m_dynamicDataRectArray[i].rectAve.Width = pixPerTransPeriod;
                m_dynamicDataRectArray[i].rectAve.Y = m_dataRect.Y + m_dataRect.Height - (int)(pixelsPerMinuteY * (period.aveDuration/60.0));
                m_dynamicDataRectArray[i].rectAve.Height = (int)(pixelsPerMinuteY *period.aveDuration/60.0);


                // Set hashmark placement and string for the lower hasmark for the time period.
                Debug.Assert(i*2 < m_hashMarkArrayXBottom.Length);
                m_hashMarkArrayXBottom[i*2].point.X = pixCenterTransPeriod - pixPerTransPeriod/2;
                m_hashMarkArrayXBottom[i*2].point.Y = m_displayRect.Y + m_displayRect.Height - BMPCONSTS.HASH_REGION_HORZ_HEIGHT_MARK - 10; // see calculation of shell height to fix this magic number 10
                m_hashMarkArrayXBottom[i*2].sz = "" + String.Format("{0:0.00}", m_data.timePeriodArray[i].start);

                // Set hashmark placement and string for the upper hasmark for the time period.
                Debug.Assert(i*2+1 < m_hashMarkArrayXBottom.Length);
                m_hashMarkArrayXBottom[i*2+1].point.X = pixCenterTransPeriod - pixPerTransPeriod/2 + pixPerTransPeriod;
                m_hashMarkArrayXBottom[i*2+1].point.Y = m_displayRect.Y + m_displayRect.Height - BMPCONSTS.HASH_REGION_HORZ_HEIGHT_MARK - 10; // see calculation of shell height to fix this magic number 10;
                m_hashMarkArrayXBottom[i*2+1].sz = "" + String.Format("{0:0.00}", m_data.timePeriodArray[i].end);

               
                m_dynamicDataRectArray[i].behavorRectArray = new Rectangle[m_numBehaviors];
                pixIncPerBehavior = m_dynamicDataRectArray[i].rectAve.Width/(m_numBehaviors);
                pixCenterBehavior = m_dynamicDataRectArray[i].rectAve.X + pixIncPerBehavior/2;
                for(j=0; j<m_dynamicDataRectArray[i].behavorRectArray.Length; j++)
                {
                    m_dynamicDataRectArray[i].behavorRectArray[j].X = pixCenterBehavior - pixPerBehavior/2;
                    m_dynamicDataRectArray[i].behavorRectArray[j].Width = pixPerBehavior;

                    m_dynamicDataRectArray[i].behavorRectArray[j].Y = m_dataRect.Y + m_dataRect.Height - (int)(pixelsPerBehCntY * period.toBehaviorCount[j]);
                    m_dynamicDataRectArray[i].behavorRectArray[j].Height = (int)(pixelsPerBehCntY * period.toBehaviorCount[j]);

                    pixCenterBehavior += pixIncPerBehavior;
                }
                pixCenterTransPeriod += pixIncPerTransPeriod;
            }
        }

        //------------------------------------------------------------------------------//
        // Behavior Transition Over Time HandleMouseDown()
        //------------------------------------------------------------------------------//
        public override Boolean HandleMouseDown(MouseEventArgs e)
        {
            base.HandleMouseDown(e);
            return false;
        }
        //------------------------------------------------------------------------------//
        // Behavior Transition Over Time HandleMouseUp()
        //------------------------------------------------------------------------------//
        public override Boolean HandleMouseUp(MouseEventArgs e)
        {
            base.HandleMouseUp(e);
            return false; 
        }
        //------------------------------------------------------------------------------//
        // Behavior Transition Over Time HandleMouseMove()
        //------------------------------------------------------------------------------//
        public override Boolean HandleMouseMove(MouseEventArgs e)
        {
            base.HandleMouseMove(e);
            return false;
        }
    }
    //***************************************************************************************************************//

}