using System;
using System.Windows.Forms;
using System.Collections.Generic;
using System.Text;
using System.Drawing;
using System.Drawing.Imaging;
using System.Diagnostics;
//using mbs;

namespace MBSGUI
{
    public class GuiUtils
    {
        //---------------------------------------------------//
        // Determine the index of a Button
        //---------------------------------------------------//
        static public int MatchIndex(Button[] BArray, Button B)
        {
            int i=0;
            while(i<BArray.Length && BArray[i].Name != B.Name)
                i++;
            Debug.Assert(i<BArray.Length);
            return i;
        }

        //---------------------------------------------------//
        // Determine the index of a TextBox
        //---------------------------------------------------//
        static public int MatchIndex(TextBox[] TBArray, TextBox TB)
        {
            int i=0;
            while(i<TBArray.Length && TBArray[i].Name != TB.Name)
                i++;
            //Debug.Assert(i<TBArray.Length);
            if(i==TBArray.Length)
                return -1;
            return i;
        }

        static public int MatchIndex(ToolStripMenuItem[] TSMIArray, ToolStripMenuItem TSMI)
        {
            int i = 0;
            while(i<TSMIArray.Length && (TSMIArray[i] == null || TSMIArray[i].Name != TSMI.Name))
                i++;
            if(i==TSMIArray.Length)
                return -1;
            return i;
        }

        //---------------------------------------------------//
        // Determine the row and column indices of a TextBox
        //---------------------------------------------------//
        static public ROWCOL MatchIndex(TextBox[][] MatrixTextbox, TextBox TB, ROWCOL LastIndex)
        {
            ROWCOL rc;
            for(rc.row = LastIndex.row; rc.row<MatrixTextbox.Length; rc.row++)
            {
                for(rc.col = LastIndex.col; rc.col<MatrixTextbox[rc.row].Length; rc.col++)
                    if(MatrixTextbox[rc.row][rc.col].Name == TB.Name)
                        return rc;
                rc.col = 0;
            }

            for(rc.row = 0; rc.row < MatrixTextbox.Length; rc.row++)
                for(rc.col = 0; rc.col < MatrixTextbox[rc.row].Length; rc.col++)
                    if(MatrixTextbox[rc.row][rc.col].Name == TB.Name)
                        return rc;

            // Failed to match index.
            rc.row = rc.col = -1;
            return rc;
        }

    }

    public struct DATASCALE { public float x; public float y; }

    public struct MINMAX_INT
    {
        public int min;
        public int max;
    }

    public struct MINMAX_DOUBLE
    {
        public double min;
        public double max;
    }

    public struct DATA_EXTREMES_SPE_BITMAP
    {
        public float xMin;
        public float xMax;
        public float yMin;
        public float yMax;
        public float zMin;
        public float zMax;
    }

    public struct HASHMARK { public Point point; public Boolean active; public String sz; }

    public struct SLIDINGHASHMARKSCALESET
    {
        public HASHMARK min; // min/left/bottom scale, min
        public HASHMARK mid; // mid/middle scale, middle
        public HASHMARK max; // max/right/top scale
    }

    public struct SLIDINGHASHRESULTS
    {
        public int start; // starting point
        public int end; // length;
    }
    public struct BITMAPLINE { public Point p1; public Point p2; public Pen pen;}

    public struct DYNAMICDATARECTCONTAINER
    {
        public Rectangle rectMin;
        public Rectangle rectMax;
        public Rectangle rectAve;
        public Rectangle[] behavorRectArray;
    }

    public class BMPCONSTS
    {

        // Behavior Transition Trials
        //public static int SPACE_BETWEEN_TRANS_PERIODS = 10; // pixels between the display of tested time periods.
        public static int TRANS_TRIAL_WIDTH_MAX_PIXEL = 100; // the maximum number of pixels single trial period may occupy
        //public static int TRANS_TRIAL_SPACE_INBETWEEN_PRCNT = 20; // each of the 8 behaviors divide up this percentage for their dislay
        public static double TRANS_SPACE_INBETWEEN_PRCNT_MIN = 40; // 
        public static double BEH_SPACE_INBETWEEN_PRCNT_MIN = 50; // 
        public static int TRANS_HASH_REGION_VERT_WIDTH_MARK = 60; // left or right side with hashmarks
        public static int TRANS_HASH_REGION_HORZ_HEIGHT_MARK = 30;
        //public static int WHITESPACE_BETWEEN_PERIODS_PERCENT = 80; // each of the 8 behaviors divide up this percentage for their dislay

        // regions
        public static int HASH_REGION_VERT_WIDTH_MARK = 35; // left or right side with hashmarks
        public static int HASH_REGION_VERT_WIDTH_NOMARK = 20; // left or right side with no hashmarks
        public static int HASH_REGION_HORZ_HEIGHT_MARK = 20; // top or bottom with hashmarks
        public static int HASH_REGION_HORZ_HEIGHT_NOMARK = 10;  // top or bottom with no hashmarks

        public static int SHELL_REGION_VERT_WIDTH = 10; // left or right side
        public static int SHELL_REGION_HORZ_HEIGHT = 10; // top or bottom

        // colors
        public static Color FRAME_COLOR = Color.Black;
        public static Color WATER_COLOR = Color.DarkBlue;
        //public static Color SHELL_COLOR = Color.LightSlateGray;
        public static Color SHELL_COLOR = Color.Lavender;
        //public static Color TRANS_PER_COLOR = Color.OliveDrab;
        public static Color TRANS_PER_MIN_COLOR = Color.DarkSlateGray;
        public static Color TRANS_PER_AVE_COLOR = Color.SlateGray;
        public static Color TRANS_PER_MAX_COLOR = Color.SkyBlue;
        //public static Color GROUND_COLOR = Color.Goldenrod;
        //public static Color GROUND_COLOR = Color.DarkOliveGreen;
        public static Color GROUND_COLOR = Color.DarkOliveGreen;
        public static Color AIR_COLOR = Color.LightBlue;
        public static Color HASH_LABEL_REGION_COLOR = Color.LightGray;
        public static Color MOVEABLE_HASH_COLOR = Color.DarkOliveGreen;
        public static Color CENTERSLIDER_HASH_COLOR = Color.Black;
        public static Color ACTIVE_HASH_LINE_COLOR = Color.Peru;
        public static Color TARGET_DEPTH_COLOR = Color.DarkCyan;

        // Several child classes will point to this same location in memory and be allowed
        // to change these values.
        public static Color[] BEHAVIOR_COLOR = { Color.DarkKhaki, 
            Color.SteelBlue, Color.Olive, Color.Brown, Color.Aqua, Color.Black, Color.Magenta, Color.Salmon};

        public static Color toggleColor{get {return Color.White;}}

        private static Color[] CONST_DEFAULT_BEHAVIOR_COLOR = { Color.DarkKhaki, 
            Color.SteelBlue, Color.Olive, Color.Brown, Color.Aqua, Color.Black, Color.Magenta, Color.Salmon};

        public static Color GetDefaultColor(uint BehaviorIndex)
        {
            return CONST_DEFAULT_BEHAVIOR_COLOR[BehaviorIndex];
        }

        public static int MOVEABLE_HASH_THICKNESS = 3;
        public static int BETWEEN_MOVEABLE_HASH_THICKNESS = 3;
        public static int LABEL_HASH_THICKNESS = 3;
        public static int BOARDER_THICKNESS = 1;

        // Hash mark spacing
        public static int NUM_BEHAVIORTRANS_Y_HASHMARKS = 4;
        public static int NUM_DEPTH_HASHMARKS = 5;
        public static int NUM_TIME_HASHMARKS = 11;
        public static int NUM_MOVMENT_X_HASHMARKS = 11;
        public static int NUM_MOVMENT_Y_HASHMARKS = 11;
    }
    //*****************************************************************************************************************

    //*****************************************************************************************************************
    //----------------------------------------------------------------------------------//
    //----------------------------------------------------------------------------------//

    //*****************************************************************************************************************
    //----------------------------------------------------------------------------------//
    // Parent Bitmap Class Window
    //----------------------------------------------------------------------------------//
    /*
               Border
               Pixels
               <------------------------------------------------- Window Rect (X-Dimension) ---------------------------------------------->
               ----------------------------------------------------------------------------------------------------------------------------
     /^\      |xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|
Border Pixels |xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|
      |       |xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|
      |       |xxx                                                                                                                      xxx|
      |       |xxx                <-------------------------------------- Display Rect (X-Dimension) -------------------------------->  xxx|
      |       |xxx                ----------------------------------------------------------------------------------------------------  xxx|
      |       |xxx     /^\       |                                                                                                    | xxx|
      |       |xxx      |        |                               <-------------------  Data Rect (X-Dimension)------------------>     | xxx|
      |       |xxx      |        |                                                                                                    | xxx|
      |       |xxx      |        |                               0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21     | xxx|
      |       |xxx      |        |              Hashmark 0y    ------------------------------------------------------------------     | xxx|
      |       |xxx      |        |     /^\             ----- 0|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |                    1|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |                    2|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |                    3|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |         Hashmark   4|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |         Region     5|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |                    6|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |                    7|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |       Hashmark 1y  8|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
 Window Rect  |xxx Display Rect  |  Data Rect         -----  9|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
 (Y-Dimension)|xxx (Y-Dimension) |(Y-Dimension)             10|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |                   11|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |         Hashmark  12|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |         Region    13|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |                   14|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |                   15|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |                   16|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |                   17|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |      |       Hashmark 2y 18|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |     \ /             -----19|  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x     | xxx|
      |       |xxx      |        |                             ------------------------------------------------------------------     | xxx|
      |       |xxx      |        |                               0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21     | xxx|
      |       |xxx      |        |                               |                          |                             |           | xxx|
      |       |xxx      |        |                               |       Hashmark           |         Hashmark            | Trailing pixels,
      |       |xxx      |        |                               |        Region            |          Region             | not included in 
      |       |xxx      |        |                               |                          |                             | any hash region.
      |       |xxx      |        |                             Hashmark 0x             Hashmark 1x                     Hashmark 2x    | xxx|
      |       |xxx      |        |                                                                                                    | xxx|
      |       |xxx     \|/       |                                                                                                    | xxx|
      |       |xxx                ----------------------------------------------------------------------------------------------------  xxx|
      |       |xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|
      |       |xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|
     \|/      |xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|
               ----------------------------------------------------------------------------------------------------------------------------
      
         prime numbers: 1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 27, 29, 31, 33, 37, 41, 43, 47, 49, 51, 53, 57,...
    */


    class CBitmapBasic
    {
        public Bitmap m_bitmap;

        // All children of CBitmap parent point to the exact same location in memory holding the behavior colors.
//        private static Color[] m_behaviorColor = BMPCONSTS.BEHAVIOR_COLOR;

        public Rectangle m_windowRect; // location and size of the bitmap window
        public Rectangle m_displayRect; // sub-rectangle of m_windowRect minus boarders
        public Rectangle m_shellRect; // sub-rectangle of m_displayRect for formatting display data if needed.
        public Rectangle m_dataRect; // sub-rectangle of m_shellRect soley for displaying data

        public DYNAMICDATARECTCONTAINER[] m_dynamicDataRectArray = null; // sub-rectangle of m_dataRect formatting display data if needed.
        
        // water brush
        public SolidBrush m_transitionPeriodBrushMin = new SolidBrush(BMPCONSTS.TRANS_PER_MIN_COLOR);
        public SolidBrush m_transitionPeriodBrushMax = new SolidBrush(BMPCONSTS.TRANS_PER_MAX_COLOR);
        public SolidBrush m_transitionPeriodBrushAve = new SolidBrush(BMPCONSTS.TRANS_PER_AVE_COLOR);
        public SolidBrush m_shellBrush = new SolidBrush(BMPCONSTS.SHELL_COLOR);
        public SolidBrush m_waterBrush = new SolidBrush(BMPCONSTS.WATER_COLOR);
        public SolidBrush m_hashAreaBrush = new SolidBrush(BMPCONSTS.HASH_LABEL_REGION_COLOR);
        public SolidBrush m_airRegionBrush = new SolidBrush(BMPCONSTS.AIR_COLOR);

        // border pen
        public Pen m_borderPen = new Pen(BMPCONSTS.FRAME_COLOR, BMPCONSTS.BOARDER_THICKNESS);
        public Pen m_scaleHashPen = new Pen(BMPCONSTS.MOVEABLE_HASH_COLOR, BMPCONSTS.MOVEABLE_HASH_THICKNESS);
        public Pen m_scaleBetwenHashPen = new Pen(BMPCONSTS.MOVEABLE_HASH_COLOR, BMPCONSTS.BETWEEN_MOVEABLE_HASH_THICKNESS);
        public Pen m_slidingHashMarkPen = new Pen(BMPCONSTS.CENTERSLIDER_HASH_COLOR, BMPCONSTS.MOVEABLE_HASH_THICKNESS);
        public Pen m_activeSideHashMarkPen = new Pen(BMPCONSTS.ACTIVE_HASH_LINE_COLOR, BMPCONSTS.MOVEABLE_HASH_THICKNESS);

        // These need to be moved.
        public Pen m_bathyPen = new Pen(BMPCONSTS.GROUND_COLOR, BMPCONSTS.MOVEABLE_HASH_THICKNESS);
        public Pen m_targetDepthPen = new Pen(BMPCONSTS.TARGET_DEPTH_COLOR, BMPCONSTS.MOVEABLE_HASH_THICKNESS);


        public Point m_mouseLastPosition;
        public Point m_mouseUp;
        public Point m_mouseMove;
        
        // Hash Marks
        public int m_timeStartIndex;
        public int m_timeEndIndex;
        public HASHMARK[] m_hashMarkArrayYLeft = null; // depth or Y coordinate hashmarks that display assoicated value
        public HASHMARK[] m_hashMarkArrayYRight = null; // depth or Y coordinate hashmarks that display assoicated value
        public HASHMARK[] m_hashMarkArrayXBottom = null; // time or X coordinate hashmarks that display associated value
        public HASHMARK[] m_hashMarkArrayXTop = null; // time or X coordinate hashmarks that display associated value

        public HASHMARK m_hashMarkTimeText; // time or X coordinate hashmarks that display associated value
        public HASHMARK m_hashMarkCountText; // time or X coordinate hashmarks that display associated value
        public HASHMARK m_hashMarkTransitionLabel;

        // Pixels per hashmark region.
        public int m_pixelsPerHashmarkRegionY;
        public int m_pixelsPerHashmarkRegionX;

        // Create text brush
        public SolidBrush m_textBrush;
        public Font m_textFont;

        public CBitmapBasic(Rectangle BitmapRec)
        {
            //--------------------------------------------------------------------------//
            // Initialize window Rectangles to default values.
            //-----------------------------------------------//
            m_windowRect = BitmapRec;

            // Shrink the display area to account for the boarder.
            m_displayRect.X = m_windowRect.X + BMPCONSTS.BOARDER_THICKNESS; // move right
            m_displayRect.Y = m_windowRect.Y + BMPCONSTS.BOARDER_THICKNESS; // move down
            m_displayRect.Width = m_windowRect.Width - 2 * BMPCONSTS.BOARDER_THICKNESS; // subtract from width
            m_displayRect.Height = m_windowRect.Height - 2 * BMPCONSTS.BOARDER_THICKNESS; // subtract from bottom

            // These Rectangles get overriden in derived classes.
            m_dataRect = m_shellRect = m_displayRect;
            //--------------------------------------------------------------------------//
        }

        public virtual void MyOnPaint(Graphics GraphicsObject)
        {
            // draw the boarders
            GraphicsObject.DrawLine(m_borderPen,
                m_windowRect.X,
                m_windowRect.Y,
                m_windowRect.X + m_windowRect.Width - 1,
                m_windowRect.Y);
            GraphicsObject.DrawLine(m_borderPen,
                m_windowRect.X + m_windowRect.Width - 1,
                m_windowRect.Y,
                m_windowRect.X + m_windowRect.Width - 1,
                m_windowRect.Y + m_windowRect.Height - 1);
            GraphicsObject.DrawLine(m_borderPen,
                m_windowRect.X + m_windowRect.Width - 1,
                m_windowRect.Y + m_windowRect.Height - 1,
                m_windowRect.X,
                m_windowRect.Y + m_windowRect.Height - 1);
            GraphicsObject.DrawLine(m_borderPen,
                m_windowRect.X,
                m_windowRect.Y + m_windowRect.Height - 1,
                m_windowRect.X,
                m_windowRect.Y);
        }

        //------------------------------------------------------------------------------//
        // Basic (parent) Bitmap LocationWithinDataBitmap
        //------------------------------------------------------------------------------//
        public Boolean LocationWithinDataBitmap(Point Location)
        {
            if(Location.X < m_dataRect.X || Location.X > m_dataRect.X + m_dataRect.Width - 1)
                return false;
            if(Location.Y < m_dataRect.Y || Location.Y > m_dataRect.Y + m_dataRect.Height - 1)
                return false;
            return true;
        }
        //------------------------------------------------------------------------------//
        // Parent Bitmap HandleMouseDown()
        //------------------------------------------------------------------------------//
        public virtual Boolean HandleMouseDown(MouseEventArgs e)
        {
            if(LocationWithinDataBitmap(e.Location))
            {
                m_mouseLastPosition = e.Location;
                return true;
            }
            return false;
        }
        //------------------------------------------------------------------------------//
        // Parent Bitmap HandleMouseUp()
        //------------------------------------------------------------------------------//
        public virtual Boolean HandleMouseUp(MouseEventArgs e)
        {
            if(LocationWithinDataBitmap(e.Location))
            {
                m_mouseUp = e.Location;
                return true;
            }
            return false;
        }
        //------------------------------------------------------------------------------//
        // Parent Bitmap HandleMouseMove()
        //------------------------------------------------------------------------------//
        public virtual Boolean HandleMouseMove(MouseEventArgs e)
        {
            if(LocationWithinDataBitmap(e.Location))
            {
                m_mouseMove = e.Location;
                return true;
            }
            return false;
        }
    }
    //*****************************************************************************************************************
}