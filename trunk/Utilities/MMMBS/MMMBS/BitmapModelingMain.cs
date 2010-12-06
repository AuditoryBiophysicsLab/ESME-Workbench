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
    //***************************************************************************************************************//
    //----------------------------------------------------------------------------------//
    // Data structures specific to the front panel species model display
    //----------------------------------------------------------------------------------//
    public struct ANIMATSTATEDATABITMAP
    {
        public float x; // in meters
        public float y; // in meters
        public float z; // in meters, but negated
        public float targetDepth; // target z.
        public mbsANIMAT_STATE animatState;
    }
    //***************************************************************************************************************//


    //***************************************************************************************************************//
    //----------------------------------------------------------------------------------//
    // Species Bitmap Window Manager Class
    //----------------------------------------------------------------------------------//
    class CBitmapModelingMain
    {
        // Cordinates the updates of the species model display windows on the main 3mb
        // species model panel.
        private CBitmapSpeDiveProfile m_dive;
        private CBitmapTimeScale m_diveTimeScale;
        private CBitmapDepthScale m_depthScale;
        private CBitmapSpeMovement m_movement;
        private ANIMATSTATEDATABITMAP[] m_stateData;
        private Form m_parentForm; // The C# Form that will hold the bitmap that this class manages.

        public Color BathyColor { get { return m_dive.BathyColor; } }
        public Color TargetDepthColor { get { return m_dive.TargetDepthColor; } }
        public Boolean DisplayBathy { get { return m_dive.DisplayBathy; } set { m_dive.DisplayBathy = value; } }
        public Boolean DisplayTargetDepth { get { return m_dive.DisplayTargetDepth; } set { m_dive.DisplayTargetDepth = value; } }

        // Constructor
        public CBitmapModelingMain(Form ParentForm, Rectangle Dive, Rectangle DiveTimeScale, Rectangle DiveDepthScale, Rectangle Movement)
        {
            m_parentForm = ParentForm;
            m_stateData = null;

            m_dive = new CBitmapSpeDiveProfile(Dive);

            // override the passed in Rectangle for the dive time scaler window so it better matches the dive window
            DiveTimeScale.X = m_dive.m_dataRect.X - 1; // -1 to account for left side border that will be automatically added
            DiveTimeScale.Width = m_dive.m_dataRect.Width + 2; // +2 accounts for left and right side borders auto added
            m_diveTimeScale = new CBitmapTimeScale(DiveTimeScale);

            // override the passed in Rectangle for the dive depth scaler window so it better matches the dive window
            DiveDepthScale.Y = m_dive.m_dataRect.Y - 1; // -1 to account for top border that will be automatically added
            DiveDepthScale.Height = m_dive.m_dataRect.Height + 2;// +2 accounts for left and right side borders auto added
            m_depthScale = new CBitmapDepthScale(DiveDepthScale);

            m_movement = new CBitmapSpeMovement(Movement);
        }



        //------------------------------------------------------------------------------//
        // BitmapManager SetDisplayData()
        //------------------------------------------------------------------------------//
        // Function may be used to either set display data or set extemes of data
        // displayed.  If paramater "StateData' then this function has to have been
        // previously called with a non-null 'StateData'
        // If parameter 'StateData' is null its corresponding member variable
        // 'm_stateData' remains untouched.
        public void SetDisplayData(ANIMATSTATEDATABITMAP[] StateData, DATA_EXTREMES_SPE_BITMAP DataExtremes,
            DATA_EXTREMES_SPE_BITMAP MovementDataExtremes, Boolean MaintainScaling)
        {
            // If parameter 'StateData' is null verify it's corresponding member variable isn't.
            if(StateData == null)
                Debug.Assert(m_stateData != null);

            // Assign parameters to their corresponding member variables.
            if(StateData != null)
                m_stateData = StateData;

            // Set up the bitmaps with the new data.
            m_diveTimeScale.SetDisplayData(m_stateData, DataExtremes, MaintainScaling);
            m_depthScale.SetDisplayData(m_stateData, DataExtremes, MaintainScaling);
            m_dive.SetDisplayData(m_stateData, DataExtremes, MaintainScaling);

            // Movement map currently always needs the extremes of the actual data passed
            // in.
            m_movement.SetDisplayData(m_stateData, MovementDataExtremes, MaintainScaling);

            m_parentForm.Invalidate();
        }


        //------------------------------------------------------------------------------//
        // BitmapManager SetDisplayColors()
        //------------------------------------------------------------------------------//
        public Color ToggleDisplayColor(int BehaviorIndex)
        {
            Color ret = CBitmapSpe.ToggleBehaviorColor((uint)BehaviorIndex); ;
            m_dive.ConstructBitmap();
            m_movement.RedrawBitmap();
            m_parentForm.Invalidate();
            return ret;
        }

        public double MouseEventToDepthValue(MouseEventArgs e)
        {
            Point p = new Point();
            p.X = e.X;
            p.Y = e.Y;
            return m_dive.DepthValueAtPoint(p);
        }

        public Boolean MouseEventWithinDiveProfileDataBitmap(MouseEventArgs e)
        {
            Point p = new Point(e.X, e.Y);
            if(m_dive.LocationWithinDataBitmap(p))
                return true;
            return false;
        }

        //------------------------------------------------------------------------------//
        // BitmapManager HandleMouseDown()
        //------------------------------------------------------------------------------//
        public Boolean HandleMouseDown(MouseEventArgs e)
        {
            // Dive
            if(m_dive.HandleMouseDown(e))
                m_parentForm.Invalidate(m_dive.m_dataRect);

            // Dive Time
            if(m_diveTimeScale.HandleMouseDown(e))
                m_parentForm.Invalidate(m_diveTimeScale.m_dataRect);

            // Dive Depth
            if(m_depthScale.HandleMouseDown(e))
                m_parentForm.Invalidate(m_depthScale.m_dataRect);

            // Animat Movement
            if(m_movement.HandleMouseDown(e))
                m_parentForm.Invalidate(m_movement.m_dataRect);

            return true;
        }

        //------------------------------------------------------------------------------//
        // BitmapManager HandleMouseUp()
        //------------------------------------------------------------------------------//
        public Boolean HandleMouseUp(MouseEventArgs e)
        {
            // Dive (currently has no impact by a mouse up event).
#if false
            if(m_dive.HandleMouseUp(e))
                m_parent.Invalidate(m_dive.m_dataRect);
#endif
            // Dive Time
            if(m_diveTimeScale.HandleMouseUp(e))
            {
                m_dive.SetTimeIndices(m_diveTimeScale.slideResults.start, m_diveTimeScale.slideResults.end);
                m_movement.RescaleTime(m_diveTimeScale.slideResults.start, m_diveTimeScale.slideResults.end);

                // Update the bitmaps impacted by chaning the time scale.
                m_parentForm.Invalidate(m_diveTimeScale.m_dataRect); // time scale bitmap
                m_parentForm.Invalidate(m_dive.m_displayRect); // dive profile bitmap
                m_parentForm.Invalidate(m_movement.m_displayRect); // movement bitmap
                //m_parent.Invalidate(m_depthScale.m_displayRect)// depth scale is not currently impacted.
            }

            // Dive Depth
            if(m_depthScale.HandleMouseUp(e))
            {
                // add a function to the depth and time scales to retrieve begining point and scale to display.
                m_dive.ScaleDepthRange(m_depthScale.slideResults.start, m_depthScale.slideResults.end);

               

                // Have the dialog box update the affected regions
                m_parentForm.Invalidate(m_dive.m_displayRect);
                m_parentForm.Invalidate(m_depthScale.m_dataRect);
            }

            // Animat Movement (currently has no impact by a mouse up event).
#if false
            if(m_movement.HandleMouseUp(e))
            {
                // Have the dialog box update the affected regions
                m_parent.Invalidate(m_movement.m_dataRect);
            }
#endif
            return true;
        }

        //------------------------------------------------------------------------------//
        // BitmapManager HandleMouseMove()
        //------------------------------------------------------------------------------//
        public Boolean HandleMouseMove(MouseEventArgs e)
        {
            // Dive
            if(m_dive.HandleMouseMove(e) == true)
                m_parentForm.Invalidate(m_dive.m_dataRect);

            // Dive Time
            if(m_diveTimeScale.HandleMouseMove(e) == true)
            {
                m_parentForm.Invalidate(m_diveTimeScale.m_dataRect);
            }

            // Dive Depth
            if(m_depthScale.HandleMouseMove(e) == true)
                m_parentForm.Invalidate(m_depthScale.m_dataRect);

            // Animat Movement
            if(m_movement.HandleMouseMove(e) == true)
                m_parentForm.Invalidate(m_movement.m_dataRect);

            return true;
        }

        //------------------------------------------------------------------------------//
        // BitmapManager OnPaint()
        //------------------------------------------------------------------------------//
        // MyOnPaint() is called from from the parent form's (main modeling page here)
        // OnPaint() function call.  Therefore the passed in GraphicsObject is associated
        // with the entire form rather than any specific rect and must be handled
        // accordingly.
        public void MyOnPaint(Graphics GraphicsObject)
        {
            // Call each regions (rects) method to repaint that region.
            m_dive.MyOnPaint(GraphicsObject);
            m_diveTimeScale.MyOnPaint(GraphicsObject);
            m_depthScale.MyOnPaint(GraphicsObject);
            m_movement.MyOnPaint(GraphicsObject);
        }
    }
}