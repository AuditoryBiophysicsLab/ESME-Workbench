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
    // Species Bitmap Window Parent Class
    //----------------------------------------------------------------------------------//
    class CBitmapSpe : CBitmapBasic
    {
        public DATA_EXTREMES_SPE_BITMAP m_dataExtremes;
        public ANIMATSTATEDATABITMAP[] m_stateData;
        public DATASCALE m_dataScale;
        public Boolean m_bMaintainsScaleOnNewData;

        //public Boolean m_bMaintainCurrentScale;

        // All children of CBitmap parent point to the exact same location in memory holding the behavior colors.
        private static Color[] m_behaviorColor = BMPCONSTS.BEHAVIOR_COLOR;


        //------------------------------------------------------------------------------//
        // Species Bitmap Window Constructor
        //------------------------------------------------------------------------------//
        public CBitmapSpe(Rectangle BitmapRec)
            : base(BitmapRec) //CBitmapParent constructor
        {
            int i;
            m_bMaintainsScaleOnNewData = false;

            m_behaviorColor = new Color[BMPCONSTS.BEHAVIOR_COLOR.Length];

            for(i = 0; i < BMPCONSTS.BEHAVIOR_COLOR.Length; i++)
                m_behaviorColor[i] = BMPCONSTS.BEHAVIOR_COLOR[i];

        }

        //------------------------------------------------------------------------------//
        // Parent Bitmap SetDisplayColors()
        //------------------------------------------------------------------------------//
        public static Color ToggleBehaviorColor(uint BehaviorIndex)
        {

            if(BehaviorIndex > m_behaviorColor.Length)
                return Color.Pink; // that should serve as a warning that something is wrong.

            if(m_behaviorColor[BehaviorIndex] == BMPCONSTS.GetDefaultColor(BehaviorIndex))
                m_behaviorColor[BehaviorIndex] = BMPCONSTS.toggleColor;
            else
                m_behaviorColor[BehaviorIndex] = BMPCONSTS.GetDefaultColor(BehaviorIndex);

            return m_behaviorColor[BehaviorIndex];
        }

        public static Color GetCurrentBehaviorColor(uint BehaviorIndex)
        {
            if(BehaviorIndex > m_behaviorColor.Length)
                return Color.Pink; // that should serve as a warning that something is wrong.
            return m_behaviorColor[BehaviorIndex];
        }

        public static Color GetDefaultBehaviorColor(uint BehaviorIndex)
        {
            if(BehaviorIndex > m_behaviorColor.Length)
                return Color.Pink; // that should serve as a warning that something is wrong.
            return BMPCONSTS.GetDefaultColor(BehaviorIndex);
        }

        //------------------------------------------------------------------------------//
        // Species Bitmap Window Constructor
        //------------------------------------------------------------------------------//
        public virtual void SetDisplayData(ANIMATSTATEDATABITMAP[] StateData, DATA_EXTREMES_SPE_BITMAP DataExtremes, Boolean MaintainScaling)
        {
            m_stateData = StateData;
            m_dataExtremes = DataExtremes;
            m_bMaintainsScaleOnNewData = MaintainScaling;
        }
    }
}