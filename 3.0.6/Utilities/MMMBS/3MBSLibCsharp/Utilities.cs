using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Runtime.InteropServices;
using mbs;
using System.Diagnostics;

namespace MMMBSLib
{

    public class CUtil
    {
#if DEBUG
        public static Boolean m_debugDef = true;
#else
        private static Boolean m_debugDef = false;
#endif
        public static Boolean DebugDefined { get { return m_debugDef; } }

        public static Boolean m_expertUser = false;
        public static Boolean ExpertUserEnabled { set { m_expertUser = value; } get { return m_expertUser; } }

        public const int NUMSECONDSPERDAY = 24*60*60;

        public static Boolean TimeIsZeroOr24(double Time)
        {
            if(Time == 0 || Time == 24.0)
                return true;
            return false;
        }

        public static int IndexToCircularIndex(int Index, int Length)
        {
            int index = Index; // for debugging
            index %= Length;
            if(index < 0)
                index = Length + index;
            Debug.Assert(index >= 0 && index < Length);
            return index;
        }

        /* Returns the number of valid spans starting at the first entry
         * 
         * Clock spans are assumed to run from 12:00:00 AM (or 00:00:00 which is 0
         * seconds) to 11:59:59 PM (or 23:59:59 which is 24*60*60-1 seconds).  For ease of
         * the user, however, end time is entered the same a start time and the code
         * treats the end time to mean "up to but excluding" the listed end time.  So
         * 00:00:00 to 24:00:00 is treated as (00:00:00 to 24:00:00], or 00:00:00 to
         * 23:59:59 */
#if false
        public static int CheckClockTimeSpanArray(INTSPAN [] SpanArray)
        {
            Boolean zeroCrossingFound = false;
            INTSPAN[] spans = new INTSPAN[span.Length];
            INTSPAN beg;
            INTSPAN end;
            int i;
            int next;

            for(i=0; i<span.Length; i++)
                spans[i] = span[i]%NUMSECONDSPERDAY;

            if(span.Length == 1)
            {
                // A single span array is a special case.  It need only span the entire possible clock period.
                if(span[0].start == span[0].end ||
                    span[0].start == 0 && span[0].end == NUMSECONDSPERDAY)
                    return 1;
                return 0;
            }

            // The first span is always valid no matter how it is set up so only check for
            // zero crossing.
            if(span[0].end < span[0].start)
                zeroCrossingFound = true;

            // All following spans neeed to be compared with the one that preceeded it.
            for(i=1; i<span.Length; i++)
            {
                if(span[i].end < span[i].start)
                {
                    // There may only be a single zero crossing.
                    if(zeroCrossingFound == true)
                    {
                        // Invalid span because this is a second zero crossing.  Return i,
                        // which is the current number of valid spans so far.
                        return i;
                    }
                    zeroCrossingFound = true;
                }
            }


            // The last span needs to be compared with the first span.


            return 0;
        }
#endif
        public static Boolean CheckIndex(int Count, int Index)
        {
            Debug.Assert(0 <= Index && Index < Count, "Index Out Of Bounds");
            if(Index < 0 || Index >= Count)
                return false;
            return true;
        }

        public static double HHMMSSToTwentyHrFloatClockInclusive(uint HH, uint MM, uint SS)
        {
            HHMMSS HMS;
            HMS.hh = HH; HMS.mm = MM; HMS.ss = SS;
            return HHMMSSToTwentyHrFloatClockInclusive(HMS);
        }

        // This function permits 24*60*60 to be the highest second
        public static double HHMMSSToTwentyHrFloatClockInclusive(HHMMSS HMS)
        {
            uint clkSecs;
            uint numSecsPerDay = 24*60*60;
            double clock;
            
            clkSecs = HMS.hh * 3600 + HMS.mm * 60 + HMS.ss; // convert to seconds

            //--------------------------------------------------------//
            // Make sure number of seconds falls within 24 hour period
            //--------------------------------------------------------//
            Debug.Assert(clkSecs >= 0 && clkSecs <= numSecsPerDay);
            while(clkSecs < 0)
                clkSecs += numSecsPerDay; // number of seconds in a day.
            while(clkSecs > numSecsPerDay)
                clkSecs -= numSecsPerDay;
            //--------------------------------------------------------//

            clock = (double)HMS.hh + (double)HMS.mm/60.0 + (double)HMS.ss/(3600.0);
            return clock;
        }

        public static HHMMSS TwentyHrFloatClockToHHMMSSOrZeroInclusive(double ClockHrs)
        {
            uint clkSecs;
            HHMMSS hhmmss = new HHMMSS();

            // Handle negative entries
            // Shouldn't happen so do an assertion followed by code that handles it.
            //Debug.Assert(ClockHrs >= 0.0 && ClockHrs <= 24.0);
            while(ClockHrs < 0)
                ClockHrs += 24.0;
            while(ClockHrs > 24.0)
                ClockHrs = 24.0;

            // Convert input to an integral value in seconds in the range of
            clkSecs = (uint)Math.Floor(ClockHrs * 60*60);

            // Capture the number of hours on the clock.
            hhmmss.hh = clkSecs / (60*60);
            clkSecs -= hhmmss.hh * 60*60;

            // Capture the number of minutes on the clock
            hhmmss.mm = clkSecs / 60;
            clkSecs -= hhmmss.mm * 60;

            // Capture the number of seconds on the clock
            hhmmss.ss = clkSecs;

            return hhmmss;
        }


        public static double HHMMSSTo3MBClock(HHMMSS HMS)
        {
            uint clkSecs;
            uint numSecsPerDay = 24*60*60;
            double clock;

            clkSecs = HMS.hh * 3600 + HMS.mm * 60 + HMS.ss; // convert to seconds

            while(clkSecs < 0)
                clkSecs += numSecsPerDay; // number of seconds in a day.

            clock = (double)HMS.hh + (double)HMS.mm/60.0 + (double)HMS.ss/(3600.0);
            return clock;
        }

        // Converts an HH.Hfraction into HHMMSS not limited.
        public static HHMMSS _3MBTimeToHHMMSS(double TimeHrs)
        {
            uint clkSecs;
            HHMMSS hhmmss = new HHMMSS();

            // Handle negative entries
            Debug.Assert(TimeHrs >= 0.0);
            while(TimeHrs < 0.0)
                TimeHrs += 24.0;

            // Convert input to an integral value in seconds in the range of
            clkSecs = (uint)Math.Floor(TimeHrs * 60*60);

            // Capture the number of hours on the clock.
            hhmmss.hh = clkSecs / (60*60);
            clkSecs -= hhmmss.hh * 60*60;

            // Capture the number of minutes on the clock
            hhmmss.mm = clkSecs / 60;
            clkSecs -= hhmmss.mm * 60;

            // Capture the number of seconds on the clock
            hhmmss.ss = clkSecs;

            return hhmmss;
        }


        // Converts an HH.Hfraction into HHMMSS limited to 24.00 hours
        public static HHMMSS _3MBClockToHHMMSS(double ClockHrs)
        {
            // Handle negative entries
            Debug.Assert(ClockHrs >= 0.0);
            while(ClockHrs < 0.0)
                ClockHrs += 24.0;

            // Keep clock under 24.00
            while(ClockHrs > 24.0)
                ClockHrs -= 24.0;
            return _3MBTimeToHHMMSS(ClockHrs);
        }

        public static int _3MBHoursToSeconds(double Hours)
        {
            // Handle negative entries
            Debug.Assert(Hours >= 0.0);
            while(Hours < 0.0)
                Hours += 24.0;

            HHMMSS hms = _3MBTimeToHHMMSS(Hours);

            return (int)hms.hh * 3600 + (int)hms.mm * 60 + (int)hms.ss;
        }

        public double _SecondsTo3MBHours(int Seconds)
        {
            return Seconds/3600.0;
        }



        public static HHMMSS Time_To24HrMinSec(int Seconds)
        {
            int numSecondsPerDay = 60 * 60 * 24;

            // If a negative time entered, convert to a positive time.
            while(Seconds < 0)
                Seconds += numSecondsPerDay;

            Seconds %= 24 * 3600;
            return Time_ToHrMinSec(Seconds);
        }


        public static HHMMSS Time_ToHrMinSec(int Seconds)
        {
            HHMMSS absoluteHMS;
            int numSecondsPerDay = 60 * 60 * 24;

            // If a negative time entered, convert to a positive time.
            while(Seconds < 0)
                Seconds += numSecondsPerDay;

            absoluteHMS.ss = (uint)Seconds % 60;
            Seconds -= Seconds % 60;
            absoluteHMS.mm = (uint)(Seconds % 3600) / 60;
            Seconds -= Seconds % 3600;

            absoluteHMS.hh = (uint)Seconds / 3600;
            return absoluteHMS;
        }

 
        // Copying a matrix from C++ code to C# code (loading into C# GUI)
        public static CMatrix CopyMatrix(mbs.mbsMATRIX Srce)
        {
            int i, j;
            CMatrix Dest = new CMatrix();
            Dest.SetInitialDimensions(Srce.rowCnt, Srce.colCnt);
            for(i=0; i<Dest.RowCount; i++)
                for(j=0; j<Dest.ColumnCount; j++)
                    Dest.a[i][j] = Srce.a[i].a[j].a;
            return Dest;
        }


        // Copying a vector from C++ code to C# code  (loading into C# GUI)
        public static CVector CopyMatrix(mbs.mbsARRAY Srce)
        {
            int i;
            CVector Dest = new CVector();
            Dest.SetInitialDimensions(Srce.rowCnt, Srce.colCnt);
            for(i=0; i<Dest.columnCount; i++)
                Dest.a[i] = Srce.a[i].a;
            return Dest;
        }

        // Copying an element from C++ code to C# code (loading into C# GUI)
        public static CElement CopyMatrix(mbs.mbsELEMENT Srce)
        {
            CElement Dest = new CElement();
            Dest.SetInitialDimensions(Srce.rowCnt, Srce.colCnt);
            Dest.a = Srce.a;
            return Dest;
        }


        // Copying a matrix from C# code to C++ code (saving to C++ libraries)
        public static mbs.mbsMATRIX CopyMatrix(CMatrix Srce)
        {
            int i, j;
            mbs.mbsMATRIX Dest;

            Dest.rowCnt = Srce.RowCount;
            Dest.colCnt = Srce.ColumnCount;
            Dest.a  = new mbsARRAY[Srce.RowCount];
            for(i=0; i<Srce.RowCount; i++)
            {
                Dest.a[i].rowCnt = 1;
                Dest.a[i].colCnt = Srce.ColumnCount;
                Dest.a[i].a = new mbsELEMENT[Srce.ColumnCount];
                for(j=0; j<Srce.ColumnCount; j++)
                {
                    Dest.a[i].a[j].rowCnt = Dest.a[i].a[j].colCnt = 1;
                    Dest.a[i].a[j].a = Srce.a[i][j];
                }
            }
            return Dest;
        }

        // Copying a vector/array from C# code to C++ code (saving to C++ libraries)
        public static mbs.mbsARRAY CopyMatrix(CVector Srce)
        {
            int i;
            mbs.mbsARRAY Dest;
            Dest.rowCnt = 1;
            Dest.colCnt = Srce.columnCount;
            Dest.a = new mbsELEMENT[Srce.columnCount];

            for(i=0; i<Srce.columnCount; i++)
            {
                Dest.a[i].rowCnt = Dest.a[i].colCnt = 1;
                Dest.a[i].a = Srce.a[i];
            }
            return Dest;
        }


        // Copying an element from C# code to C++ code (saving to C++ libraries
        public static mbs.mbsELEMENT CopyMatrix(CElement Srce)
        {
            mbs.mbsELEMENT Dest;
            Dest.rowCnt = Dest.colCnt = 1;
            Dest.a = Srce.a;
            return Dest;
        }

        public static int BooleanToInt(bool B)
        {
            if(B == true)
                return 1;
            return 0;
        }

        public static bool IntToBoolean(int I)
        {
            if(I == 0)
                return false;
            return true;
        }

        public static int MinXOrX(int X, int Value)
        {
            if(Value < X)
                return X;
            return Value;
        }
        public static double MinXOrX(double X, double Value)
        {
            if(Value < X)
                return X;
            return Value;
        }
        public static int MaxXOrX(int X, int Value)
        {
            if(Value > X)
                return X;
            return Value;
        }
        public static double MaxXOrX(double X, double Value)
        {
            if(Value > X)
                return X;
            return Value;
        }

        public static int ForceNegative(int Value)
        {
            return -Math.Abs(Value);
        }

        public static double ForceNegative(double Value)
        {
            return -Math.Abs(Value);
        }


        public static int Min0OrKeep(int Value) { return Math.Abs(Value); }
        public static double Min0OrKeep(double Value) { return Math.Abs(Value); }

        public static int Max0OrKeep(int Value) { return -1 * Math.Abs(Value); }
        public static double Max0OrKeep(double Value) { return -1 * Math.Abs(Value); }


        public static string PodTypeToString(PODTYPE Type)
        {
            switch(Type)
            {
            case PODTYPE.LEAD_ANIMAT:
                return STRINGCONSTANTS.SZ_LEADANIMAT;
            case PODTYPE.CALCULATED_CENTROID:
                return STRINGCONSTANTS.SZ_CALCULATEDCENTROID;
            }
            return "error";
        }
    }
 }
