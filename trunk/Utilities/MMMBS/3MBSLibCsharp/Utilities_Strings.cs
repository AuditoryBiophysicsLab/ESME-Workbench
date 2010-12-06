using System;
using System.Collections.Generic;
using System.Text;

namespace MMMBSLib
{
    public class CStringUtil
    {

        public static string SzEnforceUnsignedIntFmt(string Sz)
        {
            int i;
            if(Sz.Length < 1)
                return "";

            // The entire string starting at the end
            for(i=Sz.Length-1; i>=0; i--)
                if(Sz[i] < 48 || Sz[i] > 57 || Sz[i] == '-')
                    Sz = Sz.Remove(i, 1);

            // Remove uneeded leading zeros.
            while(Sz.Length > 1 && Sz[0] == '0')
                Sz = Sz.Remove(0, 1);

            return Sz;
        }

        public static string SzEnforceIntFmt(string Sz)
        {
            if(Sz.Length < 1)
                return "";

            // The entire string except for the first character starting at the end
            for(int i=Sz.Length-1; i>0; i--)
                if(Sz[i] < 48 || Sz[i] > 57)
                    Sz = Sz.Remove(i, 1);

            // The first character
            if((Sz[0] < 48 || Sz[0] > 57) && Sz[0] != '-')
                Sz = Sz.Remove(0, 1);

            return Sz;
        }

        public static string SzEnforceDoubleFmt(string Sz)
        {
            Boolean decimalFound;
            int     i;


            decimalFound = false;
            i=0;

            if(Sz.Length < 1)
                return "";

            // Remove extra decimal places.
            for(; i<Sz.Length; i++)
            {
                if(Sz[i] == 46)
                {
                    if(decimalFound == true)
                        Sz = Sz.Remove(i, 1);
                    decimalFound = true;
                }
            }

            for(i=Sz.Length-1; i>0; i--)
            {
                if((Sz[i] < 48 || Sz[i] > 57) && Sz[i] != 46)
                    Sz = Sz.Remove(i, 1);
            }

            if((Sz[0] < 48 || Sz[0] > 57) && Sz[0] != 46 && Sz[0] != '-')
                Sz = Sz.Remove(0, 1);

            return Sz;
        }

        public static string SzRemoveBrackets(string Sz)
        {
            for(int i=Sz.Length-1; i>=0; i--)
                if(Sz[i] == ']' || Sz[i] == '[')
                    Sz = Sz.Remove(i, 1);
            return Sz;
        }

        public static Boolean SzIsValidDoubleChar(char C)
        {
            if((C >= '0' && C <= '9') || C == '.')
                return true;
            return false;
        }

        public static Boolean SzIsNumericalChar(char C)
        {
            if(C >= '0' && C <= '9')
                return true;
            return false;
        }

        private static string SzRemoveExtraCRLF(string Sz)
        {
            int i;
            if(Sz == null)
                return "";
            if(Sz.Length == 0)
                return Sz;
            if(Sz.Length == 1)
            {
                if(Sz[0] == 13 || Sz[0] == 10)
                    return "";
            }
            // Length is 2 or greater
            // remove solo CR (shouldn't be possible for these to be present)
            for(i=0; i<Sz.Length-1; i++)
            {
                if(Sz[i] == 13 && Sz[i+1] != 10)
                    Sz = Sz.Remove(i--, 1);
            }
            // Remove solo LF (should be possible for these to be present)
            for(i=1; i<Sz.Length; i++)
            {
                if(Sz[i-1] != 13 && Sz[i] == 10)
                    Sz = Sz.Remove(i--, 1);
            }

            // Remove CRLF from front and rear of string
            if(Sz.Length >= 2 && Sz[0] == 13 && Sz[1] == 10)
                Sz = Sz.Remove(0, 2);
            if(Sz.Length >= 2 && Sz[Sz.Length-2] == 13 && Sz[Sz.Length-1] == 10)
                Sz = Sz.Remove(Sz.Length-2, 2);
            return Sz;
        }

        //------------------------------------------------------------------------------//
        // String Vectors
        //---------------//
        // Returns a string that is a pure double vector format.
        // This function is not dependent upon any other function
        public static string SzForceIntoVectorDoubleFormat(string Sz)
        {
            int i, j, stopWarning;
            Boolean decFound;

            if(Sz == null || Sz.Length == 0)
                return "1.000"; // return most basic vector.

            // Replace all tabs with spaces.
            Sz = SzReplaceTabsWithSpace(Sz);

            // Key:
            // w = numerical whole number
            // f = numerical fractional number
            // . = decimal point
            // x = don't care what the character is

            //--------------------------------------------------------------------------//
            // Set up acceptible initial conditions
            //-------------------------------------//
            // Remove all non double and all non space characters from the string.
            // Leaves behind only '0'~'9', ' ', and '.'
            for(i=Sz.Length-1; i>=0 && Sz.Length>0; i--)
            {
                if(SzIsValidDoubleChar(Sz[i]) == false && Sz[i] != 32)
                    Sz = Sz.Remove(i, 1);
            }

            // Remove all space characters from the front of the string
            while(Sz.Length>0 && SzIsValidDoubleChar(Sz[0]) == false)
                Sz = Sz.Remove(0, 1);

            // Remove space characters from rear of string.
            while(Sz.Length > 0 && SzIsValidDoubleChar(Sz[Sz.Length-1]) == false)
                Sz = Sz.Remove(Sz.Length-1, 1);

            // Remove duplicate/sequential spaces
            for(i=0; i<Sz.Length; i++)
            {
                if(Sz[i] == ' ')
                {
                    while(i+1 < Sz.Length && Sz[i+1] == ' ')
                        Sz = Sz.Remove(i+1, 1);
                }
            }
            if(Sz.Length == 0)
                return "1.000";

            // Make sure there are no decimal points not separated by a space and whole
            // number [ ][w][.]
            decFound = false;
            for(i=0; i<Sz.Length; i++)
            {
                if(Sz[i] == '.')
                {
                    if(decFound == true)
                    {
                        // At a '.' that wasn't separated from the previous decimal
                        // place by a space.  Insert a " 0."  'decFound' remains true.
                        Sz = Sz.Insert(i, " 0");
                    }

                    decFound = true;
                }

                if(Sz[i] == ' ')
                    decFound = false;
            }

            // If the first character is a decimal point, preappend a '0' to the front of the sting.
            if(Sz[0] == '.')
                Sz = '0' + Sz;

            //--------------------------------------------------------------------------//
            // Parse the string, make sure it has proper format
            //-------------------------------------------------//
            for(i=0; i<Sz.Length; i++)
            {
                if(SzIsNumericalChar(Sz[i]) == true)
                {
                    // anything can come after a numerical character
                    continue;
                }
                else if(Sz[i] == '.')
                {
                    // Entry guarantee:
                    //  (1) a w preceeded this '.'
                    //  (2) there will be a space somewhere between this '.' and the next.
                    //  (3) only numerical characters '0' ~ '9', spaces, and '.' remain.
                    //  (4) no double spaces (no [ ][ ] present).
                    // Exit:
                    //  Three f's must come after this '.'
                    if(Sz.Length == i+1)
                    {
                        // At the end of the string, append three '0''s
                        //     i
                        // [w][.]           starting
                        // [w][.][0][0][0]  ending
                        Sz = Sz + "000";
                    }
                    else if(Sz.Length == i+2)
                    {
                        // There's is one character following the '.'
                        //     i
                        // [w][.][?]         starting
                        if(SzIsNumericalChar(Sz[i+1]) == true)
                        {
                            //     i
                            // [x][.][f]        starting
                            // [x][.][f][0][0]  ending
                            // need to APPEND two '0''s
                            Sz = Sz + "00";
                        }
                        else
                        {
                            //     i
                            // [x][.][ ] to       starting
                            // [x][.][0][0][0][ ] ending
                            // need to INSERT three '0''s
                            Sz = Sz.Insert(i+1, "000");
                        }
                    } // end else if(Sz.Length == i+2)
                    else if(Sz.Length == i+3)
                    {
                        //     i
                        // [x][.][?][?]  starting
                        // There's are two characters folloing the '.'
                        if(SzIsNumericalChar(Sz[i+1]) == true)
                        {
                            //     i
                            // [x][.][f][?] starting
                            if(SzIsNumericalChar(Sz[i+2]) == true)
                            {
                                //     i
                                // [x][.][f][f]    starting
                                // [x][.][f][f][0] ending
                                // need to append a '0'
                                Sz = Sz + "0";
                            }
                            else
                            {
                                //     i
                                // [x][.][f][ ]        starting
                                // [x][.][f][0][0][ ]  ending
                                // Need to insert 2 '0'
                                Sz = Sz.Insert(i+2, "00");
                            }
                        }
                    } // end  else if(Sz.Length == i+3)
                    else
                    {
                        // The length of the string is more than i+3
                        if(SzIsNumericalChar(Sz[i+1]) == true && SzIsNumericalChar(Sz[i+2]) == true && SzIsNumericalChar(Sz[i+3]) == true)
                            stopWarning = i; // do nothing
                        else if(SzIsNumericalChar(Sz[i+1]) == true && SzIsNumericalChar(Sz[i+2]) == true)
                            Sz = Sz.Insert(i+3, "0");
                        else if(SzIsNumericalChar(Sz[i+1]) == true)
                            Sz = Sz.Insert(i+2, "00");
                        else
                            Sz = Sz.Insert(i+1, "000");
                    }

                    // Verify the next element in the vector is separated from this one by
                    // a space and whole number.
                    for(j=i+1; j<Sz.Length; j++)
                    {
                        if(Sz[j] == ' ')
                            break;
                        if(Sz[j] == '.')
                        {
                            Sz = Sz.Insert(j, " 0");
                            break;
                        }
                    }
                } // end if(Sz[i] == '.')
                else if(Sz[i] == ' ')
                {
                    stopWarning = i;
                }
                else
                {
                    // Shouldn't get here.
                    stopWarning = i;
                }
            }
            return Sz;
        }


        // Returns the Vector's dimensions.
        // Calls SzForceIntoVectorDoubleFormat().
        public static VECTORDIMENSIONS SzVectorGetDimensions(string Sz)
        {
            int i;
            Boolean inSpace = false, inNumber = false;
            VECTORDIMENSIONS vd;
            vd.colCnt = vd.rowCnt = 0;

            if(Sz.Length == 0)
                return vd;

            // Make this a pure string vector of doubles
            Sz = SzForceIntoVectorDoubleFormat(Sz);

            for(i=0; i<Sz.Length; i++)
            {
                // For each new number encountered, increase the column count.
                if(Sz[i] == 32 && inSpace == false)
                {
                    inNumber = false;
                    inSpace = true;
                }
                else if(((Sz[i] >=48 && Sz[i] <= 57) || Sz[i] == '.') && inNumber == false)
                {
                    inNumber = true;
                    inSpace = false;
                    vd.colCnt++;
                }
            }
            if(vd.colCnt > 0)
                vd.rowCnt = 1;
            return vd;
        }

        // Returns the matrix dimensions.
        // Calls SzRemoveExtraCRLF() to make sure there are no unneccasry CR, LF, and CRLF
        // combinations.
        // Calls SzVectorGetDimensions(), which itself calls SzForceIntoVectorDoubleFormat(),
        // to row lengths.
        public static MATRIXDIMENSIONS SzMatrixDimensions(string Sz)
        {
            int i;
            MATRIXDIMENSIONS cnt;
            VECTORDIMENSIONS vd;
            string[] sep = { "\r\n" };
            string[] szArray;

            // Initialize the matrix dimensions.
            cnt.maxCols = 0;
            cnt.minCols = 0;
            cnt.rowCnt = 0;
            cnt.colCnts = new int[0];

            // Verify a string passed in.
            if(Sz == null || Sz.Length == 0)
                return cnt;

            // Make sure CRLF proper for splitting into vector strings.
            Sz = SzRemoveExtraCRLF(Sz);
            if(Sz.Length == 0)
                return cnt;

            // Split into vector strings, make sure at least one row came out of it.
            szArray = Sz.Split(sep, StringSplitOptions.None);
            if(szArray.Length == 0)
                return cnt;

            // Determine row sizes.
            vd.rowCnt = 1; // not needed.
            cnt.rowCnt = szArray.Length;
            cnt.colCnts = new int[cnt.rowCnt];
            vd = SzVectorGetDimensions(szArray[0]);
            cnt.colCnts[0] = cnt.maxCols = cnt.minCols = vd.colCnt;
            for(i = 1; i<cnt.rowCnt; i++)
            {
                vd = SzVectorGetDimensions(szArray[i]);
                cnt.colCnts[i] = vd.colCnt;
                if(cnt.maxCols < vd.colCnt)
                    cnt.maxCols = vd.colCnt;
                if(cnt.minCols > vd.colCnt)
                    cnt.minCols = vd.colCnt;
            }
            return cnt;
        }

        public static string SzReplaceTabsWithSpace(string Sz)
        {
            int i;
            if(Sz == null || Sz.Length == 0)
                return "";

            if(Sz.Length == 1)
            {
                if(Sz[0] == 9)
                    return " ";  // return a space.
            }
            // Length is 2 or greater
            // remove solo CR (shouldn't be possible for these to be present)
            for(i=0; i<Sz.Length-1; i++)
            {
                if(Sz[i] == 9)
                {
                    Sz = Sz.Remove(i, 1);
                    Sz = Sz.Insert(i, " ");
                }
            }
            return Sz;
        }

        // Returns a string that is a pure double matrix and makes sure all rows are same length
        public static string SzForceIntoMatrixDoubleFormat(string Sz)
        {
            int i, j;
            MATRIXDIMENSIONS cnt;
            VECTORDIMENSIONS vd;
            string[] sep = { "\r\n" };
            string[] szArray;

            // Initialize the matrix dimensions.
            cnt.maxCols = 0;
            cnt.minCols = 0;
            cnt.rowCnt = 0;
            cnt.colCnts = new int[0];

            // Verify a string passed in.
            if(Sz == null || Sz.Length == 0)
                return "1.000"; // return most basic matrix.

            // Replace all tabs with spaces.
            Sz = SzReplaceTabsWithSpace(Sz);

            // Make sure CRLF proper for splitting into vector strings.
            Sz = SzRemoveExtraCRLF(Sz);
            if(Sz.Length == 0)
                return "1.000"; // return most basic matrix.

            // Split into vector strings, make sure at least one row came out of it.
            szArray = Sz.Split(sep, StringSplitOptions.None);
            if(szArray.Length == 0)
                return "1.000"; // return most basic matrix.

            // Determine row sizes.
            vd.rowCnt = 1; // not needed.
            cnt.rowCnt = szArray.Length;
            cnt.colCnts = new int[cnt.rowCnt];
            vd = SzVectorGetDimensions(szArray[0]);

            // Verify double matrix format.
            for(i=0; i<cnt.rowCnt; i++)
                szArray[i] = SzForceIntoVectorDoubleFormat(szArray[i]);

            // Get counts.
            cnt.colCnts[0] = cnt.maxCols = cnt.minCols = vd.colCnt;
            for(i = 1; i<cnt.rowCnt; i++)
            {
                vd = SzVectorGetDimensions(szArray[i]);
                cnt.colCnts[i] = vd.colCnt;
                if(cnt.maxCols < vd.colCnt)
                    cnt.maxCols = vd.colCnt;
                if(cnt.minCols > vd.colCnt)
                    cnt.minCols = vd.colCnt;
            }

            // Verify all rows have the same length
            for(i=0; i<cnt.rowCnt; i++)
            {
                for(j=cnt.colCnts[i]; j<cnt.maxCols; j++)
                {
                    if(szArray[i].Length == 0)
                        szArray[i] = "1.000"; // shouldn't ever happen.
                    else
                        szArray[i] = szArray[i] + " 1.000";
                }
            }

            // Reconstuct the matrix
            if(cnt.rowCnt > 0)
                Sz = szArray[0];
            for(i=1; i<cnt.rowCnt; i++)
                Sz = Sz + "\r\n" + szArray[i];

            return Sz;
        }


        public static double[] SzVectorToDoubleArray(string Sz)
        {
            int i;
            VECTORDIMENSIONS vd;
            double[] d = { 1.0 };
            string[] strArr;

            if(Sz == null || Sz.Length == 0)
                return d;

            Sz = SzForceIntoVectorDoubleFormat(Sz);
            vd = SzVectorGetDimensions(Sz);

            if(vd.colCnt == 0)
                return d;

            d = new double[vd.colCnt];

            strArr = Sz.Split(' ');

            // vd.colCnt and  strArr.Length should be equal.
            for(i=0; i<vd.colCnt && i<strArr.Length; i++)
                d[i] = Convert.ToDouble(strArr[i]);

            // Make sure they are equal (99999 should stand out yet be a valid number).
            for(; i<vd.colCnt; i++)
                d[i] = 99999;
            return d;
        }


        public static double[][] SzMatrixToDouble2DArray(string Sz)
        {
            int i;
            MATRIXDIMENSIONS md;
            double[][] d = { new double[] { 1.0 } };
            string[] szArray;
            string[] sep = { "\r\n" };

            if(Sz == null || Sz.Length == 0)
                return d;

            // Make sure properly formatted matrix.
            Sz = SzForceIntoMatrixDoubleFormat(Sz);
            md = SzMatrixDimensions(Sz);

            // Make sure there's something in the matrix.
            if(md.rowCnt == 0 || md.maxCols == 0)
                return d;

            // Split into vector strings, make sure at least one row came out of it.
            szArray = Sz.Split(sep, StringSplitOptions.None);
            if(szArray.Length == 0)
                return d; // return most basic matrix.

            d = new double[md.rowCnt][];
            for(i=0; i<md.rowCnt; i++)
                d[i] = SzVectorToDoubleArray(szArray[i]);

            return d;
        }


        // See about removing anything below here.
        // Remove
        public static Boolean SzVectorVerifyDoubleFormat(string Sz)
        {
            Boolean decFound = false;
            int i;

            if(Sz.Length == 0)
                return true;

            // Remove brackets before checking.
            Sz = SzRemoveBrackets(Sz);

            // Check for non double format characters (keep spaces though).
            for(i=Sz.Length-1; i>=0; i--)
            {
                //48 is '0', 57 is '9', 46 is '.', and 32 is ' ' (space).
                if((Sz[i] < 48 || Sz[i] > 57) && (Sz[i] != 46) && (Sz[i] != 32))
                    return false;
            }

            // Parse each number sequence (entire doubles) in the string, verify no
            // duplicate decimals within the same double.  
            decFound = false;
            for(i=0; i<Sz.Length; i++)
            {
                // If a space is encountered, reset decFound to false.
                if(Sz[i] == 32)
                    decFound = false;

                if(Sz[i] == '.')
                {
                    if(decFound == true)
                    {
                        // duplicate decimal within the same number returns false.
                        return false;
                    }
                    decFound = true;

                    //------------------------------//
                    // Check for solo decimal places
                    //------------------------------//
                    // A solo decimal place with no numbers before or after it
                    // returns false.
                    if(Sz.Length == 1)
                        return false;

                    // 48 = '0' and 57 = '9'.
                    if(i-1 >= 0 && i+1 < Sz.Length && (Sz[i-1] < 48 || Sz[i-1] > 57) && (Sz[i+1] < 48 || Sz[i+1] > 57))
                        return false;
                    else if(i-1 >= 0 && i+1 == Sz.Length && (Sz[i-1] < 48 || Sz[i-1] > 57))
                        return false;
                    else if(i-1 < 0 && i+1 < Sz.Length && (Sz[i+1] < 48 || Sz[i+1] > 57))
                        return false;
                }
            }
            return true;
        }



        /*
         * Remove:
         *  "  ...":        leading spaces
         *  "     ":        unneccesarry spaces.
         *  "...\n\n...":   double \n's: remove second \n
         *  "...\n  \n":    In a sequence of CRLF, 1 or more spaces, then CRLF, remove
         *                  multiple spaces and second CRLF
         *  "...\n":        ending CRLF
        */
        // See about removing.
        private static string SzMatrixRemoveUnneededChars(string Sz)
        {
            Boolean decFound = false;
            int i;

            if(Sz.Length == 0)
                return Sz; // nothing to do.

            // Remove leading spaces.
            while(Sz.Length > 0 && Sz[0] == 32)
                Sz = Sz.Remove(0, 1);
            if(Sz.Length == 0)
                return Sz;

            // Remove unnecessary spaces
            for(i=0; i<Sz.Length; i++)
                while(Sz[i] == 32 && (i+1) < Sz.Length && Sz[i+1] == 32)
                    Sz = Sz.Remove(i+1, 1);
            if(Sz.Length == 0)
                return Sz;

            // Remove trailing spaces
            i = Sz.Length-1;
            while(i >= 0)
            {
                if(Sz[i] == 32)
                    Sz = Sz.Remove(i, 1);
                else if((Sz[i] >= '0' && Sz[i] <= '9') || Sz[i] == 10 || Sz[i] == '.')
                    break;
                i--;
            }
            if(Sz.Length == 0)
                return Sz;

            // Remove spaces after CRLF (duplicate spaces already removed)
            for(i=0; i<Sz.Length; i++)
                while((i+2 < Sz.Length && Sz[i] == 13 && Sz[i+1] == 10) && (Sz[i+2] == 32))
                    Sz = Sz.Remove(i+2, 1);

            // Remove duplicate CRLF (spaces after CRLF already removed)
            for(i=0; i<Sz.Length; i++)
                while((i+3 < Sz.Length && Sz[i] == 13 && Sz[i+1] == 10) && (i+3 < Sz.Length && Sz[i+2] == 13 && Sz[i+3] == 10))
                    Sz = Sz.Remove(i+2, 2);

            // Remove ending CRLF.
            if(Sz.Length >= 2 && Sz[Sz.Length-2] == 13 && Sz[Sz.Length-1] == 10)
                Sz = Sz.Remove(Sz.Length-2, 2);

            // Remove non double format characters (keep spaces though).
            for(i=Sz.Length-1; i>=0; i--)
            {
                //48 is '0', 57 is '9', 46 is '.', and 32 is ' ' (space).
                if(i-1 >= 0 && Sz[i-1] == 13 && Sz[i] == 10)
                {
                    // Skip over CRLF
                    i--;
                    continue;
                }
                if((Sz[i] < 48 || Sz[i] > 57) && (Sz[i] != 46) && (Sz[i] != 32))
                    Sz = Sz.Remove(i, 1);
            }

            // Parse each number sequence (entire doubles) in the string, verify no
            // duplicate decimals within the same double.  
            decFound = false;
            for(i=0; i<Sz.Length; i++)
            {
                // If a space is encountered, reset decFound to false.
                if(Sz[i] == 32)
                    decFound = false;
                if(i+1 < Sz.Length && Sz[i] == 13 && Sz[i+1] == 10)
                    decFound = false;

                if(Sz[i] == '.')
                {
                    if(decFound == true)
                        Sz = Sz.Remove(i, 1);

                    decFound = true;
                }
            }
            return Sz;
        }







        private static string SzEnsureFloatingPointStringFormat(string Sz)
        {
            // Parse each number sequence (entire doubles) in the string, verify
            // all numbers extend three decimal places.
            int numFractionalDigits=0;
            int numWholeDigits = 0;
            Boolean decimalPointFound = false;
            int i;

            for(i=0; i<Sz.Length; i++)
            {
                // Count the number of whole digits and fractional digits.
                // (48 is '0', 57 is '9')
                if((Sz[i] >= 48 && Sz[i] <= 57) && decimalPointFound == true)
                    numFractionalDigits++;
                if((Sz[i] >= 48 && Sz[i] <= 57) && decimalPointFound == false)
                    numWholeDigits++;

                // If a space, or CRLF encountered verify 3 decimal places present and
                // insert 0's if needed. Reset decFound to false.
                if(Sz[i] == 32 || (Sz[i] == 13 && (i+1<Sz.Length && Sz[i+1]==10)))
                {
                    if(decimalPointFound == false)
                        Sz = Sz.Insert(i++, ".");

                    while(numFractionalDigits++ < 3)
                        Sz = Sz.Insert(i++, "0");

                    decimalPointFound = false;
                    numFractionalDigits = 0;
                }

                // If a decimal point is found verify at least one whole numbered digit
                // was also found.  Set decimalPointFound to true.
                if(Sz[i] == '.')
                {
                    decimalPointFound = true;
                    if(numWholeDigits == 0)
                        Sz = Sz.Insert(i++, "0");
                    numWholeDigits = 0;
                }

                // If end of the line encountered verify 3 decimal places present and
                // insert 0's if needed. Reset decFound to false.
                if(i == Sz.Length-1)
                {
                    if(decimalPointFound == false)
                    {
                        Sz = Sz + ".";
                        i++;
                    }

                    while(numFractionalDigits++ < 3)
                    {
                        Sz = Sz + "0";
                        i++;
                    }

                    decimalPointFound = false;
                    numFractionalDigits = 0;
                }
            }
            return Sz;
        }


        // See about removing
        public static string SzForceSquareMatrixFormatDouble(string Sz)
        {
            // Get the number of rows and columns.
            MATRIXDIMENSIONS cnts;
            int rowIndex = 0;
            int szIndex;
            int requiredSize;
            int i, j, k;

#if false
            if(Sz.Length == 0)
                return Sz;
            if(Sz.Length == 1)
                Sz = Sz + "\r\n";
            if(Sz.Length == 2)
            {
                if(Sz[0] == 13 && Sz[1] == 10)
                    Sz = "1.00\r\n";
                else
                    Sz = Sz + "\r\n";
            }
#endif
               

            // Replace all tabs with spaces.
            Sz = SzReplaceTabsWithSpace(Sz);
            Sz = SzMatrixRemoveUnneededChars(Sz);
            Sz = SzEnsureFloatingPointStringFormat(Sz);
            Sz = SzForceIntoMatrixDoubleFormat(Sz);

            cnts = SzMatrixDimensions(Sz);

            //-----------------------//
            // Make the matrix square
            //-----------------------//
            // Determine the needed length to make the matrix square.
            requiredSize = cnts.maxCols;
            if(requiredSize < cnts.rowCnt)
                requiredSize = cnts.rowCnt;

            // Verify the endo fo the string ends with a CRLF.
            //if(Sz.Length > 2 && Sz[Sz.Length-2] != 13 && Sz[Sz.Length-1] != 10)
              //  Sz = Sz + "\r\n";


            // Handle all rows but the last.  The last row doesn't end with a CRLF, so it
            // is handled  separately.
            for(rowIndex=0, szIndex=0; rowIndex<cnts.rowCnt-1 && szIndex<Sz.Length; rowIndex++)
            {
                // Move to the end of the current row, insert additional columns if needed.
                while(szIndex<Sz.Length-1 && Sz[szIndex] != 13 && Sz[szIndex+1] != 10)
                    szIndex++;

                // Insert additional columns if needed.
                if(cnts.colCnts[rowIndex] != requiredSize)
                {
                    for(k=0; k < requiredSize-cnts.colCnts[rowIndex]; k++)
                        Sz = Sz.Insert(szIndex, " 0.000"); // szIndex's index remains the same.

                    // Move to the end of the current row, now expanded
                    while(szIndex<Sz.Length-1 && Sz[szIndex] != 13 && Sz[szIndex+1] != 10)
                        szIndex++;
                }
                // move the string index to the start of the next row.
                szIndex = szIndex+2;
            }

            // Handle the last row.
            for(; rowIndex<cnts.rowCnt && szIndex<Sz.Length; rowIndex++)
            {
                // Move to the end of the current row, insert additional columns if needed.
                szIndex = Sz.Length-1;
                for(k=0; k < requiredSize-cnts.colCnts[rowIndex]; k++)
                    Sz = Sz.Insert(szIndex, " 0.000"); // szIndex's index remains the same.
            }


            for(i=0; i<requiredSize - cnts.rowCnt; i++)
            {
                Sz = Sz + "\r\n0.000";
                for(j=1; j<cnts.maxCols; j++)
                    Sz = Sz + " 0.000";
            }

            Sz = SzEnsureFloatingPointStringFormat(Sz);
            return Sz;
        }


        public static Boolean SzVerifyMatrixFormatDouble(string Sz)
        {
            Boolean decFound = false;
            int i;

            if(Sz.Length == 0)
                return true; // nothing to do.

            // Remove brackets before checking.
            Sz = SzRemoveBrackets(Sz);

            // Remove non double format characters (keep spaces though).
            for(i=Sz.Length-1; i>=0; i--)
            {
                //48 is '0', 57 is '9', 46 is '.', and 32 is ' ' (space).
                if(i-1 >= 0 && Sz[i-1] == 13 && Sz[i] == 10)
                {
                    // Skip over CRLF
                    i--;
                    continue;
                }
                if((Sz[i] < 48 || Sz[i] > 57) && (Sz[i] != 46) && (Sz[i] != 32))
                    return false;
            }

            // Parse each number sequence (entire doubles) in the string, verify no
            // duplicate decimals within the same double.  
            decFound = false;
            for(i=0; i<Sz.Length; i++)
            {
                // If a space or CRLF encountered, reset decFound to false.
                if(Sz[i] == 32)
                    decFound = false;
                if(i+1 < Sz.Length && Sz[i] == 13 && Sz[i+1] == 10)
                    decFound = false;

                if(Sz[i] == '.')
                {
                    if(decFound == true)
                        return false;
                    decFound = true;
                }
            }
            return true;
        }


        public static double SzToDouble(string ValueString)
        {
            if(ValueString.Length == 0)
                return 0;
            if(ValueString.Length == 1 && SzIsValidDoubleChar(ValueString[0]) == false)
                return 0;
            if(ValueString == ".")
                return 0;

            double v = Convert.ToDouble(ValueString);
            //if(v<0)
              //  v = 0;
            return v;
        }
        public static int SzToIntOrMin0(string ValueString)
        {
            if(ValueString.Length == 0)
                ValueString = "0";
            int v = Convert.ToInt32(ValueString);
            if(v<0)
                v = 0;
            return v;
        }

        public static int SzToIntOrMin1(string ValueString)
        {
            if(ValueString.Length == 0)
                ValueString = "1";
            int v = Convert.ToInt32(ValueString);
            if(v<1)
                v = 1;
            return v;
        }


    }
}
