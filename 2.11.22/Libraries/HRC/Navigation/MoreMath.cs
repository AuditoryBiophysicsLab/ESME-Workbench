using System;

namespace HRC.Navigation
{
    public static class MoreMath
    {
        public static double DegreesToRadians(double degrees)
        {
            return degrees * (Math.PI / 180.0);
        }

        public static double TwoPi = Math.PI * 2.0;
        public static double PiOverTwo = Math.PI / 2.0;

        /// <summary>
        /// Checks if a ~= b. Use this to test equality of floating point numbers.
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="epsilon"></param>
        /// <returns></returns>
        public static bool IsApproximatelyEqual(double a, double b, double epsilon = 0.00000001)
        {
            return (Math.Abs(a - b) <= epsilon);
        }

        /// <summary>
        /// Checks if a ~= b. Use this to test equality of floating point numbers.
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="epsilon"></param>
        /// <returns></returns>
        public static bool IsApproximatelyEqual(float a, float b, float epsilon = 0.00001f)
        {
            return (Math.Abs(a - b) <= epsilon);
        }

        /// <summary>
        /// Hyperbolic arcsine(x): log (x+sqrt(1+x^2))
        /// </summary>
        /// <param name="x"></param>
        /// <returns></returns>
        public static float Asinh(float x)
        {
            return (float) Math.Log(x + Math.Sqrt(x*x + 1));
        }

        /// <summary>
        /// Hyperbolic arcsine(x): log (x+sqrt(1+x^2))
        /// </summary>
        /// <param name="x"></param>
        /// <returns></returns>
        public static double Asinh(double x)
        {
            return Math.Log(x + Math.Sqrt(x*x + 1));
        }

        /// <summary>
        /// Hyperbolic sine(x): (e^x-e^-x)/2
        /// </summary>
        /// <param name="x"></param>
        /// <returns></returns>
        public static float Sinh(float x)
        {
            return (float) (Math.Pow(Math.E, x) - Math.Pow(Math.E, -x))/2.0f;
        }

        /// <summary>
        /// Hyperbolic sine(x): (e^x-e^-x)/2
        /// </summary>
        /// <param name="x"></param>
        /// <returns></returns>
        public static double Sinh(double x)
        {
            return (Math.Pow(Math.E, x) - Math.Pow(Math.E, -x))/2.0d;
        }

        /// <summary>
        /// Is x odd?
        /// </summary>
        /// <param name="x"></param>
        /// <returns>true if x is odd, false otherwise</returns>
        public static bool IsOdd(short x)
        {
            return !IsEven(x);
        }

        /// <summary>
        /// Is x odd?
        /// </summary>
        /// <param name="x"></param>
        /// <returns>true if x is odd, false otherwise</returns>
        public static bool IsOdd(int x)
        {
            return !IsEven(x);
        }

        /// <summary>
        /// Is x odd?
        /// </summary>
        /// <param name="x"></param>
        /// <returns>true if x is odd, false otherwise</returns>
        public static bool IsOdd(long x)
        {
            return !IsEven(x);
        }

        /// <summary>
        /// Is x even?
        /// </summary>
        /// <param name="x"></param>
        /// <returns>true if x is even, false otherwise</returns>
        public static bool IsEven(short x)
        {
            return ((x & 0x1) == 0);
        }

        /// <summary>
        /// Is x even?
        /// </summary>
        /// <param name="x"></param>
        /// <returns>true if x is even, false otherwise</returns>
        public static bool IsEven(int x)
        {
            return ((x & 0x1) == 0);
        }

        /// <summary>
        /// Is x even?
        /// </summary>
        /// <param name="x"></param>
        /// <returns>true if x is even, false otherwise</returns>
        public static bool IsEven(long x)
        {
            return ((x & 0x1) == 0);
        }

        /**
     * Converts a byte in the range of -128 to 127 to an int in the
     * range 0 - 255.
     * 
     * @param b (-128 &lt;= b &lt;= 127)
     * @return int (0 &lt;= b &lt;= 255)
     */

        public static int signedToInt(byte b)
        {
            return (b & 0xff);
        }

        /**
     * Converts a short in the range of -32768 to 32767 to an int in
     * the range 0 - 65535.
     * 
     * @param w (-32768 &lt;= b &lt;= 32767)
     * @return int (0 &lt;= b &lt;= 65535)
     */

        public static int signedToInt(short w)
        {
            return (w & 0xffff);
        }

        /**
     * Convert an int in the range of -2147483648 to 2147483647 to a
     * long in the range 0 to 4294967295.
     * 
     * @param x (-2147483648 &lt;= x &lt;= 2147483647)
     * @return long (0 &lt;= x &lt;= 4294967295)
     */

        public static long signedToLong(int x)
        {
            return (x & 0xFFFFFFFFL);
        }

        /**
     * Converts an int in the range of 0 - 65535 to an int in the
     * range of 0 - 255.
     * 
     * @param w int (0 &lt;= w &lt;= 65535)
     * @return int (0 &lt;= w &lt;= 255)
     */

        public static int wordToByte(int w)
        {
            return w >> 8;
        }

        /**
     * Build short out of bytes (in big endian order).
     * 
     * @param bytevec bytes
     * @param offset byte offset
     * @return short
     */

        public static short BuildShortBE(byte[] bytevec, int offset)
        {
            return (short) (((bytevec[0 + offset]) << 8) | (signedToInt(bytevec[1 + offset])));
        }

        /**
     * Build short out of bytes (in little endian order).
     * 
     * @param bytevec bytes
     * @param offset byte offset
     * @return short
     */

        public static short BuildShortLE(byte[] bytevec, int offset)
        {
            return (short) (((bytevec[1 + offset]) << 8) | (signedToInt(bytevec[0 + offset])));
        }

        /**
     * Build short out of bytes.
     * 
     * @param bytevec bytes
     * @param offset byte offset
     * @param MSBFirst BE or LE?
     * @return short
     */

        public static short BuildShort(byte[] bytevec, int offset,
                                       bool MSBFirst)
        {
            return MSBFirst ? (BuildShortBE(bytevec, offset)) : (BuildShortLE(bytevec, offset));
        }

        /**
     * Build short out of bytes (in big endian order).
     * 
     * @param bytevec bytes
     * @param MSBFirst BE or LE?
     * @return short
     */

        public static short BuildShortBE(byte[] bytevec, bool MSBFirst)
        {
            return BuildShortBE(bytevec, 0);
        }

        /**
     * Build short out of bytes (in little endian order).
     * 
     * @param bytevec bytes
     * @param MSBFirst BE or LE?
     * @return short
     */

        public static short BuildShortLE(byte[] bytevec, bool MSBFirst)
        {
            return BuildShortLE(bytevec, 0);
        }

        /**
     * Build short out of bytes.
     * 
     * @param bytevec bytes
     * @param MSBFirst BE or LE?
     * @return short
     */

        public static short BuildShort(byte[] bytevec, bool MSBFirst)
        {
            return BuildShort(bytevec, 0, MSBFirst);
        }

        /**
     * Build int out of bytes (in big endian order).
     * 
     * @param bytevec bytes
     * @param offset byte offset
     * @return int
     */

        public static int BuildIntegerBE(byte[] bytevec, int offset)
        {
            return (((bytevec[0 + offset]) << 24)
                    | (signedToInt(bytevec[1 + offset]) << 16)
                    | (signedToInt(bytevec[2 + offset]) << 8) | (signedToInt(bytevec[3 + offset])));
        }

        /**
     * Build int out of bytes (in little endian order).
     * 
     * @param bytevec bytes
     * @param offset byte offset
     * @return int
     */

        public static int BuildIntegerLE(byte[] bytevec, int offset)
        {
            return (((bytevec[3 + offset]) << 24)
                    | (signedToInt(bytevec[2 + offset]) << 16)
                    | (signedToInt(bytevec[1 + offset]) << 8) | (signedToInt(bytevec[0 + offset])));
        }

        /**
     * Build int out of bytes.
     * 
     * @param bytevec bytes
     * @param offset byte offset
     * @param MSBFirst BE or LE?
     * @return int
     */

        public static int BuildInteger(byte[] bytevec, int offset,
                                       bool MSBFirst)
        {
            return MSBFirst ? BuildIntegerBE(bytevec, offset) : BuildIntegerLE(bytevec, offset);
        }

        /**
     * Build int out of bytes (in big endian order).
     * 
     * @param bytevec bytes
     * @return int
     */

        public static int BuildIntegerBE(byte[] bytevec)
        {
            return BuildIntegerBE(bytevec, 0);
        }

        /**
     * Build int out of bytes (in little endian order).
     * 
     * @param bytevec bytes
     * @return int
     */

        public static int BuildIntegerLE(byte[] bytevec)
        {
            return BuildIntegerLE(bytevec, 0);
        }

        /**
     * Build int out of bytes.
     * 
     * @param bytevec bytes
     * @param MSBFirst BE or LE?
     * @return int
     */

        public static int BuildInteger(byte[] bytevec, bool MSBFirst)
        {
            if (MSBFirst)
                return BuildIntegerBE(bytevec, 0);
            return BuildIntegerLE(bytevec, 0);
        }

        /**
     * Build long out of bytes (in big endian order).
     * 
     * @param bytevec bytes
     * @param offset byte offset
     * @return long
     */

        public static long BuildLongBE(byte[] bytevec, int offset)
        {
            return (((long) signedToInt(bytevec[0 + offset]) << 56)
                    | ((long) signedToInt(bytevec[1 + offset]) << 48)
                    | ((long) signedToInt(bytevec[2 + offset]) << 40)
                    | ((long) signedToInt(bytevec[3 + offset]) << 32)
                    | ((long) signedToInt(bytevec[4 + offset]) << 24)
                    | ((long) signedToInt(bytevec[5 + offset]) << 16)
                    | ((long) signedToInt(bytevec[6 + offset]) << 8)
                    | ((long) signedToInt(bytevec[7 + offset]) << 0));
        }

        /**
     * Build long out of bytes (in little endian order).
     * 
     * @param bytevec bytes
     * @param offset byte offset
     * @return long
     */

        public static long BuildLongLE(byte[] bytevec, int offset)
        {
            return (((long) signedToInt(bytevec[7 + offset]) << 56)
                    | ((long) signedToInt(bytevec[6 + offset]) << 48)
                    | ((long) signedToInt(bytevec[5 + offset]) << 40)
                    | ((long) signedToInt(bytevec[4 + offset]) << 32)
                    | ((long) signedToInt(bytevec[3 + offset]) << 24)
                    | ((long) signedToInt(bytevec[2 + offset]) << 16)
                    | ((long) signedToInt(bytevec[1 + offset]) << 8) 
                    | ((long) signedToInt(bytevec[0 + offset]) << 0));
        }

        /**
     * Build long out of bytes.
     * 
     * @param bytevec bytes
     * @param offset byte offset
     * @param MSBFirst BE or LE?
     * @return long
     */

        public static long BuildLong(byte[] bytevec, int offset,
                                     bool MSBFirst)
        {
            return MSBFirst ? BuildLongBE(bytevec, offset) : BuildLongLE(bytevec, offset);
        }

        /**
     * Build long out of bytes (in big endian order).
     * 
     * @param bytevec bytes
     * @return long
     */

        public static long BuildLongBE(byte[] bytevec)
        {
            return BuildLongBE(bytevec, 0);
        }

        /**
     * Build long out of bytes (in little endian order).
     * 
     * @param bytevec bytes
     * @return long
     */

        public static long BuildLongLE(byte[] bytevec)
        {
            return BuildLongLE(bytevec, 0);
        }

        /**
     * Build long out of bytes.
     * 
     * @param bytevec bytes
     * @param MSBFirst BE or LE?
     * @return long
     */

        public static long BuildLong(byte[] bytevec, bool MSBFirst)
        {
            return MSBFirst ? BuildLongBE(bytevec, 0) : BuildLongLE(bytevec, 0);
        }
    }
}