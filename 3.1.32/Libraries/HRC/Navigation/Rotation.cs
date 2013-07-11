using System;

namespace HRC.Navigation
{
/**
 * Defines a 3-D rotation, M, such that the matrix multiplication, Mv, rotates
 * vector v an angle ,angle, counter clockwise about the Geo, g. See Newman and
 * Sproull, 1973, Principles of Interactive Computer Garaphics, McGraw-Hill, New
 * York, 463-465.
 */
    /**
    * Static method that does what creating a Rotation object can calling
    * rotate() on it does. Rotates vector geo2 an angle counter clockwise about
    * the Geo, v1.
    * 
    * @param v1
    * @param angle
    * @param geo2
    * @param ret The Geo to load the results in, may be null which will cause
    *        the method to allocate a new Geo object.
    * @return the ret Geo passed in, or a new one if ret was null.
    */

    public static class Rotation
    {
        /// <summary>
        /// Rotates geo2 counterclockwise about geo1 through the specified angle
        /// </summary>
        /// <param name="geo1"></param>
        /// <param name="angle"></param>
        /// <param name="geo2"></param>
        /// <param name="isDegrees"> </param>
        /// <returns></returns>
        public static Geo Rotate(Geo geo1, Geo geo2, double angle, bool isDegrees)
        {
            var x = geo1.X;
            var y = geo1.Y;
            var z = geo1.Z;
            var c = Math.Cos(isDegrees ? MoreMath.DegreesToRadians(angle) : angle);
            var s = Math.Sin(isDegrees ? MoreMath.DegreesToRadians(angle) : angle);
            var b = 1.0 - c;
            var bx = b * x;
            var by = b * y;
            var bz = b * z;
            var bxx = bx * x;
            var bxy = bx * y;
            var bxz = bx * z;
            var byy = by * y;
            var byz = by * z;
            var bzz = bz * z;
            var sx = s * x;
            var sy = s * y;
            var sz = s * z;
            var m00 = c + bxx;
            var m11 = c + byy;
            var m22 = c + bzz;
            var m01 = (-sz) + bxy;
            var m10 = sz + bxy;
            var m12 = (-sx) + byz;
            var m21 = sx + byz;
            var m02 = sy + bxz;
            var m20 = (-sy) + bxz;

            return new Geo(m00 * geo2.X + m01 * geo2.Y + m02 * geo2.Z, m10 * geo2.X + m11 * geo2.Y + m12 * geo2.Z, m20 * geo2.X + m21 * geo2.Y + m22 * geo2.Z);
        }
    }
}
