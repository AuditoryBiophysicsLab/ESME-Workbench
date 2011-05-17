using System;

namespace HRC.Navigation
{
    public class Rotation
    {
        readonly Geo _g;
        double _m00;
        double _m11;
        double _m22;
        double _m01;
        double _m10;
        double _m12;
        double _m21;
        double _m02;
        double _m20;

        public Rotation(Geo g, double angle)
        {
            _g = g;
            setAngle(angle);
        }

        void setAngle(double angle)
        {
            var x = _g.x;
            var y = _g.y;
            var z = _g.z;
            var c = Math.Cos(angle);
            var s = Math.Sin(angle);
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
            _m00 = c + bxx;
            _m11 = c + byy;
            _m22 = c + bzz;
            _m01 = (-sz) + bxy;
            _m10 = sz + bxy;
            _m12 = (-sx) + byz;
            _m21 = sx + byz;
            _m02 = sy + bxz;
            _m20 = (-sy) + bxz;

            /**
             * System.out.println (" Rotation " + m00 + " " + m11 + " " + m22 + "\n" +
             * m01 + " " + m10 + " " + m12 + "\n" + m21 + " " + m02 + " " + m20);
             */
        }

        public Geo rotate(Geo v)
        {
            double x = v.x,
                   y = v.y,
                   z = v.z;
            return new Geo(_m00 * x + _m01 * y + _m02 * z, _m10 * x + _m11 * y + _m12 * z, _m20 * x + _m21 * y + _m22 * z);
        }

        /**
         * Static method that does what creating a Rotation object can calling
         * rotate() on it does. Rotates vector v2 an angle counter clockwise about
         * the Geo, v1.
         * 
         * @param v1
         * @param angle
         * @param v2
         * @param ret The Geo to load the results in, may be null which will cause
         *        the method to allocate a new Geo object.
         * @return the ret Geo passed in, or a new one if ret was null.
         */

        public static Geo rotate(Geo v1, double angle, Geo v2, Geo ret)
        {
            var x = v1.x;
            var y = v1.y;
            var z = v1.z;
            var c = Math.Cos(angle);
            var s = Math.Sin(angle);
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

            /**
             * System.out.println (" Rotation " + m00 + " " + m11 + " " + m22 + "\n" +
             * m01 + " " + m10 + " " + m12 + "\n" + m21 + " " + m02 + " " + m20);
             */
            var x2 = v2.x;
            var y2 = v2.y;
            var z2 = v2.z;

            if (ret == null)
            {
                return new Geo(m00 * x2 + m01 * y2 + m02 * z2, m10 * x2 + m11 * y2 + m12 * z2, m20 * x2 + m21 * y2 + m22 * z2);
            }

            ret.initialize(m00 * x2 + m01 * y2 + m02 * z2, m10 * x2 + m11 * y2 + m12 * z2, m20 * x2 + m21 * y2 + m22 * z2);

            return ret;
        }
    }
}