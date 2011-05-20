using System;

namespace HRC.Navigation
{
    public class Rotation
    {
        readonly Geo _g;
        readonly double _m00;
        readonly double _m11;
        readonly double _m22;
        readonly double _m01;
        readonly double _m10;
        readonly double _m12;
        readonly double _m21;
        readonly double _m02;
        readonly double _m20;

        public Rotation(Geo g, double angle)
        {
            _g = g;
            var x = _g.X;
            var y = _g.Y;
            var z = _g.Z;
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
        }

        public Geo Rotate(Geo v)
        {
            return new Geo(_m00 * v.X + _m01 * v.Y + _m02 * v.Z, _m10 * v.X + _m11 * v.Y + _m12 * v.Z, _m20 * v.X + _m21 * v.Y + _m22 * v.Z);
        }

        /// <summary>
        /// Static method that does what creating a Rotation object and calling
        /// rotate() on it does. Rotates vector v2 an angle counter clockwise about
        /// the Geo, v1.
        /// </summary>
        /// <param name="v1"></param>
        /// <param name="angle"></param>
        /// <param name="v2"></param>
        /// <returns></returns>
        public static Geo Rotate(Geo v1, double angle, Geo v2)
        {
            return new Rotation(v1, angle).Rotate(v2);
        }
    }
}