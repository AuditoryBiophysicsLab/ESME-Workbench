using System;

namespace HRC.Utility
{
    [AttributeUsage(AttributeTargets.Field, AllowMultiple = false)]
    public sealed class BitfieldLengthAttribute : Attribute
    {
        readonly uint _length;

        public BitfieldLengthAttribute(uint length) { _length = length; }

        public uint Length
        {
            get { return _length; }
        }
    }

    public static class PrimitiveConversion
    {
        public static long ToLong<T>(T t) where T : struct
        {
            long r = 0;
            var offset = 0;

            // For every field suitably attributed with a BitfieldLength
            foreach (var f in t.GetType().GetFields())
            {
                var attrs = f.GetCustomAttributes(typeof(BitfieldLengthAttribute), false);
                if (attrs.Length != 1) continue;
                var fieldLength = ((BitfieldLengthAttribute)attrs[0]).Length;

                // Calculate a bitmask of the desired length
                long mask = 0;
                for (var i = 0; i < fieldLength; i++) mask |= ((long)1 << i);

                r |= ((UInt32)f.GetValue(t) & mask) << offset;

                offset += (int)fieldLength;
            }

            return r;
        }

        public static int ToInt<T>(T t) where T : struct
        {
            var r = 0;
            var offset = 0;

            // For every field suitably attributed with a BitfieldLength
            foreach (var f in t.GetType().GetFields())
            {
                var attrs = f.GetCustomAttributes(typeof(BitfieldLengthAttribute), false);
                if (attrs.Length != 1) continue;
                var fieldLength = ((BitfieldLengthAttribute)attrs[0]).Length;

                // Calculate a bitmask of the desired length
                var mask = 0;
                for (var i = 0; i < fieldLength; i++) mask |= (1 << i);

                r |= ((Int32)f.GetValue(t) & mask) << offset;

                offset += (int)fieldLength;
            }

            return r;
        }
    }
}