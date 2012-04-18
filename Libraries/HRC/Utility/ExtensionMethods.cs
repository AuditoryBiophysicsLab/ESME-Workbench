using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Windows.Media;

namespace HRC.Utility
{
    public static class ExtensionMethods
    {
        public static TimeSpan Modulo(this TimeSpan source, TimeSpan modulus)
        {
            var sourceMs = source.TotalMilliseconds;
            var modulusMs = modulus.TotalMilliseconds;
            var modulo = sourceMs - (modulusMs * (int) (sourceMs / modulusMs));
            return new TimeSpan(0, 0, 0, 0, (int) modulo);
        }

        public static double DivideBy(this TimeSpan numerator, TimeSpan denominator) { return numerator.TotalSeconds / denominator.TotalSeconds; }

        public static Color ColorFromHSV(double hue, double saturation, double value)
        {
            var hi = Convert.ToInt32(Math.Floor(hue / 60)) % 6;
            var f = hue / 60 - Math.Floor(hue / 60);

            value = value * 255;
            var v = Convert.ToByte(value);
            var p = Convert.ToByte(value * (1 - saturation));
            var q = Convert.ToByte(value * (1 - f * saturation));
            var t = Convert.ToByte(value * (1 - (1 - f) * saturation));

            switch (hi)
            {
                case 0:
                    return Color.FromArgb(255, v, t, p);
                case 1:
                    return Color.FromArgb(255, q, v, p);
                case 2:
                    return Color.FromArgb(255, p, v, t);
                case 3:
                    return Color.FromArgb(255, p, q, v);
                case 4:
                    return Color.FromArgb(255, t, p, v);
                default:
                    return Color.FromArgb(255, v, p, q);
            }
        }

        public static Color[] CreateHSVPalette(int paletteLength) 
        {
            var result = new Color[paletteLength];
            var angleIncrement = 360.0 / ((double)paletteLength / 3);
            var index = 0;
            var saturation = 100;
            while (true)
            {
                var hueOffset = index * angleIncrement;
                result[index++] = ColorFromHSV(hueOffset, saturation / 100.0, 1.0);
                if (index == paletteLength) break;
                result[index++] = ColorFromHSV(120.0 + hueOffset, saturation / 100.0, 1.0);
                if (index == paletteLength) break;
                result[index++] = ColorFromHSV(240.0 + hueOffset, saturation / 100.0, 1.0);
                if (index == paletteLength) break;
                switch (saturation)
                {
                    case 100:
                        saturation = 90;
                        break;
                    case 90:
                        saturation = 100;
                        break;
                }
            }

            return result;
        }

        public static bool IsValidFilename(this string fileName)
        {
            var charList = new List<char>();
            charList.AddRange(Path.GetInvalidFileNameChars());
            charList.AddRange(Path.GetInvalidPathChars());
            var invalidChars = charList.Distinct().ToList();
            var charsToCheck = fileName.ToCharArray();
            return !(from cur in charsToCheck from invalid in invalidChars where cur == invalid select cur).Any();
        }
    }
}