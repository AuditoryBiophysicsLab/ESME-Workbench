using System;
using System.Collections.Generic;
using System.Windows.Media;

namespace HRC.WPF
{
    public class Palette
    {
        public static Palette CreateHSVPalette(int paletteLength)
        {
            var result = new Palette {PaletteColors = new Color[paletteLength]};
            var angleIncrement = 360.0 / ((double)paletteLength / 3);
            var index = 0;
            var saturation = 100;
            while (true)
            {
                var hueOffset = index * angleIncrement;
                result.PaletteColors[index++] = ColorExtensions.ColorFromHSV(hueOffset, saturation / 100.0, 1.0);
                if (index == paletteLength) break;
                result.PaletteColors[index++] = ColorExtensions.ColorFromHSV(120.0 + hueOffset, saturation / 100.0, 1.0);
                if (index == paletteLength) break;
                result.PaletteColors[index++] = ColorExtensions.ColorFromHSV(240.0 + hueOffset, saturation / 100.0, 1.0);
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

        public Color[] PaletteColors { get; private set; }
        public IEnumerable<Color> NextColor { get { return PaletteColors; } }

        public Color RandomColor
        {
            get
            {
                if (_random == null) _random = new Random();
                return PaletteColors[_random.Next(PaletteColors.Length - 1)];
            }
        }
        Random _random;
    }
}
