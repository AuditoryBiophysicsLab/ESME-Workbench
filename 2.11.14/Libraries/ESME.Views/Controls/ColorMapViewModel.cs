using System;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using HRC.Aspects;
using HRC.Plotting;
using HRC.Utility;
using HRC.ViewModels;

namespace ESME.Views.Controls
{
    public class ColorMapViewModel : ViewModelBase
    {
        public ColorMapViewModel() { FullRange.RangeChanged += (s, e) => CurrentRange.Update(FullRange); }
        #region public properties

        public ObservableList<Color> Colors
        {
            get { return _colors; }
            set
            {
                _colors = value;
                _firstColor = _colors.First();
                _lastColor = _colors.Last();
                _colorCount = _colors.Count();
                RenderColorBitmap();
            }
        }
        ObservableList<Color> _colors;

        [Initialize] public Range FullRange { get; set; }
        [Initialize] public Range CurrentRange { get; set; }
        [Initialize] public Range StatisticalRange { get; set; }

        public void Reverse()
        {
            Colors.Reverse();
            _firstColor = _colors.First();
            _lastColor = _colors.Last();
            _colorCount = _colors.Count();
            RenderColorBitmap();
        }

        #endregion

        public WriteableBitmap ColorBitmap { get; private set; }

        public Color Lookup(double value)
        {
            lock (this)
            {
                var max = CurrentRange.Max;
                var min = CurrentRange.Min;
                var range = max - min;
                if (value >= max) return _firstColor;
                if (value <= min) return _lastColor;

                if (CurrentRange.Value > 0)
                {
                    var fraction = 1.0 - (value - min) / range;
                    var index = (int)(fraction * _colorCount);
                    return Colors[index];
                }
            }
            return System.Windows.Media.Colors.Black;
        }

        public void RenderColorBitmap()
        {
            var colorBitmap = ColorBitmap ?? new WriteableBitmap(1, Colors.Count, 96, 96, PixelFormats.Bgr32, null);

            colorBitmap.Lock();
            unsafe
            {
                var curOffset = (int)colorBitmap.BackBuffer;
                foreach (var curColor in Colors) 
                {
                    *((int*)curOffset) = ((curColor.A << 24) | (curColor.R << 16) | (curColor.G << 8) | (curColor.B));
                    curOffset += sizeof (Int32);
                }
            }
            colorBitmap.AddDirtyRect(new Int32Rect(0, 0, 1, Colors.Count));
            colorBitmap.Unlock();
            ColorBitmap = colorBitmap;
        }

        #region public static default ColorMapViewModel creation method

        public static ColorMapViewModel Default
        {
            get
            {
                return new ColorMapViewModel
                       {
                           FullRange = new Range(0, 200),
                           CurrentRange = new Range(50, 150),
                           StatisticalRange = new Range(75, 125),
                           //MaxValue = 1.0,
                           //MinValue = 0.0,
                           //CurMaxValue = 1.0,
                           //CurMinValue = 0.0,
                           Colors = new ObservableList<Color>
                                    {
                                        Color.FromArgb(255, 0, 0, 143),
                                        Color.FromArgb(255, 0, 0, 147),
                                        Color.FromArgb(255, 0, 0, 151),
                                        Color.FromArgb(255, 0, 0, 155),
                                        Color.FromArgb(255, 0, 0, 159),
                                        Color.FromArgb(255, 0, 0, 163),
                                        Color.FromArgb(255, 0, 0, 167),
                                        Color.FromArgb(255, 0, 0, 171),
                                        Color.FromArgb(255, 0, 0, 175),
                                        Color.FromArgb(255, 0, 0, 179),
                                        Color.FromArgb(255, 0, 0, 183),
                                        Color.FromArgb(255, 0, 0, 187),
                                        Color.FromArgb(255, 0, 0, 191),
                                        Color.FromArgb(255, 0, 0, 195),
                                        Color.FromArgb(255, 0, 0, 199),
                                        Color.FromArgb(255, 0, 0, 203),
                                        Color.FromArgb(255, 0, 0, 207),
                                        Color.FromArgb(255, 0, 0, 211),
                                        Color.FromArgb(255, 0, 0, 215),
                                        Color.FromArgb(255, 0, 0, 219),
                                        Color.FromArgb(255, 0, 0, 223),
                                        Color.FromArgb(255, 0, 0, 227),
                                        Color.FromArgb(255, 0, 0, 231),
                                        Color.FromArgb(255, 0, 0, 235),
                                        Color.FromArgb(255, 0, 0, 239),
                                        Color.FromArgb(255, 0, 0, 243),
                                        Color.FromArgb(255, 0, 0, 247),
                                        Color.FromArgb(255, 0, 0, 251),
                                        Color.FromArgb(255, 0, 0, 255),
                                        Color.FromArgb(255, 0, 3, 255),
                                        Color.FromArgb(255, 0, 7, 255),
                                        Color.FromArgb(255, 0, 11, 255),
                                        Color.FromArgb(255, 0, 15, 255),
                                        Color.FromArgb(255, 0, 19, 255),
                                        Color.FromArgb(255, 0, 23, 255),
                                        Color.FromArgb(255, 0, 27, 255),
                                        Color.FromArgb(255, 0, 31, 255),
                                        Color.FromArgb(255, 0, 35, 255),
                                        Color.FromArgb(255, 0, 39, 255),
                                        Color.FromArgb(255, 0, 43, 255),
                                        Color.FromArgb(255, 0, 47, 255),
                                        Color.FromArgb(255, 0, 51, 255),
                                        Color.FromArgb(255, 0, 55, 255),
                                        Color.FromArgb(255, 0, 59, 255),
                                        Color.FromArgb(255, 0, 63, 255),
                                        Color.FromArgb(255, 0, 67, 255),
                                        Color.FromArgb(255, 0, 71, 255),
                                        Color.FromArgb(255, 0, 75, 255),
                                        Color.FromArgb(255, 0, 79, 255),
                                        Color.FromArgb(255, 0, 83, 255),
                                        Color.FromArgb(255, 0, 87, 255),
                                        Color.FromArgb(255, 0, 91, 255),
                                        Color.FromArgb(255, 0, 95, 255),
                                        Color.FromArgb(255, 0, 99, 255),
                                        Color.FromArgb(255, 0, 103, 255),
                                        Color.FromArgb(255, 0, 107, 255),
                                        Color.FromArgb(255, 0, 111, 255),
                                        Color.FromArgb(255, 0, 115, 255),
                                        Color.FromArgb(255, 0, 119, 255),
                                        Color.FromArgb(255, 0, 123, 255),
                                        Color.FromArgb(255, 0, 127, 255),
                                        Color.FromArgb(255, 0, 131, 255),
                                        Color.FromArgb(255, 0, 135, 255),
                                        Color.FromArgb(255, 0, 139, 255),
                                        Color.FromArgb(255, 0, 143, 255),
                                        Color.FromArgb(255, 0, 147, 255),
                                        Color.FromArgb(255, 0, 151, 255),
                                        Color.FromArgb(255, 0, 155, 255),
                                        Color.FromArgb(255, 0, 159, 255),
                                        Color.FromArgb(255, 0, 163, 255),
                                        Color.FromArgb(255, 0, 167, 255),
                                        Color.FromArgb(255, 0, 171, 255),
                                        Color.FromArgb(255, 0, 175, 255),
                                        Color.FromArgb(255, 0, 179, 255),
                                        Color.FromArgb(255, 0, 183, 255),
                                        Color.FromArgb(255, 0, 187, 255),
                                        Color.FromArgb(255, 0, 191, 255),
                                        Color.FromArgb(255, 0, 195, 255),
                                        Color.FromArgb(255, 0, 199, 255),
                                        Color.FromArgb(255, 0, 203, 255),
                                        Color.FromArgb(255, 0, 207, 255),
                                        Color.FromArgb(255, 0, 211, 255),
                                        Color.FromArgb(255, 0, 215, 255),
                                        Color.FromArgb(255, 0, 219, 255),
                                        Color.FromArgb(255, 0, 223, 255),
                                        Color.FromArgb(255, 0, 227, 255),
                                        Color.FromArgb(255, 0, 231, 255),
                                        Color.FromArgb(255, 0, 235, 255),
                                        Color.FromArgb(255, 0, 239, 255),
                                        Color.FromArgb(255, 0, 243, 255),
                                        Color.FromArgb(255, 0, 247, 255),
                                        Color.FromArgb(255, 0, 251, 255),
                                        Color.FromArgb(255, 0, 255, 255),
                                        Color.FromArgb(255, 3, 255, 251),
                                        Color.FromArgb(255, 7, 255, 247),
                                        Color.FromArgb(255, 11, 255, 243),
                                        Color.FromArgb(255, 15, 255, 239),
                                        Color.FromArgb(255, 19, 255, 235),
                                        Color.FromArgb(255, 23, 255, 231),
                                        Color.FromArgb(255, 27, 255, 227),
                                        Color.FromArgb(255, 31, 255, 223),
                                        Color.FromArgb(255, 35, 255, 219),
                                        Color.FromArgb(255, 39, 255, 215),
                                        Color.FromArgb(255, 43, 255, 211),
                                        Color.FromArgb(255, 47, 255, 207),
                                        Color.FromArgb(255, 51, 255, 203),
                                        Color.FromArgb(255, 55, 255, 199),
                                        Color.FromArgb(255, 59, 255, 195),
                                        Color.FromArgb(255, 63, 255, 191),
                                        Color.FromArgb(255, 67, 255, 187),
                                        Color.FromArgb(255, 71, 255, 183),
                                        Color.FromArgb(255, 75, 255, 179),
                                        Color.FromArgb(255, 79, 255, 175),
                                        Color.FromArgb(255, 83, 255, 171),
                                        Color.FromArgb(255, 87, 255, 167),
                                        Color.FromArgb(255, 91, 255, 163),
                                        Color.FromArgb(255, 95, 255, 159),
                                        Color.FromArgb(255, 99, 255, 155),
                                        Color.FromArgb(255, 103, 255, 151),
                                        Color.FromArgb(255, 107, 255, 147),
                                        Color.FromArgb(255, 111, 255, 143),
                                        Color.FromArgb(255, 115, 255, 139),
                                        Color.FromArgb(255, 119, 255, 135),
                                        Color.FromArgb(255, 123, 255, 131),
                                        Color.FromArgb(255, 127, 255, 127),
                                        Color.FromArgb(255, 131, 255, 123),
                                        Color.FromArgb(255, 135, 255, 119),
                                        Color.FromArgb(255, 139, 255, 115),
                                        Color.FromArgb(255, 143, 255, 111),
                                        Color.FromArgb(255, 147, 255, 107),
                                        Color.FromArgb(255, 151, 255, 103),
                                        Color.FromArgb(255, 155, 255, 99),
                                        Color.FromArgb(255, 159, 255, 95),
                                        Color.FromArgb(255, 163, 255, 91),
                                        Color.FromArgb(255, 167, 255, 87),
                                        Color.FromArgb(255, 171, 255, 83),
                                        Color.FromArgb(255, 175, 255, 79),
                                        Color.FromArgb(255, 179, 255, 75),
                                        Color.FromArgb(255, 183, 255, 71),
                                        Color.FromArgb(255, 187, 255, 67),
                                        Color.FromArgb(255, 191, 255, 63),
                                        Color.FromArgb(255, 195, 255, 59),
                                        Color.FromArgb(255, 199, 255, 55),
                                        Color.FromArgb(255, 203, 255, 51),
                                        Color.FromArgb(255, 207, 255, 47),
                                        Color.FromArgb(255, 211, 255, 43),
                                        Color.FromArgb(255, 215, 255, 39),
                                        Color.FromArgb(255, 219, 255, 35),
                                        Color.FromArgb(255, 223, 255, 31),
                                        Color.FromArgb(255, 227, 255, 27),
                                        Color.FromArgb(255, 231, 255, 23),
                                        Color.FromArgb(255, 235, 255, 19),
                                        Color.FromArgb(255, 239, 255, 15),
                                        Color.FromArgb(255, 243, 255, 11),
                                        Color.FromArgb(255, 247, 255, 7),
                                        Color.FromArgb(255, 251, 255, 3),
                                        Color.FromArgb(255, 255, 255, 0),
                                        Color.FromArgb(255, 255, 251, 0),
                                        Color.FromArgb(255, 255, 247, 0),
                                        Color.FromArgb(255, 255, 243, 0),
                                        Color.FromArgb(255, 255, 239, 0),
                                        Color.FromArgb(255, 255, 235, 0),
                                        Color.FromArgb(255, 255, 231, 0),
                                        Color.FromArgb(255, 255, 227, 0),
                                        Color.FromArgb(255, 255, 223, 0),
                                        Color.FromArgb(255, 255, 219, 0),
                                        Color.FromArgb(255, 255, 215, 0),
                                        Color.FromArgb(255, 255, 211, 0),
                                        Color.FromArgb(255, 255, 207, 0),
                                        Color.FromArgb(255, 255, 203, 0),
                                        Color.FromArgb(255, 255, 199, 0),
                                        Color.FromArgb(255, 255, 195, 0),
                                        Color.FromArgb(255, 255, 191, 0),
                                        Color.FromArgb(255, 255, 187, 0),
                                        Color.FromArgb(255, 255, 183, 0),
                                        Color.FromArgb(255, 255, 179, 0),
                                        Color.FromArgb(255, 255, 175, 0),
                                        Color.FromArgb(255, 255, 171, 0),
                                        Color.FromArgb(255, 255, 167, 0),
                                        Color.FromArgb(255, 255, 163, 0),
                                        Color.FromArgb(255, 255, 159, 0),
                                        Color.FromArgb(255, 255, 155, 0),
                                        Color.FromArgb(255, 255, 151, 0),
                                        Color.FromArgb(255, 255, 147, 0),
                                        Color.FromArgb(255, 255, 143, 0),
                                        Color.FromArgb(255, 255, 139, 0),
                                        Color.FromArgb(255, 255, 135, 0),
                                        Color.FromArgb(255, 255, 131, 0),
                                        Color.FromArgb(255, 255, 127, 0),
                                        Color.FromArgb(255, 255, 123, 0),
                                        Color.FromArgb(255, 255, 119, 0),
                                        Color.FromArgb(255, 255, 115, 0),
                                        Color.FromArgb(255, 255, 111, 0),
                                        Color.FromArgb(255, 255, 107, 0),
                                        Color.FromArgb(255, 255, 103, 0),
                                        Color.FromArgb(255, 255, 99, 0),
                                        Color.FromArgb(255, 255, 95, 0),
                                        Color.FromArgb(255, 255, 91, 0),
                                        Color.FromArgb(255, 255, 87, 0),
                                        Color.FromArgb(255, 255, 83, 0),
                                        Color.FromArgb(255, 255, 79, 0),
                                        Color.FromArgb(255, 255, 75, 0),
                                        Color.FromArgb(255, 255, 71, 0),
                                        Color.FromArgb(255, 255, 67, 0),
                                        Color.FromArgb(255, 255, 63, 0),
                                        Color.FromArgb(255, 255, 59, 0),
                                        Color.FromArgb(255, 255, 55, 0),
                                        Color.FromArgb(255, 255, 51, 0),
                                        Color.FromArgb(255, 255, 47, 0),
                                        Color.FromArgb(255, 255, 43, 0),
                                        Color.FromArgb(255, 255, 39, 0),
                                        Color.FromArgb(255, 255, 35, 0),
                                        Color.FromArgb(255, 255, 31, 0),
                                        Color.FromArgb(255, 255, 27, 0),
                                        Color.FromArgb(255, 255, 23, 0),
                                        Color.FromArgb(255, 255, 19, 0),
                                        Color.FromArgb(255, 255, 15, 0),
                                        Color.FromArgb(255, 255, 11, 0),
                                        Color.FromArgb(255, 255, 7, 0),
                                        Color.FromArgb(255, 255, 3, 0),
                                        Color.FromArgb(255, 255, 0, 0),
                                        Color.FromArgb(255, 251, 0, 0),
                                        Color.FromArgb(255, 247, 0, 0),
                                        Color.FromArgb(255, 243, 0, 0),
                                        Color.FromArgb(255, 239, 0, 0),
                                        Color.FromArgb(255, 235, 0, 0),
                                        Color.FromArgb(255, 231, 0, 0),
                                        Color.FromArgb(255, 227, 0, 0),
                                        Color.FromArgb(255, 223, 0, 0),
                                        Color.FromArgb(255, 219, 0, 0),
                                        Color.FromArgb(255, 215, 0, 0),
                                        Color.FromArgb(255, 211, 0, 0),
                                        Color.FromArgb(255, 207, 0, 0),
                                        Color.FromArgb(255, 203, 0, 0),
                                        Color.FromArgb(255, 199, 0, 0),
                                        Color.FromArgb(255, 195, 0, 0),
                                        Color.FromArgb(255, 191, 0, 0),
                                        Color.FromArgb(255, 187, 0, 0),
                                        Color.FromArgb(255, 183, 0, 0),
                                        Color.FromArgb(255, 179, 0, 0),
                                        Color.FromArgb(255, 175, 0, 0),
                                        Color.FromArgb(255, 171, 0, 0),
                                        Color.FromArgb(255, 167, 0, 0),
                                        Color.FromArgb(255, 163, 0, 0),
                                        Color.FromArgb(255, 159, 0, 0),
                                        Color.FromArgb(255, 155, 0, 0),
                                        Color.FromArgb(255, 151, 0, 0),
                                        Color.FromArgb(255, 147, 0, 0),
                                        Color.FromArgb(255, 143, 0, 0),
                                        Color.FromArgb(255, 139, 0, 0),
                                        Color.FromArgb(255, 135, 0, 0),
                                        Color.FromArgb(255, 131, 0, 0),
                                        Color.FromArgb(255, 127, 0, 0),
                                    },
                       };
            }
        }

        #endregion

        #region private fields

        int _colorCount;

        //double _curMaxValue,
        //       _curMinValue,
        //       _curRange;

        Color _firstColor,
              _lastColor;

        //double _maxValue,
        //       _minValue;

        #endregion
    }
}