using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using Cinch;
using Color = System.Drawing.Color;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class ColorMapViewModel : ViewModelBase
    {
        #region public properties

        static readonly PropertyChangedEventArgs ColorsChangedEventArgs = ObservableHelper.CreateArgs<ColorMapViewModel>(x => x.Colors);
        static readonly PropertyChangedEventArgs MaxValueChangedEventArgs = ObservableHelper.CreateArgs<ColorMapViewModel>(x => x.MaxValue);
        static readonly PropertyChangedEventArgs MinValueChangedEventArgs = ObservableHelper.CreateArgs<ColorMapViewModel>(x => x.MinValue);
        static readonly PropertyChangedEventArgs CurMaxValueChangedEventArgs = ObservableHelper.CreateArgs<ColorMapViewModel>(x => x.CurMaxValue);
        static readonly PropertyChangedEventArgs CurMinValueChangedEventArgs = ObservableHelper.CreateArgs<ColorMapViewModel>(x => x.CurMinValue);
        static readonly PropertyChangedEventArgs ColorBitmapChangedEventArgs = ObservableHelper.CreateArgs<ColorMapViewModel>(x => x.ColorBitmap);

        public List<Color> Colors
        {
            get { return _colors; }
            set
            {
                if (_colors == value) return;

                _colors = value;
                _firstColor = _colors.First();
                _lastColor = _colors.Last();
                _colorCount = _colors.Count();
                NotifyPropertyChanged(ColorsChangedEventArgs);
                RenderColorBitmap();
            }
        }

        public double MaxValue
        {
            get { return _maxValue; }
            set
            {
                if (_maxValue == value) return;
                _maxValue = value;
                NotifyPropertyChanged(MaxValueChangedEventArgs);
                CurMaxValue = _maxValue;
            }
        }

        public double MinValue
        {
            get { return _minValue; }
            set
            {
                if (_minValue == value) return;
                _minValue = value;
                NotifyPropertyChanged(MinValueChangedEventArgs);
                CurMinValue = _minValue;
            }
        }

        public double CurMaxValue
        {
            get { return _curMaxValue; }
            set
            {
                if (_curMaxValue == value) return;
                _curMaxValue = value;
                _curRange = _curMaxValue - _curMinValue;
                NotifyPropertyChanged(CurMaxValueChangedEventArgs);
                Debug.WriteLine("New CurMax equals" + _curMaxValue);
            }
        }

        public double CurMinValue
        {
            get { return _curMinValue; }
            set
            {
                if (_curMinValue == value) return;
                _curMinValue = value;
                _curRange = _curMaxValue - _curMinValue;

                NotifyPropertyChanged(CurMinValueChangedEventArgs);
                Debug.WriteLine("New CurMin equals" + _curMinValue);
            }
        }

        public void Reverse()
        {
            _colors.Reverse();
            _firstColor = _colors.First();
            _lastColor = _colors.Last();
            _colorCount = _colors.Count();
            NotifyPropertyChanged(ColorsChangedEventArgs);
            RenderColorBitmap();
        }

        #endregion

        public WriteableBitmap ColorBitmap { get; private set; }

        public void ResetMinMax()
        {
            CurMaxValue = MaxValue;
            CurMinValue = MinValue;
        }

        public Color Lookup(double value)
        {
            if (value >= _curMaxValue) return _lastColor;
            if (value <= _curMinValue) return _firstColor;
            if (_curRange == 0.0) _curRange = _curMaxValue - _curMinValue;

            if (_curRange != 0.0)
            {
                double fraction = (value - _curMinValue)/_curRange;
                var index = (int) (fraction*_colorCount);
                return Colors[index];
            }
            return Color.Black;
        }

        public void RenderColorBitmap()
        {
            if (ColorBitmap == null) ColorBitmap = new WriteableBitmap(1, Colors.Count, 96, 96, PixelFormats.Bgr32, null);

            ColorBitmap.Lock();
            unsafe
            {
                var curOffset = (int) ColorBitmap.BackBuffer;
                for (var y = 0; y < Colors.Count; y++)
                {
                    // Draw from the bottom up, which matches the default render order.  This may change as the UI becomes
                    // more fully implemented, especially if we need to flip the canvas and render from the top.  Time will tell.
                    *((int*) curOffset) = Colors[y].ToArgb();
                    curOffset += sizeof (Int32);
                }
            }
            ColorBitmap.AddDirtyRect(new Int32Rect(0, 0, 1, Colors.Count));
            ColorBitmap.Unlock();
            NotifyPropertyChanged(ColorBitmapChangedEventArgs);
        }

        #region public static default ColorMapViewModel creation method

        public static ColorMapViewModel Default
        {
            get
            {
                return new ColorMapViewModel
                       {
                           MaxValue = 1.0,
                           MinValue = 0.0,
                           CurMaxValue = 1.0,
                           CurMinValue = 0.0,
                           Colors = new List<Color>
                                    {
                                        Color.FromArgb(0, 0, 143),
                                        Color.FromArgb(0, 0, 147),
                                        Color.FromArgb(0, 0, 151),
                                        Color.FromArgb(0, 0, 155),
                                        Color.FromArgb(0, 0, 159),
                                        Color.FromArgb(0, 0, 163),
                                        Color.FromArgb(0, 0, 167),
                                        Color.FromArgb(0, 0, 171),
                                        Color.FromArgb(0, 0, 175),
                                        Color.FromArgb(0, 0, 179),
                                        Color.FromArgb(0, 0, 183),
                                        Color.FromArgb(0, 0, 187),
                                        Color.FromArgb(0, 0, 191),
                                        Color.FromArgb(0, 0, 195),
                                        Color.FromArgb(0, 0, 199),
                                        Color.FromArgb(0, 0, 203),
                                        Color.FromArgb(0, 0, 207),
                                        Color.FromArgb(0, 0, 211),
                                        Color.FromArgb(0, 0, 215),
                                        Color.FromArgb(0, 0, 219),
                                        Color.FromArgb(0, 0, 223),
                                        Color.FromArgb(0, 0, 227),
                                        Color.FromArgb(0, 0, 231),
                                        Color.FromArgb(0, 0, 235),
                                        Color.FromArgb(0, 0, 239),
                                        Color.FromArgb(0, 0, 243),
                                        Color.FromArgb(0, 0, 247),
                                        Color.FromArgb(0, 0, 251),
                                        Color.FromArgb(0, 0, 255),
                                        Color.FromArgb(0, 3, 255),
                                        Color.FromArgb(0, 7, 255),
                                        Color.FromArgb(0, 11, 255),
                                        Color.FromArgb(0, 15, 255),
                                        Color.FromArgb(0, 19, 255),
                                        Color.FromArgb(0, 23, 255),
                                        Color.FromArgb(0, 27, 255),
                                        Color.FromArgb(0, 31, 255),
                                        Color.FromArgb(0, 35, 255),
                                        Color.FromArgb(0, 39, 255),
                                        Color.FromArgb(0, 43, 255),
                                        Color.FromArgb(0, 47, 255),
                                        Color.FromArgb(0, 51, 255),
                                        Color.FromArgb(0, 55, 255),
                                        Color.FromArgb(0, 59, 255),
                                        Color.FromArgb(0, 63, 255),
                                        Color.FromArgb(0, 67, 255),
                                        Color.FromArgb(0, 71, 255),
                                        Color.FromArgb(0, 75, 255),
                                        Color.FromArgb(0, 79, 255),
                                        Color.FromArgb(0, 83, 255),
                                        Color.FromArgb(0, 87, 255),
                                        Color.FromArgb(0, 91, 255),
                                        Color.FromArgb(0, 95, 255),
                                        Color.FromArgb(0, 99, 255),
                                        Color.FromArgb(0, 103, 255),
                                        Color.FromArgb(0, 107, 255),
                                        Color.FromArgb(0, 111, 255),
                                        Color.FromArgb(0, 115, 255),
                                        Color.FromArgb(0, 119, 255),
                                        Color.FromArgb(0, 123, 255),
                                        Color.FromArgb(0, 127, 255),
                                        Color.FromArgb(0, 131, 255),
                                        Color.FromArgb(0, 135, 255),
                                        Color.FromArgb(0, 139, 255),
                                        Color.FromArgb(0, 143, 255),
                                        Color.FromArgb(0, 147, 255),
                                        Color.FromArgb(0, 151, 255),
                                        Color.FromArgb(0, 155, 255),
                                        Color.FromArgb(0, 159, 255),
                                        Color.FromArgb(0, 163, 255),
                                        Color.FromArgb(0, 167, 255),
                                        Color.FromArgb(0, 171, 255),
                                        Color.FromArgb(0, 175, 255),
                                        Color.FromArgb(0, 179, 255),
                                        Color.FromArgb(0, 183, 255),
                                        Color.FromArgb(0, 187, 255),
                                        Color.FromArgb(0, 191, 255),
                                        Color.FromArgb(0, 195, 255),
                                        Color.FromArgb(0, 199, 255),
                                        Color.FromArgb(0, 203, 255),
                                        Color.FromArgb(0, 207, 255),
                                        Color.FromArgb(0, 211, 255),
                                        Color.FromArgb(0, 215, 255),
                                        Color.FromArgb(0, 219, 255),
                                        Color.FromArgb(0, 223, 255),
                                        Color.FromArgb(0, 227, 255),
                                        Color.FromArgb(0, 231, 255),
                                        Color.FromArgb(0, 235, 255),
                                        Color.FromArgb(0, 239, 255),
                                        Color.FromArgb(0, 243, 255),
                                        Color.FromArgb(0, 247, 255),
                                        Color.FromArgb(0, 251, 255),
                                        Color.FromArgb(0, 255, 255),
                                        Color.FromArgb(3, 255, 251),
                                        Color.FromArgb(7, 255, 247),
                                        Color.FromArgb(11, 255, 243),
                                        Color.FromArgb(15, 255, 239),
                                        Color.FromArgb(19, 255, 235),
                                        Color.FromArgb(23, 255, 231),
                                        Color.FromArgb(27, 255, 227),
                                        Color.FromArgb(31, 255, 223),
                                        Color.FromArgb(35, 255, 219),
                                        Color.FromArgb(39, 255, 215),
                                        Color.FromArgb(43, 255, 211),
                                        Color.FromArgb(47, 255, 207),
                                        Color.FromArgb(51, 255, 203),
                                        Color.FromArgb(55, 255, 199),
                                        Color.FromArgb(59, 255, 195),
                                        Color.FromArgb(63, 255, 191),
                                        Color.FromArgb(67, 255, 187),
                                        Color.FromArgb(71, 255, 183),
                                        Color.FromArgb(75, 255, 179),
                                        Color.FromArgb(79, 255, 175),
                                        Color.FromArgb(83, 255, 171),
                                        Color.FromArgb(87, 255, 167),
                                        Color.FromArgb(91, 255, 163),
                                        Color.FromArgb(95, 255, 159),
                                        Color.FromArgb(99, 255, 155),
                                        Color.FromArgb(103, 255, 151),
                                        Color.FromArgb(107, 255, 147),
                                        Color.FromArgb(111, 255, 143),
                                        Color.FromArgb(115, 255, 139),
                                        Color.FromArgb(119, 255, 135),
                                        Color.FromArgb(123, 255, 131),
                                        Color.FromArgb(127, 255, 127),
                                        Color.FromArgb(131, 255, 123),
                                        Color.FromArgb(135, 255, 119),
                                        Color.FromArgb(139, 255, 115),
                                        Color.FromArgb(143, 255, 111),
                                        Color.FromArgb(147, 255, 107),
                                        Color.FromArgb(151, 255, 103),
                                        Color.FromArgb(155, 255, 99),
                                        Color.FromArgb(159, 255, 95),
                                        Color.FromArgb(163, 255, 91),
                                        Color.FromArgb(167, 255, 87),
                                        Color.FromArgb(171, 255, 83),
                                        Color.FromArgb(175, 255, 79),
                                        Color.FromArgb(179, 255, 75),
                                        Color.FromArgb(183, 255, 71),
                                        Color.FromArgb(187, 255, 67),
                                        Color.FromArgb(191, 255, 63),
                                        Color.FromArgb(195, 255, 59),
                                        Color.FromArgb(199, 255, 55),
                                        Color.FromArgb(203, 255, 51),
                                        Color.FromArgb(207, 255, 47),
                                        Color.FromArgb(211, 255, 43),
                                        Color.FromArgb(215, 255, 39),
                                        Color.FromArgb(219, 255, 35),
                                        Color.FromArgb(223, 255, 31),
                                        Color.FromArgb(227, 255, 27),
                                        Color.FromArgb(231, 255, 23),
                                        Color.FromArgb(235, 255, 19),
                                        Color.FromArgb(239, 255, 15),
                                        Color.FromArgb(243, 255, 11),
                                        Color.FromArgb(247, 255, 7),
                                        Color.FromArgb(251, 255, 3),
                                        Color.FromArgb(255, 255, 0),
                                        Color.FromArgb(255, 251, 0),
                                        Color.FromArgb(255, 247, 0),
                                        Color.FromArgb(255, 243, 0),
                                        Color.FromArgb(255, 239, 0),
                                        Color.FromArgb(255, 235, 0),
                                        Color.FromArgb(255, 231, 0),
                                        Color.FromArgb(255, 227, 0),
                                        Color.FromArgb(255, 223, 0),
                                        Color.FromArgb(255, 219, 0),
                                        Color.FromArgb(255, 215, 0),
                                        Color.FromArgb(255, 211, 0),
                                        Color.FromArgb(255, 207, 0),
                                        Color.FromArgb(255, 203, 0),
                                        Color.FromArgb(255, 199, 0),
                                        Color.FromArgb(255, 195, 0),
                                        Color.FromArgb(255, 191, 0),
                                        Color.FromArgb(255, 187, 0),
                                        Color.FromArgb(255, 183, 0),
                                        Color.FromArgb(255, 179, 0),
                                        Color.FromArgb(255, 175, 0),
                                        Color.FromArgb(255, 171, 0),
                                        Color.FromArgb(255, 167, 0),
                                        Color.FromArgb(255, 163, 0),
                                        Color.FromArgb(255, 159, 0),
                                        Color.FromArgb(255, 155, 0),
                                        Color.FromArgb(255, 151, 0),
                                        Color.FromArgb(255, 147, 0),
                                        Color.FromArgb(255, 143, 0),
                                        Color.FromArgb(255, 139, 0),
                                        Color.FromArgb(255, 135, 0),
                                        Color.FromArgb(255, 131, 0),
                                        Color.FromArgb(255, 127, 0),
                                        Color.FromArgb(255, 123, 0),
                                        Color.FromArgb(255, 119, 0),
                                        Color.FromArgb(255, 115, 0),
                                        Color.FromArgb(255, 111, 0),
                                        Color.FromArgb(255, 107, 0),
                                        Color.FromArgb(255, 103, 0),
                                        Color.FromArgb(255, 99, 0),
                                        Color.FromArgb(255, 95, 0),
                                        Color.FromArgb(255, 91, 0),
                                        Color.FromArgb(255, 87, 0),
                                        Color.FromArgb(255, 83, 0),
                                        Color.FromArgb(255, 79, 0),
                                        Color.FromArgb(255, 75, 0),
                                        Color.FromArgb(255, 71, 0),
                                        Color.FromArgb(255, 67, 0),
                                        Color.FromArgb(255, 63, 0),
                                        Color.FromArgb(255, 59, 0),
                                        Color.FromArgb(255, 55, 0),
                                        Color.FromArgb(255, 51, 0),
                                        Color.FromArgb(255, 47, 0),
                                        Color.FromArgb(255, 43, 0),
                                        Color.FromArgb(255, 39, 0),
                                        Color.FromArgb(255, 35, 0),
                                        Color.FromArgb(255, 31, 0),
                                        Color.FromArgb(255, 27, 0),
                                        Color.FromArgb(255, 23, 0),
                                        Color.FromArgb(255, 19, 0),
                                        Color.FromArgb(255, 15, 0),
                                        Color.FromArgb(255, 11, 0),
                                        Color.FromArgb(255, 7, 0),
                                        Color.FromArgb(255, 3, 0),
                                        Color.FromArgb(255, 0, 0),
                                        Color.FromArgb(251, 0, 0),
                                        Color.FromArgb(247, 0, 0),
                                        Color.FromArgb(243, 0, 0),
                                        Color.FromArgb(239, 0, 0),
                                        Color.FromArgb(235, 0, 0),
                                        Color.FromArgb(231, 0, 0),
                                        Color.FromArgb(227, 0, 0),
                                        Color.FromArgb(223, 0, 0),
                                        Color.FromArgb(219, 0, 0),
                                        Color.FromArgb(215, 0, 0),
                                        Color.FromArgb(211, 0, 0),
                                        Color.FromArgb(207, 0, 0),
                                        Color.FromArgb(203, 0, 0),
                                        Color.FromArgb(199, 0, 0),
                                        Color.FromArgb(195, 0, 0),
                                        Color.FromArgb(191, 0, 0),
                                        Color.FromArgb(187, 0, 0),
                                        Color.FromArgb(183, 0, 0),
                                        Color.FromArgb(179, 0, 0),
                                        Color.FromArgb(175, 0, 0),
                                        Color.FromArgb(171, 0, 0),
                                        Color.FromArgb(167, 0, 0),
                                        Color.FromArgb(163, 0, 0),
                                        Color.FromArgb(159, 0, 0),
                                        Color.FromArgb(155, 0, 0),
                                        Color.FromArgb(151, 0, 0),
                                        Color.FromArgb(147, 0, 0),
                                        Color.FromArgb(143, 0, 0),
                                        Color.FromArgb(139, 0, 0),
                                        Color.FromArgb(135, 0, 0),
                                        Color.FromArgb(131, 0, 0),
                                        Color.FromArgb(127, 0, 0),
                                    },
                       };
            }
        }

        #endregion

        #region private fields

        int _colorCount;
        List<Color> _colors;

        double _curMaxValue,
               _curMinValue,
               _curRange;

        Color _firstColor,
              _lastColor;

        double _maxValue,
               _minValue;

        #endregion
    }
}