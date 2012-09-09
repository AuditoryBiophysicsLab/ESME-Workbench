using System;
using System.Windows;
using System.Windows.Media;
using ESME.NEMO;

namespace DavesWPFTester.Transforms
{
    /// <summary>
    /// Transforms the X and/or Y coordinate to Math.Log(X, XBase) and/or Math.Log(Y, YBase)
    /// If XBase or YBase is NaN, the corresponding coordinate is unchanged
    /// </summary>
    public class LogTransform : GeneralTransform
    {
        public LogTransform() {}

        public LogTransform(double xBase = double.NaN, double yBase = double.NaN)
        {
            if (double.IsInfinity(xBase) || xBase <= 0 || xBase == 1) throw new ParameterOutOfRangeException("xBase must be NaN (no change) or a positive number (excluding 1)");
            if (double.IsInfinity(yBase) || yBase <= 0 || yBase == 1) throw new ParameterOutOfRangeException("yBase must be NaN (no change) or a positive number (excluding 1)");
            XBase = xBase;
            YBase = yBase;
        }

        public double XBase { get; set; }
        public double YBase { get; set; }

        protected override Freezable CreateInstanceCore() { return new LogTransform(); }
        public override bool TryTransform(Point inPoint, out Point result)
        {
            result = new Point();
            if (!double.IsNaN(XBase) && inPoint.X <= 0) return false;
            if (!double.IsNaN(YBase) && inPoint.Y <= 0) return false;
            result.X = double.IsNaN(XBase) ? inPoint.X : Math.Log(inPoint.X, XBase);
            result.Y = double.IsNaN(YBase) ? inPoint.Y : Math.Log(inPoint.Y, YBase);
            return true;
        }
        public override Rect TransformBounds(Rect rect)
        {
            Point out1, out2;
            if (!TryTransform(rect.TopLeft, out out1)) throw new InvalidOperationException("LogTransform is illegal on a rect with negative coordinates");
            if (!TryTransform(rect.BottomRight, out out2)) throw new InvalidOperationException("LogTransform is illegal on a rect with negative coordinates");
            return new Rect(out1, out2);
        }

        public override GeneralTransform Inverse { get { return new PowTransform(XBase, YBase); } }
    }
}
