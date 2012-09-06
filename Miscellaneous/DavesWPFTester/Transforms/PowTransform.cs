using System;
using System.Windows;
using System.Windows.Media;
using ESME.NEMO;

namespace DavesWPFTester.Transforms
{
    /// <summary>
    /// Transforms the X and/or Y coordinate to XBase^x and/or XBase^y
    /// If XBase or YBase is NaN, the corresponding coordinate is unchanged
    /// </summary>
    public class PowTransform : GeneralTransform
    {
        public PowTransform() { }

        public PowTransform(double xBase = double.NaN, double yBase = double.NaN)
        {
            if (double.IsInfinity(xBase) || xBase <= 0) throw new ParameterOutOfRangeException("xBase must be NaN (no change) or a positive number");
            if (double.IsInfinity(yBase) || yBase <= 0) throw new ParameterOutOfRangeException("yBase must be NaN (no change) or a positive number");
            XBase = xBase;
            YBase = yBase;
        }

        public double XBase { get; set; }
        public double YBase { get; set; }

        protected override Freezable CreateInstanceCore() { return new PowTransform(); }
        public override bool TryTransform(Point inPoint, out Point result)
        {
            result = new Point
            {
                X = double.IsNaN(XBase) ? inPoint.X : Math.Pow(XBase, inPoint.X), 
                Y = double.IsNaN(YBase) ? inPoint.Y : Math.Pow(YBase, inPoint.Y),
            };
            return true;
        }
        public override Rect TransformBounds(Rect rect) { return new Rect(Transform(rect.TopLeft), Transform(rect.BottomRight)); }

        public override GeneralTransform Inverse { get { return new LogTransform(XBase, YBase); } }
    }
}